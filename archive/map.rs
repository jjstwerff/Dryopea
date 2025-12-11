use crate::data::Field;
use core::ops::{Index, IndexMut};
use eq_float::F64;
use serde_json::json;
use serde_json::value::Value;
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::{BufWriter, Result, Write};

/* Normal maps:
- 8 bits material (0: open)
- 8 bits 2:North-West wall material
- 8 bits 6:North-East wall
- 8 bits 10:East wall
- 16 bits height (65535: undefined)
- 11 bits item (0: open)
-  5 bits item rotation

Overland maps:
- 8 bits material
- 11 bits item (0: open, special structure)
- 16 bits height
- 13 bits water amount
- 3 bits water direction
*/

/* Restrictions on maps
Each height in a higher layer should higher than the value under it when there is a floor there.
Each adjacent hex is linked to the same layer. Only when there is a wall it is possible to switch layers.
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub pos: Position,
    pub height: u16,
    // hours of a clock * 1024
    pub rotation: u16,
    pub definition: u16,
}

#[derive(Copy, Hash, Default, Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub x: i32,
    // x data position = (x << 1) ignore the first bit, that is used for easier computations
    pub y: i32, // y data position
}

impl Position {
    #[must_use]
    pub fn new() -> Position {
        Position { x: 0, y: 0 }
    }

    // From data point
    #[must_use]
    pub fn data(x: i32, y: i32) -> Position {
        Position {
            x: x * 2 + i32::from(y % 2 != 0),
            y,
        }
    }

    /**
    # Panics
    If a position is invalid,
    */
    pub fn check(&self) {
        assert_eq!(self.x & 1, self.y & 1, "Incorrect position {self}");
    }

    #[must_use]
    pub fn point(&self) -> Point {
        Point {
            x: f64::from(self.x) * DX,
            y: f64::from(self.y) * DY,
            z: 0.,
        }
    }

    #[must_use]
    pub fn add(&self, p: &Position) -> Position {
        Position {
            x: self.x + p.x,
            y: self.y + p.y,
        }
    }

    #[must_use]
    pub fn sub(&self, p: &Position) -> Position {
        Position {
            x: self.x - p.x,
            y: self.y - p.y,
        }
    }

    #[must_use]
    pub fn rotate(&self, d: Direction) -> Point {
        let p = self.point();
        let (sx, sy) = STEP[d.d as usize];
        let a = -(f64::from(sy) * DY / f64::from(sx) / DX).atan();
        Point {
            x: p.x * a.cos() - p.y * a.sin(),
            y: p.x * a.sin() + p.y * a.cos(),
            z: 0.0,
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

/**
    Create a position.
    # Panics
    When a position is illegal.
*/
#[must_use]
pub fn position(x: i32, y: i32) -> Position {
    assert_eq!(y % 2, x % 2, "This x position is not allowed on this line");
    Position { x, y }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PointPos {
    pub pos: Position,
    pub dir: Direction,
}

impl PointPos {
    /**
    # Panics
    On incorrect direction
    */
    #[must_use]
    pub fn new(pos: &Position, dir: Direction) -> PointPos {
        pos.check();
        assert_eq!(0, dir.d % 4, "Incorrect point direction {}", dir.d);
        PointPos { pos:*pos, dir }
    }

    fn point(&self) -> Point {
        let c = self.corner();
        Point {
            x: f64::from(c.x) * DX,
            y: f64::from(c.y) * DY / 3.,
            z: 0.,
        }
    }

    fn corner(&self) -> Position {
        let (dx, dy) = POINT[self.dir.d as usize];
        Position {
            x: self.pos.x + i32::from(dx) / 4,
            y: self.pos.y * 3 + i32::from(dy) / 4,
        }
    }
}

impl std::fmt::Display for PointPos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let p = self.corner();
        write!(f, "({:.1},{:.1})", f64::from(p.x), f64::from(p.y) / 3.)
    }
}

// A 32*32 tile in the world. Any map here is part of this location.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Location {
    // Position in the world, with a multiple of 32 in x and y.
    position: Position,
    // Maps per layer
    maps: BTreeMap<u8, [u64; 1024]>,
    items: Vec<Item>,
}

impl Location {
    pub fn new(p: &Position) -> Location {
        Location {
            position: *p,
            maps: BTreeMap::new(),
            items: Vec::new(),
        }
    }
}

// A name and set of fields as part of a definition.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Definition {
    name: String,
    fields: Vec<Field>,
}

impl Definition {
    pub fn i32(&self, name: &str) -> i32 {
        for f in &self.fields {
            if f.is(name) {
                return f.i32();
            }
        }
        i32::MIN
    }

    pub fn f(&self, name: &str, default: f32) -> f32 {
        let v = self.i32(name);
        if v == i32::MIN {
            default
        } else {
            v as f32 / 255.0
        }
    }

    pub fn u8(&self, name: &str) -> u8 {
        for f in &self.fields {
            if f.is(name) {
                let v = f.i32();
                return if v == i32::MIN { 255 } else { v as u8 };
            }
        }
        255
    }
}

// The maps of this specific item/world. With minimum and maximum position as a bounding box.
#[derive(Default, Debug, Clone)]
pub struct Locations {
    // minimum position with data in these maps.
    min: Position,
    // maximum position with data in these maps.
    max: Position,
    // Map from position to location.
    maps: HashMap<Position, Location>,
    pub trace: bool, // do we log algorithm details
}

impl Locations {
    #[must_use]
    pub fn new() -> Locations {
        Locations {
            min: Position {
                x: i32::MAX,
                y: i32::MAX,
            },
            max: Position {
                x: i32::MIN,
                y: i32::MIN,
            },
            maps: HashMap::new(),
            trace: false,
        }
    }

    /// Validate if the world box includes the given position.
    pub fn validate(&mut self, p: Position) {
        p.check();
        if p.x > self.max.x {
            self.max.x = p.x;
        }
        if p.x < self.min.x {
            self.min.x = p.x;
        }
        if p.y > self.max.y {
            self.max.y = p.y;
        }
        if p.y < self.min.y {
            self.min.y = p.y;
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ItemDef {
    // Extra data per item, not yet in use
    def: Definition,
    // With 256 for no scaling.
    pub scale: u16,
    // What point of the image do we count as 0 and rotate around.
    pub start: Position,
    pub locations: Locations, // The maps on this specific item
}

#[derive(Default, Copy, Clone, PartialEq, Debug)]
pub struct Point {
    pub x: f64,
    pub y: f64,
    pub z: f64, // height
}

impl Point {
    fn new(x: f64, y: f64, z: f64) -> Point {
        Point { x, y, z }
    }

    fn normalize(&self) -> Point {
        let t = self.x * self.x + self.y * self.y + self.z * self.z;
        if t > 0.1 {
            let l = t.sqrt();
            Point {
                x: self.x / l,
                y: self.y / l,
                z: self.z / l,
            }
        } else {
            *self
        }
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "({:.1},{:.1},{:.1})",
            self.x / DX,
            self.y / DY,
            self.z / DZ
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Change {
    Left,
    Right,
}

impl std::fmt::Display for Change {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", if *self == Change::Left { "L" } else { "R" })
    }
}

#[derive(Copy, Clone, Debug)]
struct WallPoint {
    /// Original position that should not change.
    pos: PointPos,
    /// Direction of the wall towards this point.
    dir: Direction,
    /// Part of a specific line (vector position) `u16::MAX = none`
    line: u16,
    change: Change,
}

impl std::fmt::Display for WallPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}>{}", self.pos.corner(), self.change, self.dir)
    }
}

/// A line in the drawing, for now only used as debugging feature.
#[derive(Default, Copy, Clone, Debug)]
struct Line {
    mat: u8,    // materials of the wall
    nr: u16,    // number of the wall
    start: u16, // start position on the wall
    stop: u16,  // stop position on the wall
    dir: u8,    // general direction of the wall, or start direction of a curve
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}/{}={}:{}-{}",
            self.mat, self.nr, self.start, self.stop, self.dir
        )
    }
}

#[allow(dead_code)]
struct WallData {
    // direction used for the first point.
    pub dir: u8,
    // point (with height) from the first direction
    pub first: Point,
    // height from the other direction to the wall
    pub second: f64,
}

impl WallData {
    fn use_first(&self, l: &Locations, lay: u8, pos: Position, dir: u8) -> bool {
        let d = Direction::new(dir);
        self.second.is_nan()
            || (!self.first.z.is_nan()
                && (dir == self.dir
                    || (d.plus(8).d == self.dir && wall(l, lay, pos, d.plus(22).d) == 0)
                    || (d.plus(16).d == self.dir && wall(l, lay, pos, d.plus(2).d) == 0)))
    }

    fn height_p(&self, l: &Locations, lay: u8, pos: Position, dir: u8) -> Point {
        if self.use_first(l, lay, pos, dir) {
            self.first
        } else {
            Point {
                x: self.first.x,
                y: self.first.y,
                z: self.second,
            }
        }
    }

    fn other(&self, l: &Locations, lay: u8, pos: Position, dir: u8) -> f64 {
        if self.use_first(l, lay, pos, dir) {
            self.second
        } else {
            self.first.z
        }
    }
}

// Calculated information from a drawing.
// Data for one layer of a Location.

// Used to produce both SVG and opengl output.
// First fill all structure from the original. Also output this as a test.
// Then mutate the points to their actual position by evaluating the defined lines.
#[derive(Default)]
pub struct Drawing {
    /// point-id (x, y*3) to moved point for vertex buffer.
    /// A point can be both on a line and be part of another wall. The pattern should always be the start of a wall.
    /// For example, the outside wall of a house where the inside wall meets, or a fence built to meet a house wall.
    points: HashMap<Position, WallData>,
    /// A point if this is already part of a wall: `point.corner() -> (Material, Wall_Nr, Point_Nr)`.
    done: HashMap<Position, (u8, u16, u16)>,
    /// `Material -> Wall_nr`
    wall_nr: HashMap<u8, u16>,
    /// Used to keep track of the last given out `Wall_nr` as a `HashMap` has no ordering
    wall_nrs: Vec<(u8, u16)>,
    /// `(Material, Wall_Nr, Point_Nr) -> WallPoint`
    /// Each material will fill a separate buffer. Unless they share textures.
    walls: HashMap<(u8, u16, u16), WallPoint>,
    /// The lines that are recognized and most wall points will become part of one or more lines.
    lines: Vec<Line>,
    /// `(ground type>0, central hex, direction 0-6 6=centre)) -> (point, normal)`
    ground: HashMap<(u8, Position, u8), (Position, Point)>,
    /// Lowest heights per layer for each wall point-ids
    heights: HashMap<Position, Vec<f64>>,
}

impl Drawing {
    fn clear_layer(&mut self) {
        self.points.clear();
        self.done.clear();
        self.wall_nr.clear();
        self.wall_nrs.clear();
        self.walls.clear();
        self.lines.clear();
        self.ground.clear();
    }

    fn clear_loc(&mut self) {
        self.heights.clear();
    }

    fn height(&self, pos: Position, cur: f64) -> Option<f64> {
        assert!(!cur.is_nan());
        if let Some(p) = self.heights.get(&pos) {
            for h in p.iter().rev() {
                if *h > cur {
                    return Some(*h);
                }
            }
        }
        None
    }

    fn point(&mut self, l: &Locations, layer: u8, pp: PointPos, p: Point) {
        // show all possible height calculations from all sides
        let mut first = rotate(l, layer, pp.pos, p);
        let mut second = f64::NAN;
        let d2 = rotate(l, layer, step(pp.pos, pp.dir.plus(2).d, 1, false), p);
        let d22 = rotate(l, layer, step(pp.pos, pp.dir.plus(22).d, 1, false), p);
        if wall(l, layer, pp.pos, pp.dir.plus(2).d) == 0 {
            first = (first + d2) / 2.0;
        } else {
            second = d2;
        }
        if wall(l, layer, pp.pos, pp.dir.plus(22).d) == 0 {
            first = (first + d22) / 2.0;
        } else if second.is_nan() {
            second = d22;
        } else {
            second = (second + d22) / 2.0;
        }
        let rp = pp.corner();
        let wd = self.points.entry(rp).or_insert(WallData {
            dir: 0,
            first: p,
            second: f64::NAN,
        });
        wd.dir = pp.dir.d;
        wd.first.x = p.x;
        wd.first.y = p.y;
        wd.first.z = first;
        wd.second = second;
        let res = if first < second || second.is_nan() {
            first
        } else {
            second
        };
        self.heights.entry(rp).or_default().push(res);
    }
}

fn rotate(loc: &Locations, layer: u8, pos: Position, pt: Point) -> f64 {
    let mut r = 0.0;
    let mut c = 0;
    for d in 0..6 {
        let w1 = 2 + d * 4;
        let w2 = if d == 5 { 2 } else { 6 + d * 4 };
        if wall(loc, layer, pos, w1) == 0 && wall(loc, layer, pos, w2) == 0 {
            let h = calc_height(
                [
                    height_p(loc, layer, step(pos, w1, 1, false)),
                    height_p(loc, layer, step(pos, w2, 1, false)),
                    height_p(loc, layer, pos),
                ],
                pt,
            );
            r += h;
            c += 1;
        }
    }
    r / c as f64
}

impl ItemDef {
    #[must_use]
    pub fn new(name: String) -> ItemDef {
        ItemDef {
            def: Definition {
                name,
                fields: Vec::new(),
            },
            start: Position { x: 0, y: 0 },
            scale: 256,
            locations: Locations::new(),
        }
    }
}

// Description of a self-contained world.
// Items can have maps of their own to be shown inside this world. Think vehicles, large animals or floating islands.
#[derive(Default, Debug, Clone)]
pub struct World {
    #[allow(dead_code)]
    // Different map types in this world
    maps: Vec<Definition>,
    // Items in this world
    pub items: Vec<ItemDef>,
    grounds: Vec<Definition>,
    // Used ground materials
    walls: Vec<Definition>,
    // Used wall materials
    pub locations: Locations, // Map data
}

impl World {
    #[must_use]
    pub fn new() -> World {
        World {
            maps: vec![],
            items: vec![],
            grounds: Vec::new(),
            walls: Vec::new(),
            locations: Locations::new(),
        }
    }

    pub fn ground(&mut self, name: &str, fields: Vec<Field>) {
        self.grounds.push(Definition {
            name: name.to_string(),
            fields,
        });
    }

    pub fn wall(&mut self, name: &str, fields: Vec<Field>) {
        self.walls.push(Definition {
            name: name.to_string(),
            fields,
        });
    }
}

/*
Normally `x` is sideways to the right, `y` is away from the player and `z` is upwards.
On looking down to the ground like a map `y` is upwards and `z` is towards the player.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vector {
    x: i32,
    // real coordinates, so not tied to a grid
    y: i32,
    z: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Ray {
    origin: Vector,
    direction: Vector,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Sphere {
    center: Vector,
    radius: i32,
    radius2: i32, // radius * radius
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Direction {
    d: u8,
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.d)
    }
}

impl Direction {
    /// Create a new direction, it is just a single number of half hours on the clock.
    #[must_use]
    pub fn new(dir: u8) -> Direction {
        Direction { d: dir }
    }

    /// Move the direction a number of steps forwards. Moving back is 24-steps.
    #[must_use]
    pub fn plus(self, steps: u8) -> Direction {
        let mut res = self.d + steps;
        while res >= 24 {
            res -= 24;
        }
        Direction { d: res }
    }

    #[must_use]
    pub fn change(self, other: Direction) -> u8 {
        (if other.d > self.d { 24 } else { 0 }) + self.d - other.d
    }

    /// Move the direction back till it is a whole number of steps.
    #[must_use]
    pub fn truncate(self, step: u8) -> Direction {
        Direction {
            d: self.d - self.d % step,
        }
    }

    /// Round to the nearest step, downwards when equal distance.
    #[must_use]
    pub fn round(self, step: u8) -> Direction {
        Direction {
            d: self.d + step / 2,
        }
        .truncate(step)
    }

    /// Get the direction vector in a horizontal place.
    #[must_use]
    pub fn normal(self) -> Point {
        NORMAL[self.d as usize]
    }

    #[must_use]
    pub fn steps(self) -> [Direction; 4] {
        let [d0, d1, d2, d3] = STEP_DIR[self.d as usize];
        [
            Direction::new(d0),
            Direction::new(d1),
            Direction::new(d2),
            Direction::new(d3),
        ]
    }

    #[must_use]
    pub fn ord(self) -> u8 {
        self.d
    }
}

/// A set of directions to take a step.
const STEP_DIR: [[u8; 4]; 24] = [
    [2, 22, 2, 22], // 0,24
    [2, 2, 22, 2],
    [2, 2, 2, 2], // 2
    [2, 2, 6, 2],
    [6, 2, 6, 2], // 4
    [6, 6, 2, 6],
    [6, 6, 6, 6], // 6
    [6, 6, 10, 6],
    [10, 6, 10, 6], // 8
    [10, 10, 6, 10],
    [10, 10, 10, 10], // 10
    [10, 10, 14, 10],
    [14, 10, 14, 10], // 12
    [14, 14, 10, 14],
    [14, 14, 14, 14], // 14
    [14, 14, 18, 14],
    [18, 14, 18, 14], // 16
    [18, 18, 14, 18],
    [18, 18, 18, 18], // 18
    [18, 18, 22, 18],
    [22, 18, 22, 18], // 20
    [22, 22, 18, 22],
    [22, 22, 22, 22], // 22
    [22, 22, 2, 22],
];

/// The per direction the point to move to in 4 steps.
const STEP: [(i8, i8); 24] = [
    (0, 4), // 0,12.. via (1, 1), (0, 2), (1, 3)
    (2, 4),
    (4, 4), // 2
    (5, 3),
    (6, 2), // 4.. via (2, 0), (3, 1), (5, 1)
    (7, 1),
    (8, 0), // 6
    (7, -1),
    (6, -2), // 8.. via (1, -1), (3, -1), (4, -2)
    (5, -3),
    (4, -4), // 10
    (2, -4),
    (0, -4), // 12.. via (-1, -1), (0, -2), (-1, -3)
    (-2, -4),
    (-4, -4), // 14
    (-5, -3),
    (-6, -2), // 16.. via (-2, 0), (-3, -1), (-5, -2)
    (-7, -1),
    (-8, 0), // 18
    (-7, 1),
    (-6, 2), // 20.. via (-1, 1), (-3, 1), (-4, 2)
    (-5, 3),
    (-4, 4), // 22
    (-2, 4),
];

// Limited to 24 directions.
// Position p should be a valid position.
// Rotation should be 0 or higher and lower than 24 so not an Item rotation that holds 2048 steps per half hour.
#[must_use]
pub fn step(p: Position, rotation: u8, steps: i32, flipped: bool) -> Position {
    if steps == 0 {
        return p;
    }
    // Validate that the original position is correct.
    p.check();
    // Validate that we do not take too small steps for our rotation.
    if rotation & 1 == 1 && steps & 3 != 0 {
        let r_steps = steps & 3;
        let n = if steps > 3 {
            // Do only the complete steps
            step(p, rotation, steps & -4, false)
        } else {
            p
        };
        if rotation & 3 == 1 {
            // 1, 5, 9, 13, 17, 21 = [r+1, r-3, r+1]
            let n = step(n, (rotation + 1) % 24, 1, false);
            let n = if r_steps > 1 {
                step(n, (rotation + 21) % 24, 1, false)
            } else {
                n
            };
            return if r_steps > 2 {
                step(n, (rotation + 1) % 24, 1, false)
            } else {
                n
            };
        }
        // 3, 7, 11, 15, 19, 23 = [r-1, r+3, r-1]      ??? was   [r-1, r-1, r+3]
        let n = step(n, (rotation + 23) % 24, 1, false);
        let n = if r_steps > 1 {
            step(n, (rotation + 3) % 24, 1, false)
        } else {
            n
        };
        return if r_steps > 2 {
            step(n, (rotation + 23) % 24, 1, false)
        } else {
            n
        };
    } else if rotation & 3 != 2 && steps & 1 != 0 {
        let n = if steps > 1 {
            step(p, rotation, steps - 1, false) // Do a one less far step
        } else if steps < -1 {
            step(p, rotation, steps + 1, false) // Do a one less far step
        } else {
            p
        };
        return step(
            n,
            (rotation + if flipped ^ (steps < 0) { 22 } else { 2 }) % 24,
            steps % 2,
            false,
        );
    }
    let (sx, sy) = STEP[rotation as usize];
    let r = Position {
        x: p.x + ((sx as i32 * steps) >> 2),
        y: p.y + ((sy as i32 * steps) >> 2),
    };
    r.check();
    r
}

fn mut_value(locations: &mut Locations, layer: u8, pos: Position) -> Option<&mut u64> {
    let map_pos = Position {
        x: pos.x & -64,
        y: pos.y & -32,
    };
    locations
        .maps
        .entry(map_pos)
        .or_insert_with(|| Location::new(&map_pos))
        .maps
        .entry(layer)
        .or_insert([0; 1024])
        .get_mut(((pos.x % 64) / 2 + 32 * pos.y) as usize)
}

fn get_value(locations: &Locations, layer: u8, pos: Position) -> Option<&u64> {
    let loc = locations.maps.get(&Position {
        x: pos.x & -64,
        y: pos.y & -32,
    })?;
    let values = loc.maps.get(&layer)?;
    values.get(((pos.x % 64) / 2 + 32 * pos.y) as usize)
}

const ALL_BITS: u64 = (0 - 1) as u64;
const GET_MATERIAL: u64 = 255;
const HEIGHT_BIT: u64 = 32;
const GET_HEIGHT: u64 = 65535;
#[allow(dead_code)]
const ITEM_BIT: u64 = 48;
const GET_ITEM: u64 = 2047;
#[allow(dead_code)]
const ROTATE_BIT: u64 = 59;
const GET_ROTATE: u64 = 31;

#[must_use]
pub fn material(locations: &Locations, layer: u8, pos: Position) -> u64 {
    if let Some(val) = get_value(locations, layer, pos) {
        val & GET_MATERIAL
    } else {
        0
    }
}

pub fn set_material(locations: &mut Locations, layer: u8, pos: Position, value: u8) {
    if let Some(val) = mut_value(locations, layer, pos) {
        *val = (*val & (ALL_BITS - GET_MATERIAL)) + (value as u64 & GET_MATERIAL)
    }
}

#[must_use]
pub fn wall(locations: &Locations, layer: u8, pos: Position, dir: u8) -> u8 {
    assert!(dir < 24, "Illegal direction {dir}");
    assert_eq!(dir % 4, 2, "Need base direction {dir}");
    if dir > 6 && dir < 22 {
        wall(locations, layer, step(pos, dir, 1, false), (12 + dir) % 24)
    } else {
        let from_bit = if dir == 22 { 8 } else { 12 + dir as u64 * 2 };
        if let Some(val) = get_value(locations, layer, pos) {
            ((val >> from_bit) & GET_MATERIAL) as u8
        } else {
            0
        }
    }
}

#[must_use]
pub fn set_wall(
    locations: &mut Locations,
    layer: u8,
    pos: Position,
    dir: u8,
    value: u8,
) -> Position {
    assert!(dir < 24, "Illegal direction {dir}");
    assert_eq!(dir % 4, 2, "Need base direction {dir}");
    if dir > 6 && dir < 22 {
        set_wall(
            locations,
            layer,
            step(pos, dir, 1, false),
            (12 + dir) % 24,
            value,
        )
    } else {
        let from_bit = if dir == 22 { 8 } else { 12 + dir as u64 * 2 };
        if let Some(val) = mut_value(locations, layer, pos) {
            *val = (*val & (ALL_BITS - (GET_MATERIAL << from_bit))) + ((value as u64) << from_bit);
        }
        pos
    }
}

pub fn set_height(locations: &mut Locations, layer: u8, pos: Position, value: u64) {
    if let Some(val) = mut_value(locations, layer, pos) {
        *val = (*val & (ALL_BITS - (GET_HEIGHT << HEIGHT_BIT))) + (value << HEIGHT_BIT)
    }
}

#[must_use]
pub fn height(locations: &Locations, layer: u8, pos: Position) -> u64 {
    let Some(val) = get_value(locations, layer, pos) else {
        return 0;
    };
    (val >> HEIGHT_BIT) & GET_HEIGHT
}

pub fn set_item(locations: &mut Locations, layer: u8, pos: Position, value: u64) {
    if let Some(val) = mut_value(locations, layer, pos) {
        *val = (*val & (ALL_BITS - (GET_ITEM << 48))) + (value << 48)
    }
}

pub fn set_rotate(locations: &mut Locations, layer: u8, pos: Position, value: u64) {
    if let Some(val) = mut_value(locations, layer, pos) {
        *val = (*val & (ALL_BITS - (GET_ROTATE << 59))) + (value << 59)
    }
}

// Draw an image inside the locations.
// We do not implement a different scale from 256 = no scaling.
// This allow 12 rotations here. Where uneven rotations swap the x and y direction of the image.
// TODO more efficient drawing: skip when <min or >max.
pub fn draw(image: &Item, def: &ItemDef, locations: &mut Locations) {
    let from = &def.locations;
    let mut rot_y = (image.rotation >> 10) as u8;
    let mut rot_x = (rot_y + 6) % 24;
    let swap = rot_y & 3 == 2;
    if swap {
        std::mem::swap(&mut rot_y, &mut rot_x);
    }
    let flipped = swap ^ (def.start.y & 1 != 0);
    locations.validate(step(
        step(image.pos, rot_x, (from.min.x - def.start.x) / 2, flipped),
        rot_y,
        from.min.y - def.start.y,
        flipped,
    ));
    locations.validate(step(
        step(image.pos, rot_x, (from.min.x - def.start.x) / 2, flipped),
        rot_y,
        from.max.y - def.start.y,
        flipped,
    ));
    locations.validate(step(
        step(image.pos, rot_x, (from.max.x - def.start.x) / 2, flipped),
        rot_y,
        from.min.y - def.start.y,
        flipped,
    ));
    locations.validate(step(
        step(image.pos, rot_x, (from.max.x - def.start.x) / 2, flipped),
        rot_y,
        from.max.y - def.start.y,
        flipped,
    ));
    for (pos, loc) in &from.maps {
        let world_pos = Position {
            x: image.pos.x - pos.x,
            y: image.pos.y - pos.y,
        };
        if locations.trace {
            println!(
                "draw image {} map {pos} world {world_pos} rot {rot_x},{rot_y}",
                image.pos
            );
        }
        for (layer, values) in &loc.maps {
            for y in 0..32 {
                let line_pos = step(world_pos, rot_y, y - def.start.y, flipped);
                if locations.trace {
                    println!(
                        "line {line_pos} steps {} flipped {flipped}",
                        y - def.start.y
                    );
                }
                for x in 0..32 {
                    let val = values[(x + y * 32) as usize];
                    if val == 0 {
                        continue;
                    }
                    let to = step(line_pos, rot_x, x - (def.start.x / 2), flipped);
                    if locations.trace {
                        println!(
                            "to {to} steps {} flipped {flipped} mat {}",
                            x - (def.start.x / 2),
                            val & GET_MATERIAL
                        );
                    }
                    if val & GET_MATERIAL != 0 {
                        // skip open hexes
                        set_material(locations, *layer, to, (val & GET_MATERIAL) as u8);
                    }
                    place_wall(
                        locations,
                        *layer,
                        to,
                        (rot_y + 22) % 24,
                        ((val >> 8) & GET_MATERIAL) as u8,
                    );
                    place_wall(
                        locations,
                        *layer,
                        to,
                        (rot_y + 2) % 24,
                        ((val >> 16) & GET_MATERIAL) as u8,
                    );
                    place_wall(
                        locations,
                        *layer,
                        to,
                        (rot_y + 6) % 24,
                        ((val >> 24) & GET_MATERIAL) as u8,
                    );
                    let h = (val >> HEIGHT_BIT) & GET_HEIGHT;
                    if h != GET_HEIGHT {
                        // skip undefined heights
                        set_height(locations, *layer, to, h);
                    }
                }
            }
        }
    }
}

fn place_wall(locations: &mut Locations, layer: u8, pos: Position, dir: u8, value: u8) {
    if value == 0 {
        return;
    }
    if wall(locations, layer, pos, dir) == value {
        set_wall(locations, layer, pos, dir, 0);
    } else {
        set_wall(locations, layer, pos, dir, value);
    }
}

pub fn draw_line<T>(
    locations: &mut Locations,
    layer: u8,
    material: T,
    wall: u8,
    pos: Position,
    dir: Direction,
    length: i32,
) where
    T: Fn(&mut Locations, u8, Position, i32),
{
    let mut c_wall = dir.plus(9).truncate(4).plus(2);
    // The point on the wall to draw from.
    let steps = dir.steps();
    let mut p = pos;
    for l in 0..length {
        locations.validate(p);
        let st = steps[l as usize % 4];
        let t_wall = st.plus(8);
        let n_wall = if l < length - 1 { st.plus(4) } else { st };
        material(locations, layer, p, l);
        // Prevent an unending loop
        for _ in 0..5 {
            if wall != 255 {
                let to = set_wall(locations, layer, p, c_wall.ord(), wall);
                locations.validate(to);
            }
            if c_wall == n_wall {
                break;
            }
            c_wall = c_wall.plus(20);
        }
        p = step(p, st.ord(), 1, false);
        c_wall = t_wall;
    }
}

pub fn draw_left_wall(
    locations: &mut Locations,
    layer: u8,
    wall: u8,
    pos: Position,
    dir: Direction,
    length: i32,
) {
    let mut c_wall = dir.plus(16).truncate(4).plus(2);
    // The point on the wall to draw from.
    let steps = dir.steps();
    let mut p = pos;
    for l in 0..length {
        let st = steps[l as usize % 4];
        let t_wall = st.plus(16);
        let n_wall = st.plus(20);
        loop {
            let to = set_wall(locations, layer, p, c_wall.ord(), wall);
            locations.validate(to);
            if c_wall == n_wall {
                break;
            }
            c_wall = c_wall.plus(4);
        }
        p = step(p, st.ord(), 1, false);
        c_wall = t_wall;
    }
}

/// Draw a plane with a wall around it.
/// material: allow to set both the material as height
pub fn draw_plane<T>(
    locations: &mut Locations,
    layer: u8,
    set_mat: T,
    wall: u8,
    pos: Position,
    dir: Direction,
    size: Position,
) where
    T: Fn(&mut Locations, u8, Position, Point),
{
    draw_left_wall(locations, layer, wall, pos, dir, size.x);
    draw_line(
        locations,
        layer,
        |loc, lay, p, n| {
            if n == size.x - 1 {
                draw_left_wall(loc, lay, wall, p, dir.plus(6), size.y);
            }
            draw_line(
                loc,
                lay,
                |lc, ly, ps, w| {
                    if n == 0 && w == size.y - 1 {
                        draw_line(lc, lay, |_, _, _, _| {}, wall, ps, dir, size.x);
                    }
                    set_mat(lc, ly, ps, ps.sub(&pos).rotate(dir));
                    // Prevent drawing inner walls when two planes with the same room material intersect.
                    let m = material(lc, ly, ps);
                    if m != 0 {
                        for d in 0..6 {
                            let wd = d * 4 + 2;
                            let tps = step(ps, wd, 1, false);
                            if material(lc, ly, tps) == m {
                                set_wall(lc, ly, ps, wd, 0);
                            }
                        }
                    }
                },
                if n == 0 { wall } else { 255 },
                p,
                dir.plus(6),
                size.y,
            );
        },
        255,
        pos,
        dir,
        size.x,
    );
}

/// Find all occurrences of this specific ground type and place the specified wall around it.
pub fn circumventing(location: &mut Locations, ground: u64, wall: u8) {
    for y in location.min.y..1 + location.max.y {
        let mut x = location.min.x + (y & 1);
        while x < 1 + location.max.x {
            let p = Position { x, y };
            p.check();
            let mat = material(location, 0, p);
            if mat == ground {
                for dir in 0..6 {
                    let rot = 2 + dir * 4;
                    let to = step(p, rot, 1, false);
                    if material(location, 0, to) != ground {
                        set_wall(location, 0, p, rot, wall);
                        if (2..5).contains(&dir) {
                            location.validate(to);
                        }
                    }
                }
            }
            x += 2;
        }
    }
}

/*
     .         .         .         |         |
 .-`` ``-. .-`` ``-. .  ` `  . .-`` ``-. .-`` ``-.
|         `         `         | ####### |         |
|         .         .         | ####### |         |
 ``-. .-`` ``-. .-`` `  . .  ` `  . .-`` ``-. .-``
     |         |         `         |         |
 */
pub fn output_text(to_file: &str, location: &Locations, drawing: Option<&Drawing>) -> Result<()> {
    let f = File::create(to_file)?;
    let mut b = BufWriter::new(f);
    let mut out = format!("{} - {}\n", location.min, location.max);
    if location.min.x == i32::MAX || location.min.y == i32::MAX {
        write!(b, "{out}")?;
        return b.flush();
    }
    let mut y = location.max.y;
    while y >= location.min.y {
        let indent = y & 1 == 1;
        let mut x = if location.min.x & 1 == 1 {
            location.min.x - 1
        } else {
            location.min.x
        };
        if indent {
            out += "     ";
            x += 1;
        }
        while x < 1 + location.max.x {
            let p = Position { x, y };
            p.check();
            let nw = wall(location, 0, p, 22);
            let ne = wall(location, 0, p, 2);
            if nw == 0 {
                out += " .  `";
            } else {
                out += " .-``";
            }
            if ne == 0 {
                out += " `  .";
            } else {
                out += " ``-.";
            }
            x += 2;
        }
        out += "\n";
        for s in 0..2 {
            let mut x = if location.min.x & 1 == 1 {
                location.min.x - 1
            } else {
                location.min.x
            };
            if indent {
                out += "     ";
                x += 1;
            }
            if s == 0 {
                out += "`";
            } else {
                out += ".";
            }
            while x < 1 + location.max.x {
                let p = Position { x, y };
                let mat = material(location, 0, p);
                let height = height(location, 0, p);
                if mat > 0 {
                    if s == 0 {
                        out += &format!(" {mat:02} {height:04} ");
                    } else {
                        out += &format!(" {x:3},{y:3} ");
                    }
                } else {
                    out += "         ";
                }
                let e = wall(location, 0, p, 6);
                if e == 0 {
                    if s == 0 {
                        out += "`";
                    } else {
                        out += ".";
                    }
                } else {
                    out += "|";
                }
                x += 2;
            }
            out += "\n";
        }
        y -= 1;
    }
    let y = 1 + location.min.y;
    let indent = y % 2 == 1;
    let mut x = if location.min.x & 1 == 1 {
        location.min.x - 1
    } else {
        location.min.x
    };
    if indent {
        out += " `  .";
        x += 1;
    }
    while x < 1 + location.max.x {
        out += " .  ` `  .";
        x += 2;
    }
    out += "\n";
    if let Some(d) = drawing {
        for (mat, nr) in &d.wall_nrs {
            let mut pnr = 1;
            while let Some(wp) = d.walls.get(&(*mat, *nr, pnr)) {
                let mut pt = if let Some(p) = d.points.get(&wp.pos.corner()) {
                    p.first
                } else {
                    wp.pos.point()
                };
                // Do not confuse the output with possible inside or outside heights
                pt.z = 0.0;
                let pp = wp.pos.point();
                out += &format!(
                    "{mat}/{nr}/{pnr} {}/{} {} {}{pt}",
                    wp.pos.pos,
                    wp.pos.dir,
                    wp.change,
                    if pt.x == pp.x && pt.y == pp.y {
                        "="
                    } else {
                        ">"
                    }
                );
                for (lnr, line) in d.lines.iter().enumerate() {
                    if line.mat == *mat && line.nr == *nr {
                        if line.start == pnr {
                            out += &format!(" B{}", line.dir);
                        }
                        if line.stop == pnr {
                            out += &format!(" E{}", line.dir);
                        }
                    }
                    if wp.line == lnr as u16 {
                        out += &format!(" L{}", line.dir);
                    }
                }
                out += "\n";
                pnr += 1;
            }
        }
    }
    write!(b, "{out}")?;
    b.flush()
}

const DX: f64 = 20.0;
const DY: f64 = -34.64101615137755;
const DZ: f64 = 8.0; // This is 1/5 of the width of a hex. Or about 30cm.

const NORMAL: [Point; 24] = [
    Point {
        x: 1.,
        y: 0.,
        z: 0.,
    }, // 0
    Point {
        x: 0.965925826289068,
        y: 0.258819045102521,
        z: 0.,
    },
    Point {
        x: 0.866025403784439,
        y: 0.5,
        z: 0.,
    }, // 1
    Point {
        x: std::f64::consts::FRAC_1_SQRT_2,
        y: std::f64::consts::FRAC_1_SQRT_2,
        z: 0.,
    },
    Point {
        x: 0.5,
        y: 0.866025403784439,
        z: 0.,
    }, // 2
    Point {
        x: 0.258819045102521,
        y: 0.965925826289068,
        z: 0.,
    },
    Point {
        x: 0.,
        y: 1.,
        z: 0.,
    }, // 3
    Point {
        x: -0.258819045102521,
        y: 0.965925826289068,
        z: 0.,
    },
    Point {
        x: -0.5,
        y: 0.866025403784439,
        z: 0.,
    }, // 4
    Point {
        x: -std::f64::consts::FRAC_1_SQRT_2,
        y: std::f64::consts::FRAC_1_SQRT_2,
        z: 0.,
    },
    Point {
        x: -0.866025403784439,
        y: 0.5,
        z: 0.,
    }, // 5
    Point {
        x: -0.965925826289068,
        y: 0.258819045102521,
        z: 0.,
    },
    Point {
        x: -1.,
        y: 0.,
        z: 0.,
    }, // 6
    Point {
        x: -0.965925826289068,
        y: -0.258819045102521,
        z: 0.,
    },
    Point {
        x: -0.866025403784439,
        y: -0.5,
        z: 0.,
    }, // 7
    Point {
        x: -std::f64::consts::FRAC_1_SQRT_2,
        y: -std::f64::consts::FRAC_1_SQRT_2,
        z: 0.,
    },
    Point {
        x: -0.5,
        y: -0.866025403784439,
        z: 0.,
    }, // 8
    Point {
        x: -0.258819045102521,
        y: -0.965925826289068,
        z: 0.,
    },
    Point {
        x: 0.,
        y: -1.,
        z: 0.,
    }, // 9
    Point {
        x: 0.258819045102521,
        y: -0.965925826289068,
        z: 0.,
    },
    Point {
        x: 0.5,
        y: -0.866025403784439,
        z: 0.,
    }, // 10
    Point {
        x: std::f64::consts::FRAC_1_SQRT_2,
        y: -std::f64::consts::FRAC_1_SQRT_2,
        z: 0.,
    },
    Point {
        x: 0.866025403784439,
        y: -0.5,
        z: 0.,
    }, // 11
    Point {
        x: 0.965925826289068,
        y: -0.258819045102521,
        z: 0.,
    },
];

/// The position on a hex tile for each direction.
const POINT: [(i8, i8); 24] = [
    (0, 8), // 0
    (1, 7),
    (2, 6), // 1
    (3, 5),
    (4, 4), // 2
    (4, 2),
    (4, 0), // 3
    (4, -2),
    (4, -4), // 4
    (3, -5),
    (2, -6), // 5
    (1, -7),
    (0, -8), // 6
    (-1, -7),
    (-2, -6), // 7
    (-3, -5),
    (-4, -4), // 8
    (-4, -2),
    (-4, 0), // 9
    (-4, 2),
    (-4, 4), // 10
    (-3, 5),
    (-2, 6), // 11
    (-1, 7),
];

pub fn v_vector(p: Position, rotation: u8) -> Vector {
    let (sx, sy) = if rotation > 23 {
        (0, 0)
    } else {
        POINT[rotation as usize]
    };
    // We use a numerical approximation here to prevent floats
    Vector {
        x: (4 * p.x + sx as i32) * 45,
        y: (12 * p.y + sy as i32) * 26,
        z: 0,
    }
}

#[allow(dead_code)]
fn v_minus(a: Vector, b: Vector) -> Vector {
    Vector {
        x: a.x - b.x,
        y: a.y - b.y,
        z: a.z - b.z,
    }
}

#[allow(dead_code)]
fn v_product(a: Vector, b: Vector) -> i32 {
    a.x * b.x + a.y * b.y + a.z * b.z
}

#[allow(dead_code)]
fn v_intersect(ray: Ray, sphere: Sphere) -> bool {
    let vpc = v_minus(sphere.center, ray.origin);
    let v_dir = v_product(vpc.clone(), ray.direction);
    if v_dir < 0 {
        false
    } else {
        v_product(vpc.clone(), vpc) - v_dir * v_dir < sphere.radius2 // do we need abs here?
    }
}

// Fill the drawing details for a specific map in the world.
// This will draw all the layers that are specified.
// In the future this might be a bit too much when we want to allow for skyscrapers with many defined floors.
pub fn fill_drawing(ls: &Locations, lay: u8, p: Position, d: &mut Drawing) -> bool {
    let Some(l) = ls.maps.get(&p) else {
        return false;
    };
    d.clear_layer();
    // Fill wall structures
    for x in 0..32 {
        for y in 0..32 {
            let pp = Position::data(x, y).add(&l.position);
            for dir in 0..6 {
                let wall_d = Direction::new(dir * 4 + 2);
                let wv = wall(ls, lay, pp, wall_d.d);
                if wv != 0 {
                    let point = PointPos::new(&pp, wall_d.plus(2));
                    let ws = WallStep::new(point, wall_d, wall_d.plus(18), false);
                    walk(ls, lay, ws, wv, d);
                }
            }
        }
    }
    true
}

fn get_walk_nr(d: &mut Drawing, wv: u8) -> u16 {
    let e = d.wall_nr.entry(wv).or_insert(0);
    *e += 1;
    *e
}

fn hex_pos(x: i32, y: i32, d: usize) -> Point {
    let d = if d >= 6 { d - 6 } else { d };
    let (dx, dy) = POINT[d * 4];
    let x = (x as f64 * 2. + if y % 2 == 1 { 1. } else { 0. } + dx as f64 / 4.) * DX;
    let y = (y as f64 + dy as f64 / 12.) * DY;
    Point { x, y, z: 0.0 }
}

fn hex_grid(l: &Locations, lay: u8, b: &mut BufWriter<File>, mx: i32, my: i32) -> Result<()> {
    for x in 0..mx {
        for y in 0..my {
            let mut wall_str = "".to_string();
            let wp = Position::data(x, y);
            let p = wp.point();
            writeln!(
                b,
                "<circle stroke=\"blue\" cx=\"{}\" cy=\"{}\" r=\"0.1\" />",
                p.x, p.y,
            )?;
            writeln!(
                b,
                "<text x=\"{}\" y=\"{}\" class=\"coord\">{},{}</text>",
                p.x - DX / 2.0,
                p.y - DY / 3.0,
                wp.x,
                wp.y
            )?;
            let mut s = "M ".to_string();
            for d in 0..4 {
                if d > 0 {
                    s += "L ";
                }
                let p = hex_pos(x, y, d);
                s += &format!("{} {} ", p.x, p.y);
                if wall(l, lay, wp, d as u8 * 4 + 2) > 0 {
                    let n = hex_pos(x, y, d + 1);
                    wall_str += &format!("<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"red\" stroke-width=\"1.5\" />\n", p.x, p.y, n.x, n.y);
                }
            }
            writeln!(
                b,
                "<path d=\"{s}\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.5\" stroke-dasharray=\"1,1\" />"
            )?;
            if !wall_str.is_empty() {
                write!(b, "{wall_str}")?;
            }
        }
    }
    Ok(())
}

pub fn drawing_svg(l: &Locations, lay: u8, to_file: &str, d: &Drawing) -> Result<()> {
    let f = File::create(to_file)?;
    let mut b = BufWriter::new(f);
    writeln!(
        b,
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    )?;
    writeln!(b, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")?;
    writeln!(
        b,
        "<svg viewBox=\"{} {} {} {}\" xmlns=\"http://www.w3.org/2000/svg\">",
        0,
        33.0 * DY,
        64.0 * DX,
        -33.0 * DY,
    )?;
    writeln!(b, "<style>\n.coord {{font: 7px sans-serif;}}\n</style>")?;
    writeln!(
        b,
        "<rect x=\"{}\" y=\"{}\" width=\"100%\" height=\"100%\" fill=\"white\"/>",
        0,
        33.0 * DY
    )?;
    for (mat, wall_nr) in &d.wall_nrs {
        let mut pnr = 1;
        let mut s = "".to_string();
        while let Some(wp) = d.walls.get(&(*mat, *wall_nr, pnr)) {
            let pt = if let Some(p) = d.points.get(&wp.pos.corner()) {
                p.first
            } else {
                wp.pos.point()
            };
            s += if pnr == 1 { "M " } else { "L " };
            s += &format!("{} {} ", pt.x, pt.y);
            pnr += 1;
        }
        writeln!(b, "<path d=\"{s} Z\" fill=\"grey\" stroke=\"black\" />")?;
    }
    hex_grid(l, lay, &mut b, 32, 32)?;
    writeln!(b, "</svg>")
}

#[derive(Copy, Clone, Debug)]
pub struct WallStep {
    /// current hex position at the end of a wall
    pos: PointPos,
    /// wall direction relative to the centre of the hex
    wall_dir: Direction,
    /// move direction along the wall towards pos
    move_dir: Direction,
    /// is the wall in pos curved to the left from the last
    change: Change,
}

impl WallStep {
    pub fn new(pos: PointPos, wall_dir: Direction, move_dir: Direction, left: bool) -> WallStep {
        assert_eq!(wall_dir.d % 4, 2, "Incorrect wall direction");
        assert_eq!(
            (24 + move_dir.d - wall_dir.d) % 12,
            6,
            "Incorrect along wall move direction"
        );
        WallStep {
            pos,
            wall_dir,
            move_dir,
            change: if left { Change::Left } else { Change::Right },
        }
    }

    pub fn do_move(&self) -> PointPos {
        let right = self.wall_dir.change(self.move_dir) == 6;
        let step_dir = self.move_dir.plus(if right { 2 } else { 22 });
        let new_dir = self.pos.dir.plus(if right { 4 } else { 20 });
        PointPos {
            pos: step(self.pos.pos, step_dir.d, 1, false),
            dir: new_dir,
        }
    }

    pub fn step(&self, to: PointPos, left: bool) -> WallStep {
        WallStep::new(
            to,
            self.move_dir.plus(if left { 14 } else { 10 }),
            self.move_dir.plus(if left { 20 } else { 4 }),
            left,
        )
    }
}

impl std::fmt::Display for WallStep {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}/{}.{}m{}{}",
            self.pos.pos,
            self.pos.dir,
            self.wall_dir,
            self.move_dir,
            if self.change == Change::Left {
                "L"
            } else {
                "R"
            }
        )
    }
}

// Each points hold the direction to the next point.
// Reversing should keep this in place, so we should switch it.
fn reverse(wv: u8, nr: u16, pnr: u16, d: &mut Drawing) {
    let mut p_dir = Direction::new(0);
    // Move current wall inverse to wall nr=0
    for wp in 0..pnr {
        let owp = d.walls.get(&(wv, nr, wp)).unwrap();
        let nwp = WallPoint {
            pos: owp.pos,
            dir: p_dir,
            line: owp.line,
            change: if owp.change == Change::Left {
                Change::Right
            } else {
                Change::Left
            },
        };
        p_dir = owp.dir.plus(12);
        d.walls.insert((wv, 0, pnr - wp - 1), nwp);
    }
    // Restore the inverse wall
    for wp in 0..pnr {
        let cwp = d.walls.remove(&(wv, 0, wp)).unwrap();
        d.walls.insert((wv, nr, wp), cwp);
    }
}

/// Try to walk along a wall.
/// Halts on a Y junction with the same wall type.
/// The walls will be drawn without shared direction characteristics.
fn walk(l: &Locations, lay: u8, ws: WallStep, wv: u8, d: &mut Drawing) {
    if d.done.contains_key(&ws.pos.corner()) {
        return;
    }
    let start = ws.pos.corner();
    let mut nr = 0;
    let mut s = ws;
    let mut pnr = 1;
    let mut reversed = false;
    loop {
        // Find the continuation of the wall. Halting a Y junctions or ends.
        let Some(next) = walk_check(l, lay, s, wv, !reversed) else {
            if reversed {
                break;
            }
            if pnr == 1 {
                break;
            }
            reverse(wv, nr, pnr, d);
            reversed = true;
            s = WallStep::new(
                ws.pos,
                ws.wall_dir,
                ws.move_dir.plus(12),
                ws.change == Change::Right,
            );
            // validate walk_check here!!
            continue;
        };
        if start == s.pos.corner() && pnr > 1 {
            break;
        }
        s = next;
        if pnr == 1 {
            nr = get_walk_nr(d, wv);
            d.wall_nrs.push((wv, nr));
            d.walls.insert((wv, nr, 0), wall_point(ws));
        }
        d.done.insert(s.pos.corner(), (wv, nr, pnr));
        d.walls.insert((wv, nr, pnr), wall_point(s));
        pnr += 1;
    }
    if pnr > 1 {
        apply_pattern(l, lay, pnr - 1, wv, nr, d);
    }
}

type Routine = fn(Point, u8, Point, u8) -> (Point, u8);

struct Pattern {
    length: u8,
    pattern: u32,
    /// start point, dir, point to move, previous dir
    routine: Routine,
}

impl Pattern {
    fn matches(&self, step: u8, change: Change) -> bool {
        change
            == if (self.pattern >> (self.length - 1 - step)) & 1 == 1 {
                Change::Left
            } else {
                Change::Right
            }
    }
}

// Modify these numbers when new patterns are inserted before them.
//const BLOCK: u8 = 2;
//const WOBBLE: u8 = 3;

const PATTERNS: &[Pattern] = &[
    Pattern {
        // Half wobble
        length: 8,
        pattern: 0b10110010,
        routine: |sp, s_dir, p, p_dir| moves(half_step(sp, s_dir + 16), s_dir + 1, p, p_dir),
    },
    Pattern {
        // Inverse half wobble
        length: 8,
        pattern: 0b01001101,
        routine: |sp, s_dir, p, p_dir| moves(half_step(sp, s_dir + 8), s_dir + 23, p, p_dir),
    },
    Pattern {
        // Moved half wobble
        length: 8,
        pattern: 0b11001010,
        routine: |sp, s_dir, p, p_dir| {
            moves(
                half_step(half_step(half_step(sp, s_dir), s_dir), s_dir + 20),
                s_dir + 1,
                p,
                p_dir,
            )
        },
    },
    Pattern {
        // Moved inverse half wobble
        length: 8,
        pattern: 0b01010011,
        routine: |sp, s_dir, p, p_dir| {
            moves(
                half_step(half_step(half_step(sp, s_dir), s_dir), s_dir + 20),
                s_dir + 23,
                p,
                p_dir,
            )
        },
    },
    Pattern {
        // BLOCK pattern, move points towards the centre line
        length: 4,
        pattern: 0b1100,
        routine: |sp, s_dir, p, p_dir| moves(half_step(sp, s_dir + 16), s_dir, p, p_dir),
    },
    Pattern {
        // WOBBLE pattern, move points towards the centre line
        length: 4,
        pattern: 0b1010,
        routine: |sp, s_dir, p, p_dir| moves(half_step(sp, s_dir + 16), s_dir + 2, p, p_dir),
    },
];

/// Move p to line from sp in direction dp.
/// See doc/Derivation_to_line.txt for the math behind these formula.
fn to_line2d(sp: Point, dp: Point, p: Point) -> Point {
    let d = dp.x * dp.x + dp.y * dp.y;
    let c = dp.x * sp.y - dp.y * sp.x;
    let m = dp.x * p.x + dp.y * p.y;
    Point {
        x: (dp.x * m - dp.y * c) / d,
        y: (dp.x * c + dp.y * m) / d,
        z: p.z,
    }
}

/// Two lines: first through p1 & p2, second through p3 & p4.
fn intersect2d(p1: Point, p2: Point, p3: Point, p4: Point) -> Point {
    let d = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x);
    let m1 = p1.x * p2.y - p1.y * p2.x;
    let m2 = p3.x * p4.y - p3.y * p4.x;
    Point {
        x: (m1 * (p3.x - p4.x) - (p1.x - p2.x) * m2) / d,
        y: (m1 * (p3.y - p4.y) - (p1.y - p2.y) * m2) / d,
        z: p1.z,
    }
}

fn half_step(sp: Point, s_dir: u8) -> Point {
    let (back_x, back_y) = POINT[s_dir as usize % 24];
    Point {
        x: sp.x + back_x as f64 / 8. * DX,
        y: sp.y + back_y as f64 / 24. * DY,
        z: 0.,
    }
}

// Move the point p towards the line described by line_p/line_dir.
// Possibly only in the direction p_dir.
fn moves(line_p: Point, line_dir: u8, p: Point, p_dir: u8) -> (Point, u8) {
    let d = if line_dir > 24 {
        line_dir - 24
    } else {
        line_dir
    };
    let (sx, sy) = STEP[d as usize];
    let dp = Point {
        x: sx as f64 * DX,
        y: sy as f64 * DY,
        z: 0.,
    };
    (
        if p_dir == 255 {
            to_line2d(line_p, dp, p)
        } else {
            let (ox, oy) = POINT[p_dir as usize];
            let mdp = Point {
                x: p.x - ox as f64 * DX,
                y: p.y - oy as f64 * DY / 3.,
                z: 0.,
            };
            intersect2d(
                line_p,
                Point {
                    x: line_p.x + dp.x,
                    y: line_p.y + dp.y,
                    z: 0.,
                },
                p,
                mdp,
            )
        },
        d,
    )
}

fn write(f: &mut File, v: u32) -> Result<()> {
    f.write_all(&[v as u8, (v >> 8) as u8, (v >> 16) as u8, (v >> 24) as u8])
}

// Only on actual filling of the buffers will the values of f64 coordinates be downscaled to f32.
pub struct Vertex {
    position: Point,
    // For now, we calculate average normals
    normal: Point,
    // When the normal should not be average this points to the next vertex on the same position with different normals.
    // Default to u16::MAX when there are no duplicate points.
    // TODO next: u16,
    // Later we need texture positions too
    // texture: Position,
}

impl std::fmt::Debug for Vertex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}/{}", self.position, self.normal)
    }
}

impl Vertex {
    fn new(p: Point) -> Vertex {
        Vertex {
            position: p,
            normal: Point {
                x: 0.,
                y: 0.,
                z: 0.,
            },
            // TODO next: u16::MAX,
        }
    }
}

#[derive(Default, Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct EqPoint {
    pub x: F64,
    pub y: F64,
    pub z: F64, // height
}

impl EqPoint {
    fn new(p: Point) -> EqPoint {
        EqPoint {
            x: F64(p.x),
            y: F64(p.y),
            z: F64(p.z),
        }
    }
}

pub struct Material {
    rgb: (u8, u8, u8),
    metallic: f32,
    roughness: f32,
}

pub struct Mesh {
    pub vertexes: Vec<Vertex>,
    // The three points for a triangle
    indices: Vec<u16>,
    // Each position in this mesh for deduplication
    points: HashMap<EqPoint, u16>,
    // Minimum point inside this mesh
    min: Point,
    // Maximum point inside this mesh
    max: Point,
    // Are there normals?
    normals: bool,
    // Are there texture coordinates, and how many 0-3
    // TODO textures: u8,
    // Are there colors per vertex
    // TODO colors: bool,
    // Future TANGENT for normal mapping textures or MikkTSpace.. forth TANGENT number needed to calc BI-TANGENT
    //     bi-tangent = cross(normal, tangent.xyz) * tangent.w
    // Future possible JOINTS_0 and WEIGHTS_0 for skinning
}

impl std::fmt::Debug for Mesh {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.min, self.max)
    }
}

impl Mesh {
    fn new() -> Mesh {
        Mesh {
            vertexes: Vec::new(),
            indices: Vec::new(),
            points: HashMap::new(),
            min: Point {
                x: f64::MAX,
                y: f64::MAX,
                z: f64::MAX,
            },
            max: Point {
                x: f64::MIN,
                y: f64::MIN,
                z: f64::MIN,
            },
            normals: false,
            // textures: 0,
            // colors: false,
        }
    }

    fn point(&mut self, p: Point) -> u16 {
        self.min.x = if p.x < self.min.x { p.x } else { self.min.x };
        self.min.y = if p.y < self.min.y { p.y } else { self.min.y };
        self.min.z = if p.z < self.min.z { p.z } else { self.min.z };
        self.max.x = if p.x > self.max.x { p.x } else { self.max.x };
        self.max.y = if p.y > self.max.y { p.y } else { self.max.y };
        self.max.z = if p.z > self.max.z { p.z } else { self.max.z };
        *self.points.entry(EqPoint::new(p)).or_insert_with(|| {
            self.vertexes.push(Vertex::new(p));
            self.vertexes.len() as u16 - 1
        })
    }

    fn write(&self, f: &mut File) -> Result<()> {
        for i in &self.indices {
            f.write_all(&i.to_le_bytes())?;
        }
        for v in &self.vertexes {
            f.write_all(&(v.position.x as f32).to_le_bytes())?;
            f.write_all(&(v.position.z as f32).to_le_bytes())?;
            f.write_all(&(-v.position.y as f32).to_le_bytes())?;
            if self.normals {
                let n = v.normal.normalize();
                f.write_all(&(n.x as f32).to_le_bytes())?;
                f.write_all(&(n.z as f32).to_le_bytes())?;
                f.write_all(&(-n.y as f32).to_le_bytes())?;
            }
        }
        Ok(())
    }

    fn stride(&self) -> usize {
        12 + if self.normals { 12 } else { 0 }
    }

    fn length(&self) -> u32 {
        self.indices.len() as u32 * 2 + self.vertexes.len() as u32 * self.stride() as u32
    }

    fn json(&self, json: &mut Value, mat: u32) {
        let mut buf = json
            .index("buffers")
            .index(0)
            .index("byteLength")
            .as_u64()
            .unwrap() as usize;
        let mut acc = json.index_mut("accessors").as_array().unwrap().len();
        let ms = json.index_mut("meshes");
        let mesh = ms.as_array().unwrap().len();
        let arr = ms.as_array_mut().unwrap();
        arr.push(json!({ "primitives": [{
            "attributes": {},
            "indices": acc, // accessor nr
            "mode": 4, // the geometry holds points GL_TRIANGLES=4 (3 index positions needed)
            "material": mat
        }]}));
        acc += 1; // skip the indices accessor
        let m = arr.last_mut().unwrap();
        *m.index_mut("primitives")
            .index_mut(0)
            .index_mut("attributes")
            .index_mut("POSITION") = Value::from(acc);
        acc += 1;
        if self.normals {
            *m.index_mut("primitives")
                .index_mut("attributes")
                .index_mut("NORMAL") = Value::from(acc);
        }
        let views = json.index_mut("bufferViews").as_array_mut().unwrap();
        let index_view = views.len();
        let count = self.vertexes.len();
        let byte_length = self.indices.len() * 2;
        views.push(json!({ // Indices
            "buffer": 0, // The only buffer we have
            "byteOffset": buf, // Needs to be aligned with the size of the corresponding accessor type
            "byteLength": byte_length, // Needs to be a multiple of size of the accessor type
            "target": 0x8893, // GL_ELEMENT_ARRAY_BUFFER
        }));
        buf += byte_length;
        let vertex_view = views.len();
        let byte_length = count * self.stride();
        views.push(json!({ // Vertexes
            "buffer": 0, // The only buffer we have
            "byteOffset": buf, // Needs to be aligned with the size of the corresponding accessor type
            "byteLength": byte_length, // Needs to be a multiple of size of the accessor type
            "byteStride": self.stride(),
            "target": 0x8892, // GL_ARRAY_BUFFER
        }));
        buf += byte_length;
        let acc = json.index_mut("accessors").as_array_mut().unwrap();
        // Add indices buffer
        acc.push(json!({
            "bufferView": index_view,
            "componentType": 5123, // 5120=i8 5121=u8 5122=i16 5123=u16 5126=f32
            "count": self.indices.len(),
            "byteOffset": 0,
            "type": "SCALAR" // SCALAR / VEC2(x,y) / VEC3(x,y,z) / MAT4
        }));
        // Add positions buffer
        acc.push(json!({
            "bufferView": vertex_view,
            "componentType": 5126, // 5120=i8 5121=u8 5122=i16 5123=u16 5126=f32
            "count": count,
            "byteOffset": 0,
            "type": "VEC3", // SCALAR / VEC2(x,y) / VEC3(x,y,z) / MAT4
            "min": [self.min.x, self.min.z, self.min.y],
            "max": [self.max.x, self.max.z, self.max.y]
        }));
        if self.normals {
            acc.push(json!({
                "bufferView": vertex_view,
                "componentType": 5126, // 5120=i8 5121=u8 5122=i16 5123=u16 5126=f32
                "count": count,
                "byteOffset": 12,
                "type": "SCALAR" // SCALAR / VEC2(x,y) / VEC3(x,y,z) / MAT4
            }));
        }
        let node = json.index("nodes").as_array().unwrap().len();
        json.index_mut("nodes").as_array_mut().unwrap().push(json!({
            "mesh": mesh,
        }));
        json.index_mut("scenes")
            .index_mut(0)
            .index_mut("nodes")
            .as_array_mut()
            .unwrap()
            .push(json!(node));
        *json
            .index_mut("buffers")
            .index_mut(0)
            .index_mut("byteLength") = Value::from(buf);
    }
}

// The data for a scene.
pub struct Scene {
    // All rendering items per material
    pub materials: Vec<Material>,
    pub meshes: Vec<HashMap<u32, Mesh>>,
}

impl Scene {
    fn triangle(&mut self, m: u32, a: Point, b: Point, c: Point) {
        if a.z.is_nan() || b.z.is_nan() || c.z.is_nan() {
            panic!("nan triangle m:{m} a:{a} b:{b} c:{c}");
        }
        if self.meshes.is_empty() {
            self.meshes.push(HashMap::new());
        }
        let mesh = self.meshes.len() - 1;
        let m = self
            .meshes
            .get_mut(mesh)
            .unwrap()
            .entry(m)
            .or_insert_with(Mesh::new);
        let i = m.point(a);
        m.indices.push(i);
        let i = m.point(b);
        m.indices.push(i);
        let i = m.point(c);
        m.indices.push(i);
        // TODO calculate normals on the 3 points, add to an already existing value (we normalize later)
    }

    fn mat(&mut self, mats: &mut HashMap<u64, u32>, mat: u64, w_mats: &[Definition]) -> u32 {
        *mats.entry(mat).or_insert_with(|| {
            let g = &w_mats[mat as usize];
            self.materials.push(Material {
                rgb: (g.u8("Red"), g.u8("Green"), g.u8("Blue")),
                metallic: g.f("Metallic", 0.3),
                roughness: g.f("Roughness", 0.7),
            });
            self.materials.len() as u32 - 1
        })
    }

    fn json(&self, name: &str) -> String {
        let mut json = json!({
            "asset": {
                "version": "2.0"
            },
            "scenes": [{
                "name": name,
                "nodes": [0]
            }],
            "scene": 0,
            "nodes": [{
                "camera":0
            }],
            "meshes": [],
            "materials": [],
            "accessors": [],
            "bufferViews": [],
            "buffers": [{
                "byteLength": 0,
            }],
            "cameras": [{
                "type": "perspective",
                "perspective": {
                    "aspectRatio": 1.0,
                    "yfov": 0.7,
                    "zfar": 1000.0,
                    "znear": 0.1,
                }
            }]
        });
        let mats = json.index_mut("materials").as_array_mut().unwrap();
        for mat in &self.materials {
            let (r, g, b) = mat.rgb;
            mats.push(json!({
                "pbrMetallicRoughness": {
                    // The color of this material
                    "baseColorFactor": [ r as f32/255.0, g as f32/255.0, b as f32 / 255.0, 1.0 ],
                    "metallicFactor": mat.metallic,
                    "roughnessFactor": mat.roughness,
                }
            }));
        }
        for ms in &self.meshes {
            for (mat, mesh) in ms {
                mesh.json(&mut json, *mat);
            }
        }
        json.to_string()
    }

    fn length(&self) -> u32 {
        let mut len = 0;
        for ms in &self.meshes {
            for mesh in ms.values() {
                len += mesh.length();
            }
        }
        len
    }

    fn write(&self, f: &mut File) -> Result<()> {
        for ms in &self.meshes {
            for mesh in ms.values() {
                mesh.write(f)?;
            }
        }
        Ok(())
    }
}

fn height_p(ls: &Locations, lay: u8, pos: Position) -> Point {
    let p = pos.point();
    Point {
        x: p.x,
        y: p.y,
        z: if material(ls, lay, pos) == 0 {
            f64::NAN
        } else {
            height(ls, lay, pos) as f64
        },
    }
}

// Move the scene out of the render.
// Render the highest local layer and remember bottom points.
pub fn render(w: &World, ls: &Locations, d: &mut Drawing) -> Scene {
    let mut scene = Scene {
        materials: vec![],
        meshes: vec![],
    };
    let mut wall_mats: HashMap<u64, u32> = HashMap::new();
    let mut ground_mats: HashMap<u64, u32> = HashMap::new();
    for (p, l) in &ls.maps {
        d.clear_loc();
        for lay_pt in l.maps.keys().rev() {
            let lay = *lay_pt;
            fill_drawing(ls, lay, *p, d);
            for x in 0..32 {
                for y in 0..32 {
                    // Write triangles
                    let pp = Position::data(x, y).add(&l.position);
                    let center = height_p(ls, lay, pp);
                    let mat = material(ls, lay, pp);
                    if mat > 0 {
                        let gl_mat = scene.mat(&mut ground_mats, mat, &w.grounds);
                        for base_dir in 0..6 {
                            let pd = Direction::new(base_dir * 4);
                            let wall_mat = wall(ls, lay, pp, pd.plus(2).d);
                            let pos = PointPos::new(&pp, pd);
                            let corner = pos.corner();
                            let mut cur_oth = f64::NAN;
                            let cur_p = if let Some(p) = d.points.get(&corner) {
                                cur_oth = p.other(ls, lay, pp, pd.d);
                                p.height_p(ls, lay, pp, pd.d)
                            } else {
                                points_height(
                                    [
                                        height_p(ls, lay, step(pp, pd.plus(22).d, 1, false)),
                                        height_p(ls, lay, step(pp, pd.plus(2).d, 1, false)),
                                        center,
                                    ],
                                    pos.point(),
                                )
                            };
                            let nd = pd.plus(4);
                            let pos = PointPos::new(&pp, nd);
                            let next_corner = pos.corner();
                            let mut next_oth = f64::NAN;
                            let next_p = if let Some(p) = d.points.get(&next_corner) {
                                next_oth = p.other(ls, lay, pp, nd.d);
                                p.height_p(ls, lay, pp, nd.d)
                            } else {
                                points_height(
                                    [
                                        height_p(ls, lay, step(pp, pd.plus(2).d, 1, false)),
                                        height_p(ls, lay, step(pp, pd.plus(6).d, 1, false)),
                                        center,
                                    ],
                                    pos.point(),
                                )
                            };
                            if cur_p.z.is_nan() || next_p.z.is_nan() {
                                continue;
                            }
                            if wall_mat > 0 {
                                let gl_wal = scene.mat(&mut wall_mats, wall_mat as u64, &w.walls);
                                // Wall towards the own layer
                                if !next_oth.is_nan() && !cur_oth.is_nan() && next_oth < next_p.z {
                                    let cur_lp = Point::new(cur_p.x, cur_p.y, cur_oth);
                                    let next_lp = Point::new(next_p.x, next_p.y, next_oth);
                                    scene.triangle(gl_wal, cur_p, cur_lp, next_p);
                                    scene.triangle(gl_wal, next_p, cur_lp, next_lp);
                                }
                                if next_oth.is_nan() || cur_oth.is_nan() || next_oth < next_p.z {
                                    // Wall towards the next layer
                                    if let (Some(cur_high), Some(next_high)) =
                                        (d.height(corner, cur_p.z), d.height(next_corner, next_p.z))
                                    {
                                        let lay_cur = Point::new(cur_p.x, cur_p.y, cur_high);
                                        let lay_next = Point::new(next_p.x, next_p.y, next_high);
                                        if next_oth < next_p.z {
                                            scene.triangle(gl_wal, next_p, cur_p, lay_cur);
                                            scene.triangle(gl_wal, lay_next, next_p, lay_cur);
                                        } else {
                                            scene.triangle(gl_wal, cur_p, next_p, lay_cur);
                                            scene.triangle(gl_wal, next_p, lay_next, lay_cur);
                                        }
                                    }
                                }
                            }
                            scene.triangle(gl_mat, cur_p, next_p, center);
                        }
                    }
                }
            }
        }
    }
    scene
}

pub fn glb(to_file: &str, name: &str, scene: &Scene) -> Result<()> {
    let mut dt = scene.json(name);
    while dt.len() % 4 != 0 {
        dt += " ";
    }
    let mut f = File::create(to_file)?;
    let length = 20 + dt.len() as u32 + 8 + scene.length();
    write(&mut f, 0x46546C67)?; // magic number "glTF"
    write(&mut f, 2)?; // version 2
    write(&mut f, length)?;
    write(&mut f, dt.len() as u32)?;
    write(&mut f, 0x4E4F534A)?; // magic number "JSON"
    f.write_all(dt.as_bytes())?;
    write(&mut f, scene.length())?;
    write(&mut f, 0x004E4942)?; // magic number "BIN"
    scene.write(&mut f)
}

#[derive(Clone, Copy, Debug)]
struct Walker {
    pos: u16,
    max: u16,
    pattern: u16,
    act: u16,
    looping: bool,
}

impl std::fmt::Display for Walker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.act)
    }
}

impl Walker {
    /// Start at the first full element. Element 0 has an incomplete change field.
    fn new(max: u16, start: u16, pattern: u8, looping: bool) -> Walker {
        Walker {
            // we can walk backwards too
            pos: start,
            max,
            pattern: pattern as u16,
            act: 1,
            looping,
        }
    }

    fn plus(&mut self) -> bool {
        if (!self.looping && self.act == self.max) || self.pos == self.max + self.max + self.pattern
        {
            return false;
        }
        self.pos += 1;
        self.act = if self.act == self.max {
            1
        } else {
            self.act + 1
        };
        true
    }

    fn min(&mut self) -> bool {
        if (!self.looping && self.act == 1) || self.pos + self.pattern == self.max || self.pos == 0
        {
            return false;
        }
        self.pos -= 1;
        self.act = if self.act == 1 {
            self.max
        } else {
            self.act - 1
        };
        true
    }

    fn act(&self) -> u16 {
        self.act
    }

    fn pos(&self) -> u16 {
        self.pos
    }
}

fn claimed(wp: &WallPoint, d: &Drawing, pnr: Walker) -> bool {
    wp.line != u16::MAX
        && d.lines[wp.line as usize].start != pnr.act()
        && d.lines[wp.line as usize].stop != pnr.act()
}

// Try to match the defined patterns.
fn apply_pattern(l: &Locations, lay: u8, max: u16, wv: u8, nr: u16, d: &mut Drawing) {
    // Does the wall loop back on itself?
    let looping = d.walls.get(&(wv, nr, max)).unwrap().pos.point()
        == d.walls.get(&(wv, nr, 0)).unwrap().pos.point();
    // Routine to try to match the pattern on each part of the wall.
    // Extend the pattern both at the end of a match and possibly before
    for p in PATTERNS {
        // perform the complete wall matching here.
        let mut pnr = Walker::new(max, max + 1, p.length, looping);
        while let Some(wp) = d.walls.get(&(wv, nr, pnr.act())) {
            if claimed(wp, d, pnr) {
                if !pnr.plus() {
                    break;
                }
                continue;
            }
            // Try to match the whole pattern
            let mut matched = true;
            let mut stop = pnr;
            let mut start = pnr;
            let s_dir = wp.dir;
            for pm in 0..p.length {
                if pm > 0 && !stop.plus() {
                    matched = false;
                    break;
                }
                let Some(mp) = d.walls.get(&(wv, nr, stop.act())) else {
                    break;
                };
                if claimed(mp, d, stop) || !p.matches(pm, mp.change) {
                    matched = false;
                    break;
                }
            }
            if !matched {
                if !pnr.plus() {
                    break;
                }
                continue;
            }
            // Try to extend the pattern forwards
            let mut pm = 0;
            loop {
                if !stop.plus() {
                    break;
                }
                let Some(mp) = d.walls.get(&(wv, nr, stop.act())) else {
                    break;
                };
                if claimed(mp, d, stop) {
                    stop.min();
                    break;
                }
                if !p.matches(pm, mp.change) {
                    break;
                }
                pm = if pm == p.length - 1 { 0 } else { pm + 1 };
            }
            // Try to extend the pattern backwards
            let mut pm = p.length - 1;
            loop {
                if !start.min() {
                    break;
                }
                let Some(mp) = d.walls.get(&(wv, nr, start.act())) else {
                    break;
                };
                if claimed(mp, d, start) {
                    start.plus();
                    break;
                }
                if !p.matches(pm, mp.change) {
                    break;
                }
                pm = if pm == 0 { p.length - 1 } else { pm - 1 };
            }
            // Change points for the found range
            let mut l_dir;
            let mut step = start;
            let original = wp.pos.point();
            loop {
                let cw = d.walls.get_mut(&(wv, nr, step.act())).unwrap();
                let corner = cw.pos.corner();
                let pt = if let Some(cp) = d.points.get(&corner) {
                    cp.first
                } else {
                    cw.pos.point()
                };
                let (point, dir) = (p.routine)(
                    original,
                    s_dir.d,
                    pt,
                    if cw.line != u16::MAX && cw.line < d.lines.len() as u16 {
                        d.lines[cw.line as usize].dir
                    } else {
                        255
                    },
                );
                cw.line = d.lines.len() as u16;
                let pp = cw.pos;
                d.point(l, lay, pp, point);
                l_dir = dir;
                if step.pos() == stop.pos() || !step.plus() {
                    break;
                }
            }
            d.lines.push(Line {
                mat: wv,
                nr,
                start: start.act(),
                stop: stop.act(),
                dir: l_dir,
            });
            if !pnr.plus() {
                break;
            }
        }
    }
}

// Create a wall point from a step.
// This step indicates a wall and the relation to the past wall.
fn wall_point(s: WallStep) -> WallPoint {
    WallPoint {
        pos: s.pos,
        dir: s.move_dir,
        line: u16::MAX,
        change: s.change,
    }
}

/// TODO also stop this after some steps beyond location boundaries.
pub fn walk_check(
    l: &Locations,
    lay: u8,
    ws: WallStep,
    wv: u8,
    pref_right: bool,
) -> Option<WallStep> {
    let to = ws.do_move();
    let left = ws.step(to, true);
    let lw = wall(l, lay, left.pos.pos, left.wall_dir.d);
    let has_left = lw == wv; // TODO allow for windows and doors
    let right = ws.step(to, false);
    let rw = wall(l, lay, right.pos.pos, right.wall_dir.d);
    let has_right = rw == wv; // TODO allow for windows and doors
    if pref_right & has_right {
        Some(right)
    } else if (pref_right & has_left) || (!pref_right && has_left) {
        Some(left)
    } else if !pref_right && has_right {
        Some(right)
    } else {
        None
    }
}

pub fn set_heights<T>(l: &mut Locations, lay: u8, p: Position, range: i32, mat: u8, to_height: T)
where
    T: Fn(Point) -> f64,
{
    for x in -range..=range {
        for y in -range..=range {
            let a = Position::data(x, y).add(&p);
            a.check();
            if mat != u8::MAX {
                l.validate(a);
                set_material(l, lay, a, mat);
            }
            let to = to_height(Point {
                x: x as f64 * DX,
                y: y as f64 * DY,
                z: height(l, lay, a) as f64,
            });
            if to > 0.0 {
                set_height(l, lay, a, to as u64);
            }
        }
    }
}

fn calc_height(ps: [Point; 3], a: Point) -> f64 {
    let mut l = 0;
    let mut r = 0;
    for p in 1..3 {
        if ps[p].x < ps[l].x {
            l = p;
        } else if ps[p].x > ps[r].x {
            r = p;
        }
    }
    let c = if l == 0 || r == 0 {
        if l == 1 || r == 1 {
            2
        } else {
            1
        }
    } else {
        0
    };
    point_height(ps[l].z, ps[r].z, ps[c], a, ps[c].y < ps[l].y)
}

fn points_height(ps: [Point; 3], a: Point) -> Point {
    Point {
        x: a.x,
        y: a.y,
        z: calc_height(ps, a),
    }
}

/// Calculate the height of a point relative to the heights of the near hexes.
/// `lz` - the height of the hex left of the point.
/// `rz` - the height of the hex right of the point.
/// `p` - the center of the hex above or below the current point
/// `a` - is the actual point were we need a height for. The current z coordinate is ignored.
fn point_height(lz: f64, rz: f64, p: Point, a: Point, above: bool) -> f64 {
    let pz = if p.z.is_nan() {
        if lz.is_nan() {
            rz
        } else {
            lz
        }
    } else {
        p.z
    };
    let lz = if lz.is_nan() { pz } else { lz };
    let rz = if rz.is_nan() { pz } else { rz };
    (a.x - p.x) * (rz - lz) / 2. / DX
        + (a.y - p.y) * (lz + rz - 2. * pz) / if above { -2. } else { 2. } / DY
        + pz
}

#[cfg(test)]
fn p(x: u32, y: u32) -> Point {
    Point {
        x: x as f64,
        y: y as f64,
        z: 0.,
    }
}

#[test]
fn to_line() {
    assert_eq!(p(0, 5), to_line2d(p(0, 0), p(0, 1), p(5, 5)));
    assert_eq!(p(5, 0), to_line2d(p(0, 0), p(1, 0), p(5, 5)));
    assert_eq!(p(5, 5), to_line2d(p(0, 0), p(1, 1), p(6, 4)));
}
