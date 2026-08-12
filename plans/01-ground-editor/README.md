<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 01 — In-game ground-type editor

**Status:** Active.  Implementation under way.

## Implementation status (2026-05-27)

| Phase | What shipped | Notes |
|---|---|---|
| **E1** — Infinite sea + camera | shipped | `src/world.loft`, `src/camera.loft`, `src/render.loft`; 20 tests in `tests/01_e1_camera.loft` + 6 golden-image tests in `tests/01_e1_render.loft` |
| **E2** — Palette load + named picker | shipped | `src/palette.loft`, `src/picker.loft`; 18 tests; loads `examples/palette.json` via `text as vector<GroundType>` cast |
| **E3** — Paint sparse + mouse picking | shipped | `paint_line` (hex line walk), `screen_to_hex` (mouse-pick inverse), `render_with_hover` (outline overlay); 16 tests |
| **E4** — Save/load MapFile | shipped (constrained) | `src/map_file.loft`, `src/save.loft`; 9 round-trip tests; **MapFile schema limited to 6 fields** to dodge a loft JSON-cast hang at ≥8 fields (filed in `QUESTIONS_FOR_LOFT.md`).  Plan 04 § L1 will expand once loft fix lands. |
| **Integration smoke test** | shipped | `tests/01_integration.loft` — cold-start → paint → save → reload, 3 tests, 72/72 total green under `scripts/test.sh` |
| **E1-live** — Interactive GL editor | shipped (parse-clean) | `src/main.loft` — opens GL window, WASD pan, mouse paint, 1-0 picker, Ctrl+S save, Esc save-exit.  Compiles cleanly under `loft --native-emit`; **human playtest pending** |

The original phase write-ups below remain the source of
truth for design intent; deviations from them (the MapFile
constraint above, the deferred `markers` / `waves` /
`objective` fields, the missing `description` field, the
KEY_* qualification quirk in main.loft) are documented at
their use sites + in `QUESTIONS_FOR_LOFT.md`.

Plan 01 transitions to `finished/` once the human playtest
confirms WASD + click-paint + Ctrl+S + reload cycle is
satisfying in a real window — i.e. once it stops being a
"build sequence" and becomes a "this works" artifact.

## Goal

A minimal **in-game** editor that lets the player paint hex tiles
with ground types from a small palette. The painted hex grid is the
SUBSTRATE every later dryopea system (structures, flow-field
pathing, combat, scramble) builds on top of via a runtime override
layer — see the master design at
[`../../../docs/DESIGN.md`](../../../docs/DESIGN.md).

This is the **first** dryopea plan because every other plan assumes
"a level exists." This one creates that level.

## Two load-bearing design choices

### 1. In-game, not a separate editor

The editor is a **mode of the dryopea executable**, sharing the
same render pipeline, input system, camera, and hex math as the
running game. There is no separate editor binary.

Why:
- One executable → one render path to maintain → no
  editor-vs-game drift (the moros project has historically had
  to keep its editor and game renderers in sync; dryopea avoids
  that by construction).
- "Level designer" is just a privileged player mode. Same code
  draws a base in-edit and in-play; the only delta is which
  input gestures are wired up.
- The player can adjust terrain **between runs** without leaving
  the game — dovetails with the scramble + run meta-loop
  (DESIGN.md § scramble): tweak the next base while reviewing
  what you carried over.

### 2. Sea-default infinite world (sparse storage)

The default ground type is **sea (dark blue)** and the world is
conceptually **infinite** — the camera can pan in any direction
forever and always renders flat sea.

Storage is **sparse**: a hex only takes memory once it has been
painted to something OTHER than sea. Un-painted hexes are sea by
implicit default; reading them returns the sea palette entry
without touching memory.

Why:
- Matches the lib-plan-19 gridmesh world model
  (`hash<Chunk[cx,cy,cz]>` sparse outer, dense inner), so the
  editor naturally scales into the same data layout the game
  consumes.
- A blank world is "the sea before land was raised" — paint =
  reveal land. This dovetails with lib-plan-20's drainage-seed
  semantics: sea pins height = 0, painting hills/mountains
  raises terrain *out of* sea.
- An infinite grid frees the editor from choosing a world size
  upfront. Bases grow organically.

## Scope

In:
- A FLAT (single `cy`-layer) hex world. Camera 2D top-down for
  E1-E4; 3D extrusion + 3rd-person camera deferred to
  [lib-plan 19](https://github.com/jjstwerff/loft/blob/main/doc/claude/lib_plans/19-gridmesh/README.md)
  Phase C.
- A ground-type palette read from
  [`../../../examples/terrain.txt`](../../../examples/terrain.txt):
  `{name, color, slope}` per type. Sea is one entry — the default.
- Sparse painted-hex storage: `hash<GroundType[q,r]>`. Miss =
  sea.
- Hover-to-highlight; click to paint a hex; click-and-drag to
  paint a line.
- Camera pan (drag or WASD) over the infinite world.
- Save / load to disk; reload on launch resumes the world.

Out of scope (later plans):
- Slope-from-painted-type solver — lib-plan 20.
- Multi-layer (`cy`) editing — lib-plan 20 + dryopea D1.
- 3D extrusion / gridmesh meshing — lib-plan 19 Phase C.
- Built structures (walls / towers / bridges) — dryopea D1.
- Combat, waves, scramble, run meta — D3-D5.

## Phases

| # | Scope | Proves |
|---|---|---|
| **E1** | **Infinite sea + camera.** Render an endless 2D hex grid where every visible hex is sea (dark blue). Camera pans freely (drag or WASD). NO data, NO palette UI, NO paint. Just the empty world + a moving viewport. | The frustum→visible-hexes enumeration. The "miss = sea" path. The render pipeline against `lib/graphics`. |
| **E2** | **Palette load + named picker.** Parse [`examples/palette.json`](../../../examples/palette.json) at startup; render a **named swatch picker** on screen (each entry = colour swatch + the type's name, vertically stacked); click or hotkey selects the active type. Still no painting — just inspection.  Sketch + details below. | Palette file format. Basic 2D UI primitives (swatch, label text, hover-state). The picker is the player's primary *named* interface to the palette — colour alone is not enough. |
| **E3** | **Paint sparse.** Click a hex → write `(q,r) → active_type` into the sparse `hash<GroundType[q,r]>`. Drag → paint a line. Hover → highlight. Render: each visible hex looks up in the sparse map; miss = sea. | Picking (screen→hex). Sparse mutation. The data model the rest of dryopea inherits. |
| **E4** | **Save / load.** JSON file: list of `{q, r, type_name}` entries; load on launch repopulates the sparse map. | Persistence file format — the seed of dryopea's level format. |

E1 is the foundation everything else stacks on; E2-E4 each add
one capability on top.

## Implementation + testing

Each phase produces a **standalone runnable** that exercises
exactly its added capability.  Earlier-phase tests must keep
passing when later phases land — no regressions allowed.

### Testability discipline (built-in from the bottom up)

dryopea follows the **moros pattern** (see `lib/moros_*/tests/*.loft`
in the loft repo as the canonical reference):

- **No global state.**  Every state struct (world, camera,
  editor, game) is constructed by a factory function and
  passed explicitly.  Examples: `editor_default()`,
  `input_empty()`, `world_empty()`, `map_empty()` — pure
  constructors, never side-effects.
- **Pure functional ticks.**  Per-frame logic lives in
  `tick(state, input, dt)` functions.  Tests call the tick
  directly with synthesised input.  No render, no event
  loop, no I/O required for the test to assert on state
  transitions.
- **Input is a data struct, not a callback.**  Tests
  construct an `Input` struct (`in_tool_number = 2`,
  `in_key_q_pressed = true`) and pass it; the tick reads it
  like any other parameter.  No keyboard-listener stub
  required.
- **Direct field assertions.**  Tests read state struct
  fields after a tick and `assert(...)` on them.  No HUD
  scraping, no log parsing.

Net: every module is testable **in isolation** without any
graphics / OS / time / IO context.  A full integration test
(`05_ms5_wave1.loft`) is just a sequence of ticks fed
synthesised inputs; the renderer is *attached* in the real
game but never required by a test.

Cite this pattern as the **testability spine** of every plan;
every `fn test_*()` in `tests/scripts/*.loft` follows it.

### Save format — one format for editor and game

The editor will **eventually edit every map parameter the
game needs**, so the editor's save and the game's load
**share one format** — the `MapFile` defined in
[`plan 04 § L1`](../04-map-library/README.md#phase-l1--map-file-format).
Plan 01 E4 writes a `MapFile`, even if early phases only
populate a subset of its fields (just `ground` + `camera` for
E4; `markers`, `waves`, `objective` etc. left empty / default
until the relevant phase fills them in).

Net: there is **no separate "editor save format" vs "game
map format."**  Both phases write and read the same JSON
schema; later editor phases add fields that earlier game
phases simply ignore as default.

### Phase E1 — Infinite sea + camera

**Files**

| File | Purpose |
|---|---|
| `src/world.loft` | World-coordinate primitives.  `Hex { q: i32, r: i32 }`, conversions to/from screen / world space. |
| `src/camera.loft` | `Camera { pos: Hex, zoom: i32 }` + update functions. |
| `src/render.loft` | Visible-hex enumeration + draw of flat sea hexes. |
| `src/main.loft` | Entry point — open GL window, init camera, frame loop. |
| `loft.toml` | Adds `graphics = "..."` (loft GL bindings; path-dep to loft monorepo until loft-libs-graphics ships). |

**Data**

```loft
struct Hex { q: integer, r: integer }
struct Camera { pos: Hex, zoom: integer }

const SEA_COLOR: integer = 0x0a2c5e   // from palette.json
const HAZE_RADIUS_HEXES: integer = 40 // numbers.json
```

**Key functions**

- `fn hex_to_world(h: Hex) -> (single, single)` — axial flat-top
  conversion.  `x = hex_diameter * 1.5 * q`,
  `y = hex_diameter * sqrt(3) * (r + q/2)` (loft `single` =
  f32).
- `fn visible_hexes(c: &Camera, haze: integer) -> vector<Hex>`
  — returns the set of hex coordinates within `haze` of the
  camera centre.  Used as the draw list.
- `fn camera_update(c: &mut Camera, keys: vector<text>)` —
  apply WASD pan; clamp zoom.
- `fn render_frame(c: &Camera)` — clear screen, emit each
  visible hex as a flat coloured quad at SEA_COLOR.

**Test — `tests/scripts/01_e1_camera.loft`**

```loft
// Camera pan
let c = Camera { pos: Hex { q: 0, r: 0 }, zoom: 1 }
camera_update(&mut c, ["W"])
assert c.pos.r == -1   // W moves "north" in axial flat-top

camera_update(&mut c, ["S", "S"])
assert c.pos.r == 1

// Visible-hex enumeration: a haze=2 disc around origin
let v = visible_hexes(&c, 2)
// Hex disc radius 2 = 1 + 6 + 12 = 19 hexes (centre + 1-ring + 2-ring)
assert v.len() == 19

// hex_to_world sanity
let (x, y) = hex_to_world(Hex { q: 1, r: 0 })
assert (x - 1.5 * 1.5).abs() < 0.001  // q=1 shifts world.x by 1.5 * hex_diameter
```

**Pass criteria**

- Running `loft run src/main.loft` opens a window showing
  a uniform dark-blue sea filling the viewport.
- WASD pans the camera smoothly; scroll zooms.
- No allocation grows over time (the world is sparse +
  unpainted; the only allocation is the visible-hex list
  re-emitted per frame).
- The test script passes under `loft test`.

### Phase E2 — Palette load + named picker

**Files added/modified**

| File | Purpose |
|---|---|
| `src/palette.loft` | `GroundType` struct + JSON loader. |
| `src/ui.loft` | 2D HUD primitives: text + filled rect + hover detection. |
| `src/picker.loft` | Picker widget: ordered list of swatches + names + hotkey hints. |
| `src/main.loft` | Init: load palette from `examples/palette.json`; pass to picker. |

**Data**

```loft
struct GroundType {
    name: text,
    color: integer,
    sub_palette: text,
    slope: integer,           // null mapped to -1 (water + structure)
    drop: integer,            // null mapped to -1 (land + structure)
    drainage: boolean,
    walk_ground: boolean,
    walk_vehicle: boolean,
    buildable: boolean
}

let PALETTE: vector<GroundType> = load_palette("examples/palette.json")
let active_index: integer = 0   // index into PALETTE; 0 = sea
```

**Key functions**

- `fn load_palette(path: text) -> vector<GroundType>` —
  parse JSON; assert exactly 11 entries; assert all names
  unique.
- `fn picker_render(p: &vector<GroundType>, active: integer)`
  — draws the vertical list of swatch + name + hotkey, with
  the active row highlighted.
- `fn picker_handle_input(p: &vector<GroundType>, keys: vector<text>) -> integer`
  — `1` through `0` then `-` select entry `1..=11`; click on
  a row selects.  Returns the new active index (or current
  if no change).

**Test — `tests/scripts/01_e2_palette.loft`**

```loft
let p = load_palette("examples/palette.json")
assert p.len() == 11
assert p[0].name == "sea"
assert p[0].color == 0x0a2c5e
assert p[4].name == "sand"
assert p[4].buildable
assert !p[0].walk_ground       // sea is not walkable by ground units
assert p[9].name == "wall"
assert p[10].name == "wall_high"

// Hotkey -> index
let active = 0
let active2 = picker_handle_input(&p, ["5"])
assert active2 == 4            // "5" -> index 4 = sand
```

**Pass criteria**

- The picker is drawn over the sea floor; 11 rows are
  visible; the active row is highlighted.
- Hotkeys `1`-`0`, `-` switch the active row.
- The test script passes.
- E1 still passes (the camera works underneath the HUD).

### Phase E3 — Paint sparse

**Files added/modified**

| File | Purpose |
|---|---|
| `src/painted.loft` | `painted: hash<u8[q,r]>` + paint/lookup. |
| `src/picking.loft` | screen→hex conversion (mouse cursor + click). |
| `src/render.loft` | Modify: per-visible-hex, look up `painted`; miss = sea colour, hit = `PALETTE[idx].color`. |
| `src/main.loft` | Wire mouse click to paint; mouse drag to paint-along-line. |

**Data**

```loft
// The world: sparse painted-hex map.  loft hashes can carry
// multiple key fields directly — DESIGN.md's hash<GroundType[q,r]>
// intent maps to the form below.  Default (miss) = sea, never
// explicitly stored — sparse storage.
pub struct PaintedHex {
    q: integer not null,
    r: integer not null,
    type: u8 not null         // index into PALETTE; 0 = sea
}

let painted: hash<PaintedHex[q, r]> = []
```

**Key functions**

- `fn paint(world: &mut hash<PaintedHex[q, r]>, q: integer, r: integer, idx: u8)`
  — write a `PaintedHex{q, r, type: idx}` record.  If `idx == 0`
  (sea), **remove** the entry (sparse storage; sea is implicit).
- `fn lookup(world: &hash<PaintedHex[q, r]>, q: integer, r: integer) -> u8`
  — return the matching record's `type` or 0 (sea) on miss.
- `fn screen_to_hex(c: &Camera, sx: integer, sy: integer) -> Hex`
  — inverse of `hex_to_world`, used for mouse picking.
- `fn paint_line(world: &mut hash<PaintedHex[q, r]>, a: Hex, b: Hex, idx: u8)`
  — hex-grid line for drag-paint.

**Test — `tests/scripts/01_e3_paint.loft`**

```loft
let world: hash<PaintedHex[q, r]> = []

// Paint a hex
paint(&mut world, 0, 0, 5)     // grass at origin
assert lookup(&world, 0, 0) == 5
assert lookup(&world, 1, 0) == 0   // adjacent is still sea
assert world.len() == 1

// Re-paint = overwrite
paint(&mut world, 0, 0, 7)
assert lookup(&world, 0, 0) == 7
assert world.len() == 1            // size unchanged

// Erase by painting sea
paint(&mut world, 0, 0, 0)
assert lookup(&world, 0, 0) == 0
assert world.len() == 0            // entry removed; sparse

// Paint a line
paint_line(&mut world, Hex { q: 0, r: 0 }, Hex { q: 4, r: 0 }, 9)  // wall
assert world.len() == 5
for q in 0..=4 { assert lookup(&world, q, 0) == 9 }
```

**Pass criteria**

- Clicking a hex paints it with the active palette entry's
  colour; the change is visible on the next frame.
- Click-and-drag paints a line of hexes.
- Painting the "sea" entry erases (visibly removes painted
  hexes; sparse storage shrinks).
- The test script passes.
- E1 + E2 still pass.

### Phase E4 — Save / load (writes a MapFile)

**Files added/modified**

| File | Purpose |
|---|---|
| `src/map_file.loft` | The single `MapFile` struct used by editor + game (see plan 04 § L1 for the full schema). |
| `src/save.loft` | `MapFile` ↔ JSON. |
| `src/main.loft` | Save on `Ctrl+S` (writes a `MapFile`); load on startup if a save exists. |

**Format — `MapFile` per plan 04 § L1.**  E4 writes the same
JSON the game later reads.  Early-phase saves populate just
`ground` + `camera`; later phases (markers, waves, objective,
play_area) fill in additional fields as they land.  Existing
loaders ignore unknown fields and default missing ones — no
parallel "editor save" exists.

**Key functions** (the MapFile API is shared with plan 04):

- `fn save_map_file(m: &MapFile, path: text)` — write JSON;
  entries sorted (`(q,r)` for `ground` and `markers`) for
  deterministic output.
- `fn load_map_file(path: text, palette: &vector<GroundType>) -> MapFile`
  — read JSON; resolve `type` names → palette indices;
  default missing fields.
- `fn paint_to_mapfile(world: &hash<PaintedHex[q, r]>, c: &Camera) -> MapFile`
  — collect the painted-layer + camera into a `MapFile` with
  everything else empty/default (E4 callsite).

**Test — `tests/scripts/01_e4_saveload.loft`**

```loft
let palette = load_palette("examples/palette.json")

// Build a small world
let w: hash<PaintedHex[q, r]> = []
paint(&mut w, 0, 0, 5)   // grass
paint(&mut w, 1, 0, 9)   // wall
paint(&mut w, 2, 0, 9)   // wall

let c = Camera { pos: Hex { q: 0, r: 0 }, zoom: 1 }
let mf = paint_to_mapfile(&w, &c)

save_map_file(&mf, "/tmp/dryopea_test.json")

let mf2 = load_map_file("/tmp/dryopea_test.json", &palette)
assert mf2.ground.len() == 3
// Reconstruct the hash from the MapFile's ground array
let w2 = mapfile_to_painted(&mf2, &palette)
assert lookup(&w2, 0, 0) == 5
assert lookup(&w2, 1, 0) == 9
assert lookup(&w2, 2, 0) == 9
assert mf2.camera.q == 0 && mf2.camera.r == 0

// Fields not yet populated default sensibly
assert mf2.markers.len() == 0
assert mf2.waves.len() == 0
assert mf2.objective == "survive_waves"   // default
```

**Pass criteria**

- Painting + saving + restarting + loading reproduces the
  same painted state exactly.
- The save file is human-readable JSON; entries can be
  edited by hand and reloaded.
- `version` field present for future format-evolution
  handling.
- The test script passes.
- E1-E3 still pass.

## Integration smoke test

After all four phases:

`tests/scripts/01_integration.loft`:

```loft
// Cold-start cycle: load palette + last save, paint, save, exit.
let palette = load_palette("examples/palette.json")
let (mut world, mut cam) = load_map_or_empty("saves/last.json", &palette)

paint(&mut world, 5, 5, 6)   // hill
save_map(&world, &cam, "saves/last.json")

// Re-load to confirm persistence
let (w2, c2) = load_map("saves/last.json", &palette)
assert lookup(&w2, 5, 5) == 6
```

A human playtest after the script passes:

1. Launch the editor; see endless sea.
2. Press 6 (grass), click a hex — it turns green.
3. Press 9 (wall), click-and-drag a line — red outlines
   appear (E3 doesn't have construction-state visuals; outlines
   are immediate solid fills in editor mode).
4. Ctrl-S, close, re-launch — same view restored.

If all of that works, plan 01 is **done** for the editor's
contribution to plan 05's validation scenario.

### E2 — named picker UI sketch

The palette UI is a **vertical list of named swatches**, one
row per palette entry from
[`examples/palette.json`](../../../examples/palette.json).  Each
row is a coloured swatch + the type's name + a hotkey hint;
clicking a row or pressing the hotkey selects that type as
active for painting.  The active row gets a visible highlight
(e.g. brighter border or a leading arrow).

```
┌───────────────────────────────┐
│ Palette                       │
├───────────────────────────────┤
│ ▶ ■ sea          [1]          │  ← active (highlighted)
│   ■ water        [2]          │
│   ■ rapids       [3]          │
│   ■ waterfall    [4]          │
│   ■ sand         [5]          │
│   ■ grass        [6]          │
│   ■ hill         [7]          │
│   ■ rock         [8]          │
│   ■ steep_rock   [9]          │
│   ■ wall         [0]          │  ← placeholder colour, see GROUND_TYPES.md
│   ■ wall_high    [-]          │  ← placeholder colour, see GROUND_TYPES.md
└───────────────────────────────┘
```

Why a named picker, not colour-swatch-only:

- Some palette colours are deliberately **placeholders** (the
  red walls — see `color_status: "placeholder"` on those entries
  in `palette.json`).  A picker that only shows colour gives the
  player no name for "the red one"; with the name, the player
  reads `wall_high` and the placeholder colour stops being
  load-bearing.
- The picker IS the palette's UI vocabulary.  Names appear in
  the inspection HUD (plan 02 V5), in save files (E4), and in
  later structures spec.  The picker exposes the same names so
  the player learns them naturally.
- Hotkeys `1` through `0` then `-` cover the 11 entries; click
  still works for mouse-first users.

Open Q (E2-internal): horizontal vs vertical layout (vertical
read better for ≥10 entries); sub-palette headers (e.g. a
divider line + "water" / "land" / "structure" group label
between sections) — lean **yes**, makes the palette structure
visible.  Make in E2; cheap to revisit.

## Open questions

1. **Hex layout** — axial flat-top (matches moros) or offset
   pointy-top (matches `audience_crystal` / the 2023 proto)?
   Pick one before E1 and record the reasoning.
   **Lean axial flat-top** for future moros-data interop.
2. **Camera input mapping.** WASD pan, mouse-drag pan, scroll zoom
   — confirm before E1. Lock to a single convention so E2-E4
   don't have to support multiple.
3. **Palette file format.** Inline JSON inside `terrain.txt` (the
   2023 prototype already uses `{ name = "...", color = ..., slope = N }`
   — close to a loft struct literal) vs proper JSON `[{...}]`.
   Lean **proper JSON** so the loader is just `text as
   vector<GroundType>`. The 2023 file becomes the input shape, with
   a one-time conversion.
4. **In-game UI primitives.** Survey `lib/graphics` and the
   audience-demo projector's HUD pattern before E2 — if there is
   no shipped 2D-UI lib, the swatch picker may need a minimal
   helper inside dryopea. Note any gap as a P-issue in loft.
5. **Save format scaling.** JSON is fine for a hundred painted
   hexes; revisit when a base reaches ~10 000 painted hexes
   (binary or a custom format). E4's JSON is intentionally cheap
   and replaceable.

## Dependencies

- **loft** at a current version with `lib/graphics` GL bindings +
  a working game-loop. (Already shipped — both the audience demo
  and tic-tac-toe exercise these.)
- **No upstream lib-plan dependency** for E1-E4. The editor ships
  while terrain-heightmap and gridmesh Phase C remain on the
  design board. It will hand its painted grid up to those plans
  when they land.

## Why this plan is foundational

- **Owns the substrate.** Every dryopea phase from D0 onward
  assumes "a painted level exists." E1-E4 provide that.
- **Smallest dryopea-only scope** with no upstream blocks.
- **Exercises just enough of the eventual stack** (render, input,
  pan camera, sparse storage, save/load) to surface gaps in loft
  before bigger plans hit them — the dogfood loop applied
  immediately.
- **Sets the data shape.** The sparse `hash<GroundType[q,r]>` here
  is what the later override layer (D1 structures), flow field
  (D2 enemy guidance), and slope solver (lib-plan 20) all read or
  extend. Getting the shape right early avoids churn later.

## See also

- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md) — full
  master design.
- [`../../../examples/terrain.txt`](../../../examples/terrain.txt)
  — the palette seed (grass, hill, mountain, sea, sand, forest).
- **Upstream library plans** (gridmesh meshing + terrain
  height-map solver) — currently drafted in the loft tracker,
  expected to migrate to their own repos when loft drops
  outside-project references. The current placeholder URLs are
  recorded in [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md)
  § See also and will be updated once the libraries move.
