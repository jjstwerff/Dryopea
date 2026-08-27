<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Ground types — the visual + semantic palette

The painted hex layer (see plan 01) classifies each tile by its
**ground type**.  That single value drives THREE things at once:

1. **Visual identity** — the colour rendered on screen.
2. **Slope value** — input to the height-field solver (lib-plan
   20, upstream).  Sea / water variants act as **drainage
   seeds**; land variants raise terrain in proportion to their
   slope.
3. **Material flags** — gating predicates for buildability,
   ground-mob traversal, and rendering (water vs land, walkable
   vs sheer, walkable-by-vehicle vs walkable-on-foot).

A type is the **smallest unit of authored content**; every later
system (slope solver, gridmesh meshing, flow field, build-order
validity, AI traversal cost) reads or extends this enum.  Get the
palette right; everything downstream is cheap.

## Design constraints

- **Three sub-palettes (water 4 + land 5 + structure 2) = 11
  types the player paints.**  Enough variety to paint a
  believable base + fortify it; not so many that the player
  hesitates.  ⚠ A twelfth entry, **`rubble`**, was appended by
  plan 12 B1 and is **not** part of that count: nothing paints
  it, it has no hotkey, and the runtime deposits it (§ 11.
  rubble).  The sub-palette grouping makes the set feel
  structured rather than arbitrary.
- **Each type is visually unambiguous at a glance** — picked so
  no two adjacent rows on the master table look alike, even
  under common colour-blindness profiles (lightness contrast is
  preserved as the redundant channel).
- **Progressions, not catalogues.**  Inside each sub-palette the
  types form a sequence of increasing intensity (water: still →
  flowing → rapids → waterfall; land: sand → grass → hill →
  rock → steep-rock; structure: standard wall → high wall).
  Painting a river, a mountainside, or a base perimeter means
  picking a *direction* through the progression, not memorising
  unrelated cells.
- **Reserve room for sub-materials.**  Some types host a future
  variant overlaid on the same colour family — hills grow
  forests; light-grey rock develops vertical wall faces.  The
  variant is a property *of* the type, not a separate palette
  entry.
- **Some colours are explicit placeholders.**  The structure
  sub-palette (walls) uses **red** as a placeholder — chosen so
  the wall hexes stand out unambiguously against the land /
  water palette during development.  Final colours for walls
  will land later, once the rest of the look is settled; the
  `color_status: "placeholder"` field on those palette entries
  flags them.

## The master palette

| # | Type | Colour (hex) | Sub-palette | Slope | Drainage | Walkable (ground) | Walkable (vehicle) | Buildable | Future variant |
|---|---|---|---|---|---|---|---|---|---|
| 0 | **sea** | `#0a2c5e` (dark blue) | water | — | seed (drop 0) | swim only | yes | no | — (the default; an unpainted world is sea) |
| 1 | **water** | `#2a7ec0` (mid blue) | water | — | seed (drop 1) | swim, slow | yes | no | — |
| 2 | **rapids** | `#6fbce8` (light blue) | water | — | seed (drop 3) | swim, dangerous | yes | no | — |
| 3 | **waterfall** | `#e8f4fc` (near-white) | water | — | seed (drop 8) | no (vertical) | yes | no | — |
| 4 | **sand** | `#f0d860` (bright yellow) | land | 2 | — | yes | yes | yes | (later) palms / decoration |
| 5 | **grass** | `#4caf50` (vibrant green) | land | 6 | — | yes | yes | yes | — |
| 6 | **hill** | `#8b6240` (warm brown) | land | 12 | — | yes (slowed) | yes | yes | **forest** (tree overlay) |
| 7 | **rock** | `#b0b0b0` (light grey) | land | 20 | — | yes (slow, awkward) | yes | yes | **flat wall faces** (gridmesh T4 vertical slope-faces, visually like built walls) |
| 8 | **steep_rock** | `#555555` (dark grey) | land | 40 | — | **no** (impassable) | yes (vehicle hovers) | no | — |
| 9 | **wall** *(placeholder colour)* | `#d04848` (red) | structure | — | — | yes (top is walkable) | yes | no (no stacking) | (later) destructible HP, repair |
| 10 | **wall_high** *(placeholder colour)* | `#7a1818` (dark red) | structure | — | — | yes (top is walkable) | yes | no | (later) destructible HP, more |
| 11 | **rubble** *(placeholder colour, never painted)* | `#8878a8` (violet-grey) | debris | — | — | yes (a heap is walkable) | yes | no | (later) one entry per source material |

> **Notes on the columns**
>
> - **Sub-palette**: the family.  Water types are drainage seeds
>   with a `drop` profile; land types raise terrain with a
>   `slope` value.  No type belongs to both.
> - **Slope** numbers are dimensionless and only relative within
>   the land sub-palette.  Tunable — the values above are the
>   *intended starting point*, not load-bearing constants.
> - **Drainage** seeds set water height = 0 + the cumulative
>   `drop` along the chain.  A river is a chain of water hexes
>   in increasing drop.  ⚠⚠ **The CHAIN is still plan 02's;
>   the LOCAL reading shipped 2026-08-27** (BACKLOG C5,
>   [`../src/moat.loft`](../src/moat.loft)): a water hex sits
>   `drop` metres BELOW the ground around it, one hex at a time,
>   which is what makes a moat a hole.  The two agree wherever a
>   chain is one hex long, and plan 02 grows the term a chain
>   rather than replacing it.
> - **Walkable (ground)** for `steep_rock` is the impassable
>   gate that funnels ground mobs through gaps — the same gate
>   that blocks them from climbing a built wall.
> - **Walkable (vehicle)** is true even for `steep_rock` and
>   `waterfall` because the floating vehicle hovers above
>   terrain; per-agent-type traversal is the design (DESIGN.md
>   § Systems #4).  ⚠⚠ **THIS COLUMN IS READ BY NOTHING**
>   (`@D006`, found 2026-08-27): `passable.loft::can_climb`
>   refuses a step whose either end fails `hex_walkable`, and
>   that answers `walk_ground` for every caller — so the vehicle
>   and the crew are stopped by flat sea exactly as a robot is,
>   and a 3.0 m boost does not cross a 1 m ditch.  The column
>   states the design; the code has never implemented it.
> - **Buildable** is gated by both type and slope.  Towers,
>   walls, and bridges can be placed on land types only, and
>   only where slope is below the build threshold.  `steep_rock`
>   is explicitly excluded.

## The water sub-palette — a progressive river

Water types form a chain by **drop** (how much height the water
loses crossing this hex).  A river is authored as a sequence:

```
sea  ←  water  ←  water  ←  rapids  ←  water  ←  waterfall  ←  water  ←  rapids  ←  ...
 0       1         1         3          1          8            1         3
```

The cumulative drop along the chain is the river's elevation
above sea level — so a river ending in a waterfall is visibly a
*higher* river upstream of the drop than downstream.  Lakes are
**non-connected seas**: a pocket of `water` hexes with no
draining chain back to a `sea` hex (matches the 2023 design seed
in [DESIGN_HISTORY.md](DESIGN_HISTORY.md) § 2).

Structure types use a separate field, `height_override`, which
is an **absolute** height above local terrain (e.g. `3.0 m` for a
standard wall) rather than a slope value integrated from a drain.
Structures sit *on top of* the solved terrain — see § Structure
sub-palette below.

The colour progression — dark blue → mid blue → light blue →
near-white — visualises this physically.  Calmer, deeper water is
darker; faster, aerated water is lighter; falling water is white
foam.

| Drop label | Type | Colour cue | Real-world analogue |
|---|---|---|---|
| 0 | sea | dark blue | open ocean, calm lake |
| 1 | water | mid blue | river, slow stream, lake outlet |
| 3 | rapids | light blue | white-water section, weir, fast stream |
| 8 | waterfall | near-white | waterfall, cataract, plunge |

`drop` values are tunable.  The 1-3-8 spacing keeps the visual
gap between adjacent rows large (so a rapids surrounded by water
is unambiguous) and gives waterfalls genuine vertical drama.

## The land sub-palette — a progressive climb

Land types form a chain by **slope** (how much height each step
gains relative to a drain).  Sand sits flush with the sea; grass
rises gently; hills rise more; rock more still; steep-rock is the
cliff band that walls off areas.

| Slope label | Type | Colour cue | Real-world analogue |
|---|---|---|---|
| 2 | sand | bright yellow | beach, dune, desert |
| 6 | grass | vibrant green | meadow, grassland, gentle rolling hills |
| 12 | hill | warm brown | farmland slope, foothills |
| 20 | rock | light grey | bare rock, alpine, scrub-on-rock |
| 40 | steep_rock | dark grey | cliff, sheer rock face, mountain shoulder |

Painting a mountainside means painting `grass` near the bottom,
ringed by `hill`, ringed by `rock`, capped with `steep_rock`
where the cliff is.  The solver in lib-plan-20 integrates these
slopes into a height field; the gridmesh meshes the result with
slope-faces between hexes of different heights.

## The structure sub-palette — base fortification

Structures are **player-built defences** that share the hex grid
with terrain.  Two entries today, both placeholder-coloured
**red** so they stand out unambiguously during development (the
final colours are not red — see Design constraints).

| Height label | Type | Placeholder colour | Real-world analogue |
|---|---|---|---|
| 3 m | wall | red | concrete barrier, low fortification |
| 5 m | wall_high | dark red | reinforced rampart, higher HP, tougher |

A wall hex sits at `base_terrain_height + height_override` above
the surrounding ground.  Adjacent non-wall hexes are at base
terrain height, so the slope-face between them is *sheer* — the
wall is impassable to ground mobs by the same `md_slope` gate
that blocks them at a cliff.  The top of the wall is walkable
(the vehicle drives along it; ground troops walk it if they can
reach it).

### Wall height matters: insects climb normal walls

The two wall heights are not just visual variety — they
differ in what they actually stop:

| Wall type | Stops regular robots | Stops boss robots | Stops insects (tier 2) |
|---|---|---|---|
| `wall` (3 m) | Yes (sheer face) | No (2×2 footprint forces gaps or break-through) | **No — insects climb it with ease** |
| `wall_high` (5 m) | Yes | No (still 2×2; can break) | **Yes — height is the anti-insect barrier** |

This makes `wall_high` **vital** once tier-2 insects are part
of the threat — a base built only of `wall` is defensible
against the robot tier but completely permeable to insects.
A perimeter that's expected to face insects has to use
`wall_high` on the insect-facing sides (or everywhere).  A
perimeter against robots only stays cheaper with `wall`.

Implication for first-time players in the early maps
(plan 04): the guided road probably faces only robots; the
hidden challenges that introduce insects also introduce the
*reason* to invest in `wall_high`.

### Wall ends are drivable slopes

The signature wall mechanic.  A wall hex with **exactly one wall
neighbour** is a wall END.  The face of the wall hex opposite
that neighbour — i.e. the open end pointing into non-wall
terrain — is rendered as a **ramp**, not a sheer face.  Anything
that can drive up a `hill` (slope ~12) can drive up this ramp,
including the vehicle AND enemy ground units.

Implication for the player: **open wall ends are tactical holes**.
A wall built as a straight line with two unclosed ends lets
enemies *roll up onto* the wall top from either end, walk along
it, and reach the core — completely defeating the wall.  To
actually defend, the player has to close the perimeter:

- **Closed ring** — every wall hex has ≥2 wall neighbours → no
  ends → no ramps → all wall faces are sheer.  Enemies must
  break the wall or use an authored entrance.
- **Linear wall with both ends touching cliffs / `steep_rock` /
  water** — ends are blocked by impassable terrain → same
  effect, no exposed ramp.

Topology summary (computed from the 6-neighbourhood of each
wall hex):

| Wall-neighbour count | Topology | Rendered as |
|---|---|---|
| 0 | isolated wall hex | sheer on 6 sides — a single column.  Ground mobs can't reach it (no ramp, can't climb).  Vehicle can hover up onto it. |
| 1 | **END** | one face is a ramp (the open side); other 4 non-wall faces are sheer. |
| 2 (opposite) | linear middle | sheer on the 4 non-wall sides; walkable along the wall direction. |
| 2 (adjacent) | corner | sheer on the 4 non-wall sides; walkable around the corner. |
| ≥3 | junction | sheer on the (6 - N) non-wall sides; walkable along all wall directions. |

### Entrances — two wall ends near each other

A *deliberate* gap is the only way through a closed perimeter,
and the game recognises this topologically:

> **Two wall ends within 1-2 tiles of each other form an
> ENTRANCE.**

The non-wall hex(es) between the two ends become a recognised
entry point.  The flow-field routes enemies through this gap
preferentially (it's the path of least resistance to the core);
ground troops use it as their natural ingress.

**Why this matters — the player's choice is binary.**

- **Build an entrance on purpose.**  Place two wall ends close
  together but not touching.  Enemies flow through the gap;
  the player concentrates defensive fire on it.  The kill funnel
  is the *point* of the perimeter.
- **Fail to leave one.**  Build a fully closed wall (all walls
  joined, no gaps).  Now the flow field has **no path** to the
  core through any wall.  The enemies don't give up — they
  **start hammering the wall directly**, choosing the nearest /
  weakest section.  The wall's HP becomes the time budget; if
  the player can't sortie out and clear the attackers, the wall
  collapses and the perimeter is breached at the worst possible
  spot.

So the player MUST leave an entrance (and defend it) — or
accept that every wall section is a target.  No middle ground.

**Tolerances.**

- "Within 1-2 tiles" means the shortest hex distance between
  the two wall-END hexes is **1, 2, or 3 hexes** (1 = directly
  adjacent ends, 2 = single-hex gap between ends, 3 = two-hex
  gap between ends).
- A **1-hex gap** = single-hex passage (tight; ground mobs only).
- A **2-hex gap** = vehicle-width passage (~3 m, since hexes are
  ~1.5 m diameter — DESIGN.md § World scale).  This is the
  natural "main gate" size: wide enough for the player vehicle
  and a column of enemies, narrow enough to defend.
- A **3+ hex gap** between ends is NOT recognised as an
  entrance — the gap is too wide to read as designed, and the
  flow-field treats it as plain open ground.  (Practically, two
  ends 4+ hexes apart aren't really "the same wall" — they're
  two short walls in proximity.)

**Editor hint (plan 01 / plan 02 follow-on).**  When the
designer hovers in the editor with the wall paint selected,
highlight any *recognised entrance* in the painted layer so it's
visually obvious which gaps will be entry points and which won't.
This is a quality-of-life follow-on, not part of plan 01 E1-E4.

## Per-type detail

### 0. sea — the default ground

The world's **unpainted background**.  Storage is sparse: a hex
with no entry is sea.  An empty world is therefore an endless
flat sea (see plan 01 § "Sea-default infinite world").  Painting
a hex back to `sea` removes its entry (returns it to the sparse
default).

Colour `#0a2c5e` is a deep, slightly desaturated navy — distinct
from `water` (#2a7ec0) at any zoom level and dark enough to make
a freshly painted land tile pop visually.

### 1. water — the flowing river / lake outlet

Drainage seed with a small drop.  A `water` hex is the workhorse
of the river palette — most river length is `water`, with the
occasional `rapids` or `waterfall` inserted to add drama and
height-step semantics.

A lake is a pool of `water` hexes that doesn't reach back to
`sea` — the solver still treats every hex as drain (height = 0
+ accumulated drop along the chain from the lake centre).

### 2. rapids — the fast section

Higher drop than `water`.  Visually lighter (the surface is
aerated white-water).  A rapids hex marks where the river
shortens height steeply but continuously, as distinct from a
waterfall (a single big drop).

### 3. waterfall — the vertical drop

The drama beat of a river.  Drop = 8, visually near-white foam.
`walkable_ground` is **false** (it's a vertical face), but the
floating vehicle can cross — hovering above is fine.  Future
work: a waterfall has a *direction* (which neighbour is the
upstream water and which is downstream); the solver needs that
to know which side falls.  For plan 01 E1-E4, direction is
inferred from the chain of adjacent water hexes by drop
ordering; an explicit direction-arrow on the type may come later.

### 4. sand — the beach / coast / desert

Slope 2 — almost flat.  Bright yellow `#f0d860`.  Adjacent to a
`sea` or `water` hex, a sand tile reads visually as a beach.
Inland sand reads as desert / dune.  Buildable but limited by
slope: a tower built on sand sits roughly at sea level.

Future variant: coastal decoration (palms, coral, driftwood) —
not part of the type, layered on top via the `h_item` markers.

### 5. grass — the gentle rolling hill / meadow

Slope 6, vivid green.  The default *land* surface.  The natural
build surface for towers near the core; the friendly biome.

The name "grass" in @PLAN46 means "**flowing grass hills**" —
gentle rolling terrain, not flat lawn.  At low slope (6) the
solver produces a soft undulating surface; the green colour reads
as alive and walkable.

### 6. hill — the brown highland

Slope 12, warm brown `#8b6240`.  Higher and steeper than grass;
the bare-soil and rocky-soil look of inland hills before the
treeline.  Ground mobs traverse at higher cost.

**Future variant — forest.**  `hill` is the type that grows
trees: a sub-flag (`forest=true`) overlays tree geometry on the
brown base.  Visually the forest variant is still recognisably
brown underneath (treeline showing the slope) — the trees are
sparse, individual instanced meshes, not a green ground colour.
Adding forests doesn't change `hill`'s slope or buildable status
— it just changes what stands on top (and is destructible
cover for combat).

### 7. rock — the light-grey walkable rock

Slope 20, light grey `#b0b0b0`.  Bare rock terrain — alpine, the
sides of mountains below the cliffs.  Walkable but awkward (high
slope = slow ground mob traversal).  Buildable.

**Future variant — flat wall faces.**  When adjacent rock hexes
differ in height by more than one step, the slope-face between
them is rendered as a **vertical rock wall** rather than a
smooth ramp (gridmesh Phase C T4 auto slope-faces).  This is
visually identical to a built dryopea wall (D1 override layer),
which is intentional: natural rock walls and player-built walls
read as the same kind of barrier, and the solver / gridmesh
treats them with the same machinery.  A staircase of `rock` hexes
with different heights produces a *natural* fortification ring.

This shared idiom is why `rock` is its own type rather than just
"steeper hill": the wall-face rendering attaches to the **rock
material**, not to slope alone.

### 8. steep_rock — the cliff band

Slope 40, dark grey `#555555`.  The impassable barrier type.
Ground mobs **cannot** cross (the same `md_slope` gate that
blocks them at a built wall).  Vehicles can, because they hover.
Not buildable.

A ring of `steep_rock` is the cheapest way to author a natural
fortification — mobs route around it through whatever gap the
designer leaves.  The DESIGN.md flow-field handles this for free
(the field's markers point through gaps without any explicit
gap-finding).

## The debris sub-palette — what the runtime leaves behind

One entry, and it is the only ground type a **player never
paints**.  Plan 12 B1.

### 11. rubble — the heap a broken thing leaves

Violet-grey `#8878a8`, placeholder.  A hex carries rubble when
the **runtime rubble layer** ([`src/height.loft`](../src/height.loft))
has piled metres onto it — a robot that died, an insect that
died, a wall that broke (the last of those is live since plan 12 B2;
the deaths arrive with B4).  It is a ground type so that
passability stays one rule: `can_stand` reads `walk_ground` with
no branch for debris.

Three things about it are load-bearing:

- ⚠⚠ **A pile is a SURFACE only once it clears the water**
  (BACKLOG C5).  The threshold `hex_ground` compares against is
  the hex's own **depth**, not zero: half a metre of debris in a
  metre of water is still water.  On land — and on the `sea`,
  whose drop is 0 — that is the old *any pile at all*, unchanged.
  ⚠ It is what gives the water sub-palette's `drop` a job, and it
  is `MATERIALS.md`'s *"a trench allowed to fill with water"* as
  arithmetic: `water` swallows **two** bodies before it is ground
  again, `waterfall` sixteen.
- **It is a LAYER over the map, never a repaint.**  The painted
  ground underneath is untouched, so clearing a pile restores
  exactly what was authored with nothing to reconstruct.
  `height_override` is therefore `null` — the metres live in the
  layer, and a pile on a `wall` is the wall *plus* the pile
  rather than instead of it.
- **`walk_ground: true` is what dissolves the sea trap.**  Had a
  broken wall erased its painted hex, the breach would have read
  as `sea` — the painted layer is sparse and sea-default — and
  been *less* passable than the wall it replaced.  Rubble on
  water is somewhere to stand.
- **A heap is a ramp when it is low enough**, and that is the
  height rule and not a special case: a robot climbs 2.0 m
  (`src/passable.loft` § Why a robot climbs 2.0 m), so rubble up
  to 2.0 m is walkable and 2.1 m is not.

`extrusion_kind` is **`"heap"`** — a new value under the
append-only policy below, for geometry whose height comes from
the runtime layer rather than from the palette (`pillar` /
`cliff`) or the terrain solver (`flat` / `ramp`).

**One entry, three eventual ones.**  Source material —
wreckage, carapace, masonry — is a closed set with one value per
hex, which is a legitimate ground-type axis; the layer already
stores which, so the split is a palette row and a lookup.  What
a pile *contains* — several salvageable types at once — is open
and multiple per hex, so it is not a ground type at all and
waits for plan 06's stacked layer.

## What the colour does — three jobs in one

This is the @PLAN46 "slope serves three jobs" pattern restated
for the dryopea palette:

1. **Visual identification.**  The player paints a colour;
   reading the world is reading colour.  Yellow = beach, green =
   meadow, brown = hill, grey-light = rock, grey-dark = cliff,
   blues = water in three intensities, near-white = waterfall.
2. **Slope / drop value.**  The same enum drives the upstream
   height solver — sand is gentle, steep_rock is sheer; water is
   sea-level, waterfall has the biggest drop.
3. **Material flags.**  The same enum gates buildability, ground
   traversal, and water rendering.  No separate "walkable" or
   "buildable" layer — those flags are *properties of the type*.

One enum, three downstream consumers, **no duplication**.  A
designer who paints `steep_rock` isn't "making it grey, slow, and
unbuildable" in three places — they're picking one symbol that
*is* all three.

## File format — `examples/palette.json`

The palette is loaded at runtime from a JSON file (NOT
hard-coded in source) so:

- Plan 01 E1 has a concrete loadable.
- Variants and tunings are content edits, not code edits.
- The same loader works for future lib-plan-20 alignment.

The schema (full instance in
[`../examples/palette.json`](../examples/palette.json)):

```json
[
  {
    "name":            "sea",
    "color":           "#0a2c5e",
    "sub_palette":     "water",
    "slope":           null,
    "drop":            0,
    "drainage":        true,
    "walk_ground":     false,
    "walk_vehicle":    true,
    "buildable":       false,
    "extrusion_kind":  "flat",
    "height_override": null,
    "variant":         null
  },
  …
]
```

`slope` is `null` for water types; `drop` is `null` for land
types.  `variant` carries a future sub-material name (`"forest"`
on `hill`, `"wall_face"` on `rock`) once those land.

## Extrusion mapping — 3D consumer contract

The `extrusion_kind` + `height_override` fields are the
**cross-consumer contract** for 3D renderers (dryopea's own
runtime once it lands, plus @PLAN50 bumper-airplanes today).
The dryopea editor authors a 2D top-down map; downstream
consumers read the painted layer + palette and extrude each
hex per its palette type's mapping:

| `extrusion_kind` | Geometry | `height_override` | Examples |
|---|---|---|---|
| `"flat"` | At sea level (y = 0); planes graze the surface | null | `sea`, `water`, `rapids`, `waterfall`, `sand` |
| `"ramp"` | Solver-driven slope per `slope` field; height computed from neighbours by the terrain solver | null | `grass`, `hill`, `rock`, `steep_rock` |
| `"pillar"` | Narrow vertical column; absolute height above local terrain | metres (e.g. `3.0`) | `wall` |
| `"cliff"` | Wider vertical face / shoulder; absolute height above local terrain | metres (e.g. `5.0`) | `wall_high` |
| `"heap"` | Mound above local terrain; height comes from the RUNTIME rubble layer, not from the palette | null | `rubble` |

`height_override` is only read when `extrusion_kind` is
`"pillar"` or `"cliff"` — for `"flat"` and `"ramp"` types,
the height comes from the terrain solver (lib-plan 20), and for
`"heap"` from [`src/height.loft`](../src/height.loft), which is
runtime state and reaches no save file.  See
`src/palette.loft::GroundType` for the in-memory struct that
matches this on-disk shape.

Forward-compat policy: `extrusion_kind` is the discriminant;
new geometry types (e.g. `"bridge"`, `"arch"`, `"tent"`) get
new string values without changing the field shape.  Existing
strings are append-only — never reuse a string with new
semantics.

## Future evolution paths

Each future variant is a sub-flag *on its parent type*, NOT a
new palette entry — keeps the palette to 9 visible swatches in
the editor while letting depth grow.

| Parent | Variant flag | What it adds |
|---|---|---|
| `hill` | `forest` | tree instanced overlay on brown base; cover for combat, destructible |
| `rock` | `wall_face` (a render outcome, not a paint choice) | vertical slope-face rendering between adjacent rocks of different heights — gridmesh T4 |
| `sand` | `palms` (later) | coastal vegetation |
| `grass` | `tall_grass` (later) | line-of-sight cover variant |
| `water` | `current_direction` (later) | a vector hint for boats / mob behaviour |
| `waterfall` | `direction` (later) | explicit "this side falls" to disambiguate solver |

Variants land **after** the base palette is implemented and
playable — explicitly deferred until the substrate works.

## Open questions

1. **Boundary rendering.**  How sand-to-grass, water-to-sand,
   rock-to-steep-rock transitions render — sharp hex edges, or
   blended via gridmesh slope-face geometry?  Lean **sharp** for
   plan 01 E1-E4 (it's an editor, not a beauty pass);
   blending lands with lib-plan-19 Phase C meshing.
2. **Lake vs sea detection.**  A pool of `water` not chained to
   `sea` is a lake.  Does the solver need an explicit "lake"
   type, or is "water without a sea path" enough?  Lean **no
   explicit type** — the chain topology decides.
3. **Multi-cell waterfall.**  Some waterfalls drop multiple
   hexes vertically.  Is a 3-hex waterfall three `waterfall`
   tiles in a column?  Yes — each waterfall hex adds `drop=8`
   to the cumulative chain.
4. **Forest vs grass overlap.**  Should a forest variant of
   `hill` look closer to green to read as "forested"?  Lean
   **no** — keep `hill` brown; let the tree instances do the
   green.  Otherwise forest reads as a different hex type, not a
   variant of an existing one.
5. **Editor visibility of slope / drop values.**  When the
   player hovers a hex, show its slope / drop in the HUD?
   Useful for debugging the solver; possibly noise in normal
   play.  Lean **toggle with a hotkey**.
6. **Palette extensibility.**  Should the loader accept *extra*
   types in `palette.json` beyond the canonical 9?  Lean **yes**
   — the loader is data-driven, not hard-wired; experimental
   sub-mods can add types without code changes.  Plan 01 E2's
   palette UI reads whatever it finds.

## See also

- [`DESIGN.md`](DESIGN.md) — master design; § "Editor / game
  split" + § "Systems #4 (multi-level pathing)" use this
  palette's slope + walkable flags.
- [`../plans/01-ground-editor/README.md`](../plans/01-ground-editor/README.md)
  — the first plan; phases E1-E4 turn this palette into a
  playable painting surface.
- [`../examples/palette.json`](../examples/palette.json) — the
  loadable form of this palette.
- [`../examples/terrain.txt`](../examples/terrain.txt) — the
  2023 prototype's earlier 6-entry palette (preserved for
  historical reference; superseded by `palette.json`).
- [`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) § 2 — the 2023 hex /
  terrain design notes that seed several of these ideas
  (rivers-encode-descent, lakes-as-non-connected-seas,
  steep-sides-into-rock-faces).
