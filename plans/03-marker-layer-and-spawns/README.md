<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 03 — Marker layer + multi-direction spawn points

**Status:** **Shipped** 2026-05-27 — phases M1-M5 all landed,
129/129 tests green.

## Implementation status

| Phase | What shipped | Commit |
|---|---|---|
| **M1** | `src/markers.loft` (sparse `hash<MarkerEntry[q, r]>` + `place_spawn`/`remove_marker`/`has_marker`/`marker_kind`/`marker_direction`/`marker_count`); `src/marker_file.loft` (sidecar `MarkerFile` schema); `src/save.loft` extended with `save_markers`/`load_markers_or_empty` — the marker world is a SIDECAR JSON file (`dryopea_save_markers.json`), kept separate from MapFile to dodge the 8-field cast bug.  11 tests. | `d8f311c` |
| **M2** | `src/editor_mode.loft` (`MODE_GROUND`/`MODE_MARKER` + `toggle_mode` + `mode_name` + `render_mode_indicator` HUD badge — grass-green / hot pink); Tab edge-detected toggle in `src/main.loft`.  10 tests. | `d87d202` |
| **M3** | `rotate_direction` + `rotate_marker_at` in markers.loft; new `src/marker_render.loft` (`draw_marker_arrow` + `render_markers` + `render_ghost_arrow` — hot-pink triangles oriented per direction); editor wiring in `main.loft` for marker-mode click (place/remove toggle), R / Shift+R direction rotation, ghost-arrow hover preview, marker save on Ctrl+S + exit, marker load on startup.  Surfaced two loft bugs (filed in QUESTIONS_FOR_LOFT.md): `vector<Struct>` with trailing `u8 not null` fields corrupts on `:j` save; parser rejects `(tuple_local.N as float)`.  Workaround for the first: separate on-disk `MarkerSaveEntry` (integer fields) ↔ in-memory `MarkerEntry` (u8) — mirrors painted-layer's `PaintedHex`/`GroundEntry` pattern.  15 tests. | `6afe68e` |
| **M4** | In-game render overlay — covered by M3's `render_markers` integration in `main.loft`'s frame loop.  Markers paint on top of the painted layer, ground colour bleeds through (filled triangles don't fully occlude).  No separate phase commit; visual verification at extreme zoom is a human-eye test. | (in `6afe68e`) |
| **M5** | `src/spawn.loft` — `Enemy` + `WaveState` + `wave_state_empty` + `hex_offset` (6 axial neighbours matching the M3 direction convention) + `active_spawn_markers` (close-disable filter) + `spawn_wave` (round-robin pick across active markers, heading = marker direction) + `enemy_tick` (approach-mode one-hex-per-tick walk) + `wave_tick` + `alive_count`.  No RNG dependency — loft hasn't exposed a user-callable RNG yet; round-robin produces multi-direction pressure deterministically.  21 tests. | `afc3a79` |

The editor (M2-M3) and the wave engine (M5) are decoupled: the
editor authors the marker layer + persists the sidecar; the
runtime (future game) reads the same marker layer to drive
spawns.  Plan 03 does NOT wire the wave engine into `main.loft`
— `main.loft` is the editor, not a game runtime; the wave
engine consumer lives in plans 04-05 (the validation scenario).

**Status original (preserved for posterity):** Future (design drafted 2026-05-26; no code).

## Goal

A **second sparse data layer** parallel to the painted ground
palette (plan 01), holding **markers** — placed entities the
*game runtime* reads but the painted palette doesn't represent.
The first marker type is the **spawn point**, with these
properties:

- **Multiple spawn points per base, not one.**  Enemies should
  not always arrive from the same direction; the designer
  places several spawns around the perimeter (or beyond it) so
  pressure can come from multiple sides at once.
- **Each spawn has a rough approach direction.**  A spawn point
  is not just a hex coordinate; it's a hex + one of the **6 hex
  cardinal directions** indicating which way enemies are
  *approaching from*.  Enemies appear at (or just outside) the
  spawn hex and initially head along that direction, which only
  later resolves into flow-field guidance once they're close
  enough to a base to "see" it.

A spawn isn't a "drop here" pin.  It's a **zone of pressure with
a heading**: "enemies are coming from over there."

## Two load-bearing design choices

### 1. A separate sparse marker layer, not a palette extension

Markers do *not* belong in the painted ground palette
(GROUND_TYPES.md).  Reasons:

- A spawn marker is **content**, not terrain.  It sits at a hex
  but doesn't render as the hex's colour — the hex underneath
  keeps its painted ground type (grass, sand, water…).  Markers
  *overlay* terrain rather than replacing it.
- Markers carry **structured data beyond a single enum value** —
  a spawn has a direction; an item position has an item id; a
  patrol marker has a path.  Trying to encode this in the
  `GroundType` enum bloats the palette.
- The marker layer's data shape differs from the ground layer.
  Ground = `hash<GroundType[q,r]>`; markers =
  `hash<Marker[q,r]>` where `Marker` is a tagged union (today
  just `Spawn { direction }`; later `Item`, `Patrol`, etc.).

Both layers are **sparse** in the same sea-default sense: a hex
with no marker has no marker.  Most hexes have no marker, so
storage scales with placed-marker count, not world size.

### 2. Editor is a two-mode tool, not two tools

The editor (plan 01) gets a **mode toggle** between:

- **Ground mode** (today) — paint the `hash<GroundType[q,r]>`
  layer with palette swatches.
- **Marker mode** (this plan) — place / rotate / remove markers
  in the `hash<Marker[q,r]>` layer.

Same executable, same camera, same hex grid, same
selection-and-click mechanics; the only difference is *which
data layer* the click writes to.  This keeps the in-game-editor
discipline (DESIGN.md § Updates) without splitting into two
tools.

## Scope (validation tier)

In:
- A sparse `hash<Marker[q,r]>` layer parallel to plan 01's
  painted ground layer.  Same sparse / sea-default semantics.
- One marker variant: **spawn** (`{ direction: 0..5 }`).
- Editor: mode toggle (ground ↔ marker) via hotkey.
- Spawn placement: in marker mode, click a hex → spawn marker
  appears; click again → removed.  Hotkey **R** rotates the
  marker's direction one step clockwise; **Shift+R** rotates
  counter-clockwise.
- Spawn visual: a small **triangular arrow** rendered on the
  marker's hex, pointing along its direction.  Distinctly
  off-palette colour (default **`#ff3060`** — a vivid hot pink
  / magenta, so spawn markers can't be mistaken for terrain
  even on sand or wall hexes).  Visible at any zoom.
- Save / load: spawns persist in the same save file as the
  painted layer (plan 01 E4).
- Runtime: at validation, a constant trickle of enemies from
  each placed spawn marker.  Each enemy appears at the marker
  hex with heading = marker's direction; it travels in that
  direction at constant speed until it enters the flow-field
  range (see § Approach-mode vs engage-mode below).

Out of scope (deferred to later plans):
- **Multiple marker variants** (item positions, patrol nodes,
  treasure pins).  The marker layer is *built* to host them;
  the validation tier only places spawns.
- **Enemy "approach AI" — the real one.**  The shaped
  approach-direction behaviour is the **placeholder**; the
  enemy "no base nearby" AI mentioned by the user is its own
  later plan (see § Approach mode below).
- **Stencil markers** (multi-hex authored regions).  Single-hex
  markers cover this plan.

## Waves — a simple count list

For validation, **waves are a flat list of enemy counts**.
That's the whole schedule:

- A wave is just an **integer** — how many enemies to release
  in this wave.
- A scenario has an **ordered list of waves**:
  `[5, 8, 12, 20, 30, 50, 80]` — first wave is 5 enemies, the
  last is 80.
- Direction is **NOT** in the wave list.  Where each enemy
  comes from is decided **per-enemy at spawn time**, by picking
  a random spawn marker from the set placed on the map.  The
  picked marker's direction governs the enemy's approach
  heading.
- Wave separator: a fixed inter-wave delay (e.g. ~15 s) for now,
  long enough that the player can survey damage, repair towers,
  and reposition.
- Wave clear condition: the wave is "active" while at least one
  of its emitted enemies is alive on the map; once the last
  enemy dies, the wave is cleared.  Next wave starts after the
  inter-wave delay.

Authored in [`../../../examples/waves.json`](../../../examples/waves.json)
— a simple JSON array of integers + a single `inter_wave_delay`
seconds value.

Why a flat count list (not a richer schema):
- It's the **smallest thing that creates an ascending
  difficulty curve** for the validation scenario.  Each wave is
  larger; the player has to defend longer / better as the run
  progresses.
- Direction-randomness comes from the map's spawn markers, not
  the wave data.  Two scenarios with identical wave lists but
  different spawn-marker layouts feel completely different to
  play — which is the validation: that map authoring matters.
- Easy to extend later: a wave can grow extra fields
  (`enemy_kind`, `burst_pattern`, `boss: true`, `delay`) without
  breaking the integer-only short form (treat a bare integer as
  `{ count: N }`).

**The flat list is also a placeholder for the future
economy-driven wave source.**  DESIGN.md § Updates 2026-05-26
calls out the long-term plan: waves stop being a fixed
authored sequence and become the *output of the robot economy
state* (factories produce → supply lines deliver → mines fuel
the cycle), with the player able to **alter that economy** by
disrupting any link in the chain.  The wave list retires when
economy state becomes the input.  Plan 03 ships the placeholder;
the economy-driven replacement is a separate later plan.

What this is NOT (yet):
- No enemy type variety — every enemy in every wave uses the
  same proxy.  Multi-type waves slot in later by tagging waves
  with kinds.
- No per-wave delay override (uniform `inter_wave_delay`).
- No boss waves; no "swarm vs single elite" pacing.
- No procedural scaling — the list is **authored**, not
  generated.

## Phases

| # | Scope | Proves |
|---|---|---|
| **M1** | **Marker data layer.** Add `hash<Marker[q,r]>` parallel to the painted layer.  `Marker` is a tagged union — for now only `Spawn { direction: 0..5 }`.  Save / load includes both layers (plan 01 E4 extension). | The data model + the parallel-layer concept. |
| **M2** | **Editor mode toggle.** Hotkey (`Tab` proposed) flips between ground mode (paints palette) and marker mode (places markers).  HUD shows which mode is active. | The two-mode editor pattern; future marker variants slot in. |
| **M3** | **Spawn placement + rotation.** In marker mode, click → place / remove; **R** / **Shift+R** rotate the placed direction.  A faint *ghost* arrow appears at the hovered hex so the designer sees where the marker will go before clicking. | The placement UX. |
| **M4** | **Spawn render in-game.** The painted-layer view (plan 01) and the validation viewer (plan 02 V3+) both render the spawn-marker triangle on top of the ground colour.  Visible at any zoom. | The overlay render — markers don't replace terrain colour. |
| **M5** | **Runtime spawn execution.** When the game starts (or a validation scenario triggers), each spawn marker emits enemies at a constant rate (e.g. one every ~2 s).  Each spawned enemy carries an **initial heading** = marker's direction. | The spawn pipeline; the heading hand-off into approach behaviour. |

M5 closes the validation loop: paint a base, place spawns
around it pointing inward, run the scenario, watch enemies
flow in from multiple sides.

## Implementation + testing

### Phase M1 — Marker data layer

**Files**

| File | Purpose |
|---|---|
| `src/markers.loft` | `Marker` enum + sparse `hash<Marker[integer]>` (key packed (q,r)). |
| `src/save.loft` | Extend save format from plan 01 E4 to include the `markers` array. |

**Data**

```loft
// Marker variants — polymorphic enum (loft supports per-variant
// named fields).  Only one variant for validation; the layer is
// built to host more later.
enum Marker {
    Spawn { direction: u8 }    // 0..5 cardinal hex direction
}

// Sparse marker layer keyed by (q, r) — two-key hash, mirroring
// the painted layer in plan 01.
pub struct MarkerEntry {
    q: integer not null,
    r: integer not null,
    marker: Marker
}

let markers: hash<MarkerEntry[q, r]> = []
```

**Test — `tests/scripts/03_m1_layer.loft`**

```loft
let m: hash<MarkerEntry[q, r]> = []
place_marker(&mut m, 3, 0, 2)            // direction 2
assert m.len() == 1
let entry = lookup_marker(&m, 3, 0)
match entry {
    Some(MarkerEntry { marker: Marker::Spawn { direction }, .. }) =>
        assert direction == 2,
    None => panic("expected spawn marker"),
}
// Sparse: an unmarked hex returns None
assert lookup_marker(&m, 0, 0) == None

// Save / load roundtrip with the painted layer (plan 01 E4)
let palette = load_palette("examples/palette.json")
let painted: hash<PaintedHex[q, r]> = []
save_map_with_markers(&painted, &m, &Camera{...}, "/tmp/dryo.json")
let (_, m2, _) = load_map_with_markers("/tmp/dryo.json", &palette)
assert m2.len() == 1
```

**Pass criteria.** Parallel sparse layer round-trips through
the save format; plan 01 tests still pass.

### Phase M2 — Editor mode toggle

**Files**

| File | Purpose |
|---|---|
| `src/editor_mode.loft` | `enum EditorMode { Ground, Marker }` + toggle handler. |
| `src/ui.loft` | HUD: small mode indicator (text label or icon) — corner overlay. |

**Key functions**

- `fn toggle_mode(mode: &mut EditorMode, keys: vector<text>)`
  — Tab flips between Ground and Marker.
- `fn render_mode_indicator(mode: EditorMode)` — small text:
  `"GROUND"` / `"MARKER"` in a corner.

**Test — `tests/scripts/03_m2_toggle.loft`**

```loft
let mode = EditorMode::Ground
toggle_mode(&mut mode, ["Tab"])
assert mode == EditorMode::Marker
toggle_mode(&mut mode, ["Tab"])
assert mode == EditorMode::Ground
```

**Pass criteria.** Tab visibly flips the editor's behaviour
(paint vs marker-place); HUD reflects the current mode.

### Phase M3 — Spawn placement + rotation

**Files**

| File | Purpose |
|---|---|
| `src/marker_place.loft` | Click → place / remove; R / Shift+R rotate. |
| `src/marker_render.loft` | Triangle arrow at hex centre, oriented per direction. |

**Key functions**

- `fn place_marker(m: &mut hash<MarkerEntry[q, r]>, q: integer, r: integer, dir: u8)`
- `fn rotate_marker(m: &mut hash<MarkerEntry[q, r]>, q: integer, r: integer, delta: integer)`
  — `delta = +1` clockwise, `-1` counter-clockwise; wraps mod 6.
- `fn render_marker(c: &Camera, entry: &MarkerEntry)` —
  draws the hot-pink triangle (`#ff3060`) at the entry's hex
  centre, oriented per its `direction`.

**Test — `tests/scripts/03_m3_placement.loft`**

```loft
let m: hash<MarkerEntry[q, r]> = []
place_marker(&mut m, 5, 5, 0)
match lookup_marker(&m, 5, 5) {
    Some(MarkerEntry { marker: Marker::Spawn { direction }, .. }) =>
        assert direction == 0,
    _ => panic("expected"),
}

rotate_marker(&mut m, 5, 5, 1)
match lookup_marker(&m, 5, 5) {
    Some(MarkerEntry { marker: Marker::Spawn { direction }, .. }) =>
        assert direction == 1,
    _ => panic("expected"),
}

// Wrap-around
for _ in 0..6 { rotate_marker(&mut m, 5, 5, 1) }
match lookup_marker(&m, 5, 5) {
    Some(MarkerEntry { marker: Marker::Spawn { direction }, .. }) =>
        assert direction == 1,
    _ => panic("wraparound failed"),
}

// Re-click removes
place_marker(&mut m, 5, 5, 0)
assert lookup_marker(&m, 5, 5) == None
```

**Pass criteria.** In marker mode, clicks place / remove
markers; R / Shift+R rotate; a ghost arrow previews the
current rotation as the player hovers.

### Phase M4 — Spawn render in-game

**Files**

| File | Purpose |
|---|---|
| `src/marker_render.loft` | Extend `render_frame` (plan 01 E1) to overlay markers after painted hexes — markers don't *replace* the ground colour, they sit on top. |

**Key functions**

- `fn render_markers(c: &Camera, m: &hash<Marker[integer]>)`
  — for each visible marker, draw its triangle.

**Test — `tests/scripts/03_m4_render.loft`**

Visual / human test: place 3 spawn markers at different
positions and directions; eye-check that arrows render where
expected and point the right way.  No automated assertion
(rendering correctness is visual).

**Pass criteria.** Markers visible at any zoom; the arrow
direction matches the data field.

### Phase M5 — Runtime spawn execution

**Files**

| File | Purpose |
|---|---|
| `src/enemy.loft` | `Enemy { pos: Hex, heading: u8, hp: integer, state: EnemyState }` + spawn / update / kill. |
| `src/wave_engine.loft` | `WaveState { current_wave: integer, alive: vector<Enemy>, …}` + per-frame tick. |
| `src/spawn_director.loft` | Picks a random active marker per spawned enemy; assigns its heading from the marker's direction. |
| `examples/waves.json` | Already exists; loaded at scenario start. |

**Key functions**

- `fn active_markers(m: &hash<Marker[integer]>, core: Hex, disable_radius: integer) -> vector<Hex>`
  — return marker positions OUTSIDE the close-spawn-disable
  radius from the core.
- `fn pick_spawn(active: &vector<Hex>) -> Hex`
  — random pick.
- `fn spawn_wave(state: &mut WaveState, count: integer, markers: &hash<Marker[integer]>, core: Hex)`
  — produce `count` enemies, each at a random active marker,
  with the marker's direction as initial heading.
- `fn enemy_tick(e: &mut Enemy, world: &WorldState, dt: single)`
  — approach mode (walk along heading) until inside the
  scrambler bubble, then engage mode (flow field toward core).

**Test — `tests/scripts/03_m5_spawn.loft`**

```loft
// Fixed scenario with 3 markers around a central core
let m = hash<Marker[integer]>::new()
place_marker(&mut m, 10, 0, 3)
place_marker(&mut m, 0, 10, 0)
place_marker(&mut m, -10, -10, 1)
// And one close marker that should be auto-disabled
place_marker(&mut m, 2, 0, 0)

let core = Hex { q: 0, r: 0 }
let active = active_markers(&m, core, 10)  // close_spawn_disable_radius
assert active.len() == 3                    // the close marker is excluded

// Spawn a wave of 5 enemies
let state = WaveState::new()
spawn_wave(&mut state, 5, &m, core)
assert state.alive.len() == 5

// Each enemy's position is at one of the active markers
for e in state.alive {
    let found = active.iter().any(|h| h.q == e.pos.q && h.r == e.pos.r)
    assert found
}
```

**Pass criteria.** A wave's enemies appear at active markers
only (close-disable rule respected); each has its marker's
heading; visually visible at the spawn hex during the
pre-walk window; begin walking after the interval.

## Integration smoke test (plan 03 ↔ plan 01)

`tests/scripts/03_integration.loft`:

1. Load the painted layer + markers from a saved map.
2. Render frame — confirm both layers visible together
   (painted hexes underneath, marker arrows on top).
3. Trigger wave 1 → enemies appear, walk, eventually arrive
   at the core's hex.
4. Eyeball: enemies originate from multiple markers, not all
   the same one (random selection working).

When this passes, plans 01 + 03 are integrated; the marker +
wave mechanic is ready for plan 04 (map authoring) and plan
05 (validation scenario).

## Approach mode vs engage mode (the future-AI hand-off)

This plan ships a **placeholder approach behaviour** that gives
the player the right *feel* for now — multiple directions of
attack — and creates a clean hand-off point for the future
"enemy AI when no base nearby" the user mentioned.

**Approach mode** (placeholder, this plan's M5):
- Enemy spawns at marker, heading = marker's direction.
- Enemy walks in that direction at constant speed.
- After N hexes (default ~10) OR when the enemy enters a hex
  within the flow-field's computed region (within range of a
  wall or the core), the enemy **switches to engage mode**.
- In approach mode the enemy is essentially dumb — straight
  line; bumps into terrain by simple slope-gated movement
  rules.

**Engage mode** (existing — DESIGN.md § Systems #4):
- Enemy samples the flow field at its current hex.
- Walks toward the descent neighbour.
- Standard tower-defence behaviour.

**Future replacement of approach mode — the "enemy AI" plan.**
The user has signalled that enemies will eventually get their
own AI for when no base is nearby (the planet-scale
roam-the-world behaviour in DESIGN.md § Future expansion).
That AI replaces *only* approach mode; engage mode and the
spawn-marker data shape remain unchanged.  The hand-off point
(approach → engage when within range) is the architectural
seam that lets the future AI plug in without touching the
spawn pipeline or the base-defence flow field.

So the design constraint here is: **the spawn marker is
forever; approach behaviour is replaceable.**  Keep their
boundary clean.

## Editor visual — what spawn markers look like

When in marker mode and hovering over an empty hex, a **faint
ghost arrow** previews where the spawn will land (and which way
it will point — current rotation state).  Clicking commits;
clicking on a placed marker removes it.

```
                    Painted ground hex (any palette colour)
                    underneath; the arrow does NOT replace
                    the ground colour, it overlays.
       ┌─ hex ─┐
       │   ▲   │      ← spawn arrow ▲ at hex centre,
       │ ◣ ▲ ◢ │        pointing in the marker's
       │   ▲   │        chosen direction (0..5).
       └───────┘
       direction 0  → arrow points "up" (+r in axial)
       direction 1  → +60° clockwise
       …
       direction 5  → 300° clockwise (= 60° counter-clockwise)
```

The arrow is `#ff3060` so it stays unambiguous against any
palette hex — including the sand `#f0d860` (which would absorb
a yellow arrow) and the wall placeholder reds (which would
absorb a red arrow).  Hot pink doesn't appear anywhere else.

Hotkeys (proposed; settle in M3):

| Key | Action (marker mode) |
|---|---|
| Click hex | Place / remove spawn marker |
| `R` | Rotate active marker direction +1 (clockwise) |
| `Shift+R` | Rotate active marker direction -1 (counter-clockwise) |
| `Tab` | Switch back to ground mode |
| `Esc` | Cancel hover; no commit |

## Dependencies

- **Plan 01** — `hash<GroundType[q,r]>` painted layer; this
  plan adds a parallel `hash<Marker[q,r]>` layer using the same
  sparse / sea-default discipline.  M1-M3 can start once plan
  01 E1-E3 ship.
- **No upstream lib-plan dependency.**  The marker layer is
  pure dryopea data + UI — no slope solver, no gridmesh
  meshing involved.
- **Plan 02 (viewer)** — co-renders the markers via its V3
  overlay pass.  No coupling; the viewer just reads the same
  marker layer.

## Why this plan, why now

- The base game can't be played without spawns.  Plans 01 + 02
  + this one are the **minimum content + authoring set** for a
  validation scenario.
- A multi-spawn, multi-direction design is the **cheapest** way
  to make the base feel pressured by something other than "the
  one direction enemies always come from."  At validation tier,
  three spawn markers pointing at different sides of a base
  produce visibly different gameplay than a single spawn —
  without any wave-scheduling or AI work.
- The **marker layer** is a recurring substrate (item positions,
  patrol nodes, scripted-event triggers, future-enemy-AI
  waypoints all live here).  Building it once, for spawns, sets
  the pattern.

## Open questions

1. **Spawn off-grid?** Should enemies appear AT the marker hex,
   or at a hex one or two steps OUT from the marker in the
   marker's direction (so it reads as "they came from over
   there")?  Lean **slightly out** — visually less abrupt; an
   enemy fading in 1-2 hexes away from the marker, walking
   toward the marker, then past it.  Mark hex itself is the
   *anchor*, not the *spawn location*.
2. **Concurrent spawns from one marker?** Validation tier:
   one-at-a-time trickle.  Later: bursts (3-5 enemies emerge
   close together) — needs only a per-marker `burst_size` /
   `burst_interval` pair.
3. **Direction granularity — 6 or 12?**  6 hex cardinals is
   the natural fit.  12 (with intermediate diagonals) would let
   the designer pick "from between NE and E" precisely.  Lean
   **6** for validation; revisit if pressure-feel needs more
   resolution.
4. **Marker count cap?**  No technical cap (sparse hash), but a
   *design* cap (e.g. "no more than 8 spawn markers per base")
   helps content discipline.  Leave open until V5 inspection
   shows actual usage.
5. **Should the placement preview show the spawn's expected
   first-N-hexes trajectory?**  In M3, hover with marker mode
   active could draw a faint line in the marker's direction
   showing where enemies will walk.  Useful UX; cheap; lean
   **yes**.
6. **Marker variants in the same layer?**  Yes by design — the
   `Marker` type is a tagged union from M1.  But validation
   doesn't need any variant besides `Spawn`.  Item / patrol /
   trigger variants ship in later plans without re-doing the
   layer.

## See also

- [`../01-ground-editor/README.md`](../01-ground-editor/README.md)
  — the painted-ground sibling plan; this plan parallels it
  with a marker layer.
- [`../02-solver-validation-viewer/README.md`](../02-solver-validation-viewer/README.md)
  — the viewer; renders markers as an overlay on top of the
  ground-and-mesh stack.
- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md)
  § Systems #4 — engage-mode flow field; the hand-off target
  for the future enemy AI that replaces approach mode.
- [`../../../docs/PROXY_ART.md`](../../../docs/PROXY_ART.md)
  § Enemy — the enemy proxy that this plan's spawns produce.
