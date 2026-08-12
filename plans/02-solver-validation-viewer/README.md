<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 02 — Solver-validation viewer

**Status:** Future (design drafted 2026-05-26; no code).

## Goal

An **in-game 3D view** that renders the painted ground layer
*twice* on top of itself, so the player (or developer) can
**eyeball-check the height-solver's output against the painted
input**:

- **Bottom layer** (full opacity, full palette colour) — the
  flat painted hexes at sea level, as authored by plan 01.
  This is the **input** to the solver.
- **Top layer** (partial transparency — 40% see-through) — the
  same world rendered as **height-solved 3D terrain** (lib-plan
  20's output, meshed via lib-plan 19 gridmesh).  This is the
  **output** of the solver, drawn on top of the input.

Because the top layer is partially transparent, the painted
colours of the bottom layer show *through* the meshed terrain.
A correct solver run looks like coherent 3D hills rising out of
their painted footprints; a bad run shows the mesh wandering
off from the underlying paint.  It is **visual debugging for the
solver** — the cheapest way to find bugs in a slope / drainage
algorithm whose correctness is otherwise expensive to assert.

Style reference: the loft @PLAN36 audience-generative-art demo's
projector — a 3rd-person GL camera + GPU mesh pipeline.  This
plan reuses the same pattern (orbital camera, GPU-uploaded mesh,
per-chunk meshing via gridmesh) but at world scale: hexes are
~1.5 m (DESIGN.md § World scale), so the camera operates at
person / drone heights, not concert-stage heights.

## Two load-bearing design choices

### 1. Dual-layer overlay (not side-by-side)

Painted input and solved output share **the same world-space
position**.  The top mesh sits directly above its painted
footprint, with vertical extrusion = solved height.  You don't
have to scan between two panels to compare; you see input and
output as one composited image.

Why:
- A side-by-side view forces the eye to flick between panels and
  re-locate the same hex twice.  An overlay puts both views in
  one fixation.
- Misalignment between paint and mesh is the kind of bug you
  notice *because* the mesh and paint visibly disagree; that
  disagreement is invisible in side-by-side.
- It works at any zoom — close-up reveals per-hex detail; pulled
  out shows the regional shape.

### 2. In-game, shared with the editor

The viewer is a **mode** of the dryopea executable, alongside
the editor (plan 01) and the future game.  Same render
pipeline, same input system, same camera code; only the
HUD / input-binding changes between modes.

Why:
- Avoids a separate debug tool that drifts out of sync with the
  game's renderer.
- Lets the designer flip between "paint" (plan 01) and "see what
  the solver does with it" (this plan) without leaving the
  application — the iteration loop is *paint → toggle view →
  inspect → re-paint*.
- Stays consistent with the in-game-editor design choice in
  DESIGN.md (§ Updates 2026-05-26).

## Scope

In:
- A 3D scene where the painted hex layer (plan 01's
  `hash<GroundType[q,r]>`) is rendered as a flat coloured floor
  at y=0, with each hex at world scale (~1.5 m diameter,
  DESIGN.md § World scale).
- A **height-solved mesh** built from the same painted layer
  via lib-plan 20's solver (input) and lib-plan 19 gridmesh's
  meshing (output), rendered on top at **40% see-through alpha**
  (= 0.4 opacity, 0.6 transparency — see open Q #1 for the
  rationale).
- An orbital / 3rd-person camera: pan, orbit, zoom; defaults
  scaled to 1.5 m hexes (initial zoom showing ~30-50 hexes
  across the viewport).
- Mode toggle (hotkey): **flat-only** (paint, no mesh) | **dual-
  layer** (paint + 40% mesh, the default) | **mesh-only**
  (full-opacity meshed terrain).
- Per-hex tooltip: hover a hex → show painted type, solved
  height, and (for water hexes) accumulated drop.
- A **stub solver path** for V1-V2 before lib-plan 20 lands:
  fake heights generated locally (e.g. `slope × distance from
  nearest sea` Manhattan-style on the hex grid), so the viewer
  can be developed against a reproducible-but-fake output
  without blocking on lib-plan 20.

Out of scope (later plans):
- The actual solver implementation (lib-plan 20).  This plan is
  the **harness**, not the algorithm.
- Per-chunk meshing tuning / LOD (lib-plan 19 Phase C).
- Vehicle, towers, mobs, flow field — game systems.
- Save/load — already in plan 01.

## Phases

| # | Scope | Proves |
|---|---|---|
| **V1** | **Flat hex floor in 3D.** Render the painted layer (plan 01's sparse `hash<GroundType[q,r]>`) as a flat plane of coloured hexes at y=0, world scale (1.5 m). Orbital camera (pan / orbit / zoom). No mesh on top yet. | The 3D rendering pipeline, the camera, the world-scale assumptions. |
| **V2** | **Stub-solver mesh, full opacity.** Compute a fake per-hex height using a placeholder rule (`slope × distance from nearest sea`) and mesh it as a 3D surface via gridmesh primitives, rendered at full opacity. The flat layer is hidden when the mesh is on. | Mesh upload + per-chunk emit; the mesh-rendering pipeline. |
| **V3** | **Dual-layer overlay.** Render BOTH layers: the flat painted layer at full opacity at y=0, the meshed terrain at **40% alpha** on top. Visual check: the mesh's colour bleeds with the painted colour underneath through transparency. Mode toggle (hotkey: 1 = flat, 2 = dual, 3 = mesh-only). | The composited overlay — the design thesis of the plan. |
| **V4** | **Real solver wired in.** Replace the V2 stub with lib-plan 20's actual solver (once it lands). The viewer becomes the lib-plan-20 acceptance test. | The lib-plan-20 ↔ dryopea integration. |
| **V5** | **Inspection HUD.** Per-hex tooltip on hover: painted type, solved height, cumulative drop (for water hexes), buildable y/n, walkable-ground y/n. Toggle a hotkey to show slope / drop numbers stamped on every hex. | Per-hex introspection for design + debug. |

**Iteration loop after V4:** paint a base in plan 01, switch to
this viewer, inspect the solver result, return to the editor,
adjust the palette numbers or the painted shape, repeat.  This
*is* the lib-plan-20 tuning workflow.

## Implementation + testing

Plan 02 is **optional for the validation tier** (per
[`../05-validation-scenario/README.md`](../05-validation-scenario/README.md)).
It exists to validate lib-plan-20 once that lands.  Implementation
detail is therefore lighter than plans 01 / 03 / 04 — concrete
enough to start when needed.

### Phase V1 — Flat hex floor in 3D

**Files**

| File | Purpose |
|---|---|
| `src/viewer/floor3d.loft` | 3D draw of plan 01's painted layer at y=0. |
| `src/viewer/orbital_cam.loft` | Pan / orbit / zoom camera (distinct from the game's locked over-the-shoulder). |
| `src/viewer/main.loft` | Viewer entry point — separate binary or mode-switch from main. |

**Key functions**

- `fn render_floor_3d(c: &OrbitalCamera, painted: &hash<u8[integer]>)`
  — for each visible hex, emit a flat quad at y=0 with the
  palette colour.
- `fn orbital_update(c: &mut OrbitalCamera, mouse: MouseInput)`
  — drag = orbit, scroll = zoom, middle-drag = pan.

**Test — `tests/scripts/02_v1_floor.loft`**

Visual / human: load a small painted map, see flat coloured
hexes at y=0; orbital camera works.

```loft
// Programmatic: camera math
let c = OrbitalCamera { focus: (0.0, 0.0, 0.0), distance: 30.0, yaw: 0.0, pitch: -45.0 }
orbital_update(&mut c, MouseInput::Scroll(-5.0))
assert c.distance < 30.0   // zoomed in
```

**Pass criteria.** Painted map renders as a flat coloured
plane at world scale; orbital camera navigates around it.

### Phase V2 — Stub-solver mesh, full opacity

**Files**

| File | Purpose |
|---|---|
| `src/viewer/solver_stub.loft` | Placeholder solver: per-hex height = `slope × manhattan_distance_to_nearest_sea`. |
| `src/viewer/mesh.loft` | Per-chunk mesh emit (calls into loft `gridmesh` library once available; minimal fallback in-tree otherwise). |

**Key functions**

- `fn stub_solve(painted: &hash<u8[integer]>, palette: &vector<GroundType>) -> hash<single[integer]>`
  — returns per-hex height; sea hexes = 0; land hexes =
  slope × hop-count to nearest sea.
- `fn render_mesh(c: &OrbitalCamera, heights: &hash<single[integer]>, alpha: single)`
  — emit triangle mesh; alpha for V3.

**Test — `tests/scripts/02_v2_stub.loft`**

```loft
let palette = load_palette("examples/palette.json")
let world = hash<u8[integer]>::new()
paint(&mut world, 0, 0, 5)   // grass (slope 6)
paint(&mut world, 1, 0, 5)   // grass adjacent
paint(&mut world, 2, 0, 0)   // sea (drain)

let h = stub_solve(&world, &palette)
assert h.lookup(pack(2, 0)) == 0.0     // sea = 0
assert h.lookup(pack(1, 0)) > 0.0      // grass adjacent to sea has some height
assert h.lookup(pack(0, 0)) > h.lookup(pack(1, 0))  // further from sea = higher
```

**Pass criteria.** A meshed surface visibly *rises out of*
sea level on land hexes; sea hexes stay at y=0.

### Phase V3 — Dual-layer overlay

**Files**

| File | Purpose |
|---|---|
| `src/viewer/dual_render.loft` | Two-pass render: floor at full alpha, mesh at 40% alpha on top. |
| `src/viewer/view_mode.loft` | Hotkeys 1 / 2 / 3 = flat / dual / mesh-only. |

**Render order (V3 dual mode):**

1. Clear depth + colour.
2. Draw flat floor (alpha=1.0, writes depth).
3. Draw mesh (alpha=0.4, depth-test ON, depth-write OFF).

**Test — `tests/scripts/02_v3_overlay.loft`**

Visual: load a painted map, switch to dual mode, see the
floor's colour bleed through a faint meshed terrain on top.

**Pass criteria.** Hotkey 1/2/3 cycles modes; dual mode
shows both layers; mesh tint blends with painted colour
underneath.

### Phase V4 — Real solver wired in

Substitute `stub_solve` for lib-plan-20's actual solver once
that lands.  No code change in V1-V3 ideally; just
re-targeting the `solve` function.

**Pass criteria.** The viewer becomes the lib-plan-20
acceptance test; bugs in the solver visibly manifest as
mismatch between flat colours and meshed heights.

### Phase V5 — Inspection HUD

**Files**

| File | Purpose |
|---|---|
| `src/viewer/inspect.loft` | Hover-to-tooltip showing per-hex stats. |

**Tooltip content** for a hovered hex:

- Painted type name
- Solved height
- Cumulative drop (for water hexes; sum along the chain)
- Slope value
- Flags: walkable_ground / walkable_vehicle / buildable

**Pass criteria.** Hovering shows the data; toggling
"stamp slope/drop on every hex" overlays small numbers on
every hex.

## How the dual layer renders

A sketch of the render pass:

```
1. Clear depth + colour.
2. Render the FLAT painted layer.
     - For each visible hex in the sparse map:
         - Emit a flat hex polygon at y = 0, world-space size
           = 1.5 m diameter, colour = palette[type].color.
     - Solid; writes both colour and depth.
3. Render the MESHED terrain layer with alpha + depth.
     - For each visible chunk, run gridmesh build over the
       painted layer + solver heights → a triangle mesh.
     - Draw with `alpha = 0.4`, vertex colour = palette[type].color
       at each hex centre, interpolated across the face.
     - Depth-write OFF, depth-test ON (so the mesh occludes
       itself but lets the flat layer underneath show through
       the alpha).
4. (Optional) Inspection HUD on top.
```

The 40%-alpha overlay produces a colour blend per pixel: where
the mesh is high above the floor, the bottom layer is occluded
by 40%-tinted mesh (mostly mesh colour, somewhat flat-layer
through it).  Where the mesh sits flush with the floor (sea
level, or a hex whose solved height is 0), the two layers
*nearly* overlap and the colour is close to the painted one.

The **lib-plan 19 dirty re-mesh** machinery makes this cheap:
when the editor paints a hex, the chunk that owns it goes
dirty; both the flat layer and the meshed layer re-emit just
that chunk.  One dirty event, two consumers — same pattern
DESIGN.md § Systems #4 uses for the runtime walls + flow field.

## What this lets you catch

| Bug class | How the dual layer reveals it |
|---|---|
| Wrong slope value | A `hill` band rises too high or too low; the brown mesh peak hovers above or sinks into the brown floor underneath. |
| Drainage seed not pinning | A `sea` hex shows a non-zero mesh height — the dark-blue floor has a meshed bump where it should be flat. |
| Discontinuous slope-face | Adjacent `rock` hexes at different heights produce a visible *gap* between mesh + floor instead of a vertical wall face. |
| Wrong sub-palette gate | A `waterfall` is treated as land — the near-white mesh climbs instead of dropping. |
| Asymmetric solver | Painting the same palette in two rotational orientations produces visibly different meshes overlaid on identical flat layers. |
| Lake/sea conflation | A `water` pocket not chained to `sea` rises (correctly, drop accumulates) or doesn't (bug). Visible at a glance against the flat blue. |

Each of those bugs is **expensive to assert in code** (you'd
need fixture maps + golden numerical outputs); **trivial to see**
with the dual layer.

## Open questions

1. **40% see-through — alpha = 0.4 or 0.6?** "40% see-through"
   is ambiguous in English.  Two readings:
   - **alpha = 0.4 (60% transparent)** — the painted floor
     dominates, mesh is a faint over-tint.  Best for showing
     painted colour clearly, less good for seeing 3D shape.
   - **alpha = 0.6 (40% transparent)** — the mesh dominates,
     painted floor shows through faintly.  Better 3D shape;
     painted colour subtler.

   **Lean alpha = 0.4** (painted floor is the *input*, the
   author wants to see it most clearly; the mesh is a
   *projection* on top).  Make it a hotkey-adjustable slider
   so testing both is one keypress.
2. **Mesh colour source — per-vertex or per-face?**  Each mesh
   vertex sits over a hex of some painted type; vertex colour =
   that type's palette colour, interpolated across the triangle.
   This gives a smooth colour gradient at type boundaries.
   Alternative: per-face flat colour (sharper boundaries).
   **Lean per-vertex interpolation** — matches gridmesh's
   existing emit model and looks better at typical zooms.
3. **Camera default.**  Where does the orbital camera sit at
   startup?  Suggested: `~30 m up, looking down at 45°, focus at
   world origin (0, 0)`.  At 1.5 m hexes that frames a ~30-50
   hex area — about a chunk's worth, enough to read the shape.
4. **Per-hex height resolution.**  The solver gives one height
   per hex (gridmesh's existing model).  For waterfalls, that
   produces a step at the falls edge.  For now: render the step
   honestly — gridmesh T4 slope-faces show it as a vertical
   drop, which is *exactly* what a waterfall *is*.  No
   smoothing.
5. **Stub solver vs no-solver in V1.**  V1 is a flat-floor-only
   view — no mesh yet.  Could V1 ship without any solver path
   at all, deferring the stub to V2?  Yes; V1's value is purely
   the 3D camera + flat hex render.  This makes V1 the smallest
   plausible "view in 3D" milestone.

## Dependencies

- **Plan 01 — In-game ground-type editor** (this repo, future)
  — supplies the painted layer this viewer reads.  V1-V3 can
  start once plan 01 E1-E3 ship (a painted, visible sparse
  layer in memory).
- **loft `lib/graphics`** — GL bindings; already shipped.
- **loft gridmesh primitives** (Phase A + B done; C open) —
  V2's stub mesh + V4's real mesh both upload via these.
  Currently in the loft repo; will migrate when the upstream
  library plans move.
- **lib-plan 20 terrain solver** — required for V4 only;
  V1-V3 use a stub.  This decoupling is intentional: this plan
  ships independently and *becomes* lib-plan-20's acceptance
  test once that lands.

## Why this plan is plan 02

- **It pays back lib-plan 20 directly.** lib-plan 20 has no
  cheap way to be validated without something *like* this
  viewer.  Building the viewer first means the moment
  lib-plan 20 ships, dryopea is already its acceptance harness.
- **It's the natural next step after plan 01.**  Plan 01
  produces a painted layer in memory.  Plan 02 renders it.  No
  intermediate plan is needed.
- **It exercises the 3D pipeline at world scale early.**
  DESIGN.md says hexes are 1.5 m; this is the first plan that
  has to make that concrete (camera distance, frustum, mesh
  scale).  Catching the scale assumptions here means D0+ inherit
  them as facts, not open questions.
- **It is also the editor's preview mode.**  Once V3 ships, the
  in-game editor can flip to V3's view *as a preview* of "what
  the terrain will look like in the running game" — a
  feature, not just debug.

## See also

- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md) — master
  design (§ World scale + § Updates 2026-05-26).
- [`../../../docs/GROUND_TYPES.md`](../../../docs/GROUND_TYPES.md)
  — the palette this viewer renders.
- [`../../../examples/palette.json`](../../../examples/palette.json)
  — the loadable palette form.
- [`../01-ground-editor/README.md`](../01-ground-editor/README.md)
  — the upstream painter; this plan reads its output.
- **loft @PLAN36** audience-generative-art — the style reference
  for the 3rd-person GL camera + GPU mesh pipeline.  (Link is
  a placeholder while loft drops outside-project references.)
- **loft lib-plan 19 / 20** — the upstream meshing + solver
  primitives.  Same placeholder caveat.
