<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 07 — Shared world substrate (go 3D; interchange world-building routines)

**Status:** Active (authored 2026-05-27).
**Effort:** H–VH (foundational; reshapes the editor's data + render core).

**Progress / blockers (2026-05-28):**
- **W0 (partial):** `gridmesh` adopted as the chunk/dirty layer —
  `src/chunks.loft` + `tests/07_chunks.loft` (9 tests, suite
  green at 189).  Native `make play` is **blocked**: the
  struct-with-hash native-return bug is *not* actually fixed
  (re-opened in QUESTIONS_FOR_LOFT.md with a faithful repro);
  `make play` stays on `--interpret`.
- **W1 (blocked):** adopting `moros_map`'s `Map` is blocked by a
  loft bug — `use` does not namespace struct types per library,
  so `world::Hex` and `moros_map::Hex` cannot coexist (panic
  "Double structure type …").  Filed in QUESTIONS_FOR_LOFT.md;
  the project owner chose to **wait for the loft namespacing
  fix** rather than rename dryopea's `Hex` (renaming to dodge
  clashes does not scale as libraries grow).  W1 resumes when
  that lands.

## Goal

**dryopea and moros run the same world-building routines —
multi-floor, stairs, rounded-structure detection, wall placement,
surface generation — unmodified.** A routine is interchangeable
only if both games hand it the *same world model*, so dryopea
adopts the existing loft hex substrate as its foundation:

```
gridmesh        chunk + dirty + mesh-pipeline toolkit      (shared, axial flat-top)
moros_map       THE world model: Map / Chunk / Hex
                {q, r, cy} · height · material · walls · items
moros_render    3D mesh routines: surfaces, walls, slopes, stairs
moros_sim       logic routines: floors, collision, edit tools
   ├── moros     game: NPC sim, its UI
   └── dryopea   game: scramble-and-salvage, tower-defence, its palette/UI
```

The decision that drives the rest: **go 3D now.** dryopea's
editor moves from its 2D top-down `Canvas` software rasteriser to
**3D mesh rendering via `moros_render`**, and its world model
moves from `PaintedWorld { PaintedHex{q,r,kind} }` (a flat 2D
subset) to `moros_map`'s `Map` (multi-floor, height, walls,
items). Once dryopea's world *is* a `Map`,
`emit_spiral_stair` / `emit_thick_curved_wall` / `floor_y_at`
and the rest run on it with zero porting — that is the entire
point.

## Why now

1. **The editor is sluggish.** `src/render.loft::render_to_canvas`
   re-rasterises every painted hex and re-uploads a full-screen
   texture every frame — no chunk system, no dirty mechanism.
   `gridmesh`'s `ChunkField` + dirty set (`field_mark_dirty`,
   `collect_dirty_inputs`) fixes this by rebuilding only changed
   chunks.
2. **The routines already exist** — on moros's model, not
   dryopea's: `moros_render` has `emit_hex_surface`,
   `emit_linear_stair` / `emit_spiral_stair` /
   `emit_grand_arc_stair`, `emit_thick_flat_wall` /
   `emit_thick_curved_wall` (rounded structures), `emit_slope_face`;
   `moros_sim` has `floor_y_at` / `resolve_move` (multi-floor),
   `wall_value_on_edge` / `blocked_by_wall`, `tool_apply(pos, cy,
   m, t)`. The palette already names `floor / stair /
   spiral_stair / grand_arc_stair`. Interchange = adopt the model
   they run on.
3. **One ground-level grid, no divergence.** The shared
   convention is **axial flat-top** — what `moros_map` /
   `moros_render` use (`HexAddress{q,r,cy}`, "axial-coordinate
   neighbour offsets") and what `gridmesh`'s own header names as
   its target. `audience_crystal`'s offset-pointy is an
   extraction placeholder, not the destination, and is being
   migrated (see Dependencies). dryopea is already axial
   flat-top, so this costs **zero coordinate rewrite**.

## The core shift

| Concern | Today (2D) | After plan 07 (3D shared) |
|---|---|---|
| World model | `PaintedWorld` / `PaintedHex{q,r,kind}` | `moros_map::Map` (chunks · `cy` floors · height · material · walls · items) |
| Render | `graphics::Canvas` software raster, full redraw/frame | `moros_render` 3D `Mesh` + GL, per-chunk meshes rebuilt only when dirty |
| Camera | `EditorCamera{pos, zoom}` (2D pan) | `moros_render::RenderCamera` (orbit / zoom / pan) |
| Picking | `screen_to_hex` (2D inverse) | camera ray (`camera_ray_dir` + `hex_at`) |
| Chunking / dirty | none | `gridmesh::ChunkField` + dirty set |
| Markers / spawns | sidecar JSON layer | `Hex.h_item` + spawn/waypoint flag bits (the model already carries them) |
| Multi-floor / walls / stairs | absent | first-class via `moros_map` + `moros_render` routines |

## World-model mapping (dryopea ↔ moros_map)

- **GroundType palette ↔ material index.** dryopea's 11-entry
  palette (`examples/palette.json`) maps to `Hex.h_material`
  values; the picker selects a material. `moros_map::palette`
  is the shared palette home — reconcile dryopea's `GroundType`
  fields (slope/drop/drainage/walk/buildable + the new
  extrusion fields) against it, contributing what's missing.
- **Markers → items.** `PaintedHex` markers (spawn / target +
  direction) map onto `Hex.h_item` + `h_item_rotation`
  (which already bit-packs a spawn flag + waypoint flag +
  rotation). The marker sidecar retires.
- **Sea-default sparse → chunk-default.** dryopea stores only
  painted hexes (sea = absent); `moros_map` allocates 32×32
  chunks of default hexes on demand (`map_ensure_chunk`). The
  default hex stands in for sea. Slightly denser in memory;
  acceptable, and it's what the shared routines expect.

## Phases

| Phase | What ships | Trigger | Effort |
|---|---|---|---|
| **W0** | Native play + path-deps + linking spike | now | S |
| **W1** | World model = `moros_map::Map` (single floor) | W0 green | MH |
| **W2** | 3D mesh editor render + chunk/dirty rebuild | W1 green | H |
| **W3** | Multi-floor + walls + stairs + neighbour rules | W2 green + gridmesh axial layout landed | MH |
| **W4** | Re-home markers/spawns + retire 2D path; reframe 02/06 | W3 green | M |

### W0 — Native play + path-deps + linking spike

- **Flip `make play` to native.** The struct-with-hash-return
  bug that forced `--interpret` is fixed (`--native-emit` of
  `src/main.loft` succeeds, 12k lines). Native is a large
  speedup with zero architecture change — do it first.
- **Add path-deps** to `dryopea/loft.toml`: `gridmesh`,
  `moros_map`, `moros_render` (and `moros_sim` when W3 lands),
  each `{ path = "../loft/lib/<name>" }`, mirroring the
  existing `graphics` dep.
- **Linking spike.** A throwaway `fn main` that loads
  `maps/a.json` into a `Map`, builds a mesh via `moros_render`,
  and renders it in a GL window — proves the stack links,
  compiles native, and runs, before committing to the
  migration.

### W1 — World model = moros_map::Map (single floor)

- Replace `PaintedWorld` with `Map` as the editor's world (start
  single-floor, `cy = 0`; defer vertical to W3).
- Palette ↔ material reconciliation (see World-model mapping).
- **Persistence.** Decide between adopting `moros_map`'s save
  format and keeping a dryopea MapFile that serialises a `Map`.
  Either way, write a one-time migration for the existing
  `maps/*.json` (2D `ground` entries → `Map` hexes). Round-trip
  tested.
- The 2D render path stays alive this phase (render the `Map`'s
  ground layer through the existing rasteriser) so the editor
  keeps working while the model changes underneath.

### W2 — 3D mesh editor render + chunk/dirty rebuild

- Replace `src/render.loft` with a `moros_render` mesh build:
  `emit_hex_surface` per cell, GL upload of per-chunk meshes.
- Introduce `gridmesh::ChunkField` alongside the `Map`: edit ops
  (`paint` / place / remove) call `field_mark_dirty`; the frame
  loop rebuilds only `collect_dirty_inputs(f, 0)` chunks and
  reuses cached meshes for the rest. **This is the sluggishness
  fix.** `halo_k = 0` (per-cell-independent surfaces) means no
  neighbour stepping yet — so W2 does **not** depend on the
  gridmesh axial-layout work.
- Camera → `RenderCamera` (orbit/zoom/pan); picking via camera
  ray + `hex_at`.
- The per-frame `Canvas` allocation (and the upstream Canvas
  Store-leak it triggers, see QUESTIONS_FOR_LOFT.md) becomes
  moot for the world render — geometry goes to GL VBOs, not a
  software canvas. HUD / picker overlays may still use a small
  cached `Canvas`.

### W3 — Multi-floor + walls + stairs + neighbour rules

- `cy` floor cycling in the editor; wall painting
  (`h_wall_n/ne/se`); stair placement via `emit_linear_stair` /
  `emit_spiral_stair` / `emit_grand_arc_stair`.
- **First neighbour-dependent rules** (rounded-structure
  detection, wall-edge meshing, slope seams between materials).
  These read neighbour cells, so they need `gridmesh`'s
  axial-flat-top layout adapter (`halo_k > 0` + correct
  `step_x`/`step_y`). **This phase is gated on that loft-side
  work landing** (filed; see Dependencies).
- Proves the interchange goal: a routine authored here runs in
  moros and vice versa.

### W4 — Re-home + retire + reframe

- Markers/spawns fold into `Hex.h_item` (+ flag bits); retire
  the marker sidecar.
- Retire the superseded 2D `render.loft` / `painted.loft` /
  `map_file.loft` paths and their now-obsolete goldens.
- Reframe plans **02** (solver viewer) and **06** (stencil
  pipeline) onto this substrate: both consume `moros_map` +
  `moros_render` + `gridmesh` directly instead of waiting on a
  separate `hex_*` extraction. Plan 06 S1 (multi-layer +
  bridges) largely *is* W3.

## Testing discipline

Same posture as plan 01 (factories + pure tick + headless +
golden), adapted for 3D and a shared model:

- **Model round-trips** (W1) — `Map` save/load + the
  `maps/*.json` migration, assert-based.
- **Mesh builds** (W2/W3) — pure `Map → Mesh` functions tested
  by **mesh-property assertions** (vertex count in range,
  bounding box within tolerance, expected materials present),
  *not* byte-equal goldens (meshes are float-y) — same approach
  plan 06 S2 settled on.
- **Dirty correctness** (W2) — edit a cell, assert exactly its
  chunk (+ halo when W3) is in `collect_dirty_inputs`, and that
  an idle frame rebuilds nothing.
- **Interchange proof** (W3) — run a `moros_render` routine over
  a dryopea-authored `Map` and a moros-authored `Map`; assert
  the same routine produces structurally-consistent output on
  both (the literal goal of the plan).
- **Live GL** — manual playtest in the native editor window:
  orbit camera, paint, multi-floor, place a stair, watch it
  render.

The existing 2D goldens (16 PNGs) are obsolete once W2 lands;
they're retired in W4, replaced by mesh-property tests.

## What this supersedes / reframes

- **The lib_plan-24 `hex_*` extraction framing.** dryopea no
  longer waits on a separate universal-editor extraction; it
  adopts the *existing* `gridmesh` + `moros_*` libraries as the
  shared substrate and, as the **first polished consumer**,
  drives their hardening (the moros code is still rough). If a
  neutral rename (`hex_*`) happens later, it's a mechanical
  follow-up, not a blocker.
- **Plan 02 and Plan 06 dependencies.** Both previously listed
  "lib-plan 19/20" or "lib_plan 24" as the substrate source.
  Plan 07 *is* that substrate for dryopea; 02 and 06 rebase onto
  it in W4.

## Open questions / risks

1. **Library naming.** `gridmesh` is neutral; `moros_map` /
   `moros_render` are moros-branded. Adopt as-is now (path-dep)
   and rename to neutral shared names later, or neutralise as
   part of hardening? Leaning adopt-now; revisit when a second
   non-moros consumer (besides dryopea) appears.
2. **Persistence ownership.** Adopt `moros_map`'s save format
   vs. a dryopea MapFile wrapping a `Map`. W1 decision.
3. **Test churn.** W2 obsoletes the 2D render goldens and a
   chunk of plan 01/03 pixel-level tests. Accepted cost of the
   2D→3D shift; mesh-property tests replace them.
4. **Sparse vs. chunk-default storage.** `moros_map` allocates
   full 32×32 default chunks; dryopea was sea-sparse. Watch
   memory on large empty maps; bound chunk allocation to the
   authored extent if it bites.
5. **gridmesh axial layout timing.** W1/W2 don't need it
   (`halo_k = 0`); W3 does. The loft-side work is the only
   external gate, and only for W3.

## Dependencies

- **loft libraries (path-dep, available now):** `gridmesh`,
  `moros_map`, `moros_render`, `moros_sim`, `graphics`.
- **loft-side work (filed in
  [`QUESTIONS_FOR_LOFT.md`](../../../QUESTIONS_FOR_LOFT.md)),
  gates W3 only:** wire `gridmesh`'s axial-flat-top layout
  adapter (consume the `layout` field in `step_x`/`step_y`) and
  migrate `audience_crystal` off the offset-pointy placeholder
  onto the shared axial layout. Both are dryopea-driven
  hardening of the shared libs, done via loft's contribution
  flow — not patched from this repo.
- **Plan 01** — the editor this rebuilds the core of. E1-live
  ships; W0–W2 replace its data + render layers in place.

## See also

- [`../../../QUESTIONS_FOR_LOFT.md`](../../../QUESTIONS_FOR_LOFT.md)
  — the coordinate-convergence + gridmesh-axial-layout asks
- [`../01-ground-editor/`](../01-ground-editor/README.md) — the
  editor whose core this replaces
- [`../02-solver-validation-viewer/`](../02-solver-validation-viewer/README.md)
  — rebases onto this substrate (W4)
- [`../06-editor-stencil-pipeline/`](../06-editor-stencil-pipeline/README.md)
  — rebases onto this substrate; its S1 (multi-layer + bridges)
  overlaps W3
- [`../../ROADMAP.md`](../../ROADMAP.md) — broader tier ordering
- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md) — master
  design (walls, multi-floor, towers, units)
