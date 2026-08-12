<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 07 — Shared world substrate (go 3D; interchange world-building routines)

**Status:** **Blocked** (authored 2026-05-27; re-assessed 2026-08-12).
W0c was cut out into [plan 09](../09-lattice-conversion/README.md); W1 needs
both that and `hex_voxel` published (§ Asset interchange, W0x).  Nothing here
is startable today, so it reads Blocked rather than Active — the
[`plans/README.md`](../README.md) active cap should mean something.
**Effort:** H–VH (foundational; reshapes the editor's data + render core).

**Progress / blockers (2026-05-28):**
- **W0 (partial):** `gridmesh` adopted as the chunk/dirty layer —
  `src/chunks.loft` + `tests/07_chunks.loft` (9 tests, suite
  green at 189).  Native `make play` is still **blocked**, but no
  longer by what this line used to say: the struct-with-hash
  native-return bug is ✅ verified fixed 2026-08-12, in its repro
  AND in dryopea's real `load_markers_or_empty` /
  `load_map_or_empty`.  What blocks native now is
  [loft-lang/loft#866](https://github.com/loft-lang/loft/issues/866)
  — `text as vector<Struct>` in tail-return position silently
  answers `[]`, so `load_palette` reads 0 entries and the native
  editor cannot paint.  `make play` stays on `--interpret`.
- **W1 (was blocked — the blocker is GONE, and a bigger one took
  its place):** adopting `moros_map`'s `Map` was blocked because
  `use` did not namespace struct types per library, so
  `world::Hex` and `moros_map::Hex` could not coexist.  ✅
  **Verified fixed 2026-08-12** (loft 2026.8.0): two libraries may
  each declare `Hex`, the diagnostic names both packages, and
  qualified literals (`moros_map::Hex { … }`) resolve.  The
  proposed `Hex` → `Axial` rename is unnecessary.
  ⚠ **But see § Correction: the coordinate convention below.**
  W1's real cost is not what this plan assumed.

## Goal

⚠ **Restated 2026-08-12 by the project owner, and it is stronger
than what follows:** *"I want to be able to use all assets created
via ../moros seamlessly in our project."*  Interchange of
**assets**, not only of routines — a world or a part built in
moros opens in dryopea.  § Asset interchange: what that actually
requires records what was found when that was checked, and it
retargets this plan.

**dryopea and moros run the same world-building routines —
multi-floor, stairs, rounded-structure detection, wall placement,
surface generation — unmodified.** A routine is interchangeable
only if both games hand it the *same world model*, so dryopea
adopts the existing loft hex substrate as its foundation:

```
gridmesh        chunk + dirty + mesh-pipeline toolkit      (odd-r offset — see § Correction)
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
3. ~~**One ground-level grid, no divergence.**~~ ⚠ **This pillar
   was false — see § Correction: the coordinate convention.** It
   claimed moros is axial flat-top like dryopea, so adoption
   costs "zero coordinate rewrite". moros is pointy-top odd-r
   offset, and the rewrite is the main cost of this plan.

## Correction: the coordinate convention (2026-08-12)

The premise above was checked against the source and does not
hold. **moros is pointy-top, odd-r offset; dryopea is axial
flat-top.** They are different lattices.

Evidence, verbatim:

- `hex_grid` — *"THE CONVENTION (shared with moros — the single
  executable source of it): pointy-top hexes, odd-r offset
  coordinates, world scale L = sqrt(3) per hex step"*, and
  *"Throughout the kernel (q,r) ARE offset (col,row)."*
- `moros_map` — its line math is *"in odd-r offset coordinates"*,
  and it carries a scar from assuming otherwise: axial cube
  distance was once applied to odd-r offset coords, so `(0,0)`
  and its SW neighbour read as two steps apart, and *"the
  editor's road width, scatter reach, storey footprint and house
  outline were all sheared blobs rather than discs"* until it was
  measured (moros#10).

Where the false premise came from: `moros_map::map_set_wall_dir`
is commented "Uses axial-coordinate neighbour offsets", and
`HexAddress{q,r,cy}` *looks* axial. Neither is evidence of the
lattice — the field names are the same either way. This is the
open question [`CLAUDE.md`](../../CLAUDE.md) § Relationship to
loft flagged as "a real decision, not a detail"; it is now
answered, against dryopea's assumption.

**What it changes.** W1's cost was booked as "adopt a richer
struct". It is really "convert every coordinate dryopea owns":
~600 lines across `world` / `render` / `marker_render`, the
6-way direction tables reindexed, example maps re-coordinated,
16 goldens rebaselined — and every future read of a moros routine
done in a lattice dryopea does not think in.

**DECIDED (project owner, 2026-08-12): dryopea converts.**  One
lattice across the ecosystem, and it is not the libraries that
move — dryopea is the only axial-flat-top consumer, and the cost
of being the odd one out is paid on every future borrow rather
than once.  The upstream ask for a second gridmesh layout
([loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24))
was **withdrawn and closed** on the strength of this decision.
The conversion is § W0c.

The related ask upstream — a layout adapter so `gridmesh` can
step in axial — is filed as
[loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24),
with the same correction applied to it: it asks for a second
layout, not for the ecosystem to move.

## Asset interchange: what that actually requires (2026-08-12)

Checked against `../moros`, not assumed.

**The assets.** 38 worlds in `worlds/*.hxw`, plus parts under
`data/parts/{door,house,prop}/*.hxw` (some with a baked `.glb`
beside them).  One binary format, `.hxw`, magic `WTTH`.

**The owner of that format is `hex_voxel`** — `../moros/lib/hex_voxel`,
which is what the registry's `hex_world` was renamed to.  `hex_part`
("a house drawn away from the world") is a small `hex_voxel` store,
so parts and worlds are the same document type — moros's own
§P1 point: *"opening a part IS loading a world"*.

**Three consequences, in the order they bite:**

1. **This plan was aimed at the wrong library.**  Everything below
   targets `moros_map`.  moros's *assets* are not in `moros_map` —
   they are `hex_voxel` stores.  `moros_map` is alive (it is what
   `moros_sim` / `moros_editor` use), but reading a `.hxw` means
   `hex_voxel`.  W1 retargets.
2. **The registry copy cannot read them.**  `hex_world 0.2.0` is a
   diverged ancestor: its magic is `'WRLD'` (0x57524C44) over a
   4-byte `Cell` of `c_color` / `c_age` — the crystal/TTT world.
   moros's files open `WTTH` over a sectioned layer format.  Same
   lineage, different format; installing the published package
   would silently read nothing.
3. **`hex_voxel` is not published.**  It is moros-local today, and
   its own header says where it belongs: *"`LAVITION.md`'s naming
   principle … its `loft-libs-world` inventory already names this
   axis `hex_voxel`. Moros is one consumer."*  So the first move is
   an EXTRACTION ask upstream, not dryopea code — dryopea cannot
   consume a library that has not been published, and a path-dep
   into a sibling checkout is what `moros_map` already is and has
   never been consumed.

**Therefore the order is: convert the lattice (§ W0c) → get
`hex_voxel` published → read `.hxw`.**  The lattice conversion is
the part dryopea owns outright and can start immediately; it is
also a precondition, because a `.hxw` read into an axial world
would be sheared exactly the way moros#10 was.

### W0c — convert dryopea to odd-r offset, pointy-top

**Cut out into [`plans/09-lattice-conversion`](../09-lattice-conversion/README.md)
on 2026-08-12.**  It is multi-phase, it stands alone — the
conversion is worth doing whether or not dryopea ever reads a
`.hxw` — and carrying it here would have pushed this plan well
past its length budget.  As one `H` phase it also failed
[`plans/README.md`](../README.md) § The two mechanical checks;
plan 09 cuts it into six steps, none above `M`.

What follows is the sketch it was cut from, kept because the gate
argument is the reason the split happened.  It touches
`world.loft`'s hex math, `render.loft`, `marker_render.loft`, the
6-way direction tables, `examples/*.json`, every `.keys`
scenario's coordinates and all 16 goldens.

**The gate, and it is the interesting part.**  Not "the goldens
were rebaselined" — a rebaselined golden agrees with whatever was
drawn, including a shear.  The gate is an **oracle**: for a swept
set of cells, dryopea's neighbours, distance and line must agree
**cell-for-cell with `hex_grid`'s** answers for the same cells.
That is precisely the check moros did not have when axial cube
distance was applied to odd-r coords — `(0,0)` and its SW
neighbour read two steps apart, and *"road width, scatter reach,
storey footprint and house outline were all sheared blobs rather
than discs"* until rung W2 measured one (moros#10).  dryopea gets
to inherit the measurement instead of the bug.

Second gate, free: plan 08's `scripts/validate.sh` still green
once the scenarios' coordinates are converted — the `kind`,
`marker` and `range` assertions are exact, so a shear moves them.

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
- **Markers → items.** See § Evaluated: markers vs the placed-item
  layer below — the mapping is real, and it is not free.
- **Sea-default sparse → chunk-default.** dryopea stores only
  painted hexes (sea = absent); `moros_map` allocates 32×32
  chunks of default hexes on demand (`map_ensure_chunk`). The
  default hex stands in for sea. Slightly denser in memory;
  acceptable, and it's what the shared routines expect.

## Evaluated: markers vs the placed-item layer (2026-08-12)

Asked directly: can dryopea's marker layer be folded into the hex
placed-item support that already exists?  Answer: **yes in
principle, no as a standalone step** — and the library it belongs
to is not the one the name suggests.

### It is not `hex_place`

`hex_place` is placement of *geometry*: posed bodies (`Pose` with
exact cos/sin), footprint stencils, boundary-edge cutting with
material arbitration, and seating a footprint onto terrain
(`seat_height` / `seat_write`, LOW · MEAN · HIGH).  Nothing in it
keys data by hex.  A dryopea marker is a per-hex annotation —
a different kind of object entirely.

**One piece of it is usable now, without adopting anything:**
`seat_height(cells, terr, policy)` computes the height a footprint
sits at over uneven ground.  That is exactly the open question
`marker_file.loft` records for the target marker — "@PLAN50 snaps
targets to the top of the extruded hex".  It reduces over a cell
set and a height field, so it does not care which lattice the
caller thinks in, as long as the caller is consistent.

### It IS `moros_map`, which already has the whole shape

Verified in `../moros/lib/moros_map/src`:

| dryopea | moros_map |
|---|---|
| marker at a hex | `Hex.h_item: u8` → `m_item_palette` (`ItemDef`) |
| spawn direction 0..5 | `h_item_rotation` bits 0-4 (0..23) |
| "there is a spawn here" | `h_item_rotation` bit 5 (`hex_spawn_flag`) |
| — | bit 6 waypoint flag; bit 7 reserved |
| `place_spawn(mw, q, r, d)` | `map_place_item(m, q, r, cy, item, rotation)` — preserves the flag bits |
| spawn *record* | `m_spawn_points: vector<SpawnPoint>` + `spawn_add` / `spawn_at` |

The design is ahead of dryopea's in one respect worth stealing
whatever else is decided: `spawn_add` sets the hex flag **as a
side effect**, explicitly "so renderers can show a spawn marker
without scanning the whole spawn list".  dryopea's renderer walks
the marker hash every frame; that split is the fix for it.

### Four gaps, none fatal, all real

1. **There is no target.**  dryopea's TARGET is where the wave
   walks — a game rule.  moros's waypoint flag and `NpcWaypoint`
   are NPC-routine steps, not a destination.  dryopea would ADD
   this to `moros_map` (allowed — it may add under a library's
   existing contract), not bend the waypoint to mean it.
2. **Rotation is a different space.**  dryopea has six 60° hex
   directions.  `h_item_rotation` holds 0..23, and moros's own
   note says there are **twelve** facings — six turns plus six
   mirrored ones that land between them.  A dryopea direction
   maps IN cleanly; "rotate" does not mean the same thing coming
   back, so `do rotate` needs a decided encoding, not a cast.
3. **`SpawnPoint` is RPG-shaped.**  `sp_creature`, `sp_npc_id`,
   `sp_condition` ("NIGHT_ONLY"), `sp_count`.  A tower-defence
   wave source needs a facing and little else, so adoption means
   carrying dead fields or contributing a kind that does not.
4. **The lattice differs** — § Correction above.  Every `(q, r)`
   crossing this boundary is a conversion, and it is the whole
   `hex_*` family, not just `moros_map`: `hex_field`'s own
   neighbour table is odd-r offset, "same SET as
   `hex_grid::hex_neighbor`".

### Recommendation

**Do not do this as a marker-layer step.**  Every gap above is
cheap; the lattice is not, and folding markers into `moros_map`
means adopting `moros_map` — which is W1, gated on the decision
§ Correction leaves open.  Retiring the marker sidecar to buy
`h_item_rotation` would pay the entire conversion cost for the
smallest layer dryopea owns.

Sequence, if the answer to W1 is yes: convert the lattice → adopt
`Map` → markers follow as items, sidecar retires, and the
flag-plus-list split comes along.  If the answer is no, dryopea
keeps its markers and takes `seat_height` at the boundary when
the 3D anchor is needed.

## Phases

⚠ **Re-cut 2026-08-12** — W0c is new, W1 retargets, and W3 loses a
trigger that was withdrawn.  The rows below the correction are the
plan as it stands; the prose sections further down still describe
`moros_map` in places and are stale to that extent.

| Phase | What ships | Trigger | Effort |
|---|---|---|---|
| **W0** | Native play + path-deps + linking spike | now | S |
| **W0c** | Convert dryopea to odd-r offset, pointy-top — now [plan 09](../09-lattice-conversion/README.md) | now (owner decision 2026-08-12); independent of W0 | MH |
| **W0x** | `hex_voxel` published so `.hxw` is readable — an upstream ASK, not dryopea code | filed with loft-libs-world | — |
| **W1** | World model = `hex_voxel` (was `moros_map::Map`) + read a moros `.hxw` | W0c green **and** W0x landed | MH |
| **W2** | 3D mesh editor render + chunk/dirty rebuild | W1 green | H |
| **W3** | Multi-floor + walls + stairs + neighbour rules | W2 green | MH |
| **W4** | Re-home markers/spawns + retire 2D path; reframe 02/06 | W3 green | M |

**W0's "flip `make play` to native" is still blocked** — by
[loft#866](https://github.com/loft-lang/loft/issues/866), not by
the struct-with-hash bug that row was written for (fixed, verified
2026-08-12).

**W3's old trigger — "gridmesh axial layout landed" — is gone.**
That ask was withdrawn when the lattice decision went the other
way; after W0c, gridmesh's stepping is already dryopea's.

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
