<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 21 — The renderer: a camera that comes to the vehicle

**Value:** `G` · **Effort:** `VH`

## Status

**Designed, nothing built** (2026-08-15).  R0 is **done** — it ran before the
design was written, because the design was not affordable without its answer.

The decisions live in [`docs/RENDERER.md`](../../docs/RENDERER.md); this plan
does not restate them.

⚠⚠ **This is the largest single item in the repo's history**, and the reason is
not the camera — it is that a follow camera at ground level makes the TERRAIN
three-dimensional, and dryopea's ground has been a flat painting of hexagons
since plan 01.  § R3.

⚠ **It is `VH` and it should be split further before it is started.**  R3 alone
(the terrain mesh) is plausibly its own plan.  This document is the design and
the ordering; the phase table below is honest about which rows are still
lumps.

## Goal

The game is drawn from a camera that follows the vehicle, in three dimensions,
and **every frame is still measured by the instrument that measures the editor's**.

## Anchors

Implements, and does not restate:

- [`docs/RENDERER.md`](../../docs/RENDERER.md) — the decisions, R0–R5.
- [`docs/PARTS.md`](../../docs/PARTS.md) — what the entities are; this plan
  draws what [`plan 20`](../20-entity-art/README.md) emits.
- `../moros/lib/moros_render/src/moros_render.loft` § Camera — `RenderCamera`,
  `camera_follow`, `camera_overview`.
- [`../moros/doc/claude/CAMERA_INDOORS.md`](../../../moros/doc/claude/CAMERA_INDOORS.md)
  — the five modes, the boom's ease, and ⚠ the instrument lesson.
- `src/render.loft`, `src/editor_view.loft`, `src/measure.loft`,
  `src/chunks.loft` (the dirty set, over `gridmesh`), `src/passable.loft`
  (`hex_height` / `hex_ground`), `src/lattice.loft` (`lat_corner_metres`).

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **R0** ✓ | a GL frame captured under `xvfb` and decoded: red 9 600 px = exactly 120×80, green 8 005 ≈ π·50², **`other` = 0** | classification is an EXACT lookup and survives GL → PNG → decode | ⚠ `gl_screenshot` returning `true` proves NOTHING about pixels — probe 1 alone would have "passed" against a black frame, which is why probe 2 exists and is the one that counts |
| **R1** | `camera_follow` puts the eye behind the vehicle's facing; `camera_overview` at 89° reproduces the editor's top-down view | one camera, two presets — the editor's existing view is a MODE of the game's camera | ⚠ **read the eye out of the VIEW MATRIX** (`eye = −Rᵀt`), never out of the camera's own trace — moros spent four wrong diagnoses on a camera whose every valve read healthy while the eye was in the wrong room |
| **R2** | the boom eases toward its target over ticks after the vehicle stops | the ease is PUBLISHED, not merely solved | ⚠⚠ **the exact bug moros shipped**: the solve sat inside `if moved`, so a converged camera never left the server and the fault was invisible in precisely the case the camera exists for.  The control is a frame captured with the vehicle STANDING STILL |
| **R3** | a hex carrying rubble meshes at `hex_height` (structure **+** layer) and colours from `hex_ground` (`rubble`) | the surface is not the painted kind (`CLAUDE.md`'s oldest trap) | ⚠ get it backwards and piling debris on a wall LOWERS it — visibly, and no existing test would fail |
| **R4** | the same scenario, drawn by GL and classified, lands in the same bands the software path reports | one geometry layer under two rasterisers (§ R2) | ⚠ **lighting breaks exact classification** — a shaded frame turns one palette colour into a range and `unknown` stops meaning "fault".  The gate renders FLAT UNLIT; loosening to nearest-colour would discard what R0 measured |
| **R5** | a frame-cost RATIO inside `loft test` | cost is a ratio, never a stopwatch | ⚠ `CLAUDE.md` § Cost: an unchanged file timed 173 / 737 / 754 ms on three interpreter runs, and discarded structs are not freed — a standalone stopwatch here measures the harness |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **R0** — can a GL frame be gated headlessly? | XS | two probes: `xvfb` + GL context + `gl_screenshot`, then `imaging::png` + exact classification.  **Measured: `other` = 0 over 76 800 px** | **Done** (2026-08-15) |
| **R1** — the camera | M | `tests/21_r1_the_camera.loft` — `RenderCamera` ported with moros's fields; follow puts the eye behind the facing; overview at 89° matches the editor's projection.  ⚠ Asserted on the VIEW MATRIX, not on the struct | Next |
| **R2** — the boom: ease and occlusion | M | `tests/21_r2_the_boom.loft` — the ease converges with the vehicle still; a `wall_high` between eye and vehicle moves the boom.  ⚠ Occlusion asks `tower_sees`, not a second line-walker | Blocked on R1 |
| **R3** — the terrain mesh | **H** — ⚠ probably its own plan | `tests/21_r3_the_mesh.loft` — top faces at `hex_height`, side quads to lower neighbours, rubble surfaces coloured from `hex_ground`, rebuilt per dirty chunk | Blocked on R1 |
| **R4** — the GL path and the gate chain | MH | `scripts/validate.sh` gains GL scenarios under `xvfb`; `classify_world` reads a decoded capture.  Flat-unlit render mode for the gate | Blocked on R3 |
| **R5** — cost | S | `tests/21_r5_the_frame_budget.loft` — a RATIO, the shape `11_f8` already uses | Blocked on R4 |

### Why the order is this order

**R0 before the design, not before the code** — and that is the point.  Going
3-D would have traded 520 measurements and 32 shots for a renderer nobody could
gate, and the honest recommendation would have been *don't*.  ⚠ Two probes and
twenty minutes turned that from a judgement call into a number.

**R1 before R3** because the camera is small, self-contained, and decides what
the mesher is even for.  ⚠ And because `camera_overview` at 89° is what makes the
editor's view a MODE rather than a second renderer — if that fails to reproduce,
the two-rasteriser plan in § R2 is wrong and better to know first.

**R3 is the lump.**  Everything else on this list is a few hundred lines; the
terrain mesh is the reason this plan is `VH`.  ⚠ It should be re-read as its own
plan before it is started, and the phase table says so rather than pretending.

**R4 after R3** because a gate over an empty world is not a gate — the same
lesson `tests/11_f8_the_tick_budget.loft` learned when it ticked a MARKERLESS
world and could not have seen a line-of-sight regression.

## Cross-repo coordination

⚠ **The camera is PORTED, not depended on** — `moros_render` is unpublished and
pulls `hex_proj` + `mesh3d` + a GL surface built for a browser client, where the
camera itself is ~80 lines of spherical trigonometry.  Same shape and same
trigger as [`plan 20`](../20-entity-art/README.md)'s decision about `hex_part`.

⚠⚠ **The convention that must not be ported wrong is the UP AXIS.**  moros is
`+Y` up with the lattice in `XZ`; dryopea is `+y` **SOUTH** with `+z` up
(`CLAUDE.md` § Hex convention).  That is exactly the class of mistake
`lat_to_metres`' y-negation already exists to contain, and plan 09 C3's sign
check is the shape of gate it wants.

## What this plan does NOT build

**No indoor camera modes.**  moros ships SNUG / CUTAWAY / EYES for the case *the
character is inside a house*; dryopea's bases have walls and no roofs, so there
is no interior (`docs/RENDERER.md` § R1).  FOLLOW and Overview only.

**No exploration.**  The camera this builds is what exploration needs, and base
defence needs it anyway.  ⚠ What is out there, how big the world is, and what
travel costs are a DESIGN pass that has not happened — `docs/RENDERER.md`
§ Exploration, and § Open questions 1 below.

**No HUD, no menus, no landing flow** — still [`plans/05`](../05-validation-scenario/README.md).

**No retirement of the software rasteriser.**  It keeps the editor's view and
all 520 measurements.  ⚠ Its trigger to die is in `docs/RENDERER.md` § R2: when
GL can draw the top-down view and reproduce those measurements.

## Open questions

1. ⚠ **Exploration needs a design pass before it needs a plan.**  It is the one
   part of the project owner's statement this document does NOT commit to, and
   the reason is that it touches the run's shape, the world bound (radius 40 is
   one of the three numbers the tick budget is derived from — **a bigger world
   costs simulation whether or not anybody is looking at it**), the scrambler
   bubble, and what [`ROBOT_ECONOMY.md`](../../docs/ROBOT_ECONOMY.md) is for.
   ⚠ And it has to pass `CLAUDE.md`'s design test: *does leaving the base cost
   something?*  Exploration that is safe because the base is paused is the "lean
   back" this project's first rule refuses.
2. **Does A/D keep strafing?**  `docs/RENDERER.md` § Open 1 — moros refuses it
   because the camera derives from facing; dryopea drives a HOVER unit that has
   no reason to turn before it moves.  *Recommendation there: keep strafing and
   derive azimuth from VELOCITY.*  R1 decides.
3. **`DESIGN.md` § 11 and § 12 are now false** and need editing, not
   reinterpreting — the camera is not "locked, no mouse orbit", and the mouse is
   no longer reserved solely for UI clicks.  ⚠ *Something has to keep doing the
   work "the mouse is free for placement" was doing.*  Lands with R1.

## See also

- [`docs/RENDERER.md`](../../docs/RENDERER.md) — the decisions and the R0
  measurements.
- [`plans/20`](../20-entity-art/README.md) — what this draws; its A5 is blocked
  on R4.
- [`plans/19`](../19-the-interactive-loop/README.md) — the loop that runs the
  game; P4 was the first attempt at this and is superseded.
