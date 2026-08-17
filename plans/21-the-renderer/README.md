<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 21 — The renderer: a camera that comes to the vehicle

**Value:** `G` · **Effort:** `VH`

## Status

**R0 + R1 + R2 done** (R2 2026-08-17).  R3 is next.  Suite **1198** green, gate
33 scripts / **654 measurements unchanged** — the camera touches no simulation,
and R2 moved none of them either.

⚠⚠ **R2's headline is a rule dryopea already had and the camera nearly broke**:
`play.loft` is frame-rate independent by construction (`19-P0`), so moros's
linear `f = k·dt` ease could not be ported.  `1 − e^(−k·dt)` composes exactly —
one frame of a second and sixty of a sixtieth land on the same bits (`@M023`).

⚠⚠ **And the ease turned out to be load-bearing for a reason moros does not
have**: dryopea's vehicle is a LATTICE position, so it jumps 1.299 m on the tick
it steps.  Un-eased the camera moves on 12 frames of 240; eased, on 221, with a
worst frame nine times smaller.

⚠⚠ **R1 corrected the design in three places, and one of them was this
document's own frame note.**  `docs/RENDERER.md` § Open 5 warns that
*"dryopea is `+y` south with `+z` up"* is the convention that must not be
ported wrong — and taken as an instruction it is the **wrong frame**.  +y
south is a CANVAS convention; (east, south, up) is left-handed,
`mat4_look_at` builds a right-handed basis, and the product is a MIRROR that
**no azimuth undoes** (`@M021`: eight azimuths, one works in the north frame
and none in the south).  ⚠ So `lat_to_world` negates y and is the only place
that may — which makes the camera's world `hex_grid`'s own frame.

⚠ **`camera_overview` at 89° IS the editor's view, and it is now a number**:
0.0014 rad of bearing and 0.56% of scale over a ring of twelve hexes,
against the software rasteriser on the editor's own viewport (`@M022`).  So
§ R2's one-geometry-two-rasterisers plan holds.

⚠ **moros's follow formula puts the camera ABEAM** — along-track exactly zero
at all four cardinal headings, tracking and easing and looking like a working
camera.  `@X067` is the corrected bearing.

⚠⚠ **The overview gate read a PERFECT 0.0 rad twice while measuring
nothing** — see § R1's post-mortem.  It is the phase's most transferable
lesson and it cost two rounds to see.

R0 ran before the design was written, because the design was not affordable
without its answer.

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
| **R1** ✓ | `camera_follow` puts the eye behind the vehicle's velocity at all four cardinal headings; `camera_overview` at 89° reproduces the editor's view to **0.0014 rad and 0.56% of scale** (`@M022`) | one camera, two presets — the editor's existing view is a MODE of the game's camera | ⚠ **read the eye out of the VIEW MATRIX** (`eye = −Rᵀt`), never out of the camera's own trace.  ⚠⚠ **TWO controls FIRED and a third was missing**: § The moros formula puts the camera abeam fired as designed; `@M021`'s south-frame sweep fired (zero azimuths of eight); and the overview gate itself **read a perfect 0.0 rad twice while measuring nothing** — the missing control was *"can this gate produce a non-trivial reading at all?"*, now an assertion |
| **R2** ✓ | the ease is exponential and lands on the same camera at 60 fps and 10 fps (`@M023`); a `wall_high` on the sight line takes the boom 5.831 m → 3.624 m, a `wall` at the same cell takes nothing (`@M024`) | the ease is PUBLISHED, not merely solved | ⚠⚠ **the exact bug moros shipped**: the solve sat inside `if moved`, so a converged camera never left the server and the fault was invisible in precisely the case the camera exists for.  The control is § A standing vehicle still brings the camera home, plus § A frame that spends no tick still moves the camera — a 0.1 s frame buys zero ticks and must still move the eye.  ⚠ The linear ease is measured BESIDE the exponential one and asserted to disagree, so a stubbed ease cannot pass |
| **R3** | a hex carrying rubble meshes at `hex_height` (structure **+** layer) and colours from `hex_ground` (`rubble`) | the surface is not the painted kind (`CLAUDE.md`'s oldest trap) | ⚠ get it backwards and piling debris on a wall LOWERS it — visibly, and no existing test would fail |
| **R4** | the same scenario, drawn by GL and classified, lands in the same bands the software path reports | one geometry layer under two rasterisers (§ R2) | ⚠ **lighting breaks exact classification** — a shaded frame turns one palette colour into a range and `unknown` stops meaning "fault".  The gate renders FLAT UNLIT; loosening to nearest-colour would discard what R0 measured |
| **R5** | a frame-cost RATIO inside `loft test` | cost is a ratio, never a stopwatch | ⚠ `CLAUDE.md` § Cost: an unchanged file timed 173 / 737 / 754 ms on three interpreter runs, and discarded structs are not freed — a standalone stopwatch here measures the harness |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **R0** — can a GL frame be gated headlessly? | XS | two probes: `xvfb` + GL context + `gl_screenshot`, then `imaging::png` + exact classification.  **Measured: `other` = 0 over 76 800 px** | **Done** (2026-08-15) |
| **R1** — the camera | M | `tests/21_r1_the_camera.loft` (15 fns) — `RenderCamera` ported with moros's fields; follow puts the eye behind the VELOCITY; overview at 89° matches the editor's projection to 0.08° and 0.56%.  ⚠ Asserted on the VIEW MATRIX, not on the struct | **Done** (2026-08-17) |
| **R2** — the boom: ease and occlusion | M | `tests/21_r2_the_boom.loft` (21 fns) — the ease is exponential and frame-rate independent, the azimuth wraps the short way, the boom shortens behind a wall on the line and not beside it, and rest is EXACT.  ⚠ Occlusion asks the walker `tower_sees` asks, which R2 factored out into `passable.loft::sight_first_block`.  ⚠ Carried R1's deferred half: `PlayState.cam` (`@X014`), as a `CameraRig` | **Done** (2026-08-17) |
| **R3** — the terrain mesh | **H** — ⚠ probably its own plan | `tests/21_r3_the_mesh.loft` — top faces at `hex_height`, side quads to lower neighbours, rubble surfaces coloured from `hex_ground`, rebuilt per dirty chunk | **Next** |
| **R4** — the GL path and the gate chain | MH | `scripts/validate.sh` gains GL scenarios under `xvfb`; `classify_world` reads a decoded capture.  Flat-unlit render mode for the gate | Blocked on R3 |
| **R5** — cost | S | `tests/21_r5_the_frame_budget.loft` — a RATIO, the shape `11_f8` already uses | Blocked on R4 |

## R1 — the camera (2026-08-17)

`src/render_camera.loft` + `tests/21_r1_the_camera.loft` (15 fns).  The
decisions are [`docs/RENDERER.md`](../../docs/RENDERER.md) § R1 as built and
are not restated; `@X065`-`@X067` and `@M021`-`@M022` are the codes.  What
belongs here is what the phase LEARNED.

### ⚠⚠ The gate read a PERFECT zero, twice, while measuring nothing

§ The overview IS the editor's view projects a ring of twelve hexes through
the game camera and through `render.loft::world_to_canvas`, and compares the
bearings.  It reported a worst disagreement of **exactly 0.0 rad** and was
green.  Twice, for two unrelated reasons:

1. The ring lived in a file-scope `const vector<integer>` holding negative
   numbers, which loft reads back **EMPTY**
   ([loft#955](https://github.com/loft-lang/loft/issues/955), filed).  Every
   hex came back `Hex { q: null, r: null }`; they all land on the screen
   centre; twelve identical bearings agree perfectly.  ⚠ A loop over an empty
   vector runs zero times, so **every assertion inside it holds vacuously**.
2. Fixed, the comparison was still done in **NDC** — where the aspect ratio
   is baked into the projection's x term, so the space is anisotropic by
   construction.  That reads 0.126 rad of disagreement that is not there
   (`atan` of a 4:3 stretch), which is a *false red* hiding behind a false
   green.

⚠⚠ **The tell was the exactness.**  An integer-pixel-versus-floating-point
comparison over twelve arbitrary hexes cannot produce a true zero — the
editor rounds to whole pixels and the camera does not.  A number that is
*too good* is the same signal as a number that is wrong, and it is much
easier to miss because it looks like success.

⚠ **The control that was missing is generic and cheap**: *can this gate
produce a non-trivial reading at all?*  It is now an assertion
(`assert(to_worst > 0.0, …)`) plus a per-hex `assert(hex != origin)`, and it
costs two lines.  Reach for it wherever a gate compares two computations of
the same thing — that shape can always agree by both being empty.

### ⚠ Three things the design had wrong, and the probes that found them

Each was found by a probe costing under a minute, and each would have been
silent in the shipped renderer.

| the design said | what it is | how it was found |
|---|---|---|
| the world frame is `+y` south with `+z` up (§ Open 5) | that frame is **left-handed** and mirrors the world; `lat_to_world` negates y, so the camera's world is `hex_grid`'s own | swept 8 azimuths × 2 frames and counted which reproduce the editor: **1 and 0** (`@M021`) |
| follow is `azimuth = 270° − facing_deg` (moros's line) | in dryopea's bearing convention that is **abeam** — along-track exactly 0.0 at every cardinal heading | asserted the eye is *behind* at all four headings; the verbatim port is now a live tripwire |
| 89° avoids a **degenerate** matrix at 90° | 90° is not degenerate — `cos(π/2)` is 6.1e-17, the basis is well-formed and every valve reads healthy.  What dies is the **azimuth**: opposite azimuths move the eye 2.79 m at 89° and **2.6e-13 m** at 90° | wrote the test expecting a collapse; it failed, and the failure was the finding |

⚠ **The third row is the pattern worth keeping**: two of these came from
tests written to assert what the design *claimed*, which then failed.  A test
that fails because the design was wrong is the cheapest design review there
is — provided you write down what you expect BEFORE running it, so that a
failure is informative rather than something to tune away.

### ⚠ What R1 deliberately did not build

**The `RenderCamera` field on `PlayState`** (§ Open 4, `@X014`).  The
decision stands and is not reopened; it moves to R2 because that is where the
eased boom gives a session something to REMEMBER between frames.  A field
nobody reads and nothing draws is a decision implemented one phase early and
validated by nothing — `plans/README.md` § What makes a step SAFE is the
rule, and *as small as possible while still being validated* is two bounds.

**No mouse binding.**  `camera_orbit` and `camera_zoom` exist and nothing
presses them.  `DESIGN.md` § 11 records what the buttons will do (left =
placement, right drag = orbit, wheel = boom), which is § Open 2's ask.

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

## R2 — the boom (2026-08-17)

`src/render_camera.loft` § The ease onward, `src/passable.loft::sight_first_block`,
`PlayState.cam`, and `tests/21_r2_the_boom.loft` (21 fns).  The decisions are
[`docs/RENDERER.md`](../../docs/RENDERER.md) § R2b and are not restated;
`@X068`-`@X071` and `@M023`-`@M024` are the codes.  What belongs here is what the
phase LEARNED.

### ⚠⚠ The ease could not be ported, and the reason was in another plan

moros's `cam_approach` is `f = k · dt`, clamped.  It is the obvious shape, it is
what every engine writes, and it is **frame-rate dependent** — 0→1 over one
second reads 1.0 as one frame and 0.9982 as sixty.

dryopea cannot have that, and the reason is not aesthetic: plan 19 P0 measured
that *1200 frames of 1/60 s and one frame of 20 s reach the same state*, and
`play.loft` § And there is no epsilon is built on it.  A camera that disagreed
would put a frame-rate dependence into the one artefact the player looks at,
under a gate chain (§ R4) whose whole job is to photograph frames.

⚠ **The lesson is about where to look for a port's constraints.**  The camera's
came from `play.loft` — a file the camera does not call, in a plan two numbers
away.  *Before porting a routine, ask what the RECEIVING system has already
promised*, because the donor made no such promise and its code cannot say so.

### ⚠⚠ The ease turned out to be doing a different job here

moros eases a camera following floats.  dryopea's vehicle is `(q, r)`, two
integers, and it moves a whole hex on the tick it steps.  So the ease is not
smoothing a residual — it is the entire reason the picture moves between ticks.
Measured over 240 frames with the vehicle stepping every twentieth:

| | frames that moved | worst single frame |
|---|---|---|
| un-eased (`camera_follow_vehicle`) | **12 / 240** | 1.299 m |
| eased (`camera_rig_step`) | **221 / 240** | **0.143 m** |

⚠ That is why the ease carries THREE valves where the phase's own title names
one.  A camera easing the boom alone would ease the quantity the player changes
least and leave the two that jump.

### ⚠⚠ The azimuth bug is reachable with the shipped keys, and it looks fine

Holding **A** is due west — azimuth 360°.  Adding **S** sends the vehicle to the
odd-r south-west neighbour — azimuth 60°.  Eased as plain numbers that is
**−300°**: the camera swings five sixths of a circle the wrong way, smoothly,
decelerating properly, on a rig that is working in every other respect.

⚠ **60° and not 45°**, and the gap is the lattice: A/S names a metre heading at
45°, but `vehicle_facing` reads the velocity between hex CENTRES (`@X067`), and
odd-r puts that neighbour at −120°.  ⚠ The first draft of the test asserted 45°
and the source comment claimed it; the measurement corrected both.

### ⚠ Two tests failed on the FIXTURE, and each named something real

Both were written expecting the code to be wrong and found the fixture wrong
instead — which is the same cheap design review R1 recorded, one layer down.

| the test assumed | what is true | what it changed |
|---|---|---|
| `wall_high` is 6 m (a tower's height, misremembered) | it is **5 m**; `wall` is 3 m | the assertion now reads the two heights out of the palette and asserts the PRECONDITION (both ends stand on something, each looker is above its own surface) rather than a number |
| a wall anywhere on the line separates the two kinds | near the vehicle the ray is only **1.6 m** up and BOTH kinds stop it | the test now FINDS the cell whose ray falls between 3 m and 5 m, and fails loudly if none does |

⚠ The second one is the one to keep: walling the whole line read the same boom
twice and looked exactly like *a camera that reads a kind rather than a height*
— a false red that would have been "fixed" by adding the kind table
`12-B5b` exists to forbid.

### ⚠ One walker, and the endpoint skip was never load-bearing

`tower_sees` walked its own line and skipped both endpoints with a comment
calling the skip essential.  It is not: a tower's eye is its hex plus 6 m and
its aim is the target's hex plus the body, so **neither end can stand above a
ray that starts on top of it**.  The skip was a restatement of the heights.

Deleting it is what lets the camera share the walker, because the camera's far
endpoint is the EYE and terrain there is exactly the eye-inside-the-hillside
case.  ⚠ The walker moved to `passable.loft` — it is a question about
`hex_height`, and putting it there means the camera does not depend on towers.
`tower_sight_fault` now asks it too, so the predicate and the message it
explains cannot disagree.

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
⚠⚠ **R1 MEASURED this and the paragraph above is the trap, not the rule.**
`+y` south is a CANVAS convention; carried into 3-D it is left-handed and
mirrors the world, and no camera setting undoes it (`@M021`).  The camera's
world is `+y` **NORTH**, `lat_to_world` is the one negation, and the frame is
therefore `hex_grid`'s own.  ⚠ It also was not the up axis that went wrong —
`+z` up was right all along — it was the **handedness**, which is a property
of all three axes together and is what a "which axis is up?" question cannot
ask.  ⚠ The bearing convention was the second one, one axis over: see § R1.

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
