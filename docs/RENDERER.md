<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# RENDERER — the camera comes to the vehicle, and the frame is still measured

*(project owner, 2026-08-15: "I want the dynamic camera of ../moros and not a
static camera of the base or terrain. A big part of the game will be exploration
with your vehicle beside orders for your helpers and some direct work on tower
tops")*

This file holds the decisions about **the camera, the pipeline and the gate**.
What an entity IS lives in [`PARTS.md`](PARTS.md); the order of work is
[`plans/20-entity-art`](../plans/20-entity-art/README.md).

⚠ **This is a bigger change than entity art, and it should be read as one.**  It
retires `DESIGN.md` § 12's *"locked in pose — no mouse orbit"*, it makes the
game's view three-dimensional, and it puts **exploration** beside base defence
as a thing the game is about.  § Exploration is where that last one is unpacked,
and it is the part that most needs arguing with.

---

## R0 — what was measured before anything was designed

Two probes, run before a line of this was written, because the whole of dryopea's
engineering culture is headless measurement and a 3-D renderer is the obvious way
to lose it.

**Probe 1 — can a GL context exist with no display?**  `xvfb-run` + a 320×240
window + a `Canvas` uploaded as a texture + `gl_screenshot`:

```
PROBE: context created
PROBE: capture = true
```

**Probe 2 — can the captured frame be read back and CLASSIFIED?**  This is the
half that matters, because `gl_screenshot` returning `true` proves nothing about
the pixels.  `imaging::png` decodes the capture, and every pixel is bucketed by
**exact** colour, the way `measure.loft::classify_world` buckets a software
frame:

| bucket | measured | expected |
|---|---|---|
| red rect | 9 600 px | **exactly** 120 × 80 |
| green circle | 8 005 px | π·50² = 7 854 |
| background | 59 195 px | — |
| ⚠ **`other`** | **0 px** | — |

⚠⚠ **Zero.**  Not "close enough": over 76 800 pixels the round trip
GL → `gl_screenshot` → `imaging::png` → decode introduced **no colour drift at
all**.  So `classify_world`'s founding rule — *"classification is an EXACT
lookup, not nearest-colour … a pixel that is not a palette colour lands in
`unknown` and is a FAULT"* — survives the move to GL intact.

⚠ **This is what makes the rest of this document affordable.**  Without it, going
3-D would have meant trading 520 measurements and 32 shots for a renderer nobody
could gate, and the honest recommendation would have been *don't*.

⚠ **And the alpha hole stops mattering here.**  `imaging::Pixel` is `{r, g, b}`
with no alpha ([`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md)), which killed
the sprite pipeline — but **a captured frame is opaque**, so the decoder's one
weakness is in a place this design never stands.

---

## R1 — the camera is moros's, and it already contains dryopea's editor view

**The decision: adopt `moros_render`'s `RenderCamera` — an orbit camera around a
target, in spherical coordinates.**

```
RenderCamera { target: Vec3, azimuth, elevation, distance,
               fov_y, near, far, up: Vec3 }
```

⚠⚠ **The finding that settles it: `camera_overview` pins `elevation` at 89°,
which IS dryopea's current top-down view.**  moros's camera family already has
both of dryopea's views as modes of one struct —

| dryopea view | moros mode | elevation |
|---|---|---|
| the editor, today | `Overview` | 89° (89 rather than 90 — it avoids the gimbal clamp) |
| the game, wanted | `Follow` | ~30°, orbiting behind the facing |

— so this is not "adopt a camera and keep ours too".  It is **one camera with
two presets**, and the editor's existing view is one of them.

**Follow, exactly as moros computes it:**

```
camera_follow(head_pos, facing_rad, cam):
    cam.target  = head_pos
    cam.azimuth = 270° − facing_deg
```

⚠ **The camera is derived from the FACING**, which is the contract
`hex_editor/pose.loft` protects when it refuses to let A/D strafe: *"the camera's
whole contract is that it is derived from the facing, so a strafe would be a
second way to change where the character stands relative to the camera"*.
⚠ **dryopea's WASD does not survive that unexamined** — plan 19 P2 gives all
four keys a compass heading in metres, so A/D **strafe**.  Under a follow camera
derived from facing, that is exactly the second authority moros refused.  § Open 1.

**Elevation and distance stay under the player's control**, per moros's own note
on `camera_follow`.  ⚠ That is the actual retirement of `DESIGN.md` § 12: the
camera is no longer *"locked in pose — no mouse orbit"*, and § 11's *"Mouse …
reserved for UI clicks, **NOT camera orbit**"* is no longer true either.  Both
sections need editing, and § Open 2 says what by.

### What dryopea takes, and what it does NOT

moros's [`CAMERA_INDOORS.md`](../../moros/doc/claude/CAMERA_INDOORS.md) ships
**five** settings — AUTO, FOLLOW, SNUG, CUTAWAY, EYES — and they are answers to a
problem dryopea mostly does not have: *the character is inside a house and the
frame contains the roof from outside*.

⚠ **dryopea's bases have walls and no roofs.**  There is no interior.  So:

| moros setting | dryopea |
|---|---|
| FOLLOW | **yes** — the game's camera |
| Overview | **yes** — it is the editor's existing view |
| AUTO (degrades between them) | **later** — it needs something to degrade *to* |
| SNUG / CUTAWAY / EYES | **no** — indoor answers, and there is no indoors |

⚠ **The boom's occlusion sweep is the one indoor mechanism that DOES transfer**,
because a `wall_high` between the eye and the vehicle is exactly the case
`DESIGN.md` § 12 already anticipated (*"terrain blocks line-of-sight to the
vehicle — smooth swing to a position that can see the vehicle"*).  And dryopea
has the query already: `tower.loft::tower_sees` walks one straight line over
`hex_height`.  ⚠ **That is the same question the boom asks**, and it should be
one function asked twice, not two line-walkers that agree today.

⚠ **The EASE is not optional and is the part that will get skipped.**  moros's
whole camera bug — four wrong diagnoses deep — was that *"the camera's ease was
solved on every tick and published on none"*, and the symptom was a camera frozen
mid-ease that every valve reported as healthy.  The lesson dryopea should take
before writing the code: **a solve nobody is told about is a solve that did not
happen**, and the instrument that ends that class of bug reads the eye **out of
the view matrix the renderer actually used** (`eye = −Rᵀt`), never out of the
camera's own trace.

---

## R2 — the pipeline: geometry is shared, rasterisation is not

**The decision: one geometry layer, and the rasteriser below it is swappable.**

```
part-tree / terrain            metres, turns, hex coords    ← no pixels
        │
        ├── geometry emitter   → triangles in world space   ← no pixels
        │
        ├── camera             → view + projection matrix   ← no pixels
        │
        └── RASTERISER         ← pixels appear ONLY here
              ├── GL          the game (3-D, follow camera)
              └── software    the editor (top-down) — 520 measurements live here
```

⚠ **Two rasterisers is a real cost and it is taken deliberately.**  The
alternative — move the editor to GL too — migrates 1 094 tests, 520 measurements,
32 shots and every golden in one step, on a renderer nobody has written yet.
The alternative to *that* is writing a software 3-D rasteriser, which is a third
implementation of something GL does in hardware.

⚠ **What makes it safe is that the shared layer is the one that can DRIFT.**
Which hexes exist, how high they are, what colour they are, and what shape a
part is — those are one answer, used by both.  Only *who fills the triangles*
differs, and a rasteriser cannot disagree with another rasteriser about the
world.  This is moros's own split (`hex_mesh` — *"one chunk of ground, as
triangles, in ONE place"* — consumed by server and client alike, with plan 16 S3
proving the client's derivation matches the server's).

⚠ **The trigger to collapse them:** when the GL path can draw the editor's
top-down view and reproduce the 520 measurements, the software rasteriser is
dead weight and should go.  `camera_overview` at 89° is what makes that a
migration rather than a rewrite.

---

## R3 — the terrain has to become 3-D, and that is the biggest single item

⚠ **A follow camera at ground level over a flat top-down hex painting is
incoherent.**  This is the part of the pivot that is easy to under-scope: entity
art is a few hundred lines, and **the terrain mesh is the real work**.

What dryopea already has, and it is more than it looks:

| needed | dryopea has |
|---|---|
| which hexes exist, and their kind | `PaintedWorld` (sparse, sea-default) |
| height per hex | `passable.loft::hex_height` — authored structure **plus** the runtime rubble layer |
| a dirty-chunk set, so a rebuild is local | `chunks.loft` over `gridmesh`, wired since plan 07 |
| corner geometry in metres | `lattice.loft::lat_corner_metres` (⚠ clockwise in dryopea's frame — the y-negation) |

So the mesher is *"for each dirty chunk, for each hex, emit a top face at
`hex_height` and a side quad down to each lower neighbour"* — and `gridmesh` is
already a dependency whose whole job is the chunk/dirty/mesh pipeline.

⚠ **The trap it walks into is `CLAUDE.md`'s oldest one**: the SURFACE is not
always the painted kind.  A hex carrying rubble stands on `rubble` while the
authored ground underneath is untouched, so the mesher must colour from
`hex_ground` and take its height from `hex_height` — get that backwards and
piling debris on a wall LOWERS it, visibly.

---

## R4 — the gate chain, and it is the same instrument

```
xvfb → GL context → render one frame → gl_screenshot → imaging::png
     → exact colour classification → FrameCounts → measured bands
```

Every link is proven by § R0.  What this buys is that **the game's renderer is
gated by the tool that already gates the editor's** — `classify_world` /
`FrameCounts`, plus the `.keys` scenario runner, plus `snap`.

⚠ **And moros shows what a camera gate actually asserts**, which is not obvious:
`CAMERA_INDOORS.md` judges eleven stations on **where the eye IS, what the frame
HOLDS, and how it is LIT** — e.g. *"how many pixels of the frame are the
subject"*, with thresholds like *no single surface over 60 %*.  ⚠ Its sharpest
lesson is an instrument one: **"an instrument that cannot tell two surfaces apart
cannot judge a threshold about one of them"** — moros's floor and wall sat
0.0003 apart in chromaticity against a 0.0009 tolerance, so a gate about walls
was partly passing on a hole.  dryopea's palette has the same hazard
(`wall` `#d04848` against `wall_high` `#7a1818` is fine; `hill` `#8b6240`
against `rubble` `#8878a8` is fine; but a *lit* frame shades every one of them).

⚠⚠ **That is the real new problem: LIGHTING BREAKS EXACT CLASSIFICATION.**  The
software rasteriser does not blend, which is why exact lookup works today.  A
shaded 3-D frame turns one palette colour into a whole range, and `unknown`
stops meaning "fault".  ⚠ **Do not solve it by loosening to nearest-colour** —
that discards the property § R0 measured.  The two honest options are to classify
a frame rendered with **flat unlit** materials for the gate (a render mode, not a
different renderer), or to give each surface an ID and read an **ID buffer**
rather than the colour buffer.  § Open 3.

---

## R5 — the cost, and what actually threatens it

The tick budget (`CLAUDE.md` § Cost) is about the SIMULATION and is unchanged by
any of this — but the frame now has real work in it, where before it had a
cached texture blit.

⚠ **The number that is NOT known: how many triangles.**  80 enemies × a dozen
primitives × ~12 triangles is order 10⁴ — trivial for a GPU, and the terrain
mesh dwarfs it anyway.  ⚠ So the cost question is not the entities at all; it is
**the terrain rebuild**, and that is exactly what `gridmesh`'s dirty-chunk set
exists to bound.

⚠ **The measured warning worth carrying over:** `CLAUDE.md` § Cost records that
a standalone stopwatch under the interpreter is worthless here — an unchanged
file timed 173 ms, 737 ms and 754 ms on three runs, and discarded structs are
not freed so a long probe degrades as it measures.  Any frame-cost gate must be
a RATIO inside `loft test`, the shape `tests/11_f8_the_tick_budget.loft` already
uses.

---

## Exploration — the pillar this adds, and the one part that is not a rendering decision

*"A big part of the game will be exploration with your vehicle beside orders for
your helpers and some direct work on tower tops."*

⚠ **This is a DESIGN change, not a renderer change, and it should not arrive as a
side effect of a camera.**  What it touches:

- **`DESIGN.md`'s run shape.**  Today a run is *a sequence of bases*, chained by
  what you carry out; the base is the unit and the map is its surroundings.
  Exploration makes the space BETWEEN bases content.
- **The world bound.**  A radius-40 world is one of the three numbers the tick
  budget is derived from, and `atmosphere_haze_radius` is what bounds what is
  drawn.  Exploring wants more world than that, and the flow field is rebuilt
  **once per tick per climb limit** over it.  ⚠ That is the load-bearing
  interaction and it is not obvious: **a bigger world makes the SIMULATION more
  expensive whether or not the player is looking at it.**
- **[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md)**, which is designed and unbuilt — six
  installation types and the routes between them, *"the traffic waves are made
  of"*.  ⚠ **That document is what exploration would be FOR**: something to find,
  which is currently the only thing the design has out there.
- **The scrambler bubble** (25 hexes) and `WAVE_1_PROVOCATION_HEXES` (12) are
  distances from the CORE.  A player exploring far from base is outside every one
  of them, which is either the point (safety through distance) or a hole
  (nothing happens out there).

⚠ **What it does NOT change**: the tower-defence core.  `CLAUDE.md`'s design test
still applies — *does this put something in the player's hands at a moment when
using it costs them something?* — and exploration passes it **only if leaving the
base costs something**.  Driving away while a wave is inbound is exactly that
tension, and it is the same shape as plan 17 T3's finding that upkeep is a
POSITIONING problem.  ⚠ Exploration that is safe because the base is paused is
the "lean back" this project's first design rule refuses.

**Recommendation: give exploration its own design pass before it gets a plan.**
This document commits to the camera and the renderer, which exploration needs and
which base defence needs anyway.  It deliberately does not commit to a world
size, a travel mechanic or what is out there.

---

## Open, and decided rather than asked

1. ⚠ **A/D strafe today, and a follow camera derived from facing may not want
   that.**  Plan 19 P2 gives WASD four compass headings in metres — measured at
   zero drift, and gated.  moros refuses the same thing on purpose (*"A/D TURN,
   THEY DO NOT STRAFE … a strafe would be a second way to change where the
   character stands relative to the camera"*).  ⚠ But moros drives a WALKER and
   dryopea drives a HOVER vehicle, which has no reason to turn before it moves.
   *Recommendation: keep strafing, and derive the camera's azimuth from the
   vehicle's VELOCITY rather than a stored facing — a hover unit that slides
   sideways and keeps its nose forward is correct, and it makes the camera's
   input a thing the sim already computes.*  Decided in the plan's camera phase.

2. **`DESIGN.md` § 11 and § 12 need editing, not reinterpreting.**  § 12 says
   *locked in pose, no mouse orbit*; § 11 reserves the mouse for UI clicks and
   says explicitly **NOT camera orbit**.  Both are now false.  ⚠ *Decision: edit
   them in the same commit that lands the camera, and record what the mouse does
   instead — because "the mouse is free for placement" was doing real work in
   § 11 and something has to keep doing it.*

3. **Lighting versus exact classification** (§ R4).  *Recommendation: a flat
   unlit render mode for the gate.*  It is one uniform, it keeps `classify_world`
   exactly as it is, and it makes the gate measure GEOMETRY AND CAMERA rather
   than shading — which is what the gate is actually about.  An ID buffer is
   strictly better and strictly more work; the trigger for it is a gate that
   needs to tell two same-coloured surfaces apart.

4. **Where the game's camera lives.**  Not on `EditorState.cam` — that is
   `EditorCamera` (a hex position and an unwired zoom, `@D002`) and it is the
   editor's.  *Decision: a `RenderCamera` on `PlayState`, beside the roster and
   the clock — it is a property of a live session, the same argument plan 19 P3
   made for `playing`.*

5. **Does dryopea depend on `moros_render`, or port the camera?**  Same shape as
   `PARTS.md` § D1 and the same answer for now: `moros_render` is unpublished and
   pulls `hex_proj` + `mesh3d` + a GL surface built for a different client.  The
   camera itself is ~80 lines of spherical trigonometry.  *Decision: port the
   camera with its field names and conventions intact, cite this file, and revisit
   when the moros libraries are published.*  ⚠ **The convention that must not be
   ported wrong is the up-axis**: moros is `+Y` up with the lattice in `XZ`;
   dryopea is `+y` **south** with `+z` up.  That is exactly the class of mistake
   `lat_to_metres`' y-negation already exists to contain.

---

## See also

- [`PARTS.md`](PARTS.md) — what an entity is; § D4 is the section this file
  replaced the pixel half of.
- [`../moros/doc/claude/CAMERA_INDOORS.md`](../../moros/doc/claude/CAMERA_INDOORS.md)
  — the camera's five modes, and ⚠ the instrument lesson: read the eye out of the
  view matrix the renderer used, never out of the camera's own trace.
- `../moros/lib/moros_render/src/moros_render.loft` § Camera — `RenderCamera`,
  `camera_follow`, `camera_overview`.
- `../moros/lib/hex_editor/src/pose.loft` — why A/D turn rather than strafe.
- [`DESIGN.md`](DESIGN.md) § 11 + § 12 — the two sections this retires.
- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) — what exploration would be FOR.
