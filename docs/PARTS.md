<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# PARTS — every entity is a part-tree, and its geometry is DERIVED

*(project owner, 2026-08-15: "I want a rather detailed graphics for the
enemies, workers and especially the main hover vehicle (a big quad copter with
fixed base with bigger behind the cockpit rotors) we use the ../moros way of
vehicle building where a bigger house is also the vehicle in the world with
possible moving parts the cockpit can be opened upwards with a back hinge. We
draw png images of all the mobs and towers")*

*(and, the same day, the sentence that rewrote § D4: "I want the dynamic camera
of ../moros and not a static camera of the base or terrain. A big part of the
game will be exploration with your vehicle beside orders for your helpers and
some direct work on tower tops")*

This file holds the **decisions**.  The order of work, phase by phase with its
gates, is [`plans/20-entity-art`](../plans/20-entity-art/README.md).

⚠ **The design it inherits is moros's, and moros's copy is the authority on it.**
[`../moros/doc/claude/PARTS.md`](../../moros/doc/claude/PARTS.md) § P9.0 is the
model — a part-tree, limbs on joints, three limb kinds, scale derived, the
hitbox a subset of the skin — and this file does not restate it.  What it
records is **the places dryopea deviates and why**.

⚠ **Read § D4 before quoting anything about pixels.**  This file's first draft
specified sprites baked at a fixed three-quarter projection; the project owner's
answer — *the dynamic camera of moros, and exploration as a pillar* — replaced
that with geometry.  The camera, the renderer and their gate moved out to
[`RENDERER.md`](RENDERER.md).  What is left here is **what an entity IS**, and
it is measured in metres and turns and mentions no pixel at all.

---

## What this replaces

[`PROXY_ART.md`](PROXY_ART.md) — *"primitive geometry first … cuboids and
cylinders"*, a magenta box per entity class, facing signalled by a black front
face.  Every entry in it is honest and none of it is DRAWN: dryopea renders
ground, markers and a HUD, and **nothing of the running game appears on screen
at all** ([`plans/19`](../plans/19-the-interactive-loop/README.md) P4 is the
first phase that would have).

So this is not a rewrite of working art.  It is the first art, and the question
it answers is *what is the source of truth for what an entity looks like* —
before there is anything to be sorry about.

⚠ **PROXY_ART.md is not deleted and its numbers are not stale.**  Its sizes are
the ones the simulation was tuned against, and § D6 makes them the gate a part
has to pass.  What retires is its *shapes*, entry by entry, as § D8's catalogue
covers them.

---

## The inheritance, in one paragraph

A **part** is an object with moving pieces.  A door, a monster, a cart and a
house are the same shape and differ only in how many limbs they have.  A part
offers **sockets**, fits into one socket class, carries its own **hinge** (a
pivot point, a revolute axis, and a swing range in TURNS), and holds
**instances** of other parts bound into its sockets.  A limb is **solid**
(blocks, can be struck), **yielding** (gives way at a cost, returns) or
**visual only** (no hitbox at all).

⚠ **The units and field names are moros's, exactly** — `Hinge { ox, oy, oz,
ax, ay, az, lo, hi }` with the swing in turns, because
`../moros/lib/hex_part/src/hinge.loft` says why in its own banner: *"Two units
for one quantity is how a conversion goes missing."*  A degrees-here /
turns-there hinge looks perfectly reasonable in both files.

---

## D1 — dryopea gets its own part model, and does not take `hex_part`

**The decision: a dryopea-native `src/part.loft` carrying moros's vocabulary,
units and invariants — not a dependency on `hex_part`.**

Measured before deciding:

| | |
|---|---|
| `hex_part` | 4 003 lines, **unpublished** |
| its dependencies | `hex_voxel` (a whole voxel world + `.hxw` save format), `glb_read`, `hex_field` — all path-deps into `../moros` |
| what a part IS there | a small `hex_voxel` store — *"a part IS a world"* (moros § P1) |
| what dryopea's world IS | `PaintedWorld` (sparse, sea-default, painted `u8` kinds) + `HeightLayer` |

⚠ **Taking the dependency means two world models in one repo**, and the second
one arrives to hold *assets* rather than terrain.  dryopea already has one
unpublished path-dep (`moros_map`) and `loft api` reports it **NOT INSTALLED** —
declared for six plans and never consumable.  A build that needs a sibling
checkout at an unstated revision is a build that breaks for the next reader.

⚠ **And "reuse is the rule" is not being waived — it is being read.**
`CLAUDE.md` § Loft consumer relationship forbids *"a dryopea-local version of a
routine a library already provides"*.  `hex_part` provides a part **document
format over a voxel store**; what dryopea needs is a part **model it can draw**.
The overlap is the design, and the design is what is being reused — that is
what *"we use the ../moros way"* asks for.

⚠ **The trigger for revisiting is explicit**, so this does not quietly become
permanent: when `hex_voxel` and `hex_part` are published to the registry **and**
moros#8 (which tree owns the store) is settled, re-read this decision.  Until
then dryopea's model must stay small enough to throw away — which is § D2's
other reason.

---

## D2 — a limb's body is PRIMITIVES, not hex cells

**The decision: a limb's body is a list of BOXES and DISCS in the limb's own
frame, in metres.**  This is the first deviation, and it is the sharpest.

moros authors a limb as **cells** in a hex voxel grid, because a moros limb is a
house — a building at terrain scale, on the lattice the terrain uses.  A
dryopea limb is a **rotor 0.5 m across**.  A hex is 1.5 m.

⚠ moros's own § P9.1 answers this in principle — *"a limb is authored at its OWN
scale, and the ratio is already in the file"* — so a part could declare
0.05 m per cell and author a rotor in 200 cells.  Rejected, for two reasons
that are dryopea's rather than moros's:

1. **A hex grid is the wrong lattice for a machine.**  Rotor booms are radial
   and the canopy is a wedge; expressing either as odd-r offset cells is
   fighting the lattice for nothing, and the result still has to be *drawn* as
   polygons.
2. **The primitive list is already the project's stated convention.**
   `PROXY_ART.md` § Conventions opens with *"primitive geometry first — cuboids
   and cylinders"*.  This design makes that convention structural instead of
   replacing it.

⚠ **It is a THIRD body kind in a slot that already had two, not a new concept.**
moros § P5 gives a part two possible bodies — cells, or a `.glb` — and § P9.3
corrected the rule to *"nothing NEEDS a custom mesh, not nothing may be one"*.
A box list sits in the same slot under the same rule.  A cells body or a `.glb`
body can be added later without touching the tree, the sockets or the hinges,
because none of them ask what a limb is made of.

**Two primitives, deliberately:**

| kind | fields | drawn as |
|---|---|---|
| `PRIM_BOX` | centre (x, y, z), half-extents (hx, hy, hz) | its ≤3 visible faces, each a shaded quad |
| `PRIM_DISC` | centre, normal, radius, thickness | an N-gon fan in the disc's plane |

⚠ **No cylinder.**  A boom arm is a thin box and reads as one at 24 px/m; a
cylinder is a third rasteriser for a difference nobody can see at this scale.
Add one when a measurement says the box is the thing that looks wrong.

---

## D3 — the tower already HAS a socket, and that is this design's proof

This is the finding that turns the part model from *a nice way to draw things*
into *the shape the game already has*.

[`plans/17`](../plans/17-tower-hot-swap/README.md) T2 shipped a mechanic where a
tower's **top is a carry object**: `tower_detach_top` takes it off (the tower
stops firing), `tower_mount_top` puts it on another tower, and the magazine
travels **with the top**.  `tower_mount_top` **refuses an occupied tower**, so
a hot-swap is composed out of a detach and a mount.

⚠ That is a **socket** — offered by the base, filled by at most one instance,
refusing a second — built in the simulation eight months before anything drew
it.  So the art does not need a parallel notion of "does this tower have a top":

```
tower (part)                       the sim
  └── SOCK "top"  ──────────────>  TowerState.top / tower_detach_top / tower_mount_top
       └── INST tower_top          the CarryObject, magazine in `subj`
```

⚠ **The sprite follows the simulation, and never a second flag.**  Which pose a
tower draws in is *"is its socket filled"*, asked of `TowerState` — the same
rule `CLAUDE.md` states for a carrier and its cargo (*"a slot on the carrier and
an owner on the object are two facts that can disagree"*).  `plans/20` A6 gates
exactly this: detach the top in the sim and the top must vanish from the frame,
measured.

**The other joint the game already has** is the canopy — new, and the one the
project owner specified.  It is a `HING` and nothing else.

---

## D4 — a part emits GEOMETRY; there is no sprite and no baked scale

⚠⚠ **THIS SECTION REPLACES TWO EARLIER ONES, AND THE CORRECTION IS THE PROJECT
OWNER'S.**  The first draft of this file specified a fixed three-quarter
projection (θ = 55°), sprites cached at `SPRITE_FACINGS = 8` yaw buckets, and
`SPRITE_SUPERSAMPLE`.  The question that killed it was one sentence — *"how much
do we depend on px if the game is a GL game with a free camera?"* — followed by
the decision: **the dynamic camera of moros, not a static camera of the base or
terrain**, with exploration as a pillar of play.

The strike-through is not kept, because unlike moros § P9 the mistake here is not
instructive — it is just what happens when a design is fitted to the renderer
that happens to exist rather than to the game.  What IS worth carrying:

⚠ **A sprite sheet does not degrade under a free camera, it LIES.**  Eight yaw
buckets at one pitch is a raster baked at one viewpoint.  A camera that eases,
orbits behind the vehicle's facing and drops to ground level has continuous
distance, continuous yaw **and continuous pitch**; the pitch mismatch is visible
the moment the boom moves.  So the sprite cache is not *worse* under a dynamic
camera — it is **not to be built**.

**The decision: a part emits TRIANGLES, and pixels appear only at the final
rasterisation.**

| layer | depends on pixels? |
|---|---|
| the part-tree — metres, turns, sockets, limb kinds | **no** |
| the geometry emitter — boxes and discs → triangles in the part's frame | **no** |
| the pose — hinge angles → transforms | **no** |
| the camera — eye, target, fov | **no** |
| the rasteriser | yes, and only here |

⚠ **That is the answer to the question that prompted this rewrite: nothing above
the rasteriser depends on px** — which is exactly how a GPU is organised, and is
why the same part-tree serves a software rasteriser today and GL tomorrow.

⚠ **The hinge stops being a cache key and becomes a NUMBER.**  Under the sprite
design the canopy had two poses, `shut` and `open`, and anything between them
was unrepresentable.  Under geometry the canopy's angle is a float in turns and
the lid *swings*.  The same is true of the rotors: they were going to be one
blurred disc because animation frames multiplied the cache, and now they simply
spin.

The camera, the renderer and the gate that measures them are
[`RENDERER.md`](RENDERER.md).  This file stops at *what an entity is*.

---

## D5 — a PNG is an ARTEFACT, and the gate reads one back

**The decision: PNGs are captured FRAMES, not inputs.**  Nothing in the runtime
loads one.

This survived the rewrite unchanged, and one of its three reasons got stronger:

1. ⚠ **`imaging::Pixel` is `{ r, g, b }` with no alpha** — the registry's only
   PNG *decoder* drops transparency.  Under the sprite design that was a
   limitation to work around; under D4 it is **irrelevant**, because the thing
   being decoded is a captured frame and **a captured frame is opaque**.
2. **`graphics::Canvas` carries alpha** (`blend_pixel`, and `save_png`
   *"automatically uses RGBA if any pixel has alpha < 255"*).
3. **Per-frame part rendering is affordable precisely because it is geometry** —
   a few thousand triangles is what a rasteriser is for, where a few hundred
   cached rasters was a way of avoiding one.

⚠ **And the PNG readback is now LOAD-BEARING rather than decorative**: it is how
a GL frame gets classified.  `RENDERER.md` § The gate chain measures it end to
end — GL → `gl_screenshot` → `imaging::png` → exact classification, with **zero**
colour drift over 76 800 pixels.

⚠ So *"we draw png images of all the mobs and towers"* is still delivered, and
delivered better: a `make shots` that poses each catalogue entry in front of the
real camera and captures the real renderer, rather than a sprite baker whose
output only ever resembled the game.

---

## D6 — the durable artefact is the SIZE, and it is GATED

moros § P9.7: *"a red blob at the right size beats a good mesh at the wrong
one"* — sizes are not regenerable, art is.  dryopea can do better than record
that, because dryopea's sizes are **already written down and already load-bearing
in the simulation**.

⚠ **So a part declares its footprint, and a test compares it to the number the
simulation uses.**  A vehicle whose art grew to 3 m while `VEHICLE_*` still says
2.4 fails `scripts/test.sh` naming both numbers.  That is the same shape as
`tests/numbers_design_targets.loft`, which pins `docs/NUMBERS.md`'s design
targets against the running sim, and it exists for the same reason: **nothing
loads a number from a document, so the two copies need a gate between them.**

⚠ **The hitbox is a SUBSET of the skin, and it is the SIM's, not the art's**
(moros § P9.11).  dryopea's hitbox is already a hex — one hex, `blocker_at`,
`can_occupy`.  Art may overhang it freely, and every entity in `PROXY_ART.md`
already does: a 2.7 m robot on a 1.5 m hex is nearly two hexes long.  ⚠ **Never
derive passability from a part's extent** — that would make a drawing decide
where a robot may stand, and `passable.loft` is the one door for that.

---

## D7 — the hover unit, specified

*"a big quad copter with fixed base with bigger behind the cockpit rotors …
the cockpit can be opened upwards with a back hinge"*

```
hover_unit                       root part
  ├── base          solid  fixed    chassis slab + two skids — the "house"
  ├── boom ×4       solid  fixed    thin boxes, hub-ward from the base
  ├── rotor_fl/fr   visual fixed    discs, radius 0.36 m   ← FRONT, smaller
  ├── rotor_rl/rr   visual fixed    discs, radius 0.52 m   ← REAR, bigger
  └── canopy        solid  HINGE    rear-edge lateral axis, 0 → 0.30 turns
```

Local frame: `+y` is **forward**, `+x` starboard, `+z` up; the origin is the
centre of the footprint at hover height.

| limb | centre (x, y, z) | size | note |
|---|---|---|---|
| base | (0, 0, 0) | 1.50 × 0.80 × 0.30 | the fixed base |
| skid ×2 | (±0.34, −0.05, −0.26) | 1.20 × 0.10 × 0.22 | reads as "it lands on something" |
| boom ×4 | to each hub | 0.09 × 0.09 × length | thin boxes |
| rotor front ×2 | (±0.52, +0.55, +0.26) | r 0.36, t 0.03 | |
| rotor rear ×2 | (±0.62, −0.62, +0.30) | r 0.52, t 0.03 | **44 % larger** |
| canopy | (0, +0.28, +0.36) | 0.62 × 0.60 × 0.40 | hinge at its REAR edge |

**Overall: 2.28 m wide × 2.05 m long × ~0.9 m tall.**

⚠ **These sizes were chosen under the sprite design and want re-opening.**  They
were bounded by *"how much reads at 55 px"* and by not overlapping neighbouring
hexes too badly in a top-down view.  Under § D4's camera neither pressure
applies: the boom comes down to the vehicle, so the limit on "big" is the
gameplay one (§ below) and not the pixel one.  ⚠ A real quadcopter silhouette
wants the rotor tips further out than 1.14 m from centre, and the reason to
resist is now **only** that the hitbox is one hex.

⚠ **"Big" is bounded by the HEX, and the bound is a gameplay one.**  At 2.28 m
the unit is wider than the 1.5 m hex it occupies and overlaps its neighbours.
That is accepted (§ D6: art overhangs the hitbox, and the enemy proxy already
does at 2.7 m) — but a vehicle that reads as genuinely two hexes wide starts
telling the player something false about where it can fit, and `blocker_at` is
one hex.  ⚠ That is a SIMULATION change if it is wanted, not an art one.

**The canopy hinge, exactly:**

```
Hinge { ox: 0.0, oy: -0.02, oz: 0.56,     // the rear top edge of the canopy
        ax: 1.0, ay:  0.0,  az: 0.0,      // lateral axis — it tilts fore/aft
        lo: 0.0, hi:  0.30 }              // TURNS: shut → 108° open, backwards
```

⚠ **The axis is stored as given and is not normalised**, matching
`hex_part/hinge.loft`'s rule (*"the length of the axis carries no meaning; a
consumer that needs a unit vector normalises where it needs one"*).

⚠ **The rotors are `visual` and always spinning.**  The vehicle hovers whenever
it exists (`VEHICLE_CLIMB_METRES` 0.4 m of clearance is *"the visible baseline"*,
PROXY_ART), so a stopped rotor is a state the game does not have.

⚠ **Under § D4 they actually SPIN**, and that is the clearest small illustration
of what changed: the sprite design was going to draw one blurred disc, because
animation frames multiplied a cache and nobody would see the difference at
25 px.  With geometry a rotor is a disc with a rotation angle, the angle is a
float, and it costs one multiply per frame.  ⚠ Their spin rate is the one place
BOOST becomes visible without a HUD, which is `DESIGN.md` § HUD's *"diegetic"*
rule getting something for free.

---

## D8 — the crew is the same part, and the enemies are one part with DATA

**The crew.**  `DESIGN.md` § 9 opens with *"same chassis as the player"*, and
the code already means it: `drive_along` is *"the shared chassis"* and
`salvage_at` is one implementation with two doors.  ⚠ **So a helper is the
hover unit part with a different livery, and never a second part.**  If a
helper needs its own part-tree, the chassis claim has broken and that is worth
knowing.

**The enemies.**  `CLAUDE.md` § Movement is emphatic — *"ONE AI, per-class
DATA … a class that needs its own mover has broken it"* — and the four small
robots (scout, harvester, builder, miner) *"differ in NOTHING else"* but how
fast they chew a wall.  ⚠ **The art follows the same rule: one robot part, and a
class is a row of data** (a colour, a scale factor, at most one distinguishing
limb).  A class that needs its own part-tree has broken the same invariant one
layer up, and the catalogue test is where that shows.

**The boss** is the named exception in both places — *"their size and their
options are different, and that is what makes them special events"* — and its
2×2 footprint is not built in the sim yet either.  Out of scope; the part model
carries it with no new concept, which is the point of § D1.

**The towers** are § D3: a base part offering one socket, and a top part that
fits it.

---

## What could kill this design

⚠ **The pixel claim is GONE, and with it the probe that guarded it.**  The first
draft's load-bearing claim was *"a box-and-disc part-tree reads as a quadcopter
at 55 px"*, and A0 existed to look at one image and say.  Under § D4 the camera
comes to the vehicle, so there is no fixed pixel size to be wrong about — the
question answers itself at whatever distance the boom sits.

**What can still kill it is one layer down, and it is a COST claim:** that a
part-tree of boxes and discs is cheap enough to draw for every entity, every
frame, with no cache.  Eighty enemies at a dozen primitives each is a few
thousand triangles, which is nothing for a GPU and is **not** obviously nothing
for a software rasteriser at 960×720.  `RENDERER.md` § The cost owns that
question and the probe that answers it.

⚠ **The other survivor is the failure mode, and it moved rather than died:**
*"it is fine"* is still what a design like this fails as.  A part-tree that
renders correctly, cheaply and unremarkably passes every automated check this
design can write.  ⚠ **So the gate stays a person looking at a picture** — which
is now a captured FRAME of the real renderer at a real camera pose rather than a
sprite baked for the occasion, and is therefore better evidence than it was.

---

## Open, and decided rather than asked

1. ⚠ **`cam.zoom` changes no pixel.**  `camera_update` moves it on the wheel,
   `save.loft` persists it, `script.loft` walks to it and `script_state_line`
   reports it — and `grep` finds no renderer that reads it.  It is not a
   regression; it was never wired.  *Decision: a row in
   [`PROBLEMS.md`](../PROBLEMS.md) (`@D002`), not a phase of this plan.*
   ⚠ It was found as a *lever* for the pixel problem § D4 deleted, so it is now
   purely an **editor** defect — the game's camera is `RENDERER.md`'s and does
   not go through `EditorCamera` at all.

2. **Parts are authored in loft, not in a data file.**  moros § P1 authors a
   part *in the editor*, which is the right destination and needs a part editor
   dryopea does not have.  ⚠ The interim is **not** a JSON parts file: dryopea
   already has one authored-data file nothing loads (`examples/numbers.json`,
   hand-copied into constants with a test pinning the two together), and a
   second would be a second drift.  A part-tree in `src/catalogue.loft` is
   compiled and gated.  *Trigger to revisit: the first part authored by somebody
   who is not editing loft.*

3. ~~**The sprite cache is built at load, not lazily.**~~ ~~**Eight facings,
   not sixteen.**~~  **Both deleted by § D4** — there is no cache and there are
   no facings.  Listed as deleted rather than removed, because a reader who
   knows sprite pipelines will look for these two questions and should find out
   they are not open rather than not asked.

4. **A mover's FACING is a continuous angle, not a bucket.**  It comes from the
   metre-space direction between where the mover is and where it is going —
   the way plan 19 P2 derived the drive heading — and ⚠ **never from a
   (direction → dq, dr) table**, which `lattice.loft` deliberately does not have
   and `CLAUDE.md` § Hex convention forbids reaching for.  *Decision: a float in
   radians on the mover, derived; the renderer never asks the lattice anything.*

5. **Yielding limbs have no dryopea consumer.**  The kind is carried because it
   is moros's and costs an enum row.  ⚠ The obvious candidate is NOT trees:
   `DESIGN.md` § Trees as terrain makes a tree stem a *plateau*, so it is
   terrain and not a part.  *Decision: carry the kind, and if nothing consumes
   it by the time the catalogue is full, delete it and say so.*

---

## See also

- [`../moros/doc/claude/PARTS.md`](../../moros/doc/claude/PARTS.md) § P9.0 —
  **the model**, and the authority on it.  ⚠ Read § P9.1–§ P9.12 as the record
  of how it was arrived at, including four corrections; act on § P9.0.
- [`plans/20-entity-art`](../plans/20-entity-art/README.md) — the order of work.
- [`PROXY_ART.md`](PROXY_ART.md) — the sizes this replaces the shapes of, and
  the gate § D6 turns them into.
- [`DESIGN.md`](DESIGN.md) § 12 — the over-the-shoulder camera this design
  deliberately does not build, and § 9 for *"same chassis as the player"*.
- [`plans/17`](../plans/17-tower-hot-swap/README.md) § T2 — the socket the
  simulation already has.
