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

## D1 — REUSE the published family, and ENHANCE it where it stops  `@X001`

⚠⚠ **THIS SECTION WAS WRONG AND IS REWRITTEN** (2026-08-15).  Its first draft
decided *"a dryopea-native part model, not a dependency"* on the strength of
having read **`hex_part`** — moros's unpublished, `hex_voxel`-backed house-parts
library — and **never checking the published `hex_*` family**.  The project
owner's correction was one line: *"reuse as much of the current libraries as
possible, enhance the libs were needed"*, which is also `CLAUDE.md`'s own rule.

⚠ **The miss is instructive and is why the survey below exists**: I searched for
the thing I already had a name for.  `hex_part` was the wrong name, and the
right one — `hex_body` — was in the registry the whole time.

### The survey, so nobody repeats it

| library | published? | what it is | dryopea |
|---|---|---|---|
| **`hex_body`** | ✅ registry | ⚠⚠ *"rigs, revolute joints with computed limits, pure poses, proxies"* — **a body is a RIG, never a pose** | **the part model** |
| **`mesh3d`** | ✅ registry, **zero deps** | Vec3/Mat4, `Mesh`, `Scene`/`Node`, `cube()`, `mesh_to_floats` | the geometry (§ D9) |
| **`glb`** | ✅ registry | `save_scene_glb` — Blender/three.js readable | the artefact (§ D9) |
| **`hex_draw`** | ✅ registry | *"a wall's ANALYTIC SURFACE recovered as the exact average of its stored edges, so a wall renders as one flat quad rather than a strip"* | walls, [`RENDERER.md`](RENDERER.md) § R3 |
| **`hex_roof`** | ✅ registry | roofs/vaults/arches as one function of a DISTANCE | buildings, later |
| **`hex_form`** | ✅ registry | the exact turtle form + **canonical text, byte-for-byte** | shapes-as-text |
| **`hex_place`** | ✅ registry | place / seat / combine | placement |
| `hex_part` | ❌ `../moros` | a part as a small `hex_voxel` store | ⚠ **not this** — see below |
| `hex_mesh` | ❌ `../moros` | the hexes-and-buildings mesher | ⚠ the CONSTRUCTION is copied, `RENDERER.md` § R3 |

### `hex_body` IS the part model, and it is published

> *"a body is a **RIG** — bones and the limits in the joints between them, never a
> pose"* … *"this module never stores a pose; it COMPUTES one from the current
> joint values, and that computation is a **pure function**"*

That is § D4's design, already written and already shipped.  What it gives:

| | |
|---|---|
| `Rig` + `rig_bone(r, **parent**, ox, oy, len, lo, hi)` | ⚠ a **parented tree** — which `mesh3d::Node` does *not* have (§ D9) |
| `Joint { value, lo, hi }`, `joint_fits` / `joint_offer` / `joint_residual` | limits, and how far past one a request is |
| **`rig_write` / `rig_read`** | ⚠⚠ **a canonical TEXT round-trip** — this is *"you provide a script"*, already a format |
| `rig_world_seg(r, values, i)` | the pose COMPUTED from joint values, never stored |
| `bone_obb` / `obb_contains` | per-bone hitboxes — moros § P9.10's *"the hitboxes are the half an artist can never hand back"* |
| `rig_admissible`, `rig_eq` | validation and comparison, for free |

### ⚠⚠ Where it stops: it is strictly 2-D — and that is the enhancement

`rig_bone` takes `(ox, oy, len)`; `rig_world_seg` answers four floats; there is
**not one `z` in the package**.  Bones lie in a plane and the joint axis is
implicit.

dryopea needs three dimensions: the canopy hinges about a **lateral** axis while
the rotors sit at four different heights (§ D7).

⚠ **So the enhancement is additive and its exact shape is already agreed
elsewhere in the family.**  moros's `hex_part::Hinge` is
`{ ox, oy, oz, ax, ay, az, lo, hi }` — **the same eight numbers**, of which
`hex_body`'s are the planar six-minus-two.  A 3-D bone constructor with the 2-D
one defined in terms of it (axis = +z) unifies two libraries that already agree,
rather than inventing a dryopea shape:

```
rig_bone3(r, parent, ox, oy, oz, len, ax, ay, az, lo, hi)
rig_bone (r, parent, ox, oy,     len,             lo, hi)   // = rig_bone3(…, 0.0, …, 0,0,1, …)
```

⚠ `CLAUDE.md` § Loft consumer relationship is explicit that this is the right
move: *"dryopea may ADD to them under their existing contract, which is the right
move when dryopea needs something adjacent to what a library already does."*

⚠ **And it is a real cross-repo commitment, not a footnote.**  The enhancement is
proposed upstream, gated by that suite, and consumed as a release.  Plan 20's
§ Cross-repo coordination is where it is tracked.

⚠⚠ **CORRECTED 2026-08-18 — this paragraph named the wrong repo.**  It said
`hex_body` *"belongs to hexbody/lavition … so the enhancement is proposed there"*.
The `hexbody` repo contains **no `hex_body` source**: its own log records
*"consume hex_body (renamed from body); src/ is empty of geometry"*, and it is a
prototyping harness that CONSUMES the library.  The source lives in
**`loft-libs-world/hex_body/`**, beside the rest of the `hex_*` family, and that is
where the ask goes.  ⚠ A decision that names the wrong destination sends the next
reader to an empty tree, which is why the correction is here rather than only in
the plan.

### What is still NOT taken, and why that half of the old decision stands

⚠ **`hex_part` remains out of scope** — and this is unchanged.  It is 4 000
unpublished lines whose premise is *"a part IS a world"*, i.e. a small
`hex_voxel` store with its own `.hxw` save format.  dryopea's world is
`PaintedWorld` + `HeightLayer`, and importing a second world model to hold what
are for dryopea **assets** is a different question from reusing a rig.
⚠ Trigger unchanged: `hex_voxel` + `hex_part` published, and moros#8 settled.

---

## D2 — a limb's body is HEX CELLS WITH HEIGHTS, like everything else  `@X002`

⚠⚠ **THIS SECTION WAS WRONG AND IS REWRITTEN** (2026-08-15).  Its first draft
chose box-and-disc primitives over hex cells, on the argument that *"a hex is
1.5 m and a rotor is 0.5 m"*.  The project owner's correction: *"that is strange
— everything is built from hexes but every hex has a height to get the 3rd
dimension."*

**The decision: a limb's body is a CELL FORM WITH HEIGHTS, authored at the
part's own scale.**  The same representation as terrain, houses, roofs and
creatures — which is the whole `hex_*` family's, and the reason it composes.

### Why the size objection was not an objection

moros § P9.1 already answers it: **a limb is authored at its OWN scale, and the
ratio is in the file** — `child.w_unit / parent.w_unit`, derived and never
authored (moros § P9.0 invariant 3).  A rotor is authored in a part-world whose
cell is ~0.1 m; nothing requires a part to use the terrain's 1.5 m hex.  I read
that sentence, quoted it in the first draft, and then argued against it anyway.

### ⚠⚠ And the roundness objection dissolves on a mechanism already documented

The real worry behind "boxes" was that a disc built from cells reads as a
**hexagonal** blob rather than a rotor.  It does not, and the reason is in
[`RENDERER.md`](RENDERER.md) § R3 `@X044`:

> `corner_heights_from` averages a cell's height with each neighbour that shares
> that corner — which is what makes terrain **slope** instead of **step**.

⚠ **That same pass smooths a part.**  The mechanism that stops ground looking
like stairs is the mechanism that stops a rotor looking like a hexagon, and it
is one function serving both because both are cells with heights.  ⚠ And
`hex_roof` shows the family already expresses curvature this way — *"every roof
form anyone names is the same function of a DISTANCE"* — cones, domes and arches
over a height field.

### What choosing boxes would have cost, stated plainly

⚠⚠ **Six published libraries.**  A box list is not a cell field, so none of them
apply to it:

| library | what dryopea would have forfeited |
|---|---|
| `hex_draw` | walls as an **analytic surface** — one flat quad, not a strip of them |
| `hex_roof` | every roof / vault / arch profile, as one distance function |
| `hex_form` | the exact turtle form **and its canonical text** — the script format |
| `hex_place` | place / seat / combine |
| `hex_mesh` (construction) | corner smoothing, `faced_between`, the face pass |
| `hex_field` | the exact cell sets underneath all of it |

That is the opposite of *"reuse as much of the current libraries as possible"*,
and it is a self-inflicted fork of an ecosystem dryopea joined deliberately in
plan 09.

### ⚠ The one thing a cell field still does not carry: an out-of-plan hinge

A cell form gives shape; `hex_body`'s `Rig` gives articulation.  Its bones are
placed in **plan** (`rg_ox`, `rg_oy` — *"joint offset in the parent's frame"*)
and a joint turns about the plan's normal.

⚠ dryopea's canopy hinges about a **lateral** axis — it tips *out* of plan
(§ D7).  That is the one motion a plan-view rig cannot express, and therefore
the whole of the enhancement § D1 proposes.  ⚠ **It is much smaller than "make
the rig 3-D"**: heights already give the third dimension, so what is missing is
a joint whose axis lies **in** the plan rather than normal to it — and moros's
`hex_part::Hinge` already writes that axis down as `(ax, ay, az)`.

### ⚠⚠ And a cell is a COLUMN OF LAYERS, which is where the detail comes from  `@X048`

*(project owner, 2026-08-15: "and there are multiple layers for the extra
details")*

A cell does not carry **one** height.  `hex_voxel`'s model:

| | |
|---|---|
| `LAYER_CAP = 64` | layers per chunk |
| `Chunk { base height, ck_layers }` | *"its OWN ORDERED list of layers"* — the index means nothing outside the chunk |
| `Column { co_cells }` | **ABSOLUTE heights, one per layer, in the chunk's order** |
| `Layer { …, label }` | an optional **cross-chunk label**, so corresponding layers of different chunks are identified by name rather than by index |
| `w_eps` | ⚠ the **minimum layer separation** |

⚠ **So the vertical detail is a STACK, not a single surface.**  One cell can
carry a floor, a shelf, an overhang and a roof; a quadcopter cell can carry a
skid at 0.0, the chassis at 0.3 and a canopy at 0.7.  That is how a form built
from hexes gets detail that a single height field cannot express — and it is the
answer to the question the box-primitive draft was really asking.

⚠ **The label is what makes a layer a THING rather than a slot.**  Index-identity
is chunk-local by construction, and `hex_voxel`'s own comment records the bug
that taught it: *"a marker left on an index therefore lands on the wrong layer"*.

⚠⚠ **The trap, and it is documented as SILENT**: two layers can both fall within
θ of one height, which is what `w_eps` exists to forbid.  A part authored with a
canopy 1 mm above a chassis is not a fine detail — it is a corrupt column.

### ⚠ Layers for PARTS now; layers for the WORLD is a SEPARATE decision

dryopea's world today is one surface per hex — `PaintedWorld` (a kind) plus
`HeightLayer` (a rubble rise) — which is a **degenerate column of one**.

⚠⚠ **Adopting columns for the terrain is not an art change; it is a movement
change.**  `passable.loft::hex_height` answers one number per hex, and
`can_step` / `can_climb` / the flow field are all written against that answer.
The moment a hex has several surfaces, *"which layer is the surface"* becomes a
real question — one `hex_voxel`'s banner explicitly points at
(`WORLD_MODEL.md` § Which layer is the surface) and warns is **not** the same
rule as the mesher's.

*Decision: parts get columns (they are assets and nothing routes over them);
the terrain keeps its single surface until a mechanic needs otherwise.*

### ⚠⚠ BRIDGES — and `DESIGN.md` already dodges the layer problem on purpose

*(project owner, 2026-08-15: "there will be lots of bridges because how trees
work, but robots have build them for themselves too for roads")*

⚠ The trigger above names a bridge, so this looks like it fires immediately.
**It does not, and the reason is already written down** —
`DESIGN.md` § Trees as terrain:

> **Limbs span caverns.**  A cavern is a **hole in the surface map** — a
> **non-walkable kind, the way sea already is** — and a limb across it is a
> walkable strip at height: a bridge one or two hexes wide.
>
> ⚠ **Caverns give the underground geography WITHOUT A SECOND LEVEL.**

⚠⚠ **So a bridge over a HOLE needs no layers at all.**  The hex under the span
is not walkable, so there is never a question of *which* surface a mover is on:
the deck is simply the hex's height.  One surface per hex still answers, and
`hex_height` / `can_step` / the flow field are untouched.

⚠ **The layer question is therefore narrower than it looks**, and it is one
question: **does a bridge ever span WALKABLE ground?**

| span crosses | needs layers? | why |
|---|---|---|
| a cavern (tree limbs) | **no** | the design makes a cavern a hole — non-walkable, like sea |
| water, a ravine | **no** | same dodge; `water` is already non-walkable |
| **another road, a valley floor you can walk** | ⚠⚠ **yes** | two walkable surfaces over one hex is exactly what one height cannot say |

### ⚠⚠ Answered: the walkable case IS wanted, so the trigger FIRES  `@X052`

*(project owner, 2026-08-15: "and a base can have bridges too to reach places
where robots can still walk under, but that is not yet designed")*

So the cheap dodge covers the **natural** bridges (tree limbs over caverns) and
**not** the built ones.  A base bridge whose whole point is that robots keep
walking underneath is exactly two walkable surfaces over one hex.

⚠⚠ **`@X048`'s trigger has fired.**  It is not an art change and it is not
small:

| what changes | today | with layers |
|---|---|---|
| the flow field's NODE | `Hex` | **`(Hex, layer)`** |
| `hex_height(q, r)` | one number | one per layer — *"which layer is the surface"* becomes a real question |
| `can_step` / `can_climb` | hex → hex | (hex, layer) → (hex, layer) |
| `occupancy`, `BlockerMap` | keyed by hex | keyed by (hex, layer) |
| the corpus | 1 094 tests written against one surface per hex | re-read |

⚠ **It composes with [`plans/22`](../plans/22-the-field-cache/README.md) rather
than fighting it** — layers are sparse, so the node count grows only where a
bridge exists, and the roster-bounded sweep (`@X031`) already stops where nobody
is standing.

⚠ **The upstream answer exists**: `hex_voxel` carries a `Column` of absolute
heights per layer, and `WORLD_MODEL.md` § *Which layer is the surface* is the
rule for the FEET — which its own banner is careful to say is **not** the
mesher's rule.  ⚠ Two different questions with one plausible answer between
them is exactly the shape that goes wrong silently.

⚠ **And dryopea has already named the feature**: `DESIGN.md` § Walls — economy +
topology calls *"bridges between walls (the `cy`-layer deck mechanic)"* a
**second-phase feature** with the same free-but-timed build economics as a wall.
So the mechanic is on the list; what is undesigned is the **walk-under
semantics**, and the project owner has flagged it as such.

**Why it is likely worth the cost — three measured problems it answers**

⚠ Recorded as SEEDS, not design.  Nothing below is decided:

- ⚠⚠ **A tower on a bridge is not buried by its own kills.**  Plan 12 B7's
  sharpest finding was that a tower's bodies **ramp over the wall it defends**
  (`@M004` — which is why the pre-walk window later inverted it to +16).  A deck
  above the kill zone is the one place a tower can fire without building the
  staircase that defeats it.
- **A route the enemy does not have.**  Repair is a POSITION (20 s within one
  hex, `17-T1`) and upkeep is a positioning problem (`@M007`).  A bridge is a
  crew route to a tower that does not pass through the wave.
- **A way to NOT be in the way.**  Plan 13 V5: blocking is a property of the map,
  and a parked crew member is a liability.  A deck is somewhere to be that is
  not the enemy's path.

*Decision: author bridges over NON-WALKABLE ground for now — the single surface
holds and nothing is blocked.  The walkable case waits on its own design, and on
whether the three seeds above survive being priced.*

### ⚠ What a span already MEANS, and it is strong

`DESIGN.md`, same section:

> A span is the one chokepoint that **cannot be walled and cannot be flanked**,
> which makes it the place where **a wall is pointless and a tower is
> everything**.

⚠⚠ **That inverts the game's most-measured lesson.**  `@M004`: a sealed wall
nearly doubles the fall clock and a gate buys nothing — so *wall everything* is
the learned optimum.  A span is terrain where that optimum is **wrong**, which
is `DESIGN.md` § It shoots TOWERS' difficulty principle (*the first thing that
invalidates a LEARNED OPTIMUM*) delivered by the map instead of by an enemy.

⚠ **And the robots' roads make spans a READOUT.**  `ROBOT_ECONOMY.md` § 3 gives
transport routes a counter-play — *"block, or interdict for salvage"* — and a
bridge the robots built is both the route's weakest point and evidence of where
the route goes.  A span is intel (`@X029`) and a chokepoint at once.

⚠ **The gating note is already written**, and it is the one a builder would
miss: *"a span is a 1-hex corridor, and `CLAUDE.md` § Testing something that
moves records that a 1-hex-wide corridor cannot tell a flow field from a
heading."*  So a bridge scenario must bend, or it gates nothing.

### ⚠⚠ A PLACEMENT layer, and joints that carry their own limits  `@X049` `@X050`

*(project owner, 2026-08-15: "and there is a special layer for possible
placement of other forms and there is a method to attach limbs/things with
limits to where they can move")*

Both exist in the family; neither is dryopea's to invent.

**`@X049` — the placement layer.**  A layer whose content is not surface but
**where another form MAY GO** — moros's `SOCK` (the joints a part offers) and
`FITS` (the one class it goes into), with `hex_fit` as the predicate.  ⚠ Its
doorstep has a documented FORM: *"a value off the grid the field distinguishes
is **silently snapped, not rejected** — so the refusal has to happen at authoring
time or the round trip reports success on a model nobody wrote"*, and a refusal
must **name its restriction** (`hex_fit` § THE DOORSTEP, law `K-FIT`).

⚠⚠ **dryopea already has one of these and calls it something else.**  A tower's
socket (`@X003`) is exactly a placement slot that refuses a second occupant —
`tower_mount_top` REFUSES an occupied tower, which is `K-FIT`'s named refusal
arrived at independently.  The marker layer is the other: a hex either takes a
marker or does not.

**`@X050` — attachment with limits.**  `hex_body`'s `Joint { value, lo, hi }`,
and the three verbs that make limits usable rather than merely present:

| | |
|---|---|
| `joint_fits(j, v)` | is this value admissible |
| `joint_offer(j, v)` | ⚠ the value the joint will **give you** — clamped, not refused |
| `joint_residual(j, v)` | ⚠ **how far past the limit** the request was |

⚠ `joint_offer` / `joint_residual` are the pair worth noticing: a joint does not
merely say *no*, it says *this much, and you asked for that much more*.  That is
what lets a caller drive toward a limit smoothly — a canopy that opens as far as
it can and reports the rest, rather than snapping or refusing.

⚠ And `rig_admissible(r)` asks it of a whole rig at once, so *"is this pose
legal"* is one call rather than a loop the caller writes.

### So a part is composed, not invented

```
hex_form / hex_draw   the cell form and its heights        the SHAPE
hex_body::Rig         bones, joints, limits, hitboxes      the ARTICULATION
hex_mesh (construction) corner smoothing + the face pass   the SURFACE
mesh3d + glb          nodes, transforms, the export        the ARTEFACT (§ D9)
```

⚠ Every row is a library dryopea consumes or a construction it copies.  ⚠ The
only NEW code is the join between them, plus the one enhancement above.

---

## D3 — the tower already HAS a socket, and that is this design's proof  `@X003`

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
an owner on the object are two facts that can disagree"*).  `plans/20` **A4** gates
exactly this: detach the top in the sim and the top must vanish from the frame,
measured.  ⚠ This said *A6* until 2026-08-18 and that phase has never existed —
plan 20 runs A1–A5, and *poses come from the SIMULATION* is A4.

⚠ **The socket's other half is BUILT** (plan 20 A1, 2026-08-18):
`src/part.loft` holds the `Socket` a part offers and the `Binding` that fills
it, over `hex_body::Rig`, refusing a socket filled twice and a part that
contains itself.  ⚠⚠ **It carries no pose and no coordinate**, which is where
it deviates from `hex_part::Binding`'s `bd_open`: where a socket IS, is
computed from the posed rig by `socket_world` and stored nowhere, so A4 has one
place to ask *is this tower's socket filled* and no second flag to disagree
with.

**The other joint the game already has** is the canopy — new, and the one the
project owner specified.  It is a `HING` and nothing else.

---

## D4 — a part emits GEOMETRY; there is no sprite and no baked scale  `@X004`

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

## D5 — a PNG is an ARTEFACT, and the gate reads one back  `@X005`

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

## D6 — the durable artefact is the SIZE, and it is GATED  `@X006`

moros § P9.7: *"a red blob at the right size beats a good mesh at the wrong
one"* — sizes are not regenerable, art is.  dryopea can do better than record
that, because dryopea's sizes are **already written down and already load-bearing
in the simulation**.

⚠ **So a part declares its footprint, and a test compares it to the number the
simulation uses.**  A vehicle whose art grew to 3 m while `VEHICLE_*` still says
2.4 fails `scripts/test.sh` naming both numbers.

⚠⚠ **MEASURED 2026-08-18, and the claim above is HALF TRUE — there is no
`VEHICLE_*` size at all.**  Of the numbers this gate would bind, three are
load-bearing in the simulation and every PLAN dimension is read by nobody:

| | in `numbers.json` | a `.loft` constant | read by the sim |
|---|---|---|---|
| tower height 6.0 m | ✅ | `TOWER_HEIGHT_METRES` | ✅ the LOS eye |
| robot height 1.0 m | ✅ | `ENEMY_HEIGHT_METRES` | ✅ the aim point |
| body height 0.5 m | ✅ | `BODY_HEIGHT_METRES` | ✅ the body ramp |
| vehicle 2.4 × 1.1 m | ✅ | ❌ | ❌ |
| robot 2.7 × 1.3 m | ✅ | ❌ | ❌ |
| tower footprint | ✅ as a layout NAME | ❌ | ❌ |

⚠ `tests/numbers_design_targets.loft` gates rates, HP, DPS and speed and **not
one size**.  So the heights are gateable today and a FOOTPRINT is not — which is
[`plans/20`](../plans/20-entity-art/README.md) A3's to close, and it has to add
the constant before it can add the gate.  That is the same shape as
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

## D7 — the hover unit, specified  `@X007`

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

## D8 — the crew is the same part, and the enemies are one part with DATA  `@X008`

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

## D9 — the build pipeline: a script in, a mesh and validation frames out  `@X038`-`@X043`

*(project owner, 2026-08-15: "so we need a building pipeline when you provide a
script of how the enemy/helper/player/tower should look and where then a 3d mesh
and a couple of png images are generated to validate if the end result matches
expectations")*

```
   part script  (authored — a loft function per part)
        │
   parse│                                                        dryopea
        ├──> Part { limbs, prims, hinges, sockets }
        │
   pose │  hinge angles from the SIM, or a named pose            dryopea
        │
   bake │  ⚠ flatten the tree to world transforms               dryopea
        │
   emit ├──> mesh3d::Mesh per limb  +  mesh3d::Scene             mesh3d
        │
        ├──> save_scene_glb()  →  build/parts/<name>.glb         glb     THE ARTEFACT
        │
        └──> render N poses × M views                            dryopea THE CHECK
                 →  shots/parts/<name>_<pose>_<view>.png
```

### ⚠⚠ The geometry layer is ALREADY PUBLISHED — do not write one

Measured against the registry before designing any of it:

| dryopea needs | it already exists as | dep |
|---|---|---|
| vectors, matrices, `look_at`, `perspective`, `ortho`, `rotate_x/y`, `trs` | `mesh3d::math` | `mesh3d` |
| a mesh, and `cube()` / `plane()` / `sphere()` | `mesh3d::mesh` | `mesh3d` |
| a scene of nodes with transforms + materials | `mesh3d::scene` — `node_at(name, mesh, mat, Mat4)` | `mesh3d` |
| **write a GLB 2.0 file** — *"readable by Blender, three.js, gltf-validator"* | `glb::save_scene_glb` | `glb` |
| vertices in the shape GL wants | `mesh3d::mesh_to_floats` → `graphics::gl_upload_vertices` | both |

⚠⚠ **So `@X004`'s "geometry emitter" is mostly a MAPPING, not an implementation**, and
[`RENDERER.md`](RENDERER.md) § Open 5's *"port the camera"* shrinks with it: the
matrix half is `mesh3d`'s, and what is left is ~40 lines of spherical
trigonometry over `mat4_look_at`.  ⚠ `mesh3d` has **zero dependencies** and `glb`
has exactly one (`mesh3d`), so this is a cheap edge to add.

### ⚠ `mesh3d::Node` is FLAT, so the glb is a BAKED POSE and not a rig

`Node { name, mesh_idx, material_idx, transform }` — **there is no parent
field**.  The part-tree's hierarchy therefore has to be baked into world
transforms before nodes are emitted, which is exactly the step moros gives its
own file (`hex_part/bake.loft`).

⚠ **Consequences, stated rather than discovered later:**

- the exported `.glb` captures **one pose**.  A canopy shut and a canopy open are
  two exports, not one articulated model.
- an artist opening it gets a **posed model, not a rig** — fine for silhouette,
  proportion and scale review, which is what `@X006` says the export is *for*.
- ⚠ **the ARTICULATION stays dryopea's**, and that is the right side of the line:
  the hinge is `@X007`'s `lo`/`hi` in turns and the pose comes from the
  simulation (`@X003`), neither of which a mesh file should own.

### The script is a LOFT FUNCTION, and § Open 2 is unchanged

⚠ *"You provide a script"* is satisfied by a function per part in
`src/catalogue.loft` — and it should stay one:

- **crawler reached the same answer independently.**  `../crawler/PROPS.md`:
  *"**Not a folder of `.glb` files.**  They are drawn the way houses are drawn —
  **by a function that emits geometry from parameters** — and placed the way
  items are placed."*
- a compiled function means **the export tool and the running game share one
  constructor**, so a mesh cannot drift from what the player sees.  A data file
  would need a parser, a format gate, and a second way to be wrong.

⚠ § Open 2's trigger is untouched: *the first part authored by somebody who is
not editing loft*.  A human-authored data format is what fires it, and this
pipeline is not that.

### Validation is TWO checks, and confusing them is how it fails

| check | what it answers | who runs it |
|---|---|---|
| **measured** | extents match the simulation's constant (`@X006`); triangle counts; `classify_world` pixel shares; the poses DIFFER | `scripts/test.sh` |
| **cold read** | *"does this read as a big quadcopter with bigger rear rotors and a canopy that opens?"* | a person, looking |

⚠⚠ **The measured half cannot see the thing the pipeline exists for.**  A part
can have correct extents, correct triangle counts and correct pixel shares and
still be unrecognisable — which is `PARTS.md` § What could kill this design's
*"the failure mode is not 'it crashes', it is 'it is fine'"*.

⚠ **crawler already pinned the done-criterion for the cold read** and dryopea
should adopt it verbatim rather than invent one: *"stop when a **cold read** names
it uniquely as that form — unique recognizability is 'finished for the game';
don't over-render past it (clarity has an optimum)"*
(`../crawler/SPRITES.md` § The done-criterion).

⚠ **And the PNG set must contain the poses that DIFFER** — canopy shut *and*
open, tower topped *and* bare.  A validation set of one pose per part cannot see
a joint at all, which is plan 20 `20-A4`'s negative control stated as an
artefact.

### ⚠ No Python blueprint for this, and crawler says why

`../crawler/CLAUDE.md`:

> **A BLUEPRINT PHASE is for when the construction is UNKNOWN.**  When the
> primitives already exist in the tree, the cheapest medium is **the engine
> itself** — write the real code and its gate, not a model of it.  A model can
> disagree with the original *silently*: in plan #11 P5 a Python blueprint
> reported a **39 % wall-run overhead where the engine measured 15.5 %**, and the
> wrong number reached a design doc before anyone ran the real thing.

⚠ dryopea's primitives exist — `mesh3d`, `glb`, `Canvas`, and the GL path `@M002`
proved.  **So this pipeline is written in loft directly**, and the *"plot the
concrete end-result first"* step survives as a TEST rather than a prototype:
`20-A2`'s gate already is one (*a canopy at 0.25 turns puts its far edge where
trigonometry says, asserted as a coordinate*).

⚠ **What the `draw` skill is for here is the TARGET, not the asset** — crawler's
other use of it: *"first set the wall aesthetic target with the `draw` skill —
compose + cold-critique a reference view so 'nice' is concrete (a checkable look)
before coding"*.  That is worth doing once for the hover unit before the
catalogue is written, so the cold read has something to be read *against*.

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
