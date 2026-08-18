<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 20 — Entity art: part-trees, and the geometry derived from them

**Value:** `G` · **Effort:** `H`

## Status

**A1 SHIPPED 2026-08-18; A2 next.**  ⚠ **Re-scoped on 2026-08-15**, before any
code: the first design baked sprites at a fixed projection, and the project
owner's answer — *the dynamic camera of moros, and exploration as a pillar* —
replaced that with geometry.

### ⚠⚠ A2 — written, gated, and BLOCKED on a loft heap corruption (2026-08-18)

The emitter exists and its ten tests pass **on their own**: a box emits 12
triangles with 24 vertices (each face its own, so the normals are flat), a disc
and a cone emit `4 x PART_ROUND_SEGMENTS`, a cone's two rings measure its two
radii, the pose composes, normals stay unit length 13 m from the origin, two
emissions fold to the same `mesh_crc`, and **a quarter turn moves the canopy's
far edge 1.0 m — the one assertion that tells TURNS from DEGREES**.

⚠ **It is not in the tree.**  `part_emit` and `part_box` cannot coexist in one
process:

```
1 baseline part_emit   -> zmin 0        correct
2 after one part_box   -> zmin -3       WRONG
3 part_emit again      -> zmin 1000     NO VERTICES AT ALL
4 part_size afterwards -> (0, 0, 0)     the part has no size
```

and under `scripts/test.sh` the suite SIGSEGVs in `OpLengthVector` in whichever
file runs next.  A3 needs `part_box`, so the emitter is held out rather than
landing a suite that crashes.  ⚠ `part_box`'s own answers stay correct
throughout — it is everything AFTER it that decays, which is why A3 has been
green all along.

⚠⚠ **AND THE FAULT IS NOT WHAT IT LOOKED LIKE.**  Narrowed to ten lines and
three calls: a tiny emitter walking `p.pt_limbs` and appending to a `Mesh`,
then `part_box`, then the emitter again — which answers **1 limb where the part
has 8**, and a later `part_size` reads `(0,0,0)`.  ***`part_box` TRUNCATES the
vector it walked***, and the damage spreads to other parts in the same set.  ⚠
Each function alone repeats correctly for ever; only the interleaving corrupts,
which is why A3 has been green throughout.  ⚠ Six narrowings of `part_box`
itself found nothing: binding the rig to a local; splitting it into three small
passes; precomputing the bone bases; avoiding `Vec3` locals reused across
`add_vertex`; iterating the limb vector alone; a 6-tuple return alone.  ⚠ The byte-identical body in a program ENTRY is
correct every time — the one constant, and the same axis as [loft#962]'s family.
Filed as [loft#969](https://github.com/loft-lang/loft/issues/969), with the eight standalone ingredients that do NOT trigger it kept as a negative control in `loft_repros/emit_then_measure_corrupts/`.

⚠ **What A2 did land is upstream**: `hex_body` **0.3.0** — `Frame` /
`rig_world_frame3` / `frame_point` / `rig_world_point3`, published and signed.
`rig_world_seg3` answers a bone's base and tip, both ON its axis, and a limb is
a cloud of points that are not; the basis was computed and thrown away.  ⚠ It
also made `rig_world_seg3` two points of the frame rather than a second walk,
with its numbers bit-identical (gated).

⚠ **Still open for A2 when it resumes**: § D7's four BOOMS run diagonally and
nothing carries a rest orientation, so they are absent from the catalogue.  A
`LIMB_STRUT` defined by two endpoints and a thickness needs no rotation concept
and is the shape to reach for.

### A3 — the catalogue, and a footprint that is DERIVED (2026-08-18)

`src/catalogue.loft` + `tests/20_a3_the_catalogue.loft`, **11 tests**.  Four
entries — the hover unit, the robot, the tower base and its top — plus the
`Limb` record and `part_extent` in `src/part.loft`.  Gates:
`scripts/test.sh` **1351 green** (96 files), `scripts/validate.sh` **654
measurements green and UNMOVED**.

⚠⚠ **THE FOOTPRINT IS DERIVED, WHICH IS THE ONLY REASON ITS GATE IS REAL.**
§ D6 asks that *"a part declares its footprint, and a test compares it to the
number the simulation uses"* — and a part that DECLARED its size would agree
with itself for ever.  So the catalogue declares **limbs**, `part_size` computes
the extent, and the vehicle's **2.28 × 2.05 × 0.93 falls out of where § D7 puts
the four rotors**: the rear pair at `x ±0.62` with radius 0.52 sets the width to
2 × 1.14, and the front pair's `+0.91` against the rear pair's `−1.14` sets the
length.  Move one rotor 0.1 m outboard and the gate fails naming both numbers —
which is `test_moving_a_rotor_breaks_the_footprint`.

⚠ **Three copies, two gates**: the limb table, the `.loft` constant the
simulation reads, and `numbers.json`'s figure.  `tests/20_a3` binds the first
two, `tests/numbers_design_targets.loft` the last two.

⚠⚠ **AND THE FALSIFICATION FOUND TWO GATES WEAKER THAN THEIR OWN CLAIMS** —
nine of eleven deliberate breaks fired and the two that did not are the finding:

- *"the canopy hinges about a LATERAL axis"* asserted only `!bone_planar`, and
  swapping the axis to the planar `(0,0,1)` **passed** — because `bone_planar`
  is false whenever `oz` is non-zero, and the canopy's 0.56 already made it so.
  ***The assertion tested a consequence the other field also satisfies.***
- *"the root's own offset is not part of any base"* could not fail, because
  every catalogue entry's root sits at `(0,0,0)`.  The fixture could not
  express the thing the guard is for.

Both are now asserted directly — the axis by its components, the root by a
part deliberately authored away from the origin — and both breaks fire.

⚠ **`hex_body` 0.2.0 is load-bearing here for the first time**: the canopy's
lateral hinge is `rig_bone3`, and a planar rig cannot express it at all.

⚠⚠ **A loft CRASH is worked around and filed** (`QUESTIONS_FOR_LOFT.md`
§ Open): calling `rig_world_seg3` from a function in dryopea's LIBRARY
SIGSEGVs the interpreter (`OpGetVectorNullable`), while the **byte-identical
function body in a program ENTRY answers correctly** — the library/entry axis
of [loft#962]'s family.  ⚠ `part_box` walks the parent chain itself instead,
which at the neutral pose is what `rig_world_seg3` reduces to anyway (every
rotation is the identity), needs no per-call `zeros` vector and is cheaper — so
the workaround is not a workaround wearing a comment.

⚠ **Three things A3 deliberately does not carry**: no colour or livery (§ D8
makes a class *a row of data*, so a colour on the part would force one entry
per class); no solidity on a limb (dryopea's hitbox is a HEX, and reading a
limb's solidity would be the *derive passability from art* § D6 forbids); and
**no booms** — § D7's four run DIAGONALLY, and neither `Limb` nor
`hex_body::Rig` carries a REST ORIENTATION.  ⚠ That gap is A2's to solve and it
bites twice: it is also why the tower's socket rides a zero-length bone at an
offset rather than the tip of a 6 m one.

### ⚠⚠ Before A2 and A3 — two probes, and each moved a phase (2026-08-18)

**A2 was BLOCKED upstream, and dryopea unblocked it by shipping the
enhancement.**  `rig_bone3` is
[loft-libs-world#14](https://github.com/loft-lang/loft-libs-world/issues/14),
filed 2026-08-17, and **hex_body v0.2.0 closes it** (2026-08-18) — see
§ A1b below.  The paragraph that follows is the state it was found in.  ⚠ A2 is where the 2-D limit
becomes load-bearing rather than cosmetic: § D7 puts the four rotors at four
different heights (`z` +0.26 front, +0.30 rear), the skids at −0.26 and the
canopy hinge on a **lateral** axis (`ax: 1.0`), and `hex_body` has not one `z`
in the package.  ⚠ `~/workspace/loft-libs-world` is checked out and `hex_body`
is **307 lines of source and 95 of tests**, so contributing the enhancement is
a small job rather than a fork — but the issue deliberately offers the opposite
answer as a real option (*the planar rig stays planar and 3-D belongs to
`hex_part` once published*), and implementing it before that is answered
pre-empts a decision left open on purpose.

### A1b — the 3-D axis, shipped UPSTREAM as `hex_body` 0.2.0 (2026-08-18)

⚠ **dryopea wrote this one in another repo**, which is `CLAUDE.md`
§ Loft consumer relationship working as designed: *"dryopea may ADD to them
under their existing contract"*.  `loft-libs-world/hex_body` gains
`rig_bone3(r, parent, ox, oy, oz, len, ax, ay, az, lo, hi)` — the eight numbers
`hex_part::Hinge` already carries — with `rig_bone` re-expressed as the planar
special case and its signature unchanged, plus `rig_world_seg3`, `bone_planar` /
`rig_planar`, and a zero-length axis refused by `rig_admissible`.

⚠⚠ **Three probes ran before a line was written and two changed the shape.**

1. **A quaternion is the wrong carrier.**  The claim the whole extension rests
   on is *a planar rig is unchanged*, so it was measured over a five-bone planar
   chain rather than assumed: the quaternion form agrees with `rig_world_seg` on
   **0 of 5** bones (worst 6.7e-16 m), the matrix form on the **root exactly**
   (worst 2.2e-16 m).  ⚠ The quaternion's ROOT failing is the half composition
   cannot explain — it stores `cos(θ/2)` and rebuilds the rotation through
   double-angle arithmetic, where Rodrigues at `(0,0,1)` collapses to
   `[[c,-s,0],[s,c,0],[0,0,1]]` with `c` and `s` untouched.  ⚠ A planar CHAIN
   cannot be bit-identical in either form, because `rig_world_seg` adds ANGLES
   and any 3-D form composes ROTATIONS.
2. **Appending fields would have silently flattened every old reader.**  0.1.0's
   parser checks the keywords it knows and never the word COUNT, so a longer
   `bone` line reads back as the bone's PLANAR PROJECTION and reports success.
   Measured: appended words → 0.1.0 reads **1** bone; the tag `bone3` → 0.1.0
   reads **0**.  So a spatial bone gets its own record and an old reader refuses
   the rig rather than dropping a dimension.
3. **`rig_world_seg` is not re-expressed** through the 3-D path — it would move
   every existing consumer's numbers by an ulp to buy nothing.

⚠⚠ **AND THE PUBLISH ROUTINE REFUSED THE FIRST CUT, BECAUSE THE DROP-IN CLAIM
WAS FALSE.**  It shipped with `api_compatible_with = "0.1.0"` on the reasoning
that *every 0.1.0 signature is unchanged* — true, and beside the point.
`loft compat check --full` answered `0.1.0: BREAK`, and the break is real:
`struct Rig` is PUBLIC and gained four fields, so a consumer who built one by
LITERAL (the six-field shape `rig_new` itself uses) now omits them, and under
[loft#914] an absent axis reads as `(0,0,0)` — **measured, that literal answers
`rig_admissible = false` where 0.1.0 answered true**.  ⚠ It is refused rather
than repaired, deliberately, because a silent repair is what that library
refuses everywhere else; the pin is raised to `0.2.0`, the shape is pinned as a
test, and the release carrying the false claim was DELETED rather than amended.
⚠ dryopea is unaffected — `part.loft` builds through `rig_new` + `rig_bone`,
never a literal — which is why A1's 18 tests pass unchanged against the release.
⚠⚠ **The transferable half: `loft compat check` is a gate dryopea does not have
and could not have run itself**, and it caught a claim three humans-worth of
review in this session did not.

⚠ **And the falsification found a defect in the GATE, not the code**: of eleven
deliberate breaks ten fired, and dropping `rig_eq`'s axis comparison changed
**nothing** — the control compared `(0,1,0)` against `(1,0,0)`, which differ in
TWO components, so the surviving comparison caught it.  ***A negative control
has to be MINIMAL or it tests the whole set at once and can see none of it.***
Rewritten as four one-field-apart rigs, all four now fire separately.

Gates: `--interpret` and `--native` both **17 passed** under
`LOFT_DENY_WARNINGS=1`, and CI's *Compatibility with published releases* step
green against 0.1.0.  ⚠ The repo's aggregate CI run was RED on arrival for an
unrelated reason — `hex_roof/src/hex_roof.loft:106` named a fill loop's counter
it never reads, which a newer loft diagnoses and which failed `hex_draw` and
`hex_fit` transitively.  **Fixed and released as `hex_roof` 0.1.1** at the
project owner's word: swept first, and that one line was the ONLY warning in all
fourteen packages.  ⚠ It earned a version rather than just a commit because a
registry CONSUMER builds from the tarball — the fix on `main` turns the repo's
own CI green and does nothing for anyone downstream.

**A3's PREMISE needs correcting, and the correction is measured.**  § D6 says
dryopea's sizes are *"already written down and already load-bearing in the
simulation"*.  Half of that is true:

| what § D6 wants gated | in `numbers.json` | a `.loft` constant | READ by the sim |
|---|---|---|---|
| tower height 6.0 m | ✅ | `TOWER_HEIGHT_METRES` | ✅ `tower_sees`, the LOS eye |
| robot height 1.0 m | ✅ | `ENEMY_HEIGHT_METRES` | ✅ `damage.loft`, the aim point |
| body height 0.5 m | ✅ | `BODY_HEIGHT_METRES` | ✅ `height_raise`, the body ramp |
| **vehicle 2.4 m × 1.1 m** | ✅ | ❌ **none** | ❌ **nothing** |
| **robot 2.7 m × 1.3 m** | ✅ | ❌ **none** | ❌ **nothing** |
| **tower footprint** (7 hexes) | ✅ as a *layout name* | ❌ **none** | ❌ **nothing** |

⚠ So the three **HEIGHTS** are load-bearing and every **PLAN dimension** — which
is what a *footprint* is — is written down and read by nobody: no constant,
no consumer, and `tests/numbers_design_targets.loft` gates rates, HP, DPS and
speed and **not one size**.  A3's gate as written therefore has three numbers to
bind to and none of them is a footprint.

⚠⚠ **And the first catalogue entry contradicts the file it would be gated
against.**  § D7 specifies the hover unit at **2.28 m wide × 2.05 m long**;
`numbers.json` § vehicle says **1.1 m wide × 2.4 m long** (`PROXY_ART.md`
agrees with the JSON).  Heights agree at 0.9 m.  That is not a bigger vehicle,
it is a **different shape** — a wide, short quadcopter against a long, narrow
car, 2.07x the width — and § D7's own text flags it: the numbers *"were chosen
under the sprite design and want re-opening"*, and going wider *"is a SIMULATION
change if it is wanted, not an art one"*.  ⚠ § Open questions 3 already says
**A3 decides this**, so A3 cannot start until it is decided: whichever way it
goes, the gate A3 builds pins it.

### A1 — the socket, over a published rig (2026-08-18)

`src/part.loft` + `tests/20_a1_the_part.loft`.  `Socket` / `Binding` / `Part` /
`PartSet` over `hex_body::Rig`, twelve public functions, **18 tests**, and
`@DRY-016`..`@DRY-027` as the worked examples — the first file in the repo to
opt into `docs/EXAMPLES.md` with `// #examples`, so every one of the twelve
cites a test.  Gates: `scripts/test.sh` **1340 green** (18 new, 95 files),
`scripts/validate.sh` **654 measurements green and UNMOVED** — nothing draws a
part yet, so the scenario gate cannot see this and is not expected to.

⚠⚠ **THE PHASE'S OWN STATED NEGATIVE CONTROL COULD NOT FIRE, AND FINDING THAT
OUT IS A1'S MOST REUSABLE RESULT.**  § Invariant gate asked for *"delete
dryopea's `hex_body` dependency and A1's tests must fail to compile"*.
Measured:

| what was removed | result |
|---|---|
| nothing — the shipped state | 18 passed |
| the `hex_body` line from `loft.toml` | **18 passed** |
| that line **and** the `[[package]]` block from `loft.lock` | **18 passed** |
| `use hex_body;` from `src/part.loft` | `Error: Undefined type Rig` |

`use <pkg>;` resolves an **undeclared** registry package and writes it back
into the lock, so **a consumer can prove its IMPORT is load-bearing and can
never prove its MANIFEST is**.  The control the phase actually runs is the
fourth row.  ⚠ Queued to loft with a standalone repro —
[`loft_repros/undeclared_registry_package_resolves/`](../../loft_repros/undeclared_registry_package_resolves/README.md),
the third dryopea has hit where the manifest is not authoritative
([loft#963], [loft#966]).

⚠⚠ **And the FALSIFICATION found the thing the gate list would not have.**
Ten deliberate breaks, each firing on exactly its named test and nothing else —
but the one worth keeping is break 4: replacing the cycle walk's **path** with
a **visited set** left `test_a_part_that_contains_itself_is_refused` GREEN and
was seen by the DIAMOND control alone.  That is `hex_part::cycle.loft`'s own
argument reproduced rather than quoted: *a global "seen" set refuses a
catalogue that is fine*, and the test that says so is the one nobody writes.

⚠ Three deviations from `hex_part`'s records, each with its reason in
`src/part.loft`'s header: a socket rides a **BONE at `t`** (a part here is a
`Rig`, not a voxel world); **one class token**, not a kind AND a size (§ D6
gates the size against the simulation, which is A3); and **a binding carries no
pose**, where `hex_part::Binding` carries `bd_open` — that field is right in a
saved DOCUMENT and wrong in a catalogue asset whose angles come from the
simulation (§ D3, which is A4).

⚠⚠ **The renderer and the camera SPLIT OUT to
[`plans/21`](../21-the-renderer/README.md)**, and 21 comes first.  What is left
here is what an entity IS: the part model, the catalogue, and the triangles a
part emits.  All of that is measured in metres and turns and can be built and
gated with nothing drawing it.

The decisions live in [`docs/PARTS.md`](../../docs/PARTS.md); this plan does not
restate them.

⚠ **This supersedes [`plans/19`](../19-the-interactive-loop/README.md) P4** —
same gate (`classify_world` shares), different source of truth.

## Goal

Every entity the game runs — the hover unit, the crew, the four robot classes,
the towers — is a **part-tree**, and emits **triangles** posed by its joints.

⚠ **No pixels appear in this plan at all.**  Rasterising them is
[`plans/21`](../21-the-renderer/README.md), which is what makes these two
separable: a part-tree is metres and turns, and a gate can assert triangle
counts, extents and pivots without a frame existing.

## Anchors

Implements, and does not restate:

- [`docs/PARTS.md`](../../docs/PARTS.md) — the decisions, D1–D8.
- [`../moros/doc/claude/PARTS.md`](../../../moros/doc/claude/PARTS.md) § P9.0 —
  the model, and the authority on it.
- [`docs/PROXY_ART.md`](../../docs/PROXY_ART.md) — the sizes A3 gates against.
- [`plans/17`](../17-tower-hot-swap/README.md) § T2 — the socket the simulation
  already has (`docs/PARTS.md` § D3).
- `src/render.loft` (the rasteriser), `src/editor_view.loft` (the one frame
  composition), `src/measure.loft` (`classify_world`), `src/lattice.loft`
  (`lat_to_metres`, and the reason there is no direction table).

## Invariant gate

⚠ **Two phases have an exact-invariant surface and the rest do not**, which is
worth saying because silence reads as "gate done".

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **A1** | ⚠⚠ **REWRITTEN 2026-08-18 — the row asked for a model that is already PUBLISHED.**  `loft api hex_body` answers `Rig` + `rig_bone(parent, ox, oy, len, lo, hi)`, `Joint {value, lo, hi}` with `joint_fits` / `joint_offer` / `joint_residual`, `rig_write` / `rig_read`, `rig_world_seg`, `bone_obb` / `obb_contains`, `rig_admissible` and `rig_eq` — most of what this row asked to be written by hand.  So the expected result is now: **dryopea declares `hex_body` and re-implements none of it**, and what A1 builds is the part that is genuinely dryopea's — the SOCKET (`PARTS.md` § D3, whose simulation half has existed since plan 17 T2) over a `Rig` — refusing a socket filled twice and a part that contains itself | **reuse is the rule** (`CLAUDE.md` § Loft consumer relationship, `PARTS.md` § D1 / `@X001`): a dryopea-local `Part` / `Limb` / `Hinge` beside a published `Rig` is a second implementation of one thing, and the one a future reader would reach for | ⚠⚠ **The original row's headline gate cannot be met in dryopea at all** — *a hinge with a zero-length axis is refused by name* needs an AXIS, and `hex_body` is strictly 2-D with not one `z` in the package.  That gate belongs to the `rig_bone3` enhancement and therefore to `loft-libs-world`, not here.  ⚠ So the negative control for A1 is the one that says the reuse is real: **delete dryopea's `hex_body` dependency and A1's tests must fail to compile** — if they still pass, something local is standing in for it.  ⚠⚠ **RUN 2026-08-18, AND IT COULD NOT FIRE**: dropping the declaration from `loft.toml` — and the `[[package]]` block from `loft.lock` with it — leaves all 18 green, because `use <pkg>;` resolves an undeclared registry package and rewrites the lock to match.  The control that DOES fire is one layer in, **remove `use hex_body;` from `src/part.loft`** (`Error: Undefined type Rig`) — which proves the IMPORT is load-bearing and says nothing about the manifest.  See § Status |

| **A2** | a canopy hinged at 0.0 turns and at 0.25 turns puts its far edge exactly where trigonometry says — asserted as a COORDINATE, not as a stored angle | the pose is a transform: emitting at angle 0 equals the unposed part, and two 0.125-turn steps equal one 0.25 step | ⚠ a hinge in DEGREES is indistinguishable from one in TURNS until something swings — `0.25` must travel a quarter of the way round, which is the one assertion that can tell them apart |
| **A3** | every catalogue entry's declared footprint equals the simulation's constant for it | ⚠ **the durable artefact is the SIZE** (`PARTS.md` § D6) — the same shape as `tests/numbers_design_targets.loft` | a part whose art grew past its hitbox must FAIL naming both numbers; a part with no simulation counterpart must fail too, or the gate is vacuous over anything new |
| **A4** | a tower whose socket the SIM emptied emits no top triangles | what is DRAWN follows the simulation, never a second flag (`PARTS.md` § D3) | ⚠ emit both poses and assert they DIFFER — a pose looked up under the wrong key silently serves both states, and "it rendered" cannot see it |
| **A5** | enemies, the vehicle and the crew appear in `classify_world` shares; a thing not drawn reads as **zero** | one frame composition, and one geometry layer under two rasterisers ([plan 21](../21-the-renderer/README.md) § R2) | ⚠ a golden AGREES WITH A SHEAR, so the gate is pixel SHARES and a `snap` for review, never a rebaselined golden |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **A1** — adopt `hex_body`, and build the SOCKET | S | `tests/20_a1_the_part.loft` — **18 tests**, `src/part.loft`, and dryopea re-implements none of the rig: `Rig`, `Joint`, `rig_world_seg`, `rig_count` and `rig_admissible` all arrive as a dependency, and `part_fault` asks the LIBRARY's doorstep before any question of its own.  ⚠⚠ The 3-D axis is NOT here — it is `rig_bone3` in `loft-libs-world` (§ Cross-repo coordination), and A2 is what needs it | **SHIPPED** 2026-08-18 |
| **A2** — the geometry emitter | MH | `tests/20_a2_the_geometry.loft` — a part poses its joints and emits triangles in world space.  ⚠ Gated on NUMBERS, not pixels: a hinge at 0.25 turns puts the canopy's far edge where trigonometry says, a box emits 12 triangles, a disc emits its fan, and every vertex is inside the declared extent | ⚠⚠ **WRITTEN, TEN TESTS GREEN ON THEIR OWN, AND HELD OUT OF THE TREE** — see § A2 |
| **A3** — the catalogue | M | `tests/20_a3_the_catalogue.loft` — **11 tests**; the hover unit, the robot, the tower base + top, and the helper which is the hover unit rather than a fifth entry.  ⚠ Its real gate is the footprint, and the footprint is DERIVED from the limb table so the check is not a tautology | **SHIPPED** 2026-08-18 |
| **A4** — poses come from the SIMULATION | S | `tests/20_a4_the_joints.loft` — a tower with no top emits no top triangles; the canopy angle follows the sim; rotor spin follows boost.  ⚠ Asked of `TowerState` / `Vehicle`, never a second flag | Blocked on A2, A3 |
| **A5** — entities in the frame (was plan 19 P4) | M | `tests/20_a5_the_frame.loft` — `classify_world` shares for enemies, vehicle and crew; a `.keys` scenario with `snap` | ⚠ Blocked on **[plan 21](../21-the-renderer/README.md)** |

### Why the order is this order

⚠ **A0 is GONE, and saying so is the point.**  It was *"render one part at 24
px/m and look at whether 55 px reads"* — the probe guarding a claim the dynamic
camera deleted (`docs/PARTS.md` § What could kill this design).  A probe whose
question stopped existing is not a phase to keep out of tidiness.

**A1 first** because its refusals are the cheapest place to get moros's units
wrong and find out — a hinge in degrees where turns were meant looks perfectly
reasonable until something swings a quarter of the way round and travels 90× too
far.

**A3 parallel with A2** — the catalogue needs the model, not the emitter.  Its
footprint gate catches the expensive mistake (art and sim disagreeing about a
size) before anything is drawn at all.

**A4 before A5** so that "which pose" is settled against the simulation while it
is still a number, not a picture.  ⚠ A tower that draws its top when the sim says
it was detached is a bug that a frame gate reports as *pixel shares slightly
off*.

**A5 last, and blocked on another plan.**  ⚠ This is plan 19 P4's gate arriving,
and the reason it moved twice is worth keeping: P4 would have drawn primitives
inline in `editor_view.loft`, which is the *"second renderer that happens to live
in the test harness"* that file's own header exists to prevent.

## Cross-repo coordination

### ⚠⚠ The OUTBOUND ask: `rig_bone3` in `loft-libs-world` (2026-08-18)

`PARTS.md` § D1 already decided that the part model is **`hex_body`, published**,
and that where it stops — *it is strictly 2-D, there is not one `z` in the
package* — is an ADDITIVE enhancement dryopea proposes rather than a shape
dryopea invents:

```
rig_bone3(r, parent, ox, oy, oz, len, ax, ay, az, lo, hi)
rig_bone (r, parent, ox, oy,     len,             lo, hi)   // = rig_bone3(…, 0.0, …, 0,0,1, …)
```

— the same eight numbers as moros's `hex_part::Hinge`, of which `hex_body`'s are
the planar six-minus-two, so it UNIFIES two libraries that already agree.

⚠⚠ **And § D1 routes it to the wrong repo.**  It says *"`hex_body` belongs to
hexbody/lavition … so the enhancement is proposed there"*.  Checked: the
`hexbody` repo contains **no `hex_body` source at all** — its own log records
*"consume hex_body (renamed from body); src/ is empty of geometry"*, and the
source lives in **`loft-libs-world/hex_body/`** beside the rest of the `hex_*`
family.  The proposal therefore goes to `loft-lang/loft-libs-world`.  ⚠ The
correction is recorded in `PARTS.md` § D1 too, because a decision that names the
wrong destination sends the next reader to an empty tree.

⚠ **FILED 2026-08-18 — [loft-libs-world#14](https://github.com/loft-lang/loft-libs-world/issues/14)**, with the three questions that are decisions rather than code: the `rig_write` / `rig_read` round trip (proposed: write the 3-D fields only when they differ from the planar defaults, so a 2-D rig round-trips BYTE-IDENTICALLY and old readers keep working), a sibling `rig_world_seg3` rather than a changed return type, and `bone_obb` staying 2-D deliberately.  ⚠ The issue also offers the opposite answer as a real option — *the planar rig stays planar and 3-D belongs to `hex_part` once published* — because a proposal that cannot be refused is not one.

⚠ **A2 is what needs it, not A1.**  A1 adopts the 2-D `hex_body` and builds the
socket; the axis is only load-bearing once a canopy has to hinge about a LATERAL
axis while rotors sit at four heights (§ D7).  So the ask is filed early and
consumed as a release, rather than blocking the phase that can proceed without
it.

### moros

⚠ **moros owns the model; dryopea owns nothing of it.**  `docs/PARTS.md` § D1
records the decision NOT to depend on `hex_part`, and the trigger to revisit:
`hex_voxel` + `hex_part` published to the registry, and **moros#8** (which tree
owns the store) settled.  Nothing in this plan changes moros.

⚠ **One outbound ask, and it belongs to [plan 21](../21-the-renderer/README.md)
now**: `imaging::Pixel` is `{ r, g, b }` with no alpha
([`QUESTIONS_FOR_LOFT.md`](../../QUESTIONS_FOR_LOFT.md)).  ⚠ It blocks nothing
here — plan 21 § R0 measured the decoder carrying a captured frame with **zero**
colour drift, and a captured frame is opaque.  What it still blocks is the OTHER
pipeline: an artist's PNG loaded at runtime.

## What this plan does NOT build

**No part editor.**  moros § P1 authors a part *in the editor*, which is the
right destination; dryopea's editor paints terrain.  Parts are loft
constructors — `docs/PARTS.md` § Open 2, with its trigger.

**No renderer and no camera.**  Both moved to
[`plans/21`](../21-the-renderer/README.md).  ⚠ This plan emits triangles and
never a pixel — which is what lets it be built and gated while 21 is still open.

**No animation CYCLES.**  A joint's angle is a number the simulation supplies
(A4) — the canopy swings, the rotors spin.  A walk cycle, a death throe and a
muzzle flash need a clock of their own and are out.

**No hitbox change.**  A hitbox is a hex and stays one; art overhangs it freely
(§ D6).  ⚠ Deriving passability from a part's extent would let a drawing decide
where a robot may stand, and `passable.loft` is the one door for that.

**No `cam.zoom` fix.**  Stored, saved, scripted and read by no renderer — a real
defect (`@D002` in [`PROBLEMS.md`](../../PROBLEMS.md)), and now purely the
EDITOR's, since the game's camera is plan 21's and never touches
`EditorCamera`.

## Open questions

1. ~~**Does 55 px read?**~~ ~~**Is θ = 55° right?**~~  **Both deleted with A0** —
   there is no fixed pixel size and no fixed pitch once the camera comes to the
   vehicle.  Listed as deleted so a reader looking for them learns they are not
   open rather than not asked.
2. **Does the helper stay the player's part?**  `docs/PARTS.md` § D8 says yes on
   the strength of *"same chassis as the player"*.  A3 is where a second
   part-tree would have to be admitted, and admitting one means the chassis
   claim has broken somewhere the simulation cannot see.
3. **How big is "a big quad copter", now that pixels stopped bounding it?**
   `docs/PARTS.md` § D7 sized it at 2.28 m under the old constraint.  ⚠ The
   remaining bound is gameplay, not art: the hitbox is one 1.5 m hex, and a
   vehicle that reads as two hexes wide tells the player something false about
   where it fits.  A3 decides; going bigger is a SIMULATION change.

   ⚠⚠ **MEASURED 2026-08-18, and it is a conflict rather than an open
   preference**: § D7 says **2.28 m wide × 2.05 m long**, `numbers.json`
   § vehicle says **1.1 m wide × 2.4 m long**, and `PROXY_ART.md` agrees with
   the JSON.  Heights agree at 0.9 m.  So the two are a **different shape**, not
   two sizes of one — a wide short quadcopter against a long narrow car — and
   nothing arbitrates, because no `.loft` constant carries either pair and
   nothing in the simulation reads them.

   ⚠⚠ **ANSWERED 2026-08-18 (project owner): 2.28 × 2.05, and the SIMULATION
   moved.**  `numbers.json` § player_vehicle now reads 2.05 long × 2.28 wide ×
   **0.93** tall — the height DERIVED from § D7's limb table rather than its
   rounded *"~0.9"* — and `src/vehicle.loft` carries the three constants A3
   gates against.  ⚠ The vehicle is now wider than the hex it stands on, which
   § D7 called a simulation change and which nothing derives passability from.

## See also

- [`docs/PARTS.md`](../../docs/PARTS.md) — the decisions.
- [`plans/19`](../19-the-interactive-loop/README.md) — the loop this draws;
  P4 is superseded by A5.
- [`plans/21`](../21-the-renderer/README.md) — the camera and the renderer that
  draw what this plan builds.  A5 is blocked on it.
- [`plans/08`](../08-game-validation/README.md) § V2 — `classify_world`, the
  measurement A5 extends.
