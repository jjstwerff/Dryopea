<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 20 — Entity art: part-trees, and the geometry derived from them

**Value:** `G` · **Effort:** `H`

## Status

**Designed, nothing built** (2026-08-15).  ⚠ **Re-scoped the same day**, before
any code: the first design baked sprites at a fixed projection, and the project
owner's answer — *the dynamic camera of moros, and exploration as a pillar* —
replaced that with geometry.

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
| **A1** | a hinge with a zero-length axis is refused **by name**; a socket filled twice is refused; a part that contains itself is refused | moros's: swing in TURNS, axis stored as given (not normalised), scale DERIVED (`child.unit / parent.unit`) never authored | a degrees-valued hinge is indistinguishable from a turns-valued one — so the gate is that `0.25` turns is a QUARTER of the way round, asserted as a projected corner, not as a stored number |
| **A2** | a canopy hinged at 0.0 turns and at 0.25 turns puts its far edge exactly where trigonometry says — asserted as a COORDINATE, not as a stored angle | the pose is a transform: emitting at angle 0 equals the unposed part, and two 0.125-turn steps equal one 0.25 step | ⚠ a hinge in DEGREES is indistinguishable from one in TURNS until something swings — `0.25` must travel a quarter of the way round, which is the one assertion that can tell them apart |
| **A3** | every catalogue entry's declared footprint equals the simulation's constant for it | ⚠ **the durable artefact is the SIZE** (`PARTS.md` § D6) — the same shape as `tests/numbers_design_targets.loft` | a part whose art grew past its hitbox must FAIL naming both numbers; a part with no simulation counterpart must fail too, or the gate is vacuous over anything new |
| **A4** | a tower whose socket the SIM emptied emits no top triangles | what is DRAWN follows the simulation, never a second flag (`PARTS.md` § D3) | ⚠ emit both poses and assert they DIFFER — a pose looked up under the wrong key silently serves both states, and "it rendered" cannot see it |
| **A5** | enemies, the vehicle and the crew appear in `classify_world` shares; a thing not drawn reads as **zero** | one frame composition, and one geometry layer under two rasterisers ([plan 21](../21-the-renderer/README.md) § R2) | ⚠ a golden AGREES WITH A SHEAR, so the gate is pixel SHARES and a `snap` for review, never a rebaselined golden |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **A1** — the part model | M | `tests/20_a1_the_part.loft` — `Part` / `Limb` / `Hinge` / `Socket` / `Prim`, moros's fields and units.  Pure data + validation; nothing draws | **Next** |
| **A2** — the geometry emitter | MH | `tests/20_a2_the_geometry.loft` — a part poses its joints and emits triangles in world space.  ⚠ Gated on NUMBERS, not pixels: a hinge at 0.25 turns puts the canopy's far edge where trigonometry says, a box emits 12 triangles, a disc emits its fan, and every vertex is inside the declared extent | Blocked on A1 |
| **A3** — the catalogue | M | `tests/20_a3_the_catalogue.loft` — the hover unit, the robot, the helper livery, the tower base + top.  ⚠ Its real gate is the FOOTPRINT check against the simulation's constants | Blocked on A1 |
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

## See also

- [`docs/PARTS.md`](../../docs/PARTS.md) — the decisions.
- [`plans/19`](../19-the-interactive-loop/README.md) — the loop this draws;
  P4 is superseded by A5.
- [`plans/21`](../21-the-renderer/README.md) — the camera and the renderer that
  draw what this plan builds.  A5 is blocked on it.
- [`plans/08`](../08-game-validation/README.md) § V2 — `classify_world`, the
  measurement A5 extends.
