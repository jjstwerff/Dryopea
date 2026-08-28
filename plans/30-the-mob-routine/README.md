<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `30` — The mob routine

**Value:** `G` · **Effort:** `H`

## Status

**R2 COMPLETE 2026-08-28 — the cycle is closed-form, and it is still
INERT.**  R3, R4 and R5 are startable.  ⚠ Gates: **1714 green over 134
files** (+8, all this phase's) and `validate.sh` **50 scripts / 920
measurements, unchanged** — which is the inertness measured rather than
asserted.

⚠⚠ **`@M074` — 0 hexes and 0 legs of 8 920 swept moments disagree with a
stepped body**, over four speeds × three step lengths, on a world whose
leg BENDS (15 steps against a straight line of 12).  ⚠ And the reading
that separates the two steerings: a guard's clock is exact at a step that
divides its period and **8 of 60 adrift at one that does not**, while the
bag holds at **0 of 60** over the very same step — `@FR-E-Bag-Steers`
reached from the timestep instead of from the distance.  ⚠⚠ **Eleven
mutations, eleven caught, and two of them moved the gate** — the leg
INDEX was invisible in a position, and the plan's own named control was
being refused by a different check.  ⚠ `@X335`: the period is a count of
HEXES when the bag steers and a span of TIME when a clock does, and
**the bank does not restart at a leg boundary**.

**R1 COMPLETE 2026-08-28 — the record, the role table and the derived
destination are in, and they are INERT.**

⚠⚠ **`@M073` — the bag closes a round trip at 4, 40 and 400 hexes, and
one table column away a clock gets 13 hexes out and delivers nothing for
ever.**  That is `crawler`'s measured defect reproduced in dryopea's own
code rather than imagined, and the pair shares its harness, its anchors
and its walker.  ⚠ `@X333`: the role table is INDEXED and never
compared, and `tests/30_r1_the_errand.loft` sweeps `src/` to say so —
crawler has `role == 7` in eight places and no compiler can refuse one.
⚠ `@X334`: a mob's bag is not `carry.loft`'s ledger, and R7 is where
that stops being true.

⚠ **R0 COMPLETE 2026-08-28 — all four probes answered, and two of them
moved the plan.**

⚠⚠ **Probe 1 falsified greedy** (`@M071`): only **10 of 90** straight
crossings of the three authored maps arrive, and **44 of the 80 failures
are painted terrain** rather than the map's edge — so a leg is a PATH and
R3 needs a field.  ⚠ **Probe 3 saved the bound** (`@X331`): distance to
the destination never increases, so a deviating body stays inside
`disc(anchor, leg length)` and R5 needs no cap.  ⚠ Probe 4 says a field
is ~500 cells on a real map, and probe 2 says the crew's *selection* rule
does not generalise while its `Job` record does.

⚠⚠ **This plan builds the ERRANDS half only.**
[`docs/ERRANDS.md`](../../docs/ERRANDS.md) is the design; the world →
scenario half ([`docs/WORLDGEN.md`](../../docs/WORLDGEN.md), BACKLOG F8+)
is a **later plan** and this one is deliberately built to not need it —
every phase below runs against an **authored `.keys` snapshot**, which is
what `@X298` says a scenario is anyway.

⚠⚠ **HALF THE MACHINERY ALREADY EXISTS AND NOBODY NOTICED.**
[`plans/29`](../29-the-crews-own-work/README.md) shipped
`src/task.loft` — `Job`, four `TASK_*` kinds, `jobs_in_scope`,
`job_pick` — plus `spawn.loft::wave_assign`, *"the ONE site where a crew
member decides anything"*.  ⚠ That is this design's shape **already
built, on the crew's side**, and R1 is largely *the same thing for the
other roster*.

## Goal

A robot on the map is **going somewhere for a reason**, deviates around
what is in its way, goes **home** when its round ends, and can be drawn
off its route by something the player BUILT — with its whole life
expressible as a closed-form function of five anchors, so an un-tracked
one costs nothing.

## Anchors

- [`docs/ERRANDS.md`](../../docs/ERRANDS.md) — the design, `@X298`-`@X306`
  and `@X322`-`@X324`.
- [`docs/WORLDGEN.md`](../../docs/WORLDGEN.md) § THE THESIS (`@X323`) and
  § WHY IT IS AN OLD DESIGN (`@X324`) — the two tests every phase answers
  to.
- `src/errand.loft` (what exists: a heading, and a deletion),
  `src/task.loft` + `src/spawn.loft::wave_assign` (plan 29's half),
  `src/spawn.loft::enemy_walk_heading` / `enemy_walk_desire`,
  `src/occupancy.loft`, `src/skill.loft::detect_radius`,
  `src/script.loft`, `src/emit.loft`, `src/compare.loft`.
- [`plans/22`](../22-the-field-cache/README.md) — the cost this plan must
  not wake, and the LOD refusal `@X299` does not ask an exception to.

## ⚠⚠ THE INVARIANT — one sentence, and everything below is its enforcement

> ⚠⚠ **A mob's position at any time is `cycle(poi.state, anchors, t − slip)`,
> and the ONLY way out of that function is the bubble — which is one-way.**

⚠ That is the sentence a case nobody tested has to satisfy: whatever
happens to a mob, either it is **on its cycle**, or its lateness is in
**`slip`**, or it has **left through the one door**.  ⚠ Three states, one
function, one exit.

### ⚠⚠ Count the RE-ASSERTION SITES first — the brittleness is known now

⚠ How many independent places must re-state this for the design to be
correct?  **Twelve**, and here is the list, because a number without one
is a guess:

| # | site | how it could break the invariant |
|---|---|---|
| 1 | the errand assign | writes a destination the cycle did not choose |
| 2 | the errand mover | moves the body without the cycle |
| 3 | **the sidestep** | ⚠⚠ moves the body and **forgets `slip`** |
| 4 | `errand_depart` | removes at the wrong time |
| 5 | `wave_cutoff` | the one-way door — must be the ONLY exit |
| 6 | `wave_deaths` | a killed mob |
| 7 | `emit.loft` | writes `slip` down, or does not |
| 8 | `script.loft` | reads it back, or does not |
| 9 | `compare.loft` | compares it, or does not |
| 10 | the materialiser | places a body somewhere the rule did not say |
| 11 | the culler | skips a mob that was about to be cut off |
| 12 | a POI state change | starts a new segment without its `t0` |

⚠⚠ **And omitting it is SILENT at every one of them.**  A forgotten
`slip++` does not fail to compile and does not throw — **the mob simply
arrives early**, and nothing in the suite would notice.  ⚠ That is
`N = 12` times silence, and it is the whole risk of this plan, known
before a line is written.

### ⚠⚠ The two cures, and both are dryopea's own habits

**1. Collapse N toward 1 — ONE DOOR that writes a mob's position.**
⚠ Nothing but `errand_step` may assign `e.q` / `e.r` for a mob on a
cycle, and that function owns `slip` as well.  Then sites 1, 2, 3 and 10
**cannot** forget it, because they cannot move anything.

⚠ It is the pattern this repo already keeps **six** times and names every
time: `wave_deaths` is *"the ONE death path"*; `place_marker` is *"the
ONE dispatch"*; `break_structure` is *"the one site, and it does both
halves"*; `salvage_at` is *"the shared chassis"*; `bindings.loft` is
*"the ONE key table"*; `play.loft` is *"the ONE caller of `wave_tick`"*.

**2. Make omission LOUD — the CONFORMANCE GATE.**
⚠⚠ A loft type cannot force this, so a **test** must:

> ⚠⚠ **At the end of every tick, every mob still on a cycle is exactly
> where its rule says it is:** `cycle_at(e, now) == (e.q, e.r)`.

⚠ Run it over the whole gate corpus, not one fixture.  ⚠⚠ **It is
CONSTITUTIVE rather than confirmatory** — it is not evidence that the
invariant holds, it is *the only reason the design works*, and any of
the twelve sites forgetting turns it red.

## ⚠⚠ The safety rule this whole plan is built on

> ⚠⚠ **EVERY PHASE LANDS INERT.**  A default value that means *the game
> exactly as it is today*, so the tests and the **920 measurements** do
> not move until a scenario asks for the new thing.

⚠ It is not caution for its own sake — it is what three shipped features
already did and what [`plans/29`](../29-the-crews-own-work/README.md)
learned the hard way when a radius it had not thought about moved **18
tests across 8 files** in one direction.  ⚠ The precedents:
BACKLOG B4's `traffic` (rate defaults **0.0**, *"679 measurements did not
move"*), C1's `skill` (level 0 is *"bit-for-bit the old game"*), C3's
`jammer` (stores OFF so [loft#914]'s silent default **is** today's game).

⚠⚠ **And [loft#914] is the mechanism, used deliberately**: a struct
literal that omits a field takes that field's default silently, so **the
neutral value has to be the one today's behaviour needs** — which is why
`TASK_ANY` is 0 and `Enemy.errand` is false.

## Invariant gate

⚠ Three phases have an exact-invariant surface.  The rest are
measurements and say so.

| phase | invariant | concrete expected result | negative control |
|---|---|---|---|
| **R2** ✅ | ⚠⚠ **the closed form EQUALS the stepped one** | `cycle_at(e, t)` for arbitrary `t` is the hex reached by stepping the mover `t` ticks from `t0` — for **every** `t` in a sweep, not a sample | ⚠ a cycle whose legs do not sum to its period must be **refused at construction**, not silently wrapped |
| **R5** | ⚠⚠ **the bound is the union over legs of `disc(anchor, leg length)`, and it CONTAINS a deviating body** | every hex `cycle_at` can produce over a whole period, **plus every hex a deviating body can reach**, lies inside it — and R0 probe 3 proves the second half from `@FR-E-Non-Increasing` | ⚠⚠ **a bound that contains everything is vacuous** — the gate must show a hex *outside* it too, or it proves nothing (`CLAUDE.md` § a gate that reads PERFECT is as suspect as one that reads wrong) |
| **R6** | ⚠⚠ **materialising at `R` and at `2R` is identical WHERE NOTHING CAN DEVIATE A BODY** | in a world with one mob and clear ground, the two runs agree hex for hex | ⚠⚠ **and with a blocker in the band between `R` and `2R` they MUST DIFFER, by exactly `slip`** — a gate that could not see that is measuring an empty claim |

⚠ **R1, R3, R4, R7 and R8 have no exact-invariant surface** — they are
behaviour and clocks, gated by scenarios and counts.

## Phases

⚠ Each row is **one thing**, lands green, and does not require the next.

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **R0** — the four probes | XS | `tests/30_r0_probe.loft` (5) + readings below | ✅ **ALL FOUR ANSWERED 2026-08-28** |
| **R1** — `Errand`: five anchors, and the BAG steers | S | `tests/30_r1_the_errand.loft` (9) | ✅ **COMPLETE 2026-08-28** — `@M073`, `@X333`, `@X334` |
| **R2** — the cycle is CLOSED-FORM | S | `tests/30_r2_the_cycle.loft` (8) — ⚠ the equality gate above | ✅ **COMPLETE 2026-08-28** — `@M074`, `@X335` |
| **R3** — deviation, and `slip` | M | `tests/30_r3_the_deviation.loft` | **STARTABLE** — ⚠⚠ **R0 probe 1 says it needs a FIELD**, and `@X335` says it must not lose a hex at an anchor |
| **R4** — home is a PLACE | S | a scenario: a robot walks home and leaves the roster there | **STARTABLE** |
| **R5** — the POI, its population, its BOUND | M | ⚠ the containment gate above | **STARTABLE** — ⚠ **R0 probe 3 is ANSWERED and the bound is named** |
| **R6** — CULL / EVALUATE / MATERIALISE | M | ⚠ the `R` vs `2R` gate **and its differ-control** | Blocked on R5 |
| **Rc** — the CONFORMANCE gate | S | ⚠⚠ every mob on a cycle is where its rule says, every tick, over the whole corpus | ⚠ lands **with R3**, and guards every phase after it |
| **R7** — distraction: the hauler and your heap | M | a scenario pair + the *merely seen* negative control | Blocked on R5 |
| **R8** — what a routine is WORTH | S | a scenario pair, one token apart | Blocked on R7 |

---

## R0 — the four probes

⚠⚠ **Builds nothing.**  Four readings, and **two of them can falsify
phases below** — which is why they come first.

⚠ Probes 1 and 3 were added by running the design protocol over this
plan **after it was first written**, and both found a claim in it that is
**false as stated**.  That is the protocol working, and the finding is
worth more than the phases it corrects.

1. ❌ **ANSWERED 2026-08-28 — NO.  GREEDY IS NOT ENOUGH, and the cycle
   must be TERRAIN-AWARE.**  `@M071`, `tests/30_r0_probe.loft`, walking
   `enemy_walk_heading`'s own rule (one step, refused by `can_step`)
   across the three authored maps from every standable bounding-box edge
   hex, both directions:

   | map | straight crossings that arrive |
   |---|---|
   | `starter_01` | **2 of 30** |
   | `crossroads_02` | **2 of 26** |
   | `the_gap_03` | **6 of 34** |
   | **total** | ⚠⚠ **10 of 90 — 11 %** |

   ⚠⚠ **And the discriminator was worth building.**  A sea-default world
   is not a rectangle, so a straight line from a box edge can simply run
   out of LAND — *the map is small* and *the terrain is obstructed* are
   different answers and a count that cannot tell them apart answers
   neither.  Of the 90:

   > **10 arrived · 36 ran out of painted land · 44 met PAINTED terrain
   > they could not enter.**

   ⚠ So the blockage is **the ground**: discounting the map's edge
   entirely, **10 of 54 land-bounded crossings arrive — 19 %**.
   ⚠ Control: `the_gap_03` has **42** hexes in its bounding box a regular
   robot cannot stand on, so the probe is not reading an open plain.

   ⚠⚠ **What it costs, and it is the larger half**: a cycle computed over
   straight lines while a materialised body walks over terrain means a
   **body deviates where a rule does not**, and R6's equality claim is
   false *everywhere* rather than only near blockers.  So a leg is a
   PATH, not a line.

   ⚠⚠ **And `@X331` survives it unchanged**, which is the good news: its
   argument is about *distance to the destination never increasing*, and
   a flow-field distance is a distance.  A leg becomes a **descent of the
   field toward its anchor**, `cycle_at(t)` becomes *how far down the
   field am I at `t`*, and the bound is still `disc(anchor, L)` — in
   field distance rather than lattice distance.

   ⚠ **The bill goes to [`plans/22`](../22-the-field-cache/README.md)**,
   and `crawler` has already sized the answer: a cache keyed by
   **(destination, movement class)** at `3 × len(mobs) + 16`, derived
   from *an actor can ask for at most three destinations* — which is
   exactly `Errand`'s three anchors.  ⚠⚠ Its cap trap transfers with it:
   a flat cap crossed silently meant *"nine townsfolk walked home on a
   straight line every night"*, so **a cap that is reached must go RED**.

   ⚠ **Honest limit of this reading**: the probe holds the route fixed at
   *box edge, due east or west*.  A real route crosses on an arbitrary
   bearing between off-map anchors (`@X298`), so 11 % is not the number a
   route would score — but **44 genuine terrain blockages across three
   maps falsifies *mostly straight corridors* for these maps**, which is
   all the probe had to do.
2. ✅ **ANSWERED 2026-08-28 (`@X332`) — PARTLY, and `@X329`'s three-way test is
   what separates the halves.**  Read off `task.loft`'s actual surface:

   | piece | share it? | the nameable difference |
   |---|---|---|
   | `Job { found, kind, q, r }` | ⚠⚠ **YES** | none — a leg's destination is exactly a kind and a hex |
   | `TASK_*` as an **arrival action** | ⚠ **YES** | none — *what you do when you get there* is the same four things |
   | `jobs_in_scope` | ❌ **NO** | it takes a `Skills` and searches near a point; **a mob has no skills and does not search** |
   | `job_pick` | ❌ **NO** | nearest-wins is a CHOICE; **a mob has none — it follows the cycle it was given** |

   > ⚠⚠ **The crew CHOOSE what to do from where they stand; a mob is TOLD
   > where to go and does what the place needs on arrival.**

   ⚠ That difference is nameable, so by `@FR-F-Nameable-Difference` the
   two selection rules **stay apart** — which is loft's *four of eight
   families split rather than merged* landing on its first real
   application here.  ⚠⚠ And a ROLE is not a job KIND: a role selects a
   CYCLE and a kind is the ACTION on arrival, so **they compose rather
   than merge**, which is why sharing the vocabulary costs nothing.
3. ✅ **ANSWERED 2026-08-28 — YES, BOUNDED, and no cap is needed.**
   ⚠ The question was whether a deviating body can wander arbitrarily
   far, because `@X300`'s architecture needs a **static** region per POI
   and § Invariant gate says the bound must contain the cycle **dilated
   by the deviation**.  ⚠⚠ **The answer is already in the code, in the
   difference between two functions:**

   | | admits a neighbour when | effect on the distance |
   |---|---|---|
   | `flow_steps` (`flow.loft:476`) | `flow_distance(n) < d` — **strictly closer** | decreases it |
   | `flow_sidesteps` (`flow.loft:514`) | `flow_distance(n) == d` — **exactly equal** | holds it |

   ⚠⚠ **So a mob's distance to its destination NEVER INCREASES**, which
   is now written down as `@FR-E-Non-Increasing` and cited at the
   function that makes it true.  A leg starting at distance `L` therefore
   keeps the body inside `disc(destination, L)` for the whole leg,
   deviating or not, and:

   > ⚠⚠ **The dilated bound is the union, over legs, of the disc centred
   > on each anchor with the incoming leg's length as its radius.**
   > Static, computable from the five anchors before the mob moves, and
   > it provably CONTAINS a deviating body.

   ⚠ **It is wider than the cycle's path and that is honest**: a queued
   mob can circle an iso-distance ring, so the deviation is **bounded in
   SPACE and not in TIME** — and the time is what `slip` absorbs, which
   is what `@FR-E-Slip` is for.

   ⚠⚠ **The caveat goes to R3**: this holds *because* the existing
   sidestep is equal-distance-only.  **An errand mover's sidestep must be
   written the same way** — one that could increase the distance to the
   destination breaks the bound and takes R5 and R6 with it.
4. ✅ **ANSWERED 2026-08-28 (`@M072`) — a field is CHEAP on a real map, and a
   number `plans/22` cites turns out not to apply here.**  ⚠ A COUNT and
   never a clock (`@M029`: two identical calls differed **5.4×**).

   ⚠⚠ **A sweep is exactly the REACHABLE painted world**, which came back
   tighter than the claim it was written to test:

   | map | field cells | painted hexes |
   |---|---|---|
   | `starter_01` | **460** | 460 |
   | `crossroads_02` | **539** | 539 |
   | `the_gap_03` | **468** | 510 |

   ⚠ `the_gap_03` is short by **exactly the 42 hexes** the control counts
   as unstandable.  ⚠⚠ So **a field per anchor is ~500 cells**, and with
   three anchors per mob over 2-4 POIs that is a handful of cached
   sweeps — `crawler`'s *cache it, keyed by (destination, movement
   class)* is affordable rather than hopeful.

   ⚠⚠ **AND THE SECOND READING CORRECTS A CITED NUMBER'S REACH.**
   [`plans/22`](../22-the-field-cache/README.md) point 3 says the field
   is only read inside the 25-hex bubble, so *"about 60 % of every sweep
   is computed and never looked at"* — measured for a **radius-40**
   world.  On `the_gap_03`: **468 of 468 swept cells are inside the
   bubble.**

   > ⚠⚠ **The whole sweep is read, because the authored maps are smaller
   > than the bubble.**  `plans/22`'s waste is real and **not visible in
   > the shipped content** — which its own trigger already says: *it
   > fires when the world grows or the roster does*, and neither has.

   ⚠ The probe asserts that equality, so **a map that outgrows the bubble
   turns it red** — which is exactly when that 60 % starts to matter.

⚠ Write all four answers into this section before R1 starts.

## R1 — `Errand`: five anchors, and the BAG steers

✅ **COMPLETE 2026-08-28** — `src/errand.loft` § THE ROUTINE,
`tests/30_r1_the_errand.loft` (9 tests), `@X333`, `@X334`, `@M073`.

⚠ `Errand { role, home, work, alt, carry, slip }` on `Enemy`, and
`errand_destination` **derived** from it — never a stored waypoint list.

⚠⚠ **The phase's whole risk is one measured failure and it is not
dryopea's**: `crawler`'s calendar-steered gatherer *"oscillated between
home and 13 hexes out, forever.  In 21 days it gathered nothing."*  ⚠ So
the gate is the shape rather than the arrival:

```
destination = carry > 0 ? alt : work        # the bag, never a clock
```

**Gate** — `tests/30_r1_the_errand.loft`:
- a hauler whose round trip is **longer than any period a clock could
  use** still completes it, at 4, 40 and 400 hexes;
- ⚠ the **negative control**: the same cycle steered by a tick counter
  reproduces the oscillation, so the gate is reading the fix and not the
  distance;
- ⚠ a role table with one row per role and **no `role ==` branch outside
  it** (`@X322`; `crawler`'s own comment records its eight branches going
  stale).

⚠ **Lands inert**: `role` defaults to the value meaning *a cut-off robot
walking to the core*, which is every enemy in every scenario today.

### What was built, and the three things the phase decided

⚠⚠ **`@M073` — the bag closes the loop at 4, 40 and 400 hexes (three
bags each), and one column away a clock gets 13 hexes out and delivers
nothing, for ever.**  Same harness, same anchors, same walker; the pair
differs in `haul`'s `period` alone.  ⚠ **The reading is HOW FAR IT EVER
GOT, not the deliveries** — a clock-steered role has no bag either, so
deliveries has two causes and a control that cannot separate them is not
one.

⚠ **Four mutations, each firing the right assertion** — the gate was
green on its first run and `CLAUDE.md` § a gate that reads PERFECT says
what to do about that: the bag stops steering (10 / 100 / 1000 phantom
bags, standing on the face and flipping), the clock steers everything
(0 hexes out), a planted `role ==` in `src/`, and the realistic
authoring error — a **carrier** given a clock — which fired three gates
at once.

⚠⚠ **`@X333` — the table is INDEXED and never compared, and a test
sweeps `src/` to say so.**  A comparison is not a thing a compiler can
refuse, and `crawler` has `role == 7` in eight places with its own file
recording the bill.  ⚠ The gate carries its own control: it must catch a
planted comparison and must **not** call `ROLE_KIND_COUNT` one.  ⚠ Four
columns, because `errand_leg` reads four — *what draws a role off its
route* is R7's and is deliberately not a column yet.

⚠⚠ **`@X334` — a mob's bag is not `carry.loft`'s ledger**, and the
difference is nameable: *that file conserves an object that is on the
map and the player could pick up instead; a bag holds material that was
never on the map.*  ⚠ **R7 is where that stops being true**, and the
obligation is written at the field rather than left to be discovered.

⚠⚠ **`@X332` cashed in**: `errand_destination` answers a `task.loft`
`Job`, so Open question 1 is closed — the RECORD is shared and the
SELECTION is not.  ⚠ `Job.kind` is `TASK_ANY` for every role today and
the column is left empty rather than guessed: **not one of the four
kinds is what a hauler does at its face.**

⚠⚠ **And the bubble's one-way door is enforced in the READER.**
`errand_role` answers `ROLE_NONE` for any robot whose `errand` flag is
clear, so **site 5 of § Count the RE-ASSERTION SITES has no second write
to forget** — `wave_cutoff` is unchanged and a cut-off hauler cannot go
on running a cycle it has lost.  ⚠ That is cure 1 (*collapse N toward
1*) applied to the read side, which costs one function.

⚠ **Sites 7, 8 and 9 of the twelve**: `emit.loft`'s crop copies `route`
whole, and `compare.loft` compares it as a **tripwire, here before it
can differ** — the same move the banked carry records one file over.
⚠⚠ **When it fires, the answer is a `.keys` verb and never a looser
comparison**: `emit_keys` must write a routine down and `script.loft`
must read it back, and the writer and the reader are a PAIR (`@D007`).

⚠ **Still inert, and deliberately**: nothing in `wave_tick` calls any of
it.  R3 is the mover that descends a field toward `errand_leg`'s anchor,
and R2 is what makes the cycle evaluable at an arbitrary `t`.

## R2 — the cycle is CLOSED-FORM

✅ **COMPLETE 2026-08-28** — `src/errand.loft` § THE CYCLE,
`src/flow.loft::flow_route`, `tests/30_r2_the_cycle.loft` (8 tests),
`@X335`, `@M074`.

⚠ `cycle_at(c, row, rate, t)` — one modulo for the phase, O(legs) to find
the leg, one index.  ⚠⚠ **The gate is EQUALITY** (§ Invariant gate), and
it is the strongest gate in the plan because it is what makes `@X299`
not-LOD.

⚠ **Sweep `t`, do not sample it.**  `CLAUDE.md` § AND THE VACUITY CAN BE
IN THE NUMBERS warns that *a 1 Hz clock driven by a 30 Hz clock cannot
disagree for any implementation* — so the sweep must include `t` values
that are **not** leg boundaries and periods that do **not** divide the
tick.

⚠ **Refuse at construction**, never wrap silently: a cycle whose legs do
not sum to its period is an authoring error and must say so.

### What was built, and the four things the phase decided

⚠⚠ **THE CLOSED FORM SPLITS IN TWO AND ONLY THE SECOND HALF NEEDED A
PROBE.**  **TIME → STEPS is exact arithmetic** — `fixstep`'s `Bank` keeps
its remainder, so the hexes released by `t` are
`floor(rate × t / BANK_WHOLE)` however `t` was spent, and `cycle_walked`
is that expression.  **STEPS → HEX is a PATH**, which is `@M071` cashed
in: `flow_route` walks a field down to its core and the round is stored
as the hexes themselves, one per step offset, so `cycle_at` INDEXES it.

⚠⚠ **`@X335` — THE BANK DOES NOT RESTART AT A LEG BOUNDARY.**  A mob that
turns at an anchor carries its banked fraction across, so *how far into
this leg am I* is `walked(t) − walked(t₀)` and **never**
`walked(t − t₀)`.  ⚠ The two differ by a whole hex whenever the carry is
non-zero and nothing about the wrong form looks wrong.  ⚠⚠ **It is
equally an obligation on R3**: a mob that walks past an anchor spends its
remaining hexes on the NEXT leg, and ***a DWELL at an anchor is a LEG of
the cycle with a length, not a pause beside it*** — a pause is a second
time source and a closed form has room for exactly one.

⚠⚠ **`@M074` — 0 hexes and 0 legs of 8 920 swept moments disagree**, over
four speeds × three step lengths, on a fixture whose leg BENDS: **15
steps against a straight line of 12**, and a round of **30 against a
there-and-back of 24**.  ⚠ A cycle over straight lines could not have
been green, which is R0 probe 1 turned from a finding into a gate.

⚠⚠ **AND THE READING THAT SEPARATES THE TWO STEERINGS.**  A guard's
ten-second period is exact at a whole tick (0 of 120) and at half of one
(0 of 240) and **8 of 60 adrift at a TWO-tick step** — fifteen ticks is
7.5 of them, so the flip lands in the middle of one — while **the BAG
over the very same step holds at 0 of 60**.  ⚠ Same world, same walker,
one column apart: `@FR-E-Bag-Steers` reached from the TIMESTEP instead of
from the distance, and it is why `cycle_fault` refuses a clock period
that is not a whole number of ticks.

⚠⚠ **ELEVEN MUTATIONS, ELEVEN CAUGHT — AND TWO OF THEM MOVED THE GATE**,
which is `CLAUDE.md` § a gate that reads PERFECT is as suspect as one
that reads wrong, run rather than quoted:

- ⚠⚠ **The leg INDEX was invisible.**  Two legs of one round share every
  hex at their join, so `<` → `<=` at the boundary put the body on
  exactly the right hex and the sweep stayed green.  ⚠ It is caught only
  once the rule's leg is cross-checked against `errand_leg` — R1's own
  answer to *where am I going* — and then it is **314 legs of 8 920**.
- ⚠⚠ **The plan's own named control was being refused by a different
  check.**  `lengths [3, 3]` against `period: 9` fails the PATH-length
  test first, so deleting the sum check left the suite green: isolating
  *legs that do not sum to the period* needs a round whose path still
  agrees.

⚠⚠ **AND THE SHIPPED GUARD PERIOD CANNOT SEE ITS OWN ROUNDING** —
`@M014`'s class, one system over.  Ten seconds at 1.0, 1.5 and 2.5 hex/s
is 10, 15 and 25 WHOLE hexes, so the bank's carry is exactly zero at
every flip and the two forms above agree **for any implementation**.
⚠ The gate sweeps a **16-tick neighbour** (26.67 hexes at a scout's
pace), which is `@M013`'s *sweep the NEIGHBOURS of the shipped value*
and the only reason the branch is tested at all.

⚠ **And it cost one RENAME, which the flat namespace found rather than a
reviewer**: `part.loft::cycle_fault` — *does this part contain itself?* —
is now `part_cycle_fault`, because a part that contains itself and a
routine that comes back to where it started are both *a cycle* and only
one of them can have the bare name.  ⚠ It took the file's own prefix,
which is what the rest of `part.loft` already uses.

⚠ **Still inert**: nothing in `wave_tick` calls any of it, and no
scenario builds a cycle.  R3 is the mover, and § Rc is the conformance
gate that lands with it.

## R3 — deviation, and `slip`

⚠ F7b's *blocked by a COMPANION → step beside; blocked by the GROUND →
stand* for errand movers.  ⚠⚠ **The rule exists and the FIELD was
missing** — `spawn.loft:1222` says so itself, *"having no field to say
which way beside is"* — and a destination supplies one, so **this adds no
mover**.

⚠ `slip` is one integer: `position(t) = cycle(t - slip)`, incremented
when a step is refused.

⚠⚠ **AND THE SIDESTEP MUST BE EQUAL-DISTANCE-ONLY** — `@FR-E-Non-Increasing`,
which R0 probe 3 established is what bounds a deviating body at all.  ⚠ A
sidestep that could take a mob FURTHER from its destination looks
harmless here and **silently breaks R5's bound and R6's equality**, which
is the kind of coupling `@X324` means when it says a piece dropped for
convenience is a regression even when everything still works.

**Gate** — `tests/30_r3_the_deviation.loft`:
- a mob steps around a blocker **and still arrives** at the same hex;
- ⚠⚠ **`slip` accounts for the delay EXACTLY** — arrival tick minus the
  undisturbed arrival tick equals `slip`, which is the invariant that
  keeps R2's closed form true;
- ⚠ **one actor, ONE occupancy rule**: an errand robot and a cut-off
  robot ask `occupancy_taken` identically.  `crawler` paid for this —
  *"one actor cannot have two contradictory occupancy rules across its
  two states.  A sleeping monster is not terrain."*

⚠⚠ **R0 probe 1 has answered: it DOES need a field.**  Only **10 of 90**
straight crossings of the authored maps arrive, and **44 of the 80
failures are painted terrain** rather than the map's edge — so a leg is a
descent of a field toward its anchor, not a line.  ⚠ `@X331`'s bound
survives unchanged because a field distance is a distance.

## R4 — home is a PLACE

⚠ `errand_depart` currently **deletes** a robot that can go no further,
and its comment defends the conservation: it *"REMOVES rather than
killing"* so the wallet is not paid for traffic the player never
touched.  ⚠⚠ **The conservation is right and the PLACE was missing.**

⚠ A robot that finishes its round walks to `home` and leaves the roster
**there** — a maintenance point for a robot, a nest for an insect.

**Gate** — a scenario: a robot crosses the map, turns at its far anchor,
returns, and is gone at the home hex rather than at the map edge.  ⚠ And
the wallet is **unmoved**, which is the half `errand_depart` already
protects.

⚠ It is what makes `ROBOT_ECONOMY.md` § 5's `damage_persistence`
buildable later — *"a wounded robot walks home, is fixed, and returns
whole"* — but this phase does **not** build the return.

## R5 — the POI, its population, its BOUND

⚠ A `Poi { kind, q, r, state }`, a population attached to it, and every
attached mob's anchors derived from the POI.  ⚠⚠ **The bound is the
phase** — a static region, computed once, containing everything the
population can reach.

⚠⚠ **AND IT IS THE CYCLE'S REACH *DILATED BY THE DEVIATION*, WHICH THE
FIRST VERSION OF THIS PLAN GOT WRONG.**  A sidestep puts a body on a hex
the cycle never visits, so a bound computed from `cycle_at` alone **does
not contain the thing it is supposed to bound**.  ⚠ R0 probe 3 is what
says whether the dilation is a constant — and **if the deviation is
unbounded there is no static bound and `@X300` has to be redesigned**,
which is a plan-level answer rather than a phase-level one.

**Gate** — the containment claim in § Invariant gate, **with its vacuity
control**: a hex demonstrably outside the bound, or the assertion proves
nothing.

⚠ **Two to four per scenario** (`@X305`) is a scenario-authoring rule and
**not enforced in code** — `@X322`: the library supplies the bound, the
game supplies the number.

## R6 — CULL / EVALUATE / MATERIALISE

⚠ The three tiers, and the equality gate that makes them safe.

⚠⚠ **THE EQUALITY CLAIM IS NARROWER THAN `@X299` STATES IT, and this
plan's first version stated it wrongly.**  *Materialising at `R` and at
`2R` gives identical positions* is **false whenever anything can deviate
a body**, because the `2R` copy has been a body for longer and has
therefore deviated more.  ⚠ The claim that survives is:

> ⚠⚠ **Identical where nothing can deviate a body; and where something
> can, they differ by EXACTLY `slip`.**

⚠ Which recovers `@X299` rather than weakening it, because **two
un-materialised mobs cannot block each other** — neither has a body — so
deviation only ever happens where bodies are, which is inside the radius
by construction.  ⚠⚠ **Except for terrain, which is everywhere** — and
that is R0 probe 1's real stake: *if the cycle is not terrain-aware, the
equality fails everywhere.*

⚠ So the gate is **a pair**: the clean world where they agree, and the
blocked world where they differ and `slip` explains the difference
exactly.  A single-sided gate here would be `CLAUDE.md` § a gate that
reads PERFECT is as suspect as one that reads wrong.

⚠⚠ **A POI is never CULLED** (`@X304`) — only un-materialised.  The
phase must make that distinction impossible to get wrong: there is no
verb that removes a POI, and `@X304`'s *the workers still come and find
out* is R5a's job rather than this one's.

⚠ **Cost**: `tests/11_f8_the_tick_budget.loft` is the gate, and it is a
RATIO.  ⚠⚠ The claim to check is that the tiers make the tick **cheaper**
than materialising everything, and `@M029` warns that a cost gate should
be a COUNT rather than a clock — so count `cycle_at` calls, not
milliseconds.

## R7 — distraction: the hauler and your heap

⚠⚠ **The failure mode that eats the feature**, measured by `crawler`:
without its *an incursion does not break formation for a hero it has
merely seen* rule, raiders parked on a hero for seven days and *"the
whole mechanism silently became 'monsters walk at the player', which the
game already had."*

⚠ So the rule, and the gate is built round it:

> ⚠⚠ **A distraction must be caused by something the player DID or
> BUILT, never by the player being seen.**

**Gate** — a scenario pair plus the control:
- a hauler whose route passes a **salvage heap the player left** diverts,
  picks it up and carries it home — **the player loses the income**;
- ⚠ the same base with the heap **cleared** reads the baseline;
- ⚠⚠ **the negative control**: the player parked in plain sight, no
  heap, changes **nothing** — which is the assertion that would have
  caught `crawler`'s defect.

## Rc — the CONFORMANCE gate

⚠⚠ **The plan's one constitutive test**, and it lands with R3 because R3
is the phase that first makes it possible to fail:

> ⚠⚠ **At the end of every tick, every mob still on a cycle satisfies
> `cycle_at(e, now) == (e.q, e.r)`.**

⚠ Run over the whole gate corpus rather than one fixture — § Count the
RE-ASSERTION SITES lists **twelve** places that could break it and
**every one fails silently**, so the gate has to be where all twelve are
exercised.

⚠ **Its own negative control**: a mob the bubble has taken must be
EXCLUDED and must fail the assertion if it is not — otherwise the gate
would be green over a corpus in which nothing is on a cycle at all
(`CLAUDE.md` § A gate whose reading is already saturated).

⚠⚠ **AND R2 SHIPPED THE TRAP THAT MAKES THAT EASY TO GET WRONG.**
`cycle_where` answers a robot's OWN hex when it has no cycle, so
`cycle_where(e, c, now) == (e.q, e.r)` is **trivially true** for every
cut-off robot and for every robot in every scenario today.  ⚠ So the gate
must FILTER on `errand_role` and **count what it looked at** — the
assertion carries the count or it is measuring nothing.  The obligation
is written at the function as well.

## R8 — what a routine is WORTH

⚠ A scenario pair one token apart, and the reading this repo takes:
`@M050`'s 130 / 174, `@M059`'s 130 / 174 / 221, `@M070`'s 140 / 174.

⚠⚠ **It is also the design's own test** (`@X303`): *does this make
behaviour more BELIEVABLE, or does it only simulate MORE?*  A phase that
adds ticks of work and moves no clock has answered the second.

## What this plan does NOT build

⚠ Named so a later reader does not think they were forgotten.

- ⚠⚠ **The coarse world map** and everything deriving from it —
  [`WORLDGEN.md`](../../docs/WORLDGEN.md), BACKLOG F8+.  A later plan.
  **Every phase here runs against an authored `.keys` snapshot on
  purpose**, which is what makes that seam real rather than promised.
- **The FEATURES channel as a world-coordinate list** (`@X315`) — R5's
  POI is its scenario-local half.
- **FLOW** (`@X313`) — dryopea has `slope` and `drop` and has never
  computed a flow, and two of `@X315`'s six things are downstream of it.
- **The compact RESULT** (`@X306`) — `plans/28` S3 built the player's
  half; the world's half needs the POI states R5a would add.
- ⚠ **Crew remarks.**  R7 gives a mob a reason the player can see; a
  crew member SAYING so is `@X142`'s channel and is not built.
- ⚠⚠ **A library extraction.**  `@X322` says build the seams as library
  seams and **extract on the SECOND consumer** — so this plan lands in
  `src/` and names the seam, and `plans/10` is where it leaves.

## Open questions

1. ✅ **ANSWERED — it reuses the RECORD and grows no second door**
   (R0 probe 2 / `@X332`, cashed in by R1).  `errand_destination`
   answers a `task.loft` `Job`; `job_pick` is not called and must not be
   — *the crew CHOOSE what to do from where they stand; a mob is TOLD
   where to go.*  ⚠ `Job.kind` stays `TASK_ANY` until R7 reads one.
2. ⚠⚠ **Is a POI's population a POOL or a TAP?**  `ERRANDS.md` § Open
   questions 1 recommends a pool small enough to notice, on `@X303`'s
   grounds — *a pool the player can deplete and SEE thin is
   believability; a counter they cannot observe is simulation.*  **R5
   decides it.**
3. ✅ **ANSWERED — the deviation is bounded in SPACE** (R0 probe 3,
   `@FR-E-Non-Increasing`).  ⚠ What remains open is whether it needs
   bounding in TIME as well: a queued mob circles an iso-distance ring
   for as long as the queue lasts, and `slip` grows without limit.  ⚠ No
   gate needs it today; **R3 is where it would first be visible.**
4. ⚠⚠ **Must the CYCLE be terrain-aware?**  R0 probe 1.  ⚠ If a rule
   walks through a cliff that a body walks around, R6's equality fails
   everywhere and the closed form gets expensive.
5. ⚠⚠ **What happens when two sorties write back to the SAME cell?**
   `@X306` says the result is the snapshot changed and `@X177` settles
   the economy **per-planet**, with PvP as *a race for resources* — so
   concurrent writes are possible and **a merge is not commutative**,
   which is the one place `@X323`'s *local, deterministic, commutative*
   family has a member that may not qualify.  ⚠ Out of scope for this
   plan and it belongs to whoever builds `@X306`; recorded here because
   this is where it was noticed.
6. ⚠ **Does a mob's `carry` survive a save?**  `@X299` says an
   un-tracked mob is re-derived and loses nothing, but a **tracked** one
   has a history.  `persist.loft` holds the ground and the markers and a
   RUN is not in it, so this is the first thing that would ask.  **R6
   decides it.**
