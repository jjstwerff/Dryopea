<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `30` — The mob routine

**Value:** `G` · **Effort:** `H`

## Status

**Open.  R0 is startable and everything else waits on it.**

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
> exactly as it is today*, so the 1692 tests and 920 measurements do not
> move until a scenario asks for the new thing.

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
| **R2** | ⚠⚠ **the closed form EQUALS the stepped one** | `cycle_at(e, t)` for arbitrary `t` is the hex reached by stepping the mover `t` ticks from `t0` — for **every** `t` in a sweep, not a sample | ⚠ a cycle whose legs do not sum to its period must be **refused at construction**, not silently wrapped |
| **R5** | ⚠⚠ **the bound contains the cycle DILATED BY THE DEVIATION** | every hex `cycle_at` can produce over a whole period, **plus every hex a deviating body can reach**, lies inside the POI's static bound | ⚠⚠ **a bound that contains everything is vacuous** — the gate must show a hex *outside* it too, or it proves nothing (`CLAUDE.md` § a gate that reads PERFECT is as suspect as one that reads wrong) |
| **R6** | ⚠⚠ **materialising at `R` and at `2R` is identical WHERE NOTHING CAN DEVIATE A BODY** | in a world with one mob and clear ground, the two runs agree hex for hex | ⚠⚠ **and with a blocker in the band between `R` and `2R` they MUST DIFFER, by exactly `slip`** — a gate that could not see that is measuring an empty claim |

⚠ **R1, R3, R4, R7 and R8 have no exact-invariant surface** — they are
behaviour and clocks, gated by scenarios and counts.

## Phases

⚠ Each row is **one thing**, lands green, and does not require the next.

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **R0** — the four probes | XS | readings written into this file | Open |
| **R1** — `Errand`: five anchors, and the BAG steers | S | `tests/30_r1_the_errand.loft` | Blocked on R0 |
| **R2** — the cycle is CLOSED-FORM | S | ⚠ the equality gate above | Blocked on R1 |
| **R3** — deviation, and `slip` | M | `tests/30_r3_the_deviation.loft` | Blocked on R0 + R2 |
| **R4** — home is a PLACE | S | a scenario: a robot walks home and leaves the roster there | Blocked on R1 |
| **R5** — the POI, its population, its BOUND | M | ⚠ the containment gate above | Blocked on R2 **and R0 probe 3** |
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

1. ⚠⚠ **Is a route across a real map straight enough for greedy plus the
   F7b sidestep?**  Walk a straight line across `starter_01`,
   `crossroads_02` and `the_gap_03` from every edge hex to its opposite,
   and count how many get stuck against terrain.  ⚠ `crawler` measured
   the failure — *"it walked into the first concave obstacle and stopped
   there — permanently … in 21 simulated days delivered nothing"* — and
   dryopea's errand robots walk a heading with **no field at all** today,
   so the cheap answer may already be right.
   ⚠⚠ **And the stakes are higher than *R3 needs no field*.**  If the
   cycle is computed over **straight lines** while a materialised body
   walks over **terrain**, then a body deviates where a rule does not —
   and **R6's equality claim is false everywhere, not just near
   blockers**.  So probe 1 decides whether ***the cycle must be
   terrain-aware***, which is a much larger question than pathing cost.
2. ⚠ **Does `task.loft` generalise to an enemy?**  `jobs_in_scope` takes
   the pieces and not a `WaveState` (`@X322`'s library seam, arrived at
   for a different reason).  Read whether an `Errand` can reuse
   `job_pick` outright, or whether the crew's *nearest wins* and a mob's
   *follow your cycle* want two doors.
3. ⚠⚠ **IS THE DEVIATION BOUNDED?**  `@X300`'s whole architecture needs
   a **static** region per POI, and § Invariant gate now says the bound
   must contain the cycle **dilated by the deviation** — which is only
   possible if a deviating body cannot wander arbitrarily far.
   ⚠ F7b's sidestep is one hex, but a **queue** of blocked mobs might
   push one many hexes over many ticks, and nothing today bounds it.
   ⚠⚠ **If the deviation is unbounded, the bound is not static and
   `@X300` breaks** — so this probe gates R5 and R6, and its answer may
   force a cap (*a body more than `d` hexes off its cycle re-converges or
   becomes stateful*).
4. ⚠ **What does a tick cost per mob today?**  `cycle_at` will be called
   per candidate per tick and `plans/22` warns the field family is
   **~69 %** of the suite.  ⚠ Take the count, not the clock (`@M029`:
   two identical calls differed **5.4×**).

⚠ Write all four answers into this section before R1 starts.

## R1 — `Errand`: five anchors, and the BAG steers

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

## R2 — the cycle is CLOSED-FORM

⚠ `cycle_at(e, t)` — one modulo for the phase, O(legs) to find the leg,
one interpolation.  ⚠⚠ **The gate is EQUALITY** (§ Invariant gate), and
it is the strongest gate in the plan because it is what makes `@X299`
not-LOD.

⚠ **Sweep `t`, do not sample it.**  `CLAUDE.md` § AND THE VACUITY CAN BE
IN THE NUMBERS warns that *a 1 Hz clock driven by a 30 Hz clock cannot
disagree for any implementation* — so the sweep must include `t` values
that are **not** leg boundaries and periods that do **not** divide the
tick.

⚠ **Refuse at construction**, never wrap silently: a cycle whose legs do
not sum to its period is an authoring error and must say so.

## R3 — deviation, and `slip`

⚠ F7b's *blocked by a COMPANION → step beside; blocked by the GROUND →
stand* for errand movers.  ⚠⚠ **The rule exists and the FIELD was
missing** — `spawn.loft:1222` says so itself, *"having no field to say
which way beside is"* — and a destination supplies one, so **this adds no
mover**.

⚠ `slip` is one integer: `position(t) = cycle(t - slip)`, incremented
when a step is refused.

**Gate** — `tests/30_r3_the_deviation.loft`:
- a mob steps around a blocker **and still arrives** at the same hex;
- ⚠⚠ **`slip` accounts for the delay EXACTLY** — arrival tick minus the
  undisturbed arrival tick equals `slip`, which is the invariant that
  keeps R2's closed form true;
- ⚠ **one actor, ONE occupancy rule**: an errand robot and a cut-off
  robot ask `occupancy_taken` identically.  `crawler` paid for this —
  *"one actor cannot have two contradictory occupancy rules across its
  two states.  A sleeping monster is not terrain."*

⚠ R0's first probe decides whether this needs a field.

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

1. ⚠ **Does R1 reuse `job_pick` or grow a second door?**  R0's second
   probe decides it.
2. ⚠⚠ **Is a POI's population a POOL or a TAP?**  `ERRANDS.md` § Open
   questions 1 recommends a pool small enough to notice, on `@X303`'s
   grounds — *a pool the player can deplete and SEE thin is
   believability; a counter they cannot observe is simulation.*  **R5
   decides it.**
3. ⚠⚠ **Is the DEVIATION bounded?**  R0 probe 3.  ⚠ If it is not, `@X300`'s
   static bound does not exist and R5/R6 need redesigning rather than
   building — which is why it is a probe and not a phase.
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
