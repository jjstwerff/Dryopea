<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `29` — The crew's own work

**Value:** `G` · **Effort:** `MH`

## Status

**O0-O4 SHIPPED 2026-08-28.  The plan is COMPLETE.**

⚠⚠ **Measured twice, and the pillar comes out intact as arithmetic.**
The default is worth **+44** where the work is near (130 → 174, `@M069`)
and **0** where it is not; one order is worth **+34** where the work is
far (140 → 174, `@M070`) and **0** where it is near.  **Neither
dominates**, which is `@X198`'s *does this make ONE axis dominate?*
answered with numbers instead of an argument.

⚠ And `@M069`'s middle reading is a fixture's privilege rather than a
player's: **no key moves a crew member**, so the +44 ticks `@M050`
measured could not be reached by anybody actually playing until O1.

This is [`ROADMAP.md`](../ROADMAP.md) § Then the run becomes a RUN item
**5 — helper ORDERS**, and it is the first item past the four critical-path
gaps, all of which closed on 2026-08-27/28 ([27](../27-building/README.md)
building, [28](../28-the-scramble/README.md) the scramble).

⚠⚠ **It is also a PILLAR and not a convenience.**  `docs/DESIGN.md` § 9
§ ASSIGNMENT IS A PILLAR (`@X197`, owner 2026-08-26) is categorical —
*"decisions about what tasks to assign helpers to should be a big part of the
game"* — and today **there is no assignment at all**: `helper_drive`'s only
caller in the tree is the `.keys` script runner.

## Goal

A crew member with nobody telling them anything **finds work and goes to it**,
and a crew member the player has driven out to **hunts one kind of work across
the whole base** — so that `@X262`'s test (*does this system work with NO
input, and does its depth surface when the player reaches for it?*) has an
answer in code rather than in prose.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 9 § ASSIGNMENT IS A PILLAR
  (`@X197`), § SEMI-AUTOMATIC BY DEFAULT (`@X252`), § It is a RATCHET
  (`@X253`), § 11 (`@X261`, `@X262`).
- [`docs/PROGRESSION.md`](../../docs/PROGRESSION.md) § P2f (`@X255` — the
  debrief is how the pillar is DISCOVERED), § P2a (the skill lattice).
- `@X289` — a specialised helper **can** be widened, and it costs a trip.
- `src/helper.loft`, `src/skill.loft` (`detect_sees` — the radius that
  already exists), `src/build.loft`, `src/height.loft`, `src/tower.loft`,
  `src/trap.loft`, `src/spawn.loft` (`repair_target` / `rearm_target` — the
  nearest-wins rule already written twice).

## Invariant gate

⚠⚠ **THE REMIT TRADES BREADTH FOR REACH, and it is two exact halves.**

| half | claim | how it fails |
|---|---|---|
| **kind narrows** | a directed crew member **never** accepts a job outside their remit | a remit that also widened the kind set |
| **reach widens** | every job a GENERAL crew member would take, one directed to that job's kind takes too | a remit that narrowed the distance as well |

⚠ And the **cover**: the union over the four remits contains everything the
general rule accepts, so **no kind of work becomes unreachable by narrowing**.

⚠ The negative control is `@M025`'s shape — a gate aimed at the mechanism is
not one aimed at the hazard.  The hazard here is a default good enough that
directing buys nothing, so **O4's measurement is the falsifiable prediction**:
if a directed crew is not measurably better than the semi-automatic one, the
pillar is inert and the design has to move, not the code.

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **O0** — probe: what a crew member does when nobody tells them anything | XS | `tests/29_o0_probe.loft` | ✅ **done 2026-08-28** |
| **O1** — the semi-automatic DEFAULT: nearest job inside their own senses | M | `tests/29_o1_the_default.loft` (10) | ✅ **done 2026-08-28** |
| **O2** — what an unattended crew is WORTH | S | `a-crew-that-finds-the-work.keys` — **130 / 174 / 174** | ✅ **done 2026-08-28** |
| **O3** — the REMIT: one kind, base-wide, and a cycle that widens again | M | `tests/29_o3_the_remit.loft` (10) + key **G** | ✅ **done 2026-08-28** |
| **O4** — what DIRECTING is worth | S | `a-crew-out-of-earshot` / `a-crew-that-was-told` — **140 vs 174** | ✅ **done 2026-08-28** |

## ⚠⚠ O0 — what the probe found

Five readings, taken before a line was designed —
`tests/29_o0_probe.loft`, green against the tree at `10878a6`.

1. ⚠⚠ **AN UNATTENDED CREW MEMBER NEVER GOES TO THE WORK.**  One crew
   member four hexes from a wall order, left alone for **sixty ticks** —
   longer than two of the three authored maps take to fall — is still at
   (4, 0), the order is still an order, and **not one unit of work is in
   it.**
2. ⚠ **The CONTROL says everything works and nothing chooses**: the
   identical base with the crew member standing *on* the site finishes
   the wall inside the same sixty ticks, and `hex_ground_name` answers
   `wall`.
3. ⚠⚠ **The destination is never even SET.**  A ticked crew member
   answers `helper_arrived` **true** — because it was never sent.
   `helper_drive` is the verb, and its only caller in the whole tree is
   the `.keys` script runner.
4. ⚠⚠ **All four jobs reach exactly ONE hex** — `BUILD_REACH_HEXES`,
   `VEHICLE_SALVAGE_REACH_HEXES`, `TOWER_REPAIR_REACH_HEXES` and
   `TRAP_REACH_HEXES` are all 1.  So a crew member's entire usefulness is
   the six hexes around wherever a fixture left them, and one hex further
   out they contribute **nothing, for ever**.
5. ⚠ **The walk is CHEAP, so the DECISION is the whole cost**: four hexes
   is 1.6 s of a crew member's time, under three ticks of a base that
   runs three hundred.  ⚠⚠ Which is `@X252` stated as arithmetic — **the
   scarce thing is the player's attention, never the crew's legs** — and
   it stops a later phase quietly pricing the default by travel time.

### ⚠⚠ What O0 settles about the design

**The corpus has been paying for the missing decision by hand.**
`tests/scripts/a-base-the-player-builds.keys` says it out loud —
*"The crew, sitting on the line the player is about to draw"* — and
`tests/17_t3_what_upkeep_is_worth.loft` hand-rolls a **SHUTTLE** policy
(drive every crew member to the nearest black tower, every tick) inside
the test that measures what upkeep is worth.  ⚠ Seventeen of the 47 gate
scenarios name a helper.

⚠ So the semi-automatic default is not a new idea to be validated — it is
a rule three fixtures already implement privately, and O1 is where it
stops being three implementations.

## ⚠⚠ O1 — the semi-automatic default, and the radius that had to move

**`src/task.loft`** — `Job`, the four `TASK_*` kinds, `jobs_in_scope`
and `job_pick`; **`src/spawn.loft::wave_assign`**, which is now the only
site in the tree where a crew member decides anything.  `wave_tick` calls
it **before** the crew move, so somebody who finds work this tick starts
walking this tick.

⚠ **The four jobs are the four a crew member already did by STANDING
somewhere** — build, clear, repair, re-arm.  O1 adds no fifth job; it
answers *which one to walk to*, and every one of them still happens by
position exactly as it did.

⚠ **`job_pick` is nearest-wins, ties by kind then `q` then `r`** — the
rule `repair_target` and `rearm_target` already write twice — plus one
line of stickiness: **a job already being walked to is abandoned only for
one strictly nearer.**  That is `@X198`'s *a job started is not cheap to
abandon* as arithmetic, and mechanically it is what stops a crew member
oscillating between two jobs that trade places as they move.

### ⚠⚠ The second correction: an ORDER is an order

⚠⚠ **The first version overruled `helper_drive`, and the corpus is what
said so.**  `wave_assign` ran every tick and rewrote every destination,
so `tests/17_t3` drove a crew member at the gate on every tick of the
run and it walked off to a heap — and a `.keys` file's `send 0 8 1`
became a line the simulation ignores.

⚠ **`helper_drive` is now an ORDER and the search does not overrule
one**; `helper_seek` is the other door, and it is what a crew member
chooses for themselves.  ⚠ `helper_direct` clears the order — **one
order at a time**, so cycling round to `TASK_ANY` hands somebody back to
the default completely.

⚠ It is not a concession to the corpus: a verb that says *go here* has to
be honoured or the vocabulary lies, and `plans/18` § S1a's *a script
AUTHORS* is the rule it was breaking.

### ⚠⚠ The measured correction: the radius is the crew member's ALONE

The first version asked `skill.loft::detect_sees`, so a heap's own
`notice_of_heap` widened a crew member's work radius from **3 hexes to
6**.  ⚠⚠ **It moved 18 tests across 8 files**, and the direction is what
mattered: bases that used to fall **stood for 800 ticks with 0 enemies
alive**, and a wallet that ended at 40 ended at **299 of 300**.

⚠ **The reason it was wrong is not the size.**  `@X277`'s two radii
answer ***is this thing worth driving to***, which is the PLAYER's
question and the one [`MATERIALS.md`](../../docs/MATERIALS.md) § The
governing rule needs.  This one asks ***what is under my nose***, which
is a fact about the person and nothing about the pile — a work radius
that grew with the depth of a heap would make a crew member's attention a
property of the rubbish.

⚠⚠ **And `@X198`'s test is what it failed**: *does this make ONE axis
dominate?*  At six hexes the default absorbed the work
[`DESIGN.md`](../../docs/DESIGN.md) § 9 § The scarcity is STRUCTURAL says
growth is supposed to CREATE — towers refilled themselves, bodies cleared
themselves — and **a default that absorbs the work it creates deletes the
table the pillar stands on.**

⚠ So `in_scope` asks `lat_distance <= detect_radius(sk)` and nothing
else: **3 hexes untrained, 4 at scout 5, 5 at scout 10.**

## ⚠⚠ O2 — what the default is worth

`a-crew-that-finds-the-work.keys`, the third of `@M050`'s pair and one
token apart from it each time:

| scenario | ticks | |
|---|---|---|
| `a-base-nobody-builds` | **130** | no wall at all |
| `a-base-the-player-builds` | **174** | crew parked ON the line they raise |
| `a-crew-that-finds-the-work` | **174** | crew two hexes off, nobody told them anything |

⚠⚠ **And the middle one is a FIXTURE'S privilege, not a player's.**
There is no key that moves a crew member, so before O1 the +44 ticks
`@M050` measured could not be reached by anybody actually playing.  The
default hands them back.

⚠ **Four of the 48 scenarios needed `send` to pin a crew member placed as
a blocker**, and none was re-priced: standing somewhere is an ORDER now
(`@X296`), and a fixture whose subject is not crew assignment has to say
what it used to get for free.  ⚠ The free readings are recorded beside
the measurements they move — `a-base-behind-a-moat` falls at **216**
instead of 221, and doubling `a-crew-on-one-front`'s crew is **+1 tick**.

## ⚠⚠ O3 — the remit

**Key `G`, and it CYCLES**: none → build → clear → repair → re-arm →
none.  `wave_direct_nearest` refuses unless the vehicle is within one
hex of somebody, so the whole cost of an order is the drive out to give
it (`@X156`), and pressing `G` in an empty field does nothing at all.

⚠⚠ **The cycle answers `@X289`'s open half by construction**: widening
is the same interaction as narrowing, so a player who has narrowed
somebody too far drives back out and keeps pressing.  ⚠ That is the
**RESET** reading rather than the STEP one, and it is `@X289`'s own
recommendation for `@X252`'s reason — *the presses are free and the TRIP
is what is scarce*.

⚠ `remit <i> <any|build|clear|repair|rearm>` is the `.keys` verb and
`count idle <lo> <hi>` the measurement; `emit.loft` writes a remit only
when there is one, so the 48 captured scenarios emit exactly what they
did.

## ⚠⚠ O4 — what an order is worth, and why the pillar survives

`a-crew-out-of-earshot.keys` / `a-crew-that-was-told.keys`, **two lines
apart** — `remit 0 build` and `remit 1 build`:

| | work NEAR the crew | work FAR from the crew |
|---|---|---|
| **nobody told them anything** | 174 (`@M069`) | **140** |
| **told, one trip each** | 174 | **174** (`@M070`) |

⚠⚠ **Read it as a square and the pillar is the shape of it.**  The
default buys **+44** in the left column and **nothing** in the right; an
order buys **+34** in the right column and **nothing** in the left.
Neither dominates the other, and which one is worth reaching for depends
on where the work is — which is `@X198`'s *position* axis paying for
itself.

⚠ **The price of an order is on the other side of the ledger and no
scenario here measures it**: from now on those two do nothing but build,
so the heap by the core is somebody else's problem.  `count idle` is the
reading a crew narrowed too far moves, and `@X253` is the design that
says it should tell you.

## What this plan does NOT build

⚠ Named so a later reader does not think they were forgotten.

- **The crew SPEAKING.**  `@X142`'s idle helper who says so, and `@X255`'s
  debrief remark about a person being wasted, are how the pillar is
  *discovered*; `@X130` was lifted by BACKLOG B1 so text can be drawn, but a
  remark channel is its own piece of work.  This plan makes IDLE a state that
  is true and readable, and stops there.
- **The practice loop.**  `@X124`'s *a helper you keep directing to repair
  becomes a repairer* needs skill advancement, and `@M066` measured that nine
  of the twelve skills have no number in the tree at all.
- **Helpers boarding the rocket on their own** — `plans/28` parked it for the
  same reason.
- **Carrying as a job.**  Ferrying a beacon or a tower top is a `plans/15`
  errand with a destination rule; the four jobs here are the four a crew
  member already does by standing somewhere.

## ⚠⚠ What shipped, and the three things the build changed

**`src/task.loft`** — `Job`, the four `TASK_*` kinds, `jobs_in_scope` and
`job_pick`.  **`src/spawn.loft`** — `wave_assign` (the ONE site where a
crew member decides anything, called before the crew move) and
`wave_direct_nearest`.  **`src/helper.loft`** — `remit`, `job`,
`ordered`, and the `helper_drive` / `helper_seek` pair.  The key table
gains **G**, `script.loft` gains `remit <i> <name>` and `count idle`, and
`emit.loft` / `compare.loft` carry the remit through a round trip.

⚠ **`task.loft` cannot see a `WaveState`** — `spawn.loft` uses it, and a
`use` imports one way only.  `scramble.loft::manifest_of` and
`moat.loft::moat_depth` were shaped by the same constraint, with the same
benefit: **a job search can be asked about a world that is not in a run
at all**, which is what makes the set claims in § Invariant gate
testable.

⚠ **The four jobs are the four a crew member already did by STANDING
somewhere**, and every one of them still fires on position exactly as it
did.  This plan added no fifth job; it answers *which one to walk to*.

## ⚠ Open, and deliberately left

- ⚠⚠ **Sending somebody to a HEX is not a player verb.**  The remit says
  *what*, never *where*, and `helper_drive` has no key.  `@X252`'s
  interaction is about the kind of work; a *go and stand there* order is
  a different mechanic and would want its own argument under `@X139`.
- ⚠ **Nobody SPEAKS.**  `wave_idle_crew` is the true state `@X142`'s
  helper and `@X255`'s debrief would read; the channel is not built.
- ⚠ **A crew member can walk into a trench of their own accord**, which
  is `@M058`'s *the crew dug themselves in* arriving without the player
  having decided it.  Left as it is: the same cost the moat already
  carries, and a guard would be the game protecting the player from a
  consequence the design puts in their hands.
- ⚠ **The search re-picks every tick.**  `job_pick`'s stickiness stops
  oscillation between equidistant jobs, but a job that is UNREACHABLE
  still attracts somebody who then walks at it for ever.  No scenario
  produces one, so it is a hazard rather than a defect — and the honest
  place to fix it is when one does.
