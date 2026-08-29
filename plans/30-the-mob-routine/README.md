<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `30` — The mob routine

**Value:** `G` · **Effort:** `H`

## Status

**⚠⚠ PLAN COMPLETE 2026-08-29 — R8 closed the last phase, and a robot
road is worth 146 ticks of a base's life.**  ⚠ Gates: **1777 green over
143 files**, `validate.sh` **55 scripts / 974 measurements** (+2 scripts,
+18 — R8's pair) and `validate_gl.sh` **3 fixtures / 55 measurements**,
unchanged.

⚠⚠ **`@M085` — 123 ticks against 269, and the SWEEP is the finding.**
The same map, wall, waves and painted road; four coordinates put the
round inside the scrambler bubble or 36 hexes out of it.  Four haulers
nobody sent at the player lose their link crossing it and join the siege
— seven alive against three.  ⚠ But the road is worth **196** ticks
against a wave of two, **146** against three, **10** against five and
**0** against eight: the siege front is the WALL'S WIDTH (`@M020`), so a
wave of eight saturates it and four more bodies change nothing.
⚠⚠ **The first version of this pair ran the authored 5 + 8 and read 118
against 118 with four extra besiegers plainly on the map** — which is
`CLAUDE.md` § *a gate whose reading is already saturated* caught by
pricing the supply against the capacity.

⚠ So `@X303`'s test is answered the right way: the routine makes
behaviour more BELIEVABLE **and** moves the clock, and `@X305`'s *a POI
earns its place only if REMOVING it moves the clock* has a number.

**R7b COMPLETE 2026-08-29 — a hauler turns for what you left, and it
costs you thirty points.**  ⚠ Gates: **1777 green over 143 files** (+7, all this phase's),
`validate.sh` **53 scripts / 956 measurements** (+2 scripts, +24 — the
pair and its control) and `validate_gl.sh` **3 fixtures / 55
measurements**, unchanged.

⚠⚠ **`@X346` — A HEX WALKED AWAY FROM ITS ANCHOR COSTS TWO, AND THAT IS
WHAT LETS A MOB LEAVE ITS ROUTE AT ALL.**  `@FR-E-Slip` says a deviation
costs TIME and never DESTINATION and that *a body pushed off its cycle
re-converges on the same hex* — so a detour is paid for **both ways**,
and the return leg is indistinguishable from ordinary progress.  ⚠ The
charge is read from the ANCHOR's distance before and after — 0 closer, 1
equal, **2 further** — and then the rule's distance to the anchor equals
the body's at every moment of the detour **with nothing remembered**,
which is what `@FR-E-Place-State` requires: a mob may hold nothing
beyond `carry` and `slip`, so there is no debt counter to reach for.

⚠⚠ **AND THE BAG IS NOT TOUCHED BY A THEFT.**  The bag is the ROUND's
state and `cycle_phase` reads the WALK, so a pickup that flipped it
would put the body on a different leg from its own rule.  The stolen
heap is a `CarryObject` instead — `@X334`'s obligation coming due — keyed
on `BLOCKER_MOB + PoiMob index`, an identity that **outlives a body**
where a roster slot does not.

⚠⚠ **`@M084` — 230.0 against 200.0, one table column apart.**  And the
negative control is in the same file and it is the half that matters:
the player parks one hex off the road, in plain sight, for **thirty
ticks**, and the robot is on its rule's own hex to the tick.  ⚠ Thirteen
mutations, twelve caught, and the survivor was the load's MATERIAL — a
spill that lost its source hands the player back the wrong rubble, and
every count stayed green because *a robot's corpse is wreckage too*.

**R7a COMPLETE 2026-08-29 — a PLACE is four verbs in a `.keys` file,
and the corpus has one.**  ⚠ Gates: **1770 green over 142 files** (+9, all this phase's), `validate.sh` **51 scripts / 932
measurements** (+1 script, +12 — the first captured scenario with a POI
in it) and `validate_gl.sh` **3 fixtures / 55 measurements**,
unchanged.

⚠⚠ **R7 IS SPLIT, for R6's reason and R5's and R6b's own words** — both
closed saying *no `.keys` vocabulary, and R7's scenario pair is what
needs one*.  R7a is the vocabulary; R7b is the distraction and the
scenario pair it gates.

⚠⚠ **`@X345` — `poi` / `route` / `mob` SAY WHAT A PLACE ISSUES AND
`routine` SAYS WHAT ONE BODY HOLDS**, and neither is derivable from the
other once a tick has run: `errand_arrive` flips the bag and
`errand_step` adds to `slip` without asking any place.  ⚠ **A route
ENLISTS its own population** — a file says *how many* and never *which
ones* — which works only because a record is never removed.
⚠⚠ **And `routine` is the first command in the vocabulary carrying THREE
hexes**, so `KeysSchema` grew a third pair position.

⚠⚠ **`@M083` — twelve mutations, twelve caught, and BOTH real defects
came from somewhere else.**  `compare.loft`'s `errand` row, laid the same
hour, went red on `a-road-that-passes-by`: **the flag and the role are
two facts**, and an AMBIENT robot carries `errand: true` with
`ROLE_NONE`.  And `tests/18_s2`'s corpus sweep went red on the first
scenario with a POI: **`slip` is the one duration in the game that may be
NEGATIVE** and the authoring door refuses one on purpose (`@D009`).
⚠ Neither was on the mutation list, *because a mutation list is written
by the person who wrote the code*.

**R6b COMPLETE 2026-08-29 — the game materialises its own mobs, and two
window sizes give one answer.**  R7 was startable.  ⚠ Gates: **1761 green
over 141 files** (+8, all this phase's), `validate.sh` **50 scripts / 920
measurements** and `validate_gl.sh` **3 fixtures / 55 measurements**,
both **UNCHANGED**.

⚠⚠ **`@X344` — A POI'S REACH IS ONE INTEGER, AND THE CULL IN THE TICK IS
ONE SUBTRACTION.**  R5's bound is a union of discs, and asking it needs
the rounds — two flow sweeps a route — so a POI culled every tick for a
whole sortie would pay for the rounds that prove it can be.  ⚠ Folded
once to `max(lat_distance(poi, centre_i) + radius_i)`, the question
becomes `lat_distance(poi, player) <= reach + window`: no world read, no
round built.  ⚠ The cache is a `vector<integer>` on `PlayState` —
DERIVED rather than authored — and it carries no vectors-in-structs,
because that is [loft#974]'s shape and **a green reading of it is not
evidence**.

⚠⚠ **`@M082` — eleven mutations, seven caught, and ALL FOUR SURVIVORS
WERE THINGS THE GATE COULD NOT SEE.**  ⚠⚠ ***A saving is not a
behaviour***: deleting the cull changed no position and made no extra
body, so the cull had to be read as **work NOT DONE** — `poi_step` grew a
fourth answer and the far run reads **0 against 160**.  ⚠ And the
fixture's geometry hid the reach formula: `max(radius)` is exactly right
on a straight out-and-back and a defect on every bend, so that claim is
asserted directly rather than played.

**R6a COMPLETE 2026-08-29 — a mob nobody can see costs one integer, and
giving it a body back changes nothing.**  R6b is startable.
⚠ Gates: **1753 green over 140 files** (+8, all this phase's),
`validate.sh` **50 scripts / 920 measurements** and `validate_gl.sh`
**3 fixtures / 55 measurements**, both **UNCHANGED**.

⚠⚠ **R6 WAS SPLIT** — the phase as written needs a `WaveState` field and
a materialiser inside `wave_tick`, which is the first change in this plan
that is **not inert by construction**.  R6a is the mechanism and its
claim; R6b is the wiring and the `R` vs `2R` pair.

⚠⚠ **`@X343` — AN UN-MATERIALISED MOB IS A `slip` AND NOTHING ELSE.**
Everything about it that CAN be a function of `t` is one: which round it
walks is an index, where on it it started is a **SEAT** whose offset is
derived, which hex it is on is `cycle_at`, **what is in its bag is
derived from the leg** (`errand_bag_for`) and **what it carries toward
its next hex is derived too** (`cycle_carry`).  ⚠ Only a BODY can be
pushed, so only `slip` accumulates — and a mob that has had one must keep
its lateness or `@FR-E-Slip` is refunded exactly where nobody can watch.

⚠⚠ **AND THE BANK IS THE ONE NOBODY WOULD THINK OF** (`@M081`): a fresh
body arrives with `bank_new()` while the rule is generally part-way
through a hex, so a materialised mob releases its next hex **late by
exactly `cycle_carry`** — perfect at the moment it appears, drifting from
the tick after.  ⚠ At 1.5 hex/s a hex is one tick and the carry is ZERO
on 24 of 24 ticks, so the shipped robot cannot see it at all (`@M014`'s
class, a fifth instance); the fixture walks a SCOUT and says so.

⚠⚠ **`@M081` — ten mutations, nine caught, and BOTH survivors were
faults in the GATE.**  The round cache indexed by POI instead of by route
read green because the test compared `poi_bound_from` against
`poi_bound` — and `poi_bound` **is** `poi_bound_from` over `poi_cycles`.
***A wrapper is a golden of its own delegate***, which is `plans/09`'s
rule with a new subject, and the fix is the same one: an independent
ORACLE in the test.  ⚠ The second survivor is a redundancy with a name —
`errand_bag_for`'s two conditions cannot both hold, because
`errand_terminal` already refuses a row whose `laden` is home.

**R5 COMPLETE 2026-08-29 — a PLACE owns its mobs, and its reach is a
region you can ask about before anything moves.**  R6 is startable.
⚠ Gates: **1745 green over 139 files** (+8, all this phase's),
`validate.sh` **50 scripts / 920 measurements** and `validate_gl.sh`
**3 fixtures / 55 measurements**, both **UNCHANGED**.

⚠⚠ **`@X342` — THE BOUND'S CLAIM IS PER-LEG, AND THE UNION IS ONLY THE
QUERY.**  `@FR-E-Poi-Owns` states the bound as *the union over legs of
the disc centred on each anchor with the incoming leg's length as its
radius*, and the containment gate was written to match that sentence.
⚠⚠ **It is very nearly unfalsifiable**: against the union, a radius one
hex short, a terminal leg given no disc at all, a rim excluded by an
off-by-one, and **a sidestep that could increase the distance** all read
GREEN — four of nine mutations, and the four that matter.  ⚠ The reason
is geometry: consecutive discs are centred a leg apart with that leg's
length as their radius, so **a hex outside its own leg's disc sits inside
its neighbour's**.  ⚠⚠ What `@FR-E-Non-Increasing` and `@X336` prove is
about **one leg at a time**, so the CLAIM is `bound_disc_holds(b, leg,
h)` and the union is a strictly larger promise nobody made.  ⚠ Two doors,
one implementation, and the difference is nameable: the claim asks a body
about its own leg; the query asks *could anything bounded by this be
there*, where slack is what is wanted.

⚠⚠ **`@M080` — nine mutations, four survived the union and nine are
caught by the leg**, and the sidestep is the reading that matters: **13
of 120 body-ticks leave the leg's disc where none leave the union.**
⚠⚠ **`@M079` — and the probe chose the fixture.**  On the three authored
maps the bound covers **1466 of 1467** standable hexes, because a round
that crosses its patch has legs as long as the patch — so a containment
gate over one of them would be satisfied by a region excluding ONE hex.
⚠ The fixture is therefore a world much wider than its round, and what
the bound LEAVES OUT is asserted before what it holds.  ⚠ The same probe
prices the design's one shortcut: reading the bound in **lattice**
distance rather than field distance admits **one hex** across three maps,
which is what buys `2 × legs` integers and no world to read.

**R4b COMPLETE 2026-08-29 — a round can end somewhere it does not
pass, and the commonest robot in the world now has an ending.**
R5 is startable.  ⚠ Gates: **1737 green over 138 files** (+6, all this
phase's), `validate.sh` **50 scripts / 920 measurements** and
`validate_gl.sh` **3 fixtures / 55 measurements**, both **UNCHANGED** —
the inertness measured rather than asserted.

⚠⚠ **`@X341` — THE TURN IS A THIRD VALUE OF THE BAG, AND THAT IS THE
DECISION RATHER THAN THE ARITHMETIC.**  The closed form was never in
doubt: with `S = cycle_walked(rate, shift)` the mob turns for home at
`T = ceil(S / period) × period`, so `cycle_phase` is *one modulo below
`T`, one subtraction above it* and `@FR-E-Closed-Form` is untouched.
⚠ What had to be decided is how the BODY reaches the same moment — it
has no cycle and cannot know `period`, so *have I reached T* is
unanswerable in the tick.  ⚠⚠ **What it CAN see is the moment its bag
empties at the drop-off, and the first empty leg after the shift is
exactly `T`** — so the turn is latched into `carry` as
`ERRAND_BAG_HOMEWARD` and `errand_leg` reads ONE number for all three
legs.  A second field saying *am I finished* would be a clock in all but
name; **the bag is where an ending belongs because the bag steers.**

⚠⚠ **`@M078` — eight mutations, seven caught, and the fixture's LEG is
what catches the mover.**  A round of 18 makes the shipped 180 s shift
**exactly 15 rounds at 1.5 hex/s and exactly 25 at 2.5**, so the turn
lands ON an arrival and the scout's turning hex is the SECOND of its
tick: a mover reading only the tick's opening hex count is then one hex
short, answers *not yet*, and takes another whole round.  ⚠ A regular
robot releases one hex a tick and cannot see it at any leg length —
`@M014`'s class, a fifth instance, and `@M076`'s method reused.
⚠⚠ **And the gate needed a state the corpus had not got**: a harvester
standing on its depot in a session whose clock is already past the shift
— what a cropped `.keys` fixture is (`@X335`) — is the one state in
which *standing on home* and *finished* come apart.  ⚠ The one survivor
is inert across all six plan-30 files and its reason is nameable, which
is why it is a redundancy rather than a gap.

**R4 COMPLETE 2026-08-28 — a round has an ENDING, and it is a PLACE.**
⚠⚠ **AND ITS REACH WAS CORRECTED THE NEXT DAY** (`@X339`): a
plant-material harvester's **dump** and its **repair point** are
different places, so `haul` is that robot's shape and `gather` is the
NEST's — and R4's ending, which needs `home` ON the round, reaches the
nest and not the harvester.  ⚠ **R4b** is the terminal leg that closed
it, and it matters because carbon is one of the planet's two bottlenecks
(`@X340`), which makes the harvester the commonest errand in the world.
⚠ Gates at R4: **1731 green over 137 files** (+6, all that
phase's), `validate.sh` **50 scripts / 920 measurements** and
`validate_gl.sh` **3 fixtures / 55 measurements**, both **UNCHANGED**.

⚠⚠ **`@X338` — home is a LEG OF THE ROUND, not a place a finished mob
walks to**, and the plan's own one-sentence invariant is what decided
it: *three states and ONE exit* leaves no room for a mob that breaks off
its cycle to walk somewhere the round never goes.  ⚠ So the mob is
exactly where its rule says right up to the tick it is gone, the mover
and the cycle are untouched — and the bill was that **two of the three
shipped roles could not END**, which `errand_row_fault` NAMED rather
than leaving open.  ⚠ **R4b paid it** (`@X341`).  ⚠⚠ **And the departure moved from the consequence
stage to the TOP of the next tick**: taken where it was, a robot arrives
at its nest and is removed inside one tick, so the last frame that ever
holds it has it **one hex short** — *deleted where it happened to stop*
in the new rule's clothes.  ***What the player cannot see the gate
cannot see either.***

⚠⚠ **`@M076` — eight mutations, eight caught, and TWO of them chose the
fixture.**  At a 13-hex leg the file was green with the mover's
finishing stop deleted and green again with the mob's own clock replaced
by the run's; sweeping the leg length is what found an 8 that cannot be.
⚠⚠ **And the scout found `@D008` on the way in** — `errand_fields` built
one field per DESTINATION rather than one per ANCHOR, so a mob that
turned mid-tick had nothing to descend and **its hexes went to `slip`**:
twelve hexes of drift over three minutes, with every equality in § Rc
green, because the rule reads `now − slip` and follows the body down.
***`slip` is a currency that can pay for a defect***, and only a
LIVENESS gate with a scout in it could say so — `@X337` a second time.

**R3 + Rc COMPLETE 2026-08-28 — the mob MOVES, and it is still INERT.**
R4 and R5 are startable.  ⚠ Gates: **1725 green over 136 files** (+11,
all this phase's), `validate.sh` **50 scripts / 920 measurements** and
`validate_gl.sh` **3 fixtures / 55 measurements**, both **UNCHANGED** —
which is the inertness measured rather than asserted.

⚠⚠ **`@M075` — the conformance gate reads 0 hexes, 0 phases and 0 legs
of 320 mob-ticks, and it took SEVEN mutations to make it able to say
so.**  ⚠ `@X336`: **the claim splits in two, and only the PHASE half is
total** — a mob whose first choice is taken walks a different route of
the same length and has lost no time at all, so hex equality holds where
nothing can deviate a body and the DISTANCE holds everywhere.  That is
`@FR-E-Slip`'s own *"re-converges on the same hex"* read exactly, and it
is R6's *identical where nothing can deviate* arriving three phases
early.  ⚠⚠ `@X337`: **a DWELL is not a BLOCK** — charging `slip` for the
ticks a guard stands at its post freezes its clock for ever **with every
conformance count green**, because a frozen rule agrees with a frozen
body.  ⚠ Conformance is an equality between two things that can stop
together; **liveness has to be asked separately**, and two of the six
gates now do.  ⚠ Two more findings: **a lost hex must be a whole number
of base units** (`cycle_fault` refuses a rate that has not got one —
`@M074`'s family, one subject over) and **the mover resolves its
destination at the moment the step BEGINS**, which is invisible for
every role whose clock leaves it dwelling.

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
⚠ `@X334`: a mob's bag is not `carry.loft`'s ledger, and R7b is where
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
| **R3** — deviation, and `slip` | M | `tests/30_r3_the_deviation.loft` (5) | ✅ **COMPLETE 2026-08-28** — `@M075`, `@X336`, `@X337` |
| **R4** — home is a PLACE | S | `tests/30_r4_home.loft` (6) — a played session | ✅ **COMPLETE 2026-08-28** — `@X338`, `@M076`, `@D008` |
| **R4b** — the TERMINAL leg: a round that ends somewhere it does not pass | M | `tests/30_r4b_the_terminal_leg.loft` (6) — a played HARVESTER, plus R2's sweep extended | ✅ **COMPLETE 2026-08-29** — `@X341`, `@M078` |
| **R5** — the POI, its population, its BOUND | M | `tests/30_r5_the_bound.loft` (8) — ⚠ the containment gate above, asked PER LEG | ✅ **COMPLETE 2026-08-29** — `@X342`, `@M079`, `@M080` |
| **R6a** — the three tiers, and what an un-materialised mob IS | M | `tests/30_r6a_the_tiers.loft` (8) — the round trip, and its blocked pair | ✅ **COMPLETE 2026-08-29** — `@X343`, `@M081` |
| **R6b** — the materialiser in the TICK, and the `R` vs `2R` pair | M | `tests/30_r6b_the_materialiser.loft` (8) — ⚠ the `R` vs `2R` gate **and its differ-control** | ✅ **COMPLETE 2026-08-29** — `@X344`, `@M082` |
| **Rc** — the CONFORMANCE gate | S | `tests/30_rc_the_conformance.loft` (6) | ✅ **COMPLETE 2026-08-28** — ⚠ and it needed a LIVENESS gate beside it (`@X337`) |
| **R7a** — the places, said in a `.keys` file | M | `tests/30_r7a_the_places_said.loft` (9) + `tests/scripts/a-place-that-sends-robots.keys` (12) | ✅ **COMPLETE 2026-08-29** — `@X345`, `@M083`, `@D009` |
| **R7b** — distraction: the hauler and your heap | M | `tests/30_r7b_the_distraction.loft` (7) + the scenario pair (24) and its *merely seen* control | ✅ **COMPLETE 2026-08-29** — `@X346`, `@M084` |
| **R8** — what a routine is WORTH | S | `a-base-on-a-robot-road.keys` + `a-base-beside-a-robot-road.keys` (18) | ✅ **COMPLETE 2026-08-29** — `@M085` |

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
route* is R7b's and is deliberately not a column yet.

⚠⚠ **`@X334` — a mob's bag is not `carry.loft`'s ledger**, and the
difference is nameable: *that file conserves an object that is on the
map and the player could pick up instead; a bag holds material that was
never on the map.*  ⚠ **R7b is where that stops being true**, and the
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

✅ **COMPLETE 2026-08-28** — `src/errand.loft` § THE MOVER,
`src/spawn.loft` (`WaveState.now`, the fork in `wave_tick`, a public
`enemy_move_to`), `tests/30_r3_the_deviation.loft` (5 tests), `@X336`,
`@X337`, `@M075`.

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

### What was built, and the four things the phase decided

⚠⚠ **`@X336` — THE CLAIM SPLITS IN TWO, AND ONLY THE PHASE HALF IS
TOTAL.**  § Invariant gate and § Rc both say *the mob is on
`cycle_at(t − slip)`*, and building the mover showed that is exact only
where nothing can push a body:

> ⚠ a mob whose first choice is taken by a companion walks a **different
> route of the same length** and has **lost no time at all**, so it is
> neither on the rule's hex nor late.

⚠ `@FR-E-Slip` already said so and the plan had read it too strongly: *"a
body pushed off its cycle **re-converges on the same hex**"* is a claim
about where it ENDS UP — the anchor — and not about where it stands on
the way.  ⚠⚠ **What is total is the DISTANCE**: a mob's field distance to
its current anchor equals the rule's, under every deviation, at every
moment.  Hex equality is the special case of it where nothing blocked.
⚠ That is the currency `@FR-E-Boundable` is already stated in, so R5's
bound is untouched — and it is **R6's *identical where nothing can
deviate a body* arriving three phases early**, which is the reading to
carry into R5 rather than rediscover in R6.

⚠⚠ **`@X337` — A DWELL IS NOT A BLOCK, AND CONFORMANCE CANNOT SEE THE
DIFFERENCE.**  The first mover charged `slip` for every released hex it
could not spend, which is right for a blocked mob and **wrong for a guard
standing at its post**: `cycle_phase` clamps a clock leg's offset at the
leg's own length, so those hexes are ones the RULE does not spend either.
⚠ Charging them drags `now − slip` backwards for as long as the guard
stands, its clock never flips, and **the guard freezes at its first post
with every conformance count green** — because a frozen rule agrees with
a frozen body.

> ⚠⚠ **Conformance is an equality between two things that can stop
> together.  LIVENESS has to be asked separately.**

⚠ Two of § Rc's six gates now do: *nothing blocked it so nothing slipped*
(slip is **0** and both anchors are reached) and *a mob that stood is
exactly that late*.  ⚠ The measurement is the mutation's: a guard that
slips while dwelling reads **112 000 000 units of slip, ONE anchor and a
reach of 4** where the true one reads 0 / 2 / 4.

⚠⚠ **A LOST HEX MUST BE A WHOLE NUMBER OF BASE UNITS** — `@M074`'s family
with a different subject.  For `cycle_walked(rate, t − slip)` to be
exactly one hex short at **every** later `t`, `rate × slip` has to be a
whole multiple of `BANK_WHOLE`; otherwise the floor disagrees wherever
the fraction falls under the remainder.  ⚠ So one lost hex costs
`BANK_WHOLE / rate` and `cycle_fault` **refuses a rate that has not got
one**.  Every shipped speed — 1.0, 1.5 and 2.5 hex/s — divides exactly,
which is why the constraint has never been felt and exactly why it needs
a gate rather than a comment (`@M014`).

⚠⚠ **AND THE MOVER RESOLVES ITS DESTINATION AT THE MOMENT THE STEP
BEGINS**, never at the moment it ends — `tests/30_r2_the_cycle.loft`'s
harness contract, which the first version of `wave_tick` broke by
advancing the clock at the TOP of the tick.  ⚠ It is **invisible for
every role whose clock leaves it DWELLING at its post**, and the shipped
guard dwells eleven ticks in fifteen, so § Rc carries a **guard with no
dwell at all** — a miner walks exactly 10 hexes in the guard's fifteen
ticks — and that member alone reads **34 hexes and 34 phases adrift**
when the clock moves to the top.  ⚠ `WaveState.now` is therefore *the
moment the run has reached*, advanced at the END of the tick.

### ⚠⚠ And the TRIPWIRE fired the same hour it was laid

⚠⚠ **Sites 7, 8 and 9 went LIVE rather than waiting.**  R1 put `route`
into `compare.loft` as *a tripwire, here before it can differ*, and R3
put `now` beside it on the same argument — *nothing in the corpus
advances a clock any measurement reads*.  ⚠ **That was wrong within the
hour**: `tests/18_s2`'s round trip plays a scenario, writes it down with
`emit_keys` and replays it, and every `tick` in the corpus was advancing
a clock nothing wrote — **`'now: 442000000 vs 0'`**, on two of its six
tests, the first time the suite ran with a clock in it.

⚠ **The answer was the one the plan had already written down**: a `.keys`
verb and never a looser comparison.  `emit_keys` writes `now <seconds>`
when the moment is not the run's first, `script.loft` reads it back, and
**the writer and the reader are a PAIR** (`@D007`) — deleting either half
turns the round trip red, which is checked.  ⚠ `crop_state` carries it
whole with the other run-wide switches, because a crop that reset the
moment would put every routine in the cropped fixture back at the start
of its round while leaving the bodies where they stood.

⚠⚠ **The reading is about the tripwire, not about the clock**: a
comparison added *before anything could tell* cost one line and found a
real gap on its first full run.  The banked carry's row one file over did
exactly the same thing, and this is the second instance.

### ⚠⚠ Seven mutations, and THREE of them moved the gate

⚠ `CLAUDE.md` § a gate that reads PERFECT is as suspect as one that reads
wrong — both files were green on their first run.

| mutation | caught by the first version? |
|---|---|
| the slip increment deleted | ✅ 555 of 1149 swept moments |
| a sidestep counted as progress | ✅ same |
| slip charged in TICKS instead of hexes | ⚠ **only by the cross-product** — at the shipped 1.5 hex/s a tick IS a hex |
| no sidestep at all | ✅ 9 of 18 runs never arrived |
| the divisibility refusal deleted | ✅ |
| the mover ignores companions | ✅ |
| **the sidestep may go FURTHER** | ❌ **no** — see below |
| **a dwelling guard slips** | ❌ **no** — `@X337` |
| **the clock advances at the top** | ❌ **no** — until the zero-dwell guard |

⚠⚠ **The sidestep widening is the one `@X331` predicted and the file
still could not see** — replacing `flow_sidesteps` with every neighbour,
the exact change `@X331` calls *"harmless-looking and it silently breaks
R5's bound and R6's equality"*, changed **nothing**, because on the
fixture's axis the first free neighbour in direction order happens to be
an equal one anyway.  ⚠ Seeing it needs a mob whose **equal ring is full
while the hexes behind it are open**, so the only thing left to take is a
step backwards — and the first attempt at that gate hemmed the further
hexes in too, which makes a mover that would walk backwards stand still
for the same reason the right one does.  ⚠⚠ **The instrument was wrong
twice before the tree was**, which is `@X328` reproduced a third time.

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

### What was built, and the four things the phase decided

✅ **COMPLETE 2026-08-28** — `src/errand.loft` (`Role.shift`,
`GATHER_SHIFT_UNITS`, `errand_home_done`, the mover's finishing stop,
the row refusal, and `errand_fields` rebuilt), `src/spawn.loft`
(`errand_depart` moved to the top of the tick),
`tests/30_r4_home.loft` (6), `tests/30_rc_the_conformance.loft`
(strengthened), `@X338`, `@M076`, `@D008`.

⚠⚠ **1. HOME IS A LEG OF THE ROUND, AND THE INVARIANT IS WHAT SAID SO.**
The obvious reading of `@FR-E-Home-Is-A-Place` — *when its round is over
it walks home* — is refused by § THE INVARIANT: a mob is on its cycle,
its lateness is in `slip`, or it left through the bubble.  **Three
states, ONE exit.**  A mob breaking off to walk somewhere the round
never goes is a fourth state and a second exit, and `cycle_at` could not
answer for it at all.

> ⚠⚠ **So home is not somewhere a finished mob GOES — it is a hex the
> round already passes through, and the mob leaves the roster the tick
> its own cycle brings it there.**

⚠ The mover, the cycle and § Rc are untouched by that, which is the
whole benefit.  ⚠⚠ **The bill is real and it is NAMED**: `haul` runs
`work ↔ alt` and `guard` runs its post, so **neither round touches home
and neither could end at R4** — and `errand_row_fault` refused a shift
on a row like that, so the column could not promise behaviour the cycle
had not got (`@X112` from the other end).  ⚠⚠ **R4b is what changed
that** (`@X341`): a round whose home is off it grows a TERMINAL leg, so
the refusal moved to the CYCLE and `harvest` is the row that ends.

⚠⚠ **2. WHAT ENDS A ROUND IS A SPAN, NOT A COUNT OF ROUNDS.**  A count
needs to ask the CYCLE how long a round is — two flow sweeps per mob per
tick — while `now − slip` is already the only argument the rest of the
file reads.  ⚠ And it cannot reproduce `@M073`: `@FR-E-Bag-Steers`
forbids a clock from choosing an ANCHOR, and a shift chooses nothing
about direction, so a shift shorter than a round is simply *one round*
rather than an oscillation.

⚠⚠ **3. THE DEPARTURE MOVED TO THE TOP OF THE NEXT TICK, AND THAT IS
THE PHASE'S CLAIM RATHER THAN A TIDY-UP.**  Taken at the consequence
stage, a robot walking to its nest **arrives and is removed inside one
tick**, so the last frame that ever holds it has it one hex short — and
a scout two.  That is *deleted where it happened to stop* wearing the
new rule's clothes.

> ⚠⚠ **What the player cannot see the GATE cannot see either** — the
> departure hex is unobservable from outside a tick that both moves and
> removes, and the first version of `tests/30_r4_home.loft` read
> `(41,4)` for a nest at `(40,4)` and could not say why.

⚠ Measured inert: `validate.sh` **50 scripts / 920 measurements**,
unchanged, so the one-tick-later ending for an AMBIENT robot moved
nothing in the corpus.

⚠⚠ **4. A MOVER THAT RELEASES MORE THAN ONE HEX A TICK STEPS OVER ITS
OWN ENDING**, so the stop belongs to `errand_step` — the ONE DOOR — and
the remaining hexes are DROPPED rather than slipped, because the mob has
arrived rather than been held up.  ⚠ It is `@M014`'s class: the shipped
robot releases exactly one hex a tick and cannot see it.

### ⚠⚠ And the SCOUT found `@D008` on the way in

⚠⚠ **`errand_fields` built one field per DESTINATION where it needed one
per ANCHOR.**  A mob that reaches an anchor with hexes left in its bank
turns and spends them on the next leg (`@X335`) — and the vector held no
field for that leg, so the mover could not place the step and
`errand_step` **charged the hexes to `slip`**.

⚠ It is silent **twice**:

- the shipped robot releases exactly one hex a tick, so it never turns
  with hexes left, and every carrier in the R1-R3 corpus was one;
- ⚠⚠ **§ Rc AGREES WITH IT.**  The rule is read at `now − slip`, so
  charging the lost hex moves the rule down onto the body and
  `cycle_at(e, now) == (e.q, e.r)` stays exactly true.

> ⚠⚠ ***`slip` is a currency that can pay for a defect.***  The reading
> is a scout whose phase had drifted **twelve hexes** from its own rule
> over three minutes with every conformance count green — `@X337`'s
> *conformance is an equality between two things that can stop together*
> reached from a second direction, and the fix to the gate is the same
> one: **a LIVENESS member that can actually turn mid-tick.**

⚠ `tests/30_rc_the_conformance.loft` gained a **scout carrier** in
*nothing blocked it so nothing slipped*, and pins the field count at
**2 per routine**.

### ⚠⚠ Eight mutations, and TWO of them chose the fixture

⚠ Both files were green on their first run, which `CLAUDE.md` says to
distrust.

| mutation | caught by the first version? |
|---|---|
| `errand_home_done` always false | ✅ 5 of 6 |
| the departure back at the consequence stage | ✅ 3 of 6 — it reads the hex `(41,4)` |
| it leaves wherever it happens to be | ✅ 3 of 6 |
| a shift of 0 reads as *no shift* rather than *never* | ✅ the one-column control |
| a shift the round cannot reach is accepted | ✅ |
| **the mover walks through its own ending** | ❌ **no** — at a 13-hex leg |
| **the run's clock instead of the mob's** | ❌ **no** — at a 13-hex leg |
| `@D008` restored (one field per destination) | ✅ once the leg was 8 |

⚠⚠ **The two that survived were both fixture accidents, and sweeping the
LEG LENGTH is what fixed them.**  A scout that walks through its nest is
invisible whenever it also happens to land on it exactly on some later
round; *now* and *now − slip* pick the same round unless a held-up mob
has an arrival in the window between them.  ⚠ At 8 hexes neither is
true, and the two answers are a whole round and 16 ticks apart.  ⚠ That
is `@M075`'s own finding repeated: **a gate aimed at a hazard somebody
NAMED can still miss it, because the fixture may make the wrong version
behave identically.**

## R4b — the TERMINAL leg: a round that ends somewhere it does not pass

⚠⚠ **R4 shipped an ending that only a NEST-shaped round can use, and the
world's commonest robot is not nest-shaped** (`@X339`, owner
2026-08-29).  A plant-material harvester has **three** places — the
picking ground, the **first-pass processing point** it dumps at every
round, and a **repair point** it visits once in a while — so `home` is
off its round, and `@X338`'s *home is a leg of the round* does not reach
it.

⚠ And it is not a corner case: [`docs/SETTING.md`](../../docs/SETTING.md)
§ There is no fossil carbon makes carbon one of the planet's **two
bottlenecks** (`@X340`), so the harvester round is the most common
errand there is.

**What it builds** — a round that repeats and then ENDS:

```
  alt → work → alt → work → … → alt → home        (and off the roster)
  └──────── the period ────────┘   └ the terminal leg ┘
```

⚠⚠ **It stays CLOSED-FORM, which is the whole constraint** — the turn
point is arithmetic, not a search: with `S = cycle_walked(rate, shift)`
the mob turns for home at `T = ceil(S / period) × period`, so
`cycle_phase` is *one modulo below `T`, one subtraction above it*, and
`@FR-E-Closed-Form` is untouched.  ⚠ The body agrees by construction:
`errand_arrive` empties the bag at `alt`, so *the first empty leg after
the shift* is exactly the same moment.

### ⚠⚠ PROBE — the turn point must be compared in HEXES, not in TIME  `@M077`

⚠ Answered before a line of loft, because it decides the shape rather
than the code.  **R4 shipped `errand_home_done` comparing
`now − slip >= row.shift` — a boundary in TIME — and the closed form
cannot use one.**

⚠⚠ Swept over **192 cases** (3 rates × 4 periods × 4 shifts × 4 step
lengths, one of them 137 113 units, which divides neither the tick nor a
hex): the stepped body turning on the TIME comparison disagrees with
`ceil(S / period) × period` in **12**; turning on the HEX comparison
`walked(now − slip) >= walked(shift)` disagrees in **0**.

⚠ **And the failure is a WHOLE ROUND, not a rounding error.**  At a
shift of 361 111 111 units the arrival at hex 180 happens a hair BEFORE
the shift, so the time test says *not yet* and the mob walks another
**30 hexes** — body 210 against a rule of 180.  A closed form that said
180 while the body walked to 210 is `@X335`'s leg boundary again with a
new subject.

⚠⚠ **It is `@FR-E-Bag-Steers` a THIRD time, and from a third direction**:
*a boundary in DISTANCE is exact at any timestep and one in TIME lands
wherever the arrival happens to fall.*  `@M073` reached it from the
route's length and `@M074` from the timestep; this reaches it from the
ENDING.

⚠⚠ **AND THE SHIPPED SHIFT CANNOT SEE IT** (`@M014`'s class, a fourth
instance): 120 s at 1.5, 2.5 and 0.5 hex/s is **180, 300 and 60 WHOLE
hexes**, so the boundary lands exactly on an arrival at every shipped
speed and both forms agree for any implementation.  ⚠ The sweep's
361 111 111 is the neighbour that can see it, and R4b's gate must keep
one — never rely on the shipped value.

**So R4b owes three things this probe has already decided:**

1. the turn point is compared in **hexes**, converted once with
   `cycle_walked(rate, shift)`;
2. `cycle_fault` **refuses a shift that is not a whole number of hexes**
   at the mover's rate — the same refusal, in the same function, as the
   clock period that is not a whole number of ticks (`@M074`);
3. ⚠ `errand_home_done`'s comparison moves to hexes with it, so the body
   and the rule read **one** currency — which is what makes them agree
   by construction rather than by being kept in step.

**Gate** — a played scenario in which a harvester ends at its repair
point, plus:
- ⚠⚠ **R2's sweep extended to a cycle with a terminal leg** — the
  stepped body against the closed form over the cross-product, because
  the turn point is new arithmetic and `@M074` is what a leg boundary
  costs when it is not exact;
- ⚠ the two-sided bound of `@M076`, restated for the terminal leg: the
  mob leaves at most one whole round plus one terminal leg after the
  shift;
- ⚠⚠ **and the control that separates it from R4**: the same anchors
  with `alt` ON the home hex is a nest round, which must end the way R4
  already ends it — one fixture, two shapes, one column apart.

⚠ **`errand_row_fault` held the gap shut in the meantime**: a shift on a
round that cannot reach home was refused at construction, so the missing
ending was NAMED rather than discovered as a robot that works for ever.

### What was built, and the three things the phase decided

✅ **COMPLETE 2026-08-29** — `src/errand.loft`
(`ERRAND_BAG_HOMEWARD`, `ROLE_HARVEST` + `HARVEST_SHIFT_UNITS`,
`errand_terminal`, `errand_shift_over`, `cycle_turn`, `Cycle.terminal`
and the terminal branches of `cycle_build` / `cycle_phase` / `cycle_at`
/ `cycle_fault`, the mover's hex COUNT, and `errand_fields`' third
anchor), `tests/30_r4b_the_terminal_leg.loft` (6),
`tests/30_r4_home.loft` § `@DRY-177` restated, `@X341`, `@M078`.

⚠⚠ **1. THE BODY REACHES `T` WITHOUT KNOWING WHAT `T` IS, AND THAT IS
THE PHASE.**  `cycle_turn` is arithmetic — `ceil(S / period) × period`
— but the MOVER has no cycle: nothing in the tick knows `period`, and
building one to find out is two flow sweeps per mob per tick, which is
exactly the cost `@FR-E-Closed-Form` exists to avoid.

> ⚠⚠ **What the body CAN see is the moment its bag empties at the
> drop-off, and *the first empty leg after the shift* is exactly `T`.**

⚠ So the turn is a **third value of the bag**, latched at an ARRIVAL and
never at a moment — which is also why the comparison had to move to
hexes (`@M077`): an arrival is a distance, and a boundary in TIME lands
wherever the timestep puts it.  ⚠ A separate *am I finished* field would
be a clock in all but name and would give `errand_leg` two things to
read; **the bag is where an ending belongs because the bag steers.**

⚠⚠ **2. THE REFUSAL MOVED FROM THE ROW TO THE CYCLE, AND THAT IS A
WEAKENING WORTH NAMING.**  `errand_row_fault` can no longer ask for
`home` on the round — that is the harvester's whole shape — so what it
refuses now is only a CLOCK-steered row with a shift, whose phase is a
time where a turn point is a distance.  ⚠ Reachability became
`cycle_build`'s question: a walk home that does not connect refuses the
WHOLE cycle, exactly as a working leg that does not already did, and
`cycle_fault` names it.  ⚠ A row can therefore promise an ending its
GEOMETRY cannot keep, and only building the cycle finds out — which is
the honest place for it, because a row does not know where its anchors
are.

⚠⚠ **3. `harvest` IS A NEW CATALOGUE ROW BESIDE `haul`, NOT A CHANGE TO
IT.**  Same two working legs, same walker; the difference is a column.
⚠ `@X329` asks whether two sites enforcing one rule may stay apart, and
the answer here is that they are not two sites — they are **one
mechanism and two rows of DATA**, which is what a catalogue is for
(`@X322`).  ⚠ It also keeps R4's *one column away* control intact: the
role that never ends is still in the table, still shiftless, and still
walks the same two hexes.

### ⚠⚠ Eight mutations, and the fixture's LEG is what caught the mover

⚠ The file was green on its first run, which `CLAUDE.md` says to
distrust.

| mutation | caught? |
|---|---|
| the turn compared in TIME instead of hexes (`@M077`'s own hazard) | ✅ |
| `cycle_turn` flooring instead of ceiling | ✅ |
| `cycle_at` indexing the terminal path one off | ✅ |
| the arrival asked at `walked` instead of `walked + 1` | ✅ |
| `errand_fields` omitting the third anchor (`@D008`'s shape, new leg) | ✅ (2 of 6) |
| `cycle_phase` losing its clamp at the terminal leg's end | ✅ |
| **the mover reading only the tick's OPENING hex count** | ✅ |
| `errand_arrive` latching without asking whether the round is terminal | ❌ |

⚠⚠ **The leg is NINE and the number is chosen.**  A round of 18 makes
the shipped 180 s shift **exactly 15 rounds at 1.5 hex/s and exactly 25
at 2.5**, so the turn lands ON an arrival rather than a whole round away
from one — and the scout's turning hex is then the SECOND of its tick.
A count read only at the tick's start is one hex short there, answers
*not yet*, and the mob takes another whole round, which the two-sided
bound refuses.  ⚠ At most leg lengths it does not, and a REGULAR robot
releases one hex a tick and cannot see it at any of them.  That is
`@M014`'s class a fifth time and `@M076`'s method — **sweep the
fixture's one free number** — reused rather than rediscovered.

⚠⚠ **AND THE SURVIVOR IS A REDUNDANCY WITH A NAME, not a gap.**  A
NEST-shaped round's only latching arrival is at home, where
`errand_home_done` removes the mob in the same tick — so latching there
changes nothing, across all six plan-30 files.  The guard stays because
`errand_terminal` is the ONE place the round's shape is decided and four
sites read it; claiming a gate for it would be `@M025`'s *the right code
with the wrong justification*.

⚠⚠ **The state the corpus had not got, and it is what makes the latch
load-bearing**: a harvester standing on its DEPOT in a session whose
clock is already past the shift — which is what a cropped `.keys`
fixture is (`@X335`) — is the one state in which *standing on home* and
*finished* come apart.  R4 answered it for the nest with *the mob is on
the home anchor*, and that answer does not carry: a nest round passes
home every trip, so the position discriminates there.  A harvester's
depot is off the round, so a mob standing on it is either finished or
freshly landed, and **only the latch knows which**.

## R5 — the POI, its population, its BOUND

✅ **COMPLETE 2026-08-29** — `src/poi.loft`, `tests/30_r5_the_bound.loft`
(8).  ⚠ Gates: **1745 green over 139 files** (+8, all this phase's),
`validate.sh` **50 scripts / 920 measurements** and `validate_gl.sh`
**3 fixtures / 55 measurements**, both **UNCHANGED** — the inertness
measured rather than asserted.

A `Poi { kind, q, r, state, since }`, a population attached to it, and
every attached mob's anchors derived from the POI.  ⚠⚠ **The bound was
the phase** — a static region, computed once, containing everything the
population can reach.

⚠⚠ **AND IT IS THE CYCLE'S REACH *DILATED BY THE DEVIATION*, WHICH THE
FIRST VERSION OF THIS PLAN GOT WRONG.**  A sidestep puts a body on a hex
the cycle never visits, so a bound computed from `cycle_at` alone **does
not contain the thing it is supposed to bound**.  ⚠ R0 probe 3 answered
that the deviation IS bounded — `flow_sidesteps` admits only
equal-distance neighbours, so `@FR-E-Non-Increasing` holds — and R5
inherited the answer rather than re-deriving it.

### What was built, and the four things the phase decided

**1. ⚠⚠ THE CLAIM IS PER-LEG, AND THE UNION IS ONLY THE QUERY**
(`@X342`).  ⚠ This is the phase, and it was found by mutation rather
than by design.  `@FR-E-Poi-Owns` states the bound as *the union over
legs of the disc centred on each anchor with the incoming leg's length
as its radius*, and the containment gate was written to match that
sentence.  ⚠⚠ **Against the union, four of nine mutations survived and
they were the load-bearing four** (`@M080`): a radius **one hex short**,
a **terminal leg given no disc at all**, a **rim excluded by an
off-by-one**, and **a sidestep that could increase the distance** —
`@FR-E-Non-Increasing` broken outright, the one rule the whole bound
rests on.  ⚠ The reason is geometry and not a weak fixture: consecutive
discs are centred a leg apart with that leg's length as their radius, so
**a hex outside its own leg's disc sits inside its neighbour's**.

> ⚠⚠ **A UNION IS SLACK, AND ASKING IT IS NOT ASKING ANYTHING.**  What
> `@FR-E-Non-Increasing` and `@X336` prove is about ONE LEG AT A TIME.

⚠ So there are two doors and the difference between them is nameable
(`@FR-F-Nameable-Difference`): **`bound_disc_holds(b, leg, h)` is the
CLAIM** and a gate must ask it; **`bound_holds` / `bound_meets` is the
QUERY**, where slack is exactly what is wanted, because over-answering
costs work and under-answering is a defect.  ⚠ `bound_holds` is
`bound_disc_holds` over every disc rather than a second copy of the
comparison.  ⚠⚠ **Pointing the same two gates at the leg took it to 9 of
9**, and the sidestep mutation is the reading that matters: **13 of 120
body-ticks leave the leg's disc where none leave the union.**

**2. ⚠⚠ THE POPULATION IS A SET OF ROUTES AND NEVER A LIST OF BODIES.**
⚠ `@X301`'s *the POI IS the bound, and culling one culls its whole
population — one query instead of `N`* is only true if the bound cannot
see a body, so `poi_bound` ranges over `PoiRoute`s and a `count` is a
column it never reads.  ⚠ **Forty haulers on one route have the bound of
one**, which is asserted rather than described — and the mutation that
unions once per body is caught by it.

**3. ⚠ THE BOUND IS READ IN LATTICE DISTANCE, AND THE PROBE PRICED THE
CHOICE** (`@M079`).  ⚠ The region is proved in FIELD distance;
`bound_holds` asks `lat_distance`, which is at most any path length and
therefore makes the lattice disc a **superset** — the safe direction,
because a query that answers *maybe* costs work where one that answered
*no* would be a defect.  ⚠⚠ **Across the three authored maps the
superset admits ONE hex the tight region excludes** (1466 against 1465
of 1467), which is what buys a bound of `2 × legs` integers, readable
with no world at all.

**4. ⚠ STATE BELONGS TO THE PLACE, AND THE BOUND DOES NOT MOVE WITH IT.**
⚠ `poi_state_set` is the ONE door and writes `since` with the state,
because the closed form becomes **piecewise** at a state change and a
segment with no `t0` is `@X335`'s leg boundary with a new subject.  ⚠⚠
The payoff is asserted: the bound folds to the same value under all five
states, because every cycle a POI can issue is anchored on that POI.
⚠ And the assertion that `poi_state_set` actually took comes first, or
five identical folds would agree for the wrong reason.

### ⚠⚠ AND THE PROBE CHOSE THE FIXTURE  `@M079`

⚠ The probe was written to ask *is the bound worth having* and answered
something sharper: **on an authored map it covers 1466 of 1467 standable
hexes.**  A round that crosses its patch has legs as long as the patch,
so the disc round each anchor covers the patch.

> ⚠⚠ **The bound is not a filter for the POIs a base lands among** —
> those are exactly the ones that must be materialised.  What it excludes
> is the world OFF the patch, which dryopea has not got yet (`@X298`,
> `@X299`).

⚠ It is `@M072`'s second reading with a new subject — *the authored maps
are smaller than the thing being measured* — and the probe asserts the
FINDING rather than the counts, so a world that finally outgrows a round
turns it red, which is when the bound starts paying.

⚠⚠ **And it is why the fixture is a world much wider than its round.**  A
containment gate over a map this size would be satisfied by a region that
excluded one hex of 1467, so `r5_outside` counts what the bound LEAVES
OUT and that count is asserted — at a third of the fixture, not *more
than none* — **before** anything is asserted about what it holds.

### What R5 did NOT build

⚠ **No `.keys` vocabulary and no `WaveState` field.**  Nothing in the
corpus authors a routine at all yet — `compare.loft`'s `errand_same` row
is still the tripwire it was laid as — so a POI list on the wave state
would be a field nothing could write.  ⚠ R7's scenario pair is what
needs the verbs, and it is where the writer and the reader are added as a
PAIR (`@D007`).

⚠ **Nothing reads a POI's `state` yet.**  The five states are the
catalogue's rows and `poi_state_set` is the door; *what a mob does on
arrival is a leg selected by the POI's state* is R5a/R6's work, and the
one claim R5 owed about state — that the bound does not move under it —
is gated.

## R6a — the three tiers, and what an un-materialised mob IS

✅ **COMPLETE 2026-08-29** — `src/poi.loft` § WHAT AN UN-MATERIALISED MOB
IS + § THE THREE TIERS + § MATERIALISE, AND RELEASE,
`tests/30_r6a_the_tiers.loft` (8).

⚠⚠ **R6 WAS SPLIT, AND THE SPLIT IS WHERE THE TICK BEGINS.**  The phase
as written needs a `WaveState` field, a materialiser inside `wave_tick`
and three round-trip sites — a bigger, more invasive change than any
phase of this plan so far, and **the first that is not inert by
construction**.  ⚠ So R6a is the mechanism and its claim, gated as
functions the way R1-R5 were; R6b is the wiring and the `R` vs `2R` pair.
Same move R4b was added by, and for the same reason: *each row is one
thing, lands green, and does not require the next.*

### What was built, and the three things the phase decided

**1. ⚠⚠ AN UN-MATERIALISED MOB IS A `slip` AND NOTHING ELSE** (`@X343`).
⚠ The question R6a had to answer is what a mob nobody can see must
REMEMBER, and the answer is **one field**:

| | where it comes from |
|---|---|
| which round it walks | an index, fixed for the sortie |
| where on that round it started | its **SEAT**, and the offset is DERIVED — a rota of forty needs no forty authored phases |
| which hex it is on | `cycle_at`, at its own clock |
| what is in its bag | ⚠⚠ **derived from the leg** — `errand_bag_for`, `errand_leg`'s own map read backwards |
| what it carries toward its next hex | ⚠⚠ **derived too** — `cycle_carry` |
| how late it is | ⚠ **`slip`, and this is the only thing that accumulates** |

⚠ Only a BODY can be pushed, so a mob nobody has ever looked at needs no
memory whatever — and one that HAS had a body must keep its lateness, or
`@FR-E-Slip` is refunded exactly where nobody can watch it happen.

**2. ⚠⚠ THE BANK IS THE ONE NOBODY WOULD THINK OF** (`@M081`).  A fresh
body arrives with `bank_new()` while the rule is generally part-way
through a hex, so a materialised mob **releases its next hex late by
exactly `cycle_carry`** — and it looks perfect at the moment it appears,
drifting only from the tick after.  ⚠⚠ **At the shipped robot's 1.5 hex/s
a hex is exactly one tick, so the carry is ZERO on 24 of 24 ticks and the
whole defect is invisible** (`@M014`'s class, a fifth instance in this
plan).  A SCOUT is what can see it, which is why the fixture walks one —
and `test_r6a_only_a_fast_mover_can_see_the_carry` asserts that rather
than leaving it to the next reader to notice.

**3. ⚠ THE THREE TIERS ARE SHAPED BY WHERE THE QUESTION IS ASKED.**
CULL is **per POI** — one `bound_meets` for a whole population, which is
`@FR-E-Poi-Owns` finally cashed in — and EVALUATE and MATERIALISE are per
mob.  ⚠ `poi_survey`'s shape IS the architecture: the cull test is
outside the loop, and it is all a distant POI ever costs.  ⚠⚠ **And a
round ENDS on the mob's own clock, body or no body** — a population whose
far half worked for ever while its near half retired would break R6b's
equality outright, and nobody could see it, because the far half is
outside the window by definition.

### ⚠⚠ Ten mutations, and BOTH survivors were faults in the GATE  `@M081`

⚠⚠ **SURVIVOR 1 WAS A GATE COMPARING A FUNCTION WITH ITSELF.**  The round
cache indexed by POI instead of by route read GREEN, because the test
compared `poi_bound_from` against `poi_bound` — and `poi_bound` **is**
`poi_bound_from` over `poi_cycles`.

> ⚠⚠ **A wrapper is a golden of its own delegate.**

⚠ That is `plans/09`'s rule with a new subject — *a golden that was
rebaselined during the change under test has verified nothing* — and the
fix is the same one plan 09 used: an independent **ORACLE** in the test,
which builds its own rounds and never touches the cache.

⚠ **SURVIVOR 2 is a redundancy with a name**, R4b's shape again:
`errand_bag_for` asks `row.laden` before the terminal leg, and swapping
them changes nothing, because `errand_terminal` already refuses a row
whose `laden` is `ANCHOR_HOME` — the two conditions cannot both hold.
⚠ The comment claiming the order was load-bearing was corrected by the
sweep.

## R6b — the materialiser in the TICK, and the `R` vs `2R` pair

✅ **COMPLETE 2026-08-29** — `src/poi.loft` § THE REACH + § THE STEP,
`WaveState.pois`, `PlayState.reach`, `src/play.loft::play_one_tick`,
`compare.loft::pois_diff`, `emit.loft::crop_state`,
`tests/30_r6b_the_materialiser.loft` (8).

### What was built, and the three things the phase decided

**1. ⚠⚠ A POI'S REACH IS ONE INTEGER, AND THE CULL IS ONE SUBTRACTION**
(`@X344`).  ⚠ R5's bound is a union of discs and R6a asks it with
`bound_meets` — but **the bound needs the rounds, and the rounds are two
flow sweeps a route**, so a POI culled every tick for a whole sortie
would pay for the rounds that prove it can be.  ⚠⚠ So the bound is folded
ONCE to a single radius about the POI's own hex,
`max(lat_distance(poi, centre_i) + radius_i)`, and the tick's per-POI
question becomes `lat_distance(poi, player) <= reach + window` — no world
read, no round built.  ⚠ A superset again, and the same safe direction
`bound_holds` takes: **an UNDER-estimate is the one direction that is a
defect**, and `max(radius)` is exactly that.

**2. ⚠ THE CACHE IS INTEGERS, AND IT LIVES ON THE SESSION.**
`PlayState.reach` rather than `WaveState`, because it is **derived rather
than authored** — the same division `state_diff` already draws.  ⚠⚠ And
a `vector<Cycle>` or a `vector<Bound>` in a long-lived field is
[loft#974]'s shape; a probe of exactly that shape read correctly four
times of four, and **that gotcha's own warning is that a green reading is
not evidence**, so the cache carries no vectors-in-structs at all.
⚠ The materialiser runs in `play.loft` **before** `wave_tick`, for
`errand_depart`'s own reason — a body made now must be one this tick's
fields, occupancy and move order already know about — and OUTSIDE
`wave_tick` because a sortie-long cache threaded through it would be a
parameter thirty test call sites had to carry.

**3. ⚠⚠ A DEAD ROBOT IS NOT A BODY ITS POI OWNS.**  `wave_deaths` MARKS
rather than removes — a corpse stays on the roster and raises rubble,
because *bodies are terrain* — so a sweep that did not ask `alive` would
report a body for ever and the record would never learn the player had
killed it.  ⚠ That is what `live` is for: the roster's *no body here*
means both *we never made one* and *the one we made is dead*, and only
the record tells them apart.  **Getting it wrong is a RESURRECTION** —
a POI quietly making a second copy of a robot the player watched die.

### The pair, and what it actually claims

⚠⚠ **THE EQUALITY CLAIM IS NARROWER THAN `@X299` STATES IT.**
*Materialising at `R` and at `2R` gives identical positions* is false
whenever anything can deviate a body.  What survives:

> ⚠⚠ **Identical where nothing can deviate a body; and where something
> can, they differ by EXACTLY `slip`.**

⚠ Both halves are gated and **the differ half asserts that they DID
differ**, or the clean half is simply being proved twice.  ⚠⚠ **And the
boundary leaks, which the file states rather than assumes**: in the `2R`
run the extra bodies live in the band `R < d ≤ 2R`, and a mob at `d = R`
can be blocked by one at `d = R+1`, materialised only in the larger run.
So the two runs are not identical even inside `R` — the difference is a
one-hex collar, paid in `slip`.  ⚠ That is `@X336` one radius over, which
is why the crowded half asks the **DISTANCE** and never the hex.

### ⚠⚠ Eleven mutations, and all four survivors were things the GATE could not see  `@M082`

⚠⚠ **A SAVING IS NOT A BEHAVIOUR.**  Deleting the cull entirely changed
no position and made no extra body — a mob outside the window gets none
either way — so **the cull is work NOT DONE, and only a COUNT can see
it**.  `poi_step` grew a fourth answer, *mobs looked at*, and the far run
reads **0 against 160**.  ⚠ `@M029` from the other side: that one says
*do not reach for a stopwatch*; this one says *a count is the only
instrument there is*.

⚠⚠ **THE FIXTURE'S GEOMETRY HID THE FORMULA.**  `poi_reach` taking the
largest RADIUS is exactly right on a straight out-and-back, where the
furthest a mob gets from its place IS one leg — so no played fixture on
that shape can tell it from the truth.  ⚠ It is a defect on a
three-anchor round (`@X341`'s terminal leg) and on any bend, and the
claim is therefore asserted DIRECTLY, on two discs ten apart.

⚠ The other two: a **corpse still on the roster** (above), and **two
fields moving together** — `poi_state_set` writes `state` and `since` at
once, so a gate that changed both could not say which it was reading.

### What R6b did NOT build

⚠ **No `.keys` vocabulary.**  `compare.loft::pois_diff` and
`emit.loft::crop_state`'s carry are TRIPWIRES, laid before they can
fire — nothing writes a non-empty `PoiWorld` from a script, so a captured
scenario has no places to lose.  ⚠⚠ When it does fire the answer is a
verb and never a looser comparison: the writer and the reader are a PAIR
(`@D007`), and R7's scenario pair is what needs them.

## R7a — the places, said in a `.keys` file

⚠⚠ **R5 and R6b both closed on the same sentence** — *no `.keys`
vocabulary, and R7's scenario pair is what needs one* — so this is the
phase that pays that bill, and it is a phase rather than a preamble
because it found two defects and grew the corpus.

**What it built**

| verb | says |
|---|---|
| `poi <kind> <q> <r> [state] [since]` | a PLACE, named by its kind and its state and never numbered |
| `route <poi> <role> <class> <count> <aq> <ar> <bq> <br>` | a population — and it ENLISTS one |
| `mob <i> <slip> <working\|done> <rule\|body>` | what a played tick changed about ONE record |
| `routine <i> <role> <bag> <slip> <mob> <hq> <hr> <wq> <wr> <aq> <ar>` | what one BODY on the roster holds |

⚠⚠ **A ROUTE ENLISTS ITS OWN POPULATION**, and that is the shape worth
keeping: a file says *how many* and never *which ones*, because the mobs
a route stands for are `count` records on dense seats `0 … count-1` and
nothing else.  ⚠ It works only because a record is never REMOVED —
`poi_retire` sets `gone` and keeps the slot, exactly as `CARGO_GONE`
does — so the seats a reader rebuilds are the seats the writer had.

⚠⚠ **AND `routine` IS THE FIRST COMMAND IN THE VOCABULARY CARRYING THREE
HEXES.**  `KeysSchema` had two pair positions since plan 09; a converter
that rewrote two of a routine's three anchors would leave a fixture in
two lattices at once, which is the exact silent failure that table
exists to refuse.

### ⚠⚠ Both defects came from a TRIPWIRE, not from the sweep  `@M083`

⚠ Twelve mutations, twelve caught — and **a green mutation sweep is a
statement about the gate rather than about the tree**.  The three things
that were actually wrong were found by gates laid before they could
fire:

- ⚠⚠ **The FLAG is a second fact.**  `Enemy.errand` is *this robot is on
  an errand*; `route.role` is *which routine, if any*.  An AMBIENT robot
  (BACKLOG B4) carries the flag with `ROLE_NONE`, so an emitter keyed on
  the ROUTINE loses it on every traffic scenario in the corpus — and
  `wave_cutoff` clears exactly that flag, so a capture without it is a
  robot the bubble has already taken walking on.  `compare.loft` gained
  the row this phase and it went red on `a-road-that-passes-by` within
  one suite run.
- ⚠⚠ **`slip` is the one duration that may be NEGATIVE** (`@D009`).
  `poi_materialise` folds a mob's seat offset in as a negative lateness,
  and `fixstep`'s authoring door refuses a negative on purpose.  A
  captured sortie's whole ROTA came back stacked.  ⚠ And the half worth
  copying is not the fix but the MESSAGE: `compare.loft`'s first version
  named the role and the bag alone, so the failure read *"role 1 carry 0
  vs role 1 carry 0"* — a difference that refuses to say where it is.
- ⚠⚠ **And R1's own sweep caught the third**:
  `test_no_role_is_ever_compared_anywhere_in_src` named both new verbs
  refusing an unknown role with `== ROLE_NONE`, which is `@X333`
  exactly.  ⚠ The cause is that **`errand_role_named` cannot say no** —
  it answers the zero row for a word it does not know, and `none` is a
  legal word, so *a typo* and *the role with no round* come back one
  number.  `errand_role_known` is the answer (*the row I got back is
  called what I asked for*), and `poi_kind_known` beside it, because the
  kind table had the same hole.

### ⚠ The scenario, and what it measures

`tests/scripts/a-place-that-sends-robots.keys` — a working face 50 hexes
east shipping to a depot 16 back along a road that never enters the
bubble.  **Nothing in the file places a robot**: `poi_step` gives the two
mobs a body the moment the player is near enough.

⚠⚠ **And it is the first place in the corpus where `slip` is VISIBLE.**
Two mobs half a period apart on an out-and-back round meet HEAD-ON twice
a round; a mob that can only step BESIDE has not got closer, so the hex
is spent and recorded.  Sixty-one ticks at 1.5 hex/s is 61 hexes and the
closed form puts them at (37, 0) and (47, 0); they are at (42, −1) and
(43, −1), off the middle row and still on the road — `@FR-E-Non-Increasing`
bounding the deviation in SPACE while `@FR-E-Slip` pays for it in TIME.

⚠ **The first draft's road was ONE hex wide** and the two haulers simply
jammed — `CLAUDE.md` § Testing something that moves, measured again: a
corridor has no *beside*, so the fixture would have gated a queue.

### What R7a did NOT build

⚠ **The distraction.**  Nothing here draws a mob off its route; R7b is
that.  ⚠ **No `Role` column for what lures one** — the plan's own note at
`@X333` says *what draws a role off its route is R7's and is deliberately
not a column yet*, and it is still not one.

## R7b — distraction: the hauler and your heap

⚠⚠ **The failure mode that eats the feature**, measured by `crawler`:
without its *an incursion does not break formation for a hero it has
merely seen* rule, raiders parked on a hero for seven days and *"the
whole mechanism silently became 'monsters walk at the player', which the
game already had."*

⚠ So the rule, and the gate is built round it:

> ⚠⚠ **A distraction must be caused by something the player DID or
> BUILT, never by the player being seen.**  (`@FR-E-Built-Not-Seen`)

**The gate, as built** — a scenario pair plus the control:
- `a-heap-a-hauler-took.keys` — a hauler whose route passes a **salvage
  heap the player left** turns four hexes up a dead-end side track for
  it, takes it, and carries on.  The player drives out and finds
  nothing: **200.0**;
- `a-heap-a-gatherer-walked-past.keys` — the same everything, one table
  column apart, and the player collects: **230.0**;
- ⚠⚠ **the negative control, in the first file's own opening half**: the
  player parks one hex off the road, in plain sight, for **thirty
  ticks**, and the robot is on its rule's own hex to the tick
  (`enemy 0 36 0`, which the closed form gives for offset 30 of a 32-hex
  round).  That is `crawler`'s defect made assert-able.

### ⚠⚠ THE FINDING — a hex walked AWAY from your anchor costs TWO  `@X346`

⚠ The rule wants a mob drawn OFF its route; `@FR-E-Slip` wants a
deviation to cost TIME and never DESTINATION, and says *a body pushed
off its cycle re-converges on the same hex*.  ⚠⚠ **A detour therefore
has to be paid for both ways — and the return leg is indistinguishable
from ordinary progress.**  A mover charging one hex per hex spent going
out leaves the body permanently short of its own rule by the depth of
the detour, silently and for ever.

⚠ So the charge is read from the **anchor's** distance before and after:

| the step | costs | since |
|---|---|---|
| CLOSER | 0 | it is progress |
| EQUAL | 1 | the sidestep, unchanged since R3 |
| FURTHER | **2** | the hex it spent, and the hex it owes |

⚠⚠ With `i` steps out and `d` back the body stands at `D + i − d`, the
rule has advanced `(i + d) − 2i = d − i`, and `D − (d − i)` **is the same
number** — so the two agree in DISTANCE at every moment of the detour,
with nothing remembered.  ⚠ `@FR-E-Place-State` is what makes that the
required answer rather than a clever one: a mob may hold nothing beyond
`carry` and `slip`, so a debt COUNTER is not available and the debt has
to be paid at the moment it is incurred.

### ⚠⚠ And the BAG is not touched by a theft

⚠ The bag is the ROUND's state and `cycle_phase` reads the WALK rather
than the bag, so a pickup that flipped it would put the body on a
different leg from its own rule — permanently, silently.  ⚠⚠ **So the
stolen heap is a `CarryObject` and not a bag**, which is `@X334`'s
obligation coming due: *a bag holds material that was never on the map;
`carry.loft` conserves an object that IS on the map and the player could
have picked up instead.*

⚠ It is keyed on `BLOCKER_MOB + PoiMob index` — **an identity that
outlives a body**, where a roster slot does not: `errand_depart`,
`wave_deaths` and `poi_drop_body` all rebuild the roster.  ⚠ A body no
place owns therefore never steals, and that is nameable rather than a
restriction: it has no identity to hold cargo against.

⚠ **And killing the thief gives it back**, as the same stuff, on the hex
it fell on — which is the counter-play the theft is worth having.

### ⚠ What the bound paid

⚠⚠ **R7b is the first thing in the plan that can take a body FURTHER
from its anchor.**  Until now `@FR-E-Non-Increasing` did the whole job
and `disc(anchor, leg length)` held a deviating body by construction.  A
mob turning aside can be its lure's REACH outside that disc, so every
radius of a lured row grows by exactly that much — and a row with no
lure pays nothing, which is why the 932 measurements R7a left did not
move.

### What R7b did NOT build

- ⚠ **The other four rows of `docs/ERRANDS.md` § What each role comes
  for** — the gatherer's seam, the builder's nibble, the guard's post,
  the insect alarm.  Each is a lure ROW plus what it looks for; the
  hauler is *"the one worth building first"* by that document's own
  word, and the mechanism is now there for the rest.
- ⚠ **A crew REMARK when it happens.**  R7b gives a mob a reason the
  player can see; a crew member SAYING so is `@X142`'s channel.
- ⚠⚠ **A cheap early-out for the lure sweep.**  `errand_lure_at` walks
  `lat_disc(reach)` — 61 hexes at reach 4 — per lured mob per hex.  A
  mob with no lure column pays nothing and a laden one pays nothing, so
  the corpus is untouched; a base with forty haulers beside it is where
  this stops being free, and `plans/22`'s cache is the shape of the
  answer.

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

⚠ A scenario pair, and the reading this repo takes: `@M050`'s 130 / 174,
`@M059`'s 130 / 174 / 221, `@M070`'s 140 / 174.

⚠⚠ **It is also the design's own test** (`@X303`): *does this make
behaviour more BELIEVABLE, or does it only simulate MORE?*  A phase that
adds ticks of work and moves no clock has answered the second.

### ⚠⚠ The answer: 123 against 269  `@M085`

`a-base-on-a-robot-road.keys` and `a-base-beside-a-robot-road.keys` —
the same map, the same wall, the same waves, the same painted road, and
four coordinates that put the round inside the scrambler bubble or 36
hexes out of it.  ⚠ Four haulers nobody sent at the player lose their
link crossing it (`wave_cutoff`, one way) and join the siege on the same
front: **seven alive against three, and 123 ticks against 269.**

⚠⚠ **AND THE SWEEP IS THE FINDING, not the headline.**

| the wave | on the road | beside it | the road is worth |
|---|---|---|---|
| 2 | 123 | 319 | **196** |
| 3 | 123 | 269 | **146** |
| 5 | 118 | 128 | 10 |
| 8 | 118 | 118 | **0** |

⚠ The siege front is the WALL'S WIDTH (`@M020`) — about four hexes on a
five-row wall — so a wave of eight already saturates it and four more
bodies change **nothing**.  ⚠⚠ **The first version of this pair ran
`a-defended-base.keys`'s authored 5 + 8 and read 118 against 118 with
four extra besiegers plainly on the map.**  That is `CLAUDE.md` § *a gate
whose reading is already saturated cannot see what you built*, met by
pricing the supply against the capacity rather than by believing the
flat reading.

⚠ **It answers three questions at once**: `@X305`'s *a POI earns its
place only if REMOVING it moves the clock*, `@X317`'s *land in the
overlap* priced from the player's side, and `@X303`'s own test.

### ⚠⚠ And the pair found a THIRD independence

⚠ R7a found the FLAG and the ROLE to be two facts one way round — an
AMBIENT robot is `errand: true` with `ROLE_NONE`.  ⚠⚠ **R8's own
scenario produced the other way round**: `wave_cutoff` clears the flag
and **leaves the anchors where they are**, so a hauler the bubble took is
`ROLE_HAUL` with the flag CLEARED, and nothing reads its routine because
`errand_role` guards on the flag first.

⚠ `emit_enemies` keyed on the flag alone and dropped the whole routine;
`tests/18_s2`'s corpus sweep named it the first time the pair ran —
*role 1 … mob 1 vs role 0 … mob 0*.  ⚠ The `routine` verb gained an
`<on|cut>` token, because a fact the game reaches and the vocabulary
cannot say is `@D007`'s shape however it is spelled.

### What R8 did NOT build

⚠ **A gate for the saturation curve.**  The pair gates one column of the
table; the other three are a measurement, recorded here and in `@M085`
and not re-run.  A fixture per column would be four files saying one
thing.

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
   where to go.*  ⚠ `Job.kind` stays `TASK_ANY` until R7b reads one.
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
