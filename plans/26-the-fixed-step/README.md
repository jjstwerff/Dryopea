<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 26 — the fixed step: one clock, and the epsilons die

**Value:** `S` (silent failure) · **Effort:** `MH`

## Status

**Opened 2026-08-17.  L0 shipped 2026-08-17; L1 next.**  This plan came out
of a comparison of dryopea's tick pacing against moros's, run before it was
written; the readings are § What was measured first and three of them are
defects nothing in either repo can currently see.

⚠⚠ **L0 confirmed § 2's seventh row and made it WORSE than the plan
predicted** (`@M030`, `@D003`).  The plan expected the vehicle to cover
*2 / 1 / 0* hexes a tick at 667 / 333 / 100 ms; measured over one simulated
minute at seven tick lengths, it covers **180 / 120 / 180 / 0 / 0 / 0 / 0**
hexes against a true 180 — so the fall to zero starts at **200 ms**, not 100,
and 500 ms costs a third of its speed while still looking like a moving
player.  Every banked mover (miner 1.0, robot 1.5, scout 2.5, helper 2.5) is
exact at all seven, so the instrument reads both answers on one axis.
⚠ End to end the player **never leaves its hex** at 200 and 100 ms across a
whole minute of a 40-hex corridor, while a robot beside it arrives every time.

⚠⚠ **The transferable half is why nothing had seen it, and it is a NEW shape
of blind gate**: three accidents in a row — both shipped vehicle speeds exact
at the shipped tick, the half-tick `23_k2a` sweeps exact too, and the one
shortened timestep in the repo that *would* have caught it banking an ENEMY.
That last is `tests/11_f8`'s markerless-world trap with the axis and the
subject swapped — **the right sweep over a roster with none of the broken
thing in it** — and it is harder to spot, because the sweep looks thorough.

⚠ **No fix landed and that is deliberate**: the four vehicle functions in
`tests/26_l0_the_timestep_sweep.loft` assert today's WRONG numbers as the
defect's record and L2's tripwire, in `tests/23_k1`'s "not yet" sense.  A
phase that changed the thing it measured would have measured nothing
(`@X064`).

⚠ **The evidence is a defect, and the deliverable is the library.**
**dryopea has seven independent implementations of "do not lose a fraction",
one of them is wrong, and no test can see it** — `vehicle_hexes_this_tick`
truncates with no carry and is exact today only by arithmetic accident.  Nine
such implementations across dryopea and moros exist for one reason: there is
nothing to consume.  ⚠ Extraction is nonetheless the LAST phase — a contract
proved against a real consumer first, per § FLEXIBLE's own evidence that the
capabilities a consumer skips are the ones the next consumer rebuilds.

⚠⚠ **And the thing this plan is most likely to get wrong is the thing it was
asked for.**  *"Flexible for shooter, tactical and single-person RPG"* is the
cleanest claim in the document, which by `design-protocol` § step 4 makes it
the one to attack rather than the one to celebrate.  § The four games states
each fit as a **prediction with a named refutation**, and two of them are
predicted NOT to fit unchanged.

## Goal

Simulation time in dryopea is an exact integer count of a fixed step, every
rate is consumed by one banking primitive with no epsilon anywhere, and both
are a library moros consumes without changing what it draws.

## Anchors

Implements, and does not restate:

- `src/play.loft` § TWO ways to ask, ONE tick — the two doors and the
  602/1000 measurement (`tests/19_p1_the_seam.loft:97`).
- `src/spawn.loft` § What a tick is worth — `TICK_SECONDS`, `enemy_bank`,
  `ENEMY_PROGRESS_EPSILON`; `@X058`, `@X059`, `@X060`, `@X061`.
- `CLAUDE.md` § Timers and epsilons — `@M013` / `@M014` / `@M017`, the three
  measurements this plan intends to make unnecessary rather than restate.
- `src/helper.loft::helper_bank`, `src/vehicle.loft::vehicle_hexes_this_tick`,
  `src/tower.loft::tower_repair_tick`, `src/main.loft` § The wall clock.
- `../moros/lib/hex_editor/src/pose.loft` (`TICK_US`, `tick_dt`) and
  `src/tick.loft` (§ the invariant, § WHAT IS DELIBERATELY NOT HERE) — the
  **construction to follow**, and it is already right about the half dryopea
  has wrong.
- `../moros/src/editor_client.loft:1831` (the integer accumulator + backlog
  cap), `../moros/src/editor_server.loft:6048` (`sim_rate`),
  `../moros/src/editor_run.loft` (`step <n>` — a driver with no clock).
- [`plans/22`](../22-the-field-cache/README.md) — the shorter tick this
  unblocks, and whose § What this plan does NOT build already carries the
  *bank progress, never multiply it* rule.

## What was measured first

Read off both trees on 2026-08-17, before this document existed.  ⚠ Four of
the five changed its shape.

### ⚠⚠ 1. moros's accumulator is exact and dryopea's is not

Both spend a backlog one fixed step at a time, and both learned that the hard
way — moros's server integrated `steps × TICK_US` in a single pass and got
machine-dependent trajectories; dryopea's `@M013` is the same defect one unit
down.  The difference is the arithmetic underneath:

| | moros | dryopea |
|---|---|---|
| step | `TICK_US = 33000` — an integer, **chosen** | `TICK_SECONDS = 1.0 / 1.5` — a float, **derived from a speed** |
| accumulate | `while now - tick_at >= TICK_US { tick_at += TICK_US }` — integer µs | `banked += seconds; while banked >= TICK_SECONDS { banked -= TICK_SECONDS }` — float |
| count door | `step <n>`, no clock at all | `play_ticks(n)`, does not touch `banked` |
| `advance(n × step) == step(n)`? | **yes, by construction** | **no — wrong for 602 of the first 1000 `n`** |

⚠ dryopea's own clock source is already integer microseconds
(`src/main.loft:258` computes `tnow - last_frame_us`) and **divides the
exactness away at the boundary** before `play_advance` ever sees it.

⚠⚠ **The step being derived rather than chosen is not cosmetic.**
`1e6 / 1.5` is 666666.67 — so there is no integer µs step that IS today's
tick, and going exact means *choosing* the number.  `@X058` already released
the constraint that forced it: since plan 23 K2a `TICK_SECONDS` **holds** the
timestep rather than defining it.

### ⚠⚠ 2. Seven sites re-assert "do not lose a fraction", and one of them omits it

`design-protocol` § step 2 asks how many independent sites must re-state the
invariant, and whether omitting it is silent.  Counted:

| site | shape | epsilon |
|---|---|---|
| `play.loft::play_advance` | float bank, carries | none — deliberate, and argued (§ And there is no epsilon) |
| `spawn.loft::enemy_bank` | float bank, carries | `ENEMY_PROGRESS_EPSILON` |
| `helper.loft::helper_bank` | float bank, carries | `HELPER_PROGRESS_EPSILON` |
| `tower.loft::tower_repair_tick` | float, counts UP | its own |
| `helper.loft::helper_recover_tick` | float, counts DOWN | its own |
| `vehicle.loft` boost / cooldown | float seconds | — |
| **`vehicle.loft::vehicle_hexes_this_tick`** | **float, TRUNCATES, carries nothing** | **none, and none is correct for it** |

**N = 7, and omission is silent.**  That is the brittleness known before a
line of code, and the seventh row is the proof that it is real rather than
theoretical:

> `(VEHICLE_SPEED_HEX_PER_SECOND * tick_seconds) as integer?`
> — `src/vehicle.loft:303`, and `Vehicle` has no `progress` field.

Enemies and helpers keep their remainder.  The player throws his away.  It is
exact today **only** because `3.0 × 0.6667 = 2.0` to the bit — the same
accident that makes `ENEMY_PROGRESS_EPSILON` unfirable at 1.5 hex/s
(`@M014`).

⚠⚠ **At a 100 ms tick, `(3.0 × 0.1) as integer` is 0 — the player does not
move at all**, while enemies at 1.5 hex/s correctly bank a hex every seven
ticks.  `@M013` swept enemy SPEEDS and could not see this; nothing in the
corpus asks whether a mover is tick-length independent.  **This is a
dryopea-internal defect and it should be filed as `@D003` whether or not this
plan proceeds.**

### ⚠ 3. The backlog cap is a DRIVER policy, and both repos are right

moros's page caps catch-up (`LOCAL_TICK_MAX`) and **drops** time rather than
compressing it — *"a compressed catch-up is the variable-step bug wearing a
bound's clothes."*  Its **server has no cap at all**.  dryopea's
`play_advance` refuses one deliberately: 1200 frames of 1/60 s and one frame
of 20 s must reach the same state, which is what the 654 gate measurements
rest on.

⚠ So the two are not in conflict and neither is a default — **the cap belongs
to the driver, never to the tick.**  dryopea will need one in `main.loft` the
first time a real player alt-tabs, and it must not go anywhere near
`play_advance`.

### ⚠ 4. A loft `integer` is 64-bit and exact past 2⁵³

Probed directly, because the exact bank's accumulator needs ~1e12 and a
32-bit integer would have forced a different scaling:

```
1e6 * 666667 = 666667000000   (exact)
2^53+1       = 9007199254740993   (exact — not a float in disguise)
2^62         = 4611686018427387904
```

⚠ So `progress += rate_micro × step_us` compared against `1e12` is exact
arithmetic with **no epsilon at any speed**, which is the whole of § L2.

### ⚠ 5. dryopea has no interpolation alpha, and its camera ease is standing in for one

`@M023` measured the un-eased camera moving on **12 frames of 240** and
jumping a whole hex.  That is the signature of a 1.5 Hz simulation drawn at
60 fps with no alpha: the standard fix is `banked / step` handed to the
renderer, and dryopea reached for an exponential ease instead.

⚠ **The ease is not wrong and this plan does not remove it** — it is a
feel/lag decision as well as a smoothing one, and `@M023` refused moros's
linear form for a good reason.  What is open is whether alpha is a
*complement* (the vehicle's own position interpolated, the camera still
eased) or whether the ease was only ever a substitute.  § L5 decides it, and
it is the phase most likely to be cut.

## The invariant

> **Every DURATION in the game is an exact integer count of one chosen base
> unit, and every RATE is consumed by one integer accumulator that carries
> its remainder.  Floating point appears only where something is DRAWN.**

That is one sentence and it is falsifiable: a site that keeps a float
duration, or that drops a remainder, breaks it.

⚠⚠ **What it does not absorb is a claim about TYPES, never about the
package.**  § FLEXIBLE is the other half and the two are easy to confuse:
everything below stays in the library and gets tested there — what it must
not do is share a *mechanism* with something that is not the same arithmetic.
The over-unification guard, run in advance rather than after:

- **The tick BODY — and this one really is out of the package.** moros walks
  a continuous person, dryopea steps a lattice roster.  Two games, two
  bodies; moros's `tick.loft` header already says the sequencing is the
  shared half and the clock is not.
- **The ease is genuinely FLOAT.**  `1 − e^(−k·dt)` is a *presentation* rate
  over real frame time, frame-rate independent for a different reason.  It
  ships as `approach` (§ FLEXIBLE) because both consumers built one and moros
  built it wrong — but forcing it into integer steps would make the camera
  stutter at 1.5 Hz, so it is outside the invariant while inside the package.
- **One-shot timers are a different FAMILY, in the same package.**
  `helper_recover_tick`'s 90 ticks and `tower_repair_tick`'s 20 s fire once
  at a boundary; their remainder is meaningless afterwards, where a bank's is
  load-bearing for ever.  `play.loft` § And there is no epsilon already draws
  this line and it survives.  ⚠ They **do** become exact, because the
  *duration* goes integer — but a `Timer` is not a `Bank` and the two must
  not share a primitive.

⚠ **§ L2's negative control is exactly this claim.**  If a one-shot timer
CAN be expressed as a bank with nothing lost, the family boundary above is
wrong, `Timer` collapses into `Bank`, and the finding is recorded rather than
argued away.

## The four games — as predictions, not as features

⚠⚠ **Only dryopea and moros are real consumers.**  A shooter, a tactical
game and a single-person RPG are **axes to vary the design against**, not
customers, and `design-protocol` § The residual is blunt about the
difference: the axis invisible at design time survives any discipline.  So
each is written as a prediction with the reading that would refute it.

| game | what it needs | prediction | refuted if |
|---|---|---|---|
| **dryopea** (TD/strategy) | 1.5 Hz, lattice movers, uncapped catch-up, a 654-measurement gate pinned to exact tick counts | fits; the count door is the gate's door | any of the 654 moves in L1 or L2 |
| **moros** (first-person builder) | 30 Hz, continuous positions, capped page + uncapped server + clockless script runner | fits; it is where the construction came from | its world digest changes for any script |
| **tactical** (turn / grid) | **no wall clock at all** — the sim advances only when a turn is committed | fits, and is the strongest case: the COUNT door is its *primary* door, not a test affordance | the library cannot be used without a clock source.  ⚠ moros's `editor_run` is the existing proof it can |
| **shooter** (60–128 Hz, rollback) | re-run N ticks from a snapshot and get bit-identical results | fits **only with the integer clock** — and this case *falsifies dryopea today*, because `advance(n × step) ≠ step(n)` | integer `advance` and `step` still diverge after L1 |
| **single-person RPG** | 30–60 Hz, long timers, save/load mid-tick | ⚠ **predicted NOT to fit unchanged** — a save at a fractional tick must persist `banked`, and nothing here says what a saved remainder means across a version change | L4 cannot state a serialisation rule for `banked` in one sentence |

⚠ **A second predicted non-fit, recorded now so it is not discovered as a
surprise:** a game needing **two rates at once** (unit physics at 60 Hz over
a strategy layer at 1 Hz) needs clocks to COMPOSE — a slow clock driven by a
fast clock's ticks.  dryopea already has the shape twice (the camera steps at
frame rate while the sim steps at 1.5 Hz; `plans/22` § LOD contemplates
ticking distant things every N ticks), so this is not invented.  **L4 gates
it or the plan records that it does not compose.**

## FLEXIBLE, which is why it is not minimal

⚠⚠ **The design rule, and it is the plan's second load-bearing decision after
the invariant** (project owner, 2026-08-17):

> **A game must never have to write timing arithmetic.**  A slightly larger,
> well-tested system beats a small one that every project then rebuilds on
> top of.

⚠ **That is not a preference, it is what this repo already measured.**  § 2
counted **seven** hand-rolled remainder sites in dryopea and found one of them
wrong.  moros independently built **two** different backlog policies and a
rate scaler.  Nine implementations of the same arithmetic across two
projects — and every one of them exists because there was nothing to consume.
A library that shipped only `clock_advance` would have prevented none of
them: the vehicle's missing bank, moros's page cap and `sim_rate` are all
*exactly* the things a minimal surface pushes back onto the consumer.

**So the admission test is the opposite of a size budget:** *could a consumer
be tempted to hand-roll this?*  If yes, it belongs in — built once, tested
once, and never argued about again.

### What that admits

| | why a consumer would otherwise build it |
|---|---|
| `TickClock` — integer step, both doors, `alpha` | all five games in § The four games |
| **backlog policy** — uncapped, or capped-and-dropped | moros's page built one; dryopea will need one the first time a player alt-tabs |
| **rate scaling** — pause, slow-mo, fast-forward, free-run | moros's `sim_rate`; every game with a speed control |
| **clock composition** — a slow clock driven by a fast one | dryopea has the shape twice already (camera vs sim; `plans/22` § LOD) |
| `Bank` — a rate consumed in whole units | dryopea built it **three** times and forgot the fourth |
| `Timer` — one-shot, counting UP *and* DOWN, both exact | dryopea built it three times, each with its own epsilon (`CLAUDE.md` § Timers and epsilons: *neither direction is safe*) |
| `approach` — the frame-rate-independent ease and its short-way angle | dryopea built it; moros built the **linear** version, which `@M023` refused |

⚠⚠ **`Timer` is in the package and is NOT a `Bank`**, and holding both facts
at once is the point.  § The invariant keeps them apart as *arithmetic* — a
one-shot's remainder dies at its boundary, a bank's is load-bearing for
ever — and that is exactly why a consumer picking wrong is silent.  Two
small exact types in one package is the answer to both problems; **one clever
type that "handles both" is the over-unification this plan is written to
avoid.**

### A DOOR PER USE CASE — so flexibility costs nothing at the call site

⚠⚠ **The second half of the owner's rule** (2026-08-17): *flexibility must
not impede ease of use — give each case its own usage function, so each one
is easy on its own.*  The surface grows in **doors**, never in parameters:
one function per way a game drives a clock, each with the arguments that case
actually has and no others.

| the case | the door | the whole call site |
|---|---|---|
| a frame loop (GL, browser) | `clock_advance(clk, elapsed_us)` | `for _ in 0..clock_advance(clk, dt_us) { tick(); }` |
| a script / test / turn commit | `clock_step(clk, n)` | `clock_step(clk, 30)` |
| a server pump with a speed control | `clock_pump(clk, now_us)` | owns `sim_rate` and the wall entirely |
| a background-safe frame loop | `clock_advance_capped(clk, dt_us, max)` | drops the excess, never compresses it |
| a rollback re-simulation | `clock_step` from a restored clock | exact by L1's gate |

⚠ **Nothing here is a mode flag on `clock_advance`.**  dryopea already paid
for that lesson at one level up — a count and a duration look like one
question with a switch, and `play.loft` measured that they are not (602 of
the first 1000 `n`).  The two-door split is the precedent this generalises,
and it worked.

⚠ **The doors are THIN and share one accumulator.**  Five entry points over
five copies of the arithmetic is the seven-sites problem wearing a library's
clothes; five entry points over ONE is the point.  L1's gate asserts they
agree tick-for-tick.

### ⚠⚠ The TEST is the example, and the doc links to it

**Every door in that table gets a test function named for its use case, and
the documentation links to that function rather than quoting a snippet.**

That is the owner's second requirement and it is the one that keeps the first
one true over time:

- a snippet in prose **rots silently** — nothing compiles it, so it goes
  stale at the first signature change and the reader finds out;
- a test that nobody links to **is not documentation** — it is correct and
  invisible, which is how a consumer ends up hand-rolling the thing the
  library already does.

⚠ So the pair is the deliverable: the test proves the door works, the link
makes it the example, and the example cannot drift from working code because
it *is* working code.  ⚠ Gated at L6 — every row above has a named test, and
the docs reference each by name.  A door with no linked test is not shipped.

⚠⚠ **The mechanism for this is BUILT and is not this plan's to design** —
[`docs/EXAMPLES.md`](../../docs/EXAMPLES.md) is the convention and
`scripts/examples.sh` is the gate (landed 2026-08-17, eight self-test
controls, wired into `scripts/test.sh`).  A test carries an index TAG in a
comment above it and the function cites it on an `// Example:` line; dangling,
duplicate and orphan citations all fail.  **This plan's library takes the
acronym `FIX` and is its first consumer** — so L6 opts its files in with
`// #examples` and the coverage half of the gate does the rest.

⚠ **And this is already how this repo teaches its own seams** — `.keys`
scenarios in `tests/scripts/` are simultaneously the gate and the worked
example of the editor's door, and `plans/08` is the plan that made them so.

### The two rules under all of it

1. **No callbacks and no inversion of control.**  The library never calls
   your tick; it answers *how many*.  That is what lets one clock serve a GL
   loop, a server pump, a script runner and a test — moros's
   `tick.loft` § WHAT IS DELIBERATELY NOT HERE is the same rule, and its
   three drivers are the evidence it works.  ⚠ It is also what makes the
   surface affordable to grow: a new door is a function a caller may ignore,
   never a shape every caller must adopt.
2. **Defaults mean *the behaviour you already had*.**  `clock_new(step_us)`
   takes one argument; a clock nobody configures is uncapped, unscaled and
   behaves exactly as dryopea's does today.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **L0** ✅ | a scenario run at **667 / 333 / 100 ms** ticks covers the same ground per SECOND for every mover — and today the vehicle covers **2 / 1 / 0** hexes a tick, so the gate is RED on arrival | a rate is a rate; the timestep is a free choice (`@X058`) | ⚠ A sweep over enemy SPEEDS cannot see this (`@M013` is exactly that sweep and missed it) — the axis is the TICK LENGTH.  ⚠ And the fixture must contain a vehicle: `11_f8`'s markerless-world trap, one plan on |
| ↳ **measured** | **worse than predicted, and the shape of the miss is the finding.**  Seven tick lengths (667 / 500 / 333 / 200 / 100 / 50 / 33 ms) over one simulated minute: miner / robot / scout / helper **exact at all seven**; vehicle **180 / 120 / 180 / 0 / 0 / 0 / 0** against a true 180; boost **360 / 360 / 360 / 300 / 0 / 0 / 0** against 360.  ⚠ The cliff starts at **200 ms**, not 100 — and 500 ms costs a third of the player's speed while it still moves, which reads as feel rather than as arithmetic.  ⚠⚠ At 200 ms a boosting player covers 300 hexes and a cruising one **none**, so a truncation does not scale a rate down, it **reorders which rates exist** | `@M030`, `@D003` | ⚠⚠ The negative control the plan asked for was **not the one that mattered**.  `@M013`'s speed axis is blind, as predicted — but so is the repo's ONE tick-length gate (`23_k2a`), because its half-tick is exact for the vehicle and its tenth-tick, which is not, banks an **ENEMY**.  ⚠ Falsified six ways on arrival: all 13 functions shown to speak, and a vehicle that ROUNDS instead of truncating fires all four defect records while **overshooting** (240 at 500 ms) rather than fixing them — so L2's bank has to be a bank, not a nudge |
| **L1** | `clock_advance(clk, n × step_us) == clock_step(clk, n)` for **all** of 1..100000, against today's **602 of 1000** | an integer accumulator has no rounding to carry | ⚠ The float path must be KEPT as the control, or the gate proves only that integers equal integers.  ⚠ `tests/19_p1:97` asserts the 602 — its premise changes, and deciding what it becomes is part of L1, not after it |
| **L2** | every one of the **1255 tests and 654 measurements** green with `ENEMY_PROGRESS_EPSILON` and `HELPER_PROGRESS_EPSILON` **deleted** (not zeroed — removed) | exact arithmetic needs no guard | ⚠⚠ `@M017` says zeroing the float epsilon today turns the suite RED, so a green integer run is the proof.  ⚠ **And the one-shot timers must be tried as banks and must break** — if they do not, this document's family boundary is wrong and that is the finding |
| **L3** | a `Timer` counting UP to 20.0 s and one counting DOWN from it both fire on the **same** tick, with no epsilon in either | a one-shot duration is exact because it is an integer, not because it was nudged | ⚠⚠ `CLAUDE.md` § Timers and epsilons: *neither direction is safe*, measured — 20.0 s counting up lands exactly and counting down leaves a residue.  **Both directions or the gate is half a gate.**  ⚠ And this is where `Timer`-as-`Bank` is attempted and must break |
| **L4** | a capped driver and an uncapped one produce **different tick counts** and **identical worlds per tick**; a 1 Hz clock driven by a 30 Hz clock's ticks equals one driven from the wall | policy is the DRIVER's, arithmetic is the clock's | ⚠ Identical worlds per tick is the whole assertion — equal tick counts would mean the cap did nothing, and equal wall-clock outcomes would mean it compressed rather than dropped |
| **L5** | `clock_alpha()` in `[0, 1)`, and the vehicle drawn at alpha moves on **>200 of 240 frames** un-eased | a fixed sim and a free frame rate meet at one number | ⚠ Alpha and the ease must be measured SEPARATELY, or a green reading is the ease's (`@M023` is the prior).  ⚠⚠ If alpha adds nothing over the ease, **L5 is cut** and that is a result |
| **L6** | dryopea's 1255 + 654 and moros's world digests unchanged across the extraction, and **every door in § A DOOR PER USE CASE has a test named for its case that the docs link to** | a library is a move, not a rewrite — and a door nobody can find is a door a consumer rebuilds | ⚠ Byte-identical digests on BOTH sides; a consumer that only compiles has verified nothing.  ⚠⚠ **And the example gate needs both halves**: a test with no link is invisible, a link to prose is a snippet that rots.  The refutation is a door whose "example" is not a compiled test |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **L0** — the instrument: is any mover tick-length independent? | S | `tests/26_l0_the_timestep_sweep.loft` — one scenario at three tick lengths.  ⚠ **Expected RED on arrival**; that is the point.  File `@D003` | **Done** 2026-08-17 — 13 tests, **seven** tick lengths not three, `@M030` + `@D003` filed.  ⚠ The four vehicle functions assert today's WRONG numbers as L2's tripwire; the suite stays green and L2 must break them |
| **L1** — the clock, in integer µs | M | `tests/26_l1_the_clock.loft` — `advance(n × step) == step(n)` over 1..100000, float path kept as control.  `scripts/validate.sh` 654 unchanged | **Next** |
| **L2** — `Bank`: a rate in whole units; both mover epsilons deleted | M | `scripts/test.sh` + `scripts/validate.sh` with `ENEMY_PROGRESS_EPSILON` and `HELPER_PROGRESS_EPSILON` **removed**.  ⚠ And the vehicle gains the bank it never had | Blocked on L1 |
| **L3** — `Timer`: one-shot, UP and DOWN, and the family boundary | S | `tests/26_l3_the_timers.loft` — both directions on one target, plus the `Timer`-as-`Bank` refutation | Blocked on L2 |
| **L4** — the policies dryopea does NOT need: cap, rate, composition | S | `tests/26_l4_the_policies.loft` — capped vs uncapped, and a nested clock | Blocked on L3 |
| **L5** — alpha, or the finding that the ease already covers it | S | `tests/26_l5_the_alpha.loft` — frames moved, alpha and ease measured apart.  ⚠ May be CUT | Blocked on L4 |
| **L6** — extract; a door per use case, each with the test that IS its example | M | both suites, both digests, and one named test per door that the docs link to | Blocked on L5 |

⚠ **L0 before L1 is not ceremony.**  `design-protocol` § step 3 asks for the
cheapest test that could prove the design UNNECESSARY, and L0 is it: if every
mover is already tick-length independent, the seventh row of § 2 is wrong and
this plan is a refactor rather than a fix.  It is also the only phase that
delivers value if the rest is never built.

⚠⚠ **L4 builds what dryopea has no use for, deliberately** — a cap it must
not apply to `play_advance`, a rate scaler it gets no key for, and a
composition it does not yet need.  § FLEXIBLE is why: moros built two of the
three already, and a phase skipped because *this* consumer does not need it
is exactly how the next consumer comes to write it again.  ⚠ Its gate is
therefore dryopea's 654 measurements **unchanged** — the capability lands and
nothing in this game consumes it.

## What this plan does NOT build

**No change to what anything DRAWS.**  The 654 measurements are the gate for
that and they are asserted unchanged at every phase.

**No shorter tick.**  This plan makes one *affordable* — `plans/22` is still
the prerequisite, and `@X058`'s trigger is unchanged.  Shortening it here
would confound every reading L0 takes.

**No netcode, no rollback, no snapshots** — and this is the one place
§ FLEXIBLE's *could a consumer be tempted to hand-roll this?* answers **yes**
and the answer is still no.  Rollback needs to rewind **game state**, which
the library cannot hold without owning the game.  What it owns is the timing
half, exactly: an exact `step(n)` that re-runs N ticks identically.  ⚠ The
line is *the library owns everything a game would rebuild about TIME*, not
everything a game would rebuild.

**No fast-forward key in dryopea.**  L4 builds rate scaling because moros
needs it and the next consumer would rebuild it; dryopea binds nothing to it.

**No retirement of the ease.**

## Cross-repo coordination

⚠ **There is no timing library today** — `time` (registry 0.3.0) is calendar
arithmetic and does not overlap.  So this is a NEW library, which makes
ownership a decision rather than a lookup: `CLAUDE.md` § Loft consumer
relationship says libraries are owned by their first-class projects and
dryopea may ADD to existing ones, and neither clause covers creating one.

Done means: dryopea's **1255 tests + 654 measurements** green, and moros's
world digests **byte-identical** for `house.keys`, `deck.keys` and
`cellar.keys`.  A library change is not done when one consumer compiles.

⚠ L0-L5 land entirely inside dryopea and are useful with no library at all.
**L6 is the only phase that needs the cross-repo decision**, which is why it
is last.

## Open questions

1. **Where does the library live, and what is it called?**  *Recommendation:
   a new registry package, and **not** `tick` — `ticks()` is a loft builtin
   and `CLAUDE.md` § Loft language gotchas already records a probe that
   shadowed it and reported a tick 4× cheaper than it was.  `fixstep` says
   what it is and cannot collide; `sim_clock` is the alternative.*  L6
   decides, and the ownership half is the project owner's.
2. **What is the chosen step?**  `1e6 / 1.5` is not an integer, so L1 must
   pick one.  *Recommendation: **666667 µs**, which is today's tick to within
   1e-6 s and moves nothing.*  ⚠ The tempting round number (500000, a 2 Hz
   tick) re-prices every one of the 654 measurements and belongs to
   `plans/22`, not here.
3. **What happens to `tests/19_p1`?**  Its 602/1000 assertion is the record
   of a defect L1 removes.  *Recommendation: keep it, over the float path, as
   the negative control that proves the integer path is doing something* —
   a gate whose control is deleted is a gate that can agree by being empty
   (`@M022`'s lesson, plan 21 R1).  L1 decides.
4. **Does a saved game persist `banked`?**  § The four games predicts the RPG
   case does not fit without an answer.  Not L-anything's today — dryopea
   saves a map, never a run — but the first phase that saves a run inherits
   it.
5. **Is alpha a complement to the ease or a replacement?**  L5, and it is
   allowed to answer *neither*.

## See also

- [`plans/22`](../22-the-field-cache/README.md) — the shorter tick, which
  this makes safe and does not take.
- [`plans/19`](../19-the-interactive-loop/README.md) — the two doors, and the
  measurement this plan retires.
- [`plans/23`](../23-the-small-robots/README.md) — `@X058`, which released
  the constraint that made the tick a hex.
- [`plans/21`](../21-the-renderer/README.md) — the ease, and `@M023`.
