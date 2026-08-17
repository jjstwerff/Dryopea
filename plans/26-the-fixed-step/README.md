<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 26 — the fixed step: one clock, and the epsilons die

**Value:** `S` (silent failure) · **Effort:** `MH`

## Status

**Opened 2026-08-17.  L0 through L6 shipped 2026-08-17 — the plan is COMPLETE on the dryopea side; publishing and the moros half remain.**  This
plan came out of a comparison of dryopea's tick pacing against moros's, run
before it was written; the readings are § What was measured first and three of
them are defects nothing in either repo can currently see.

⚠⚠ **L6 EXTRACTED the library and it is a MOVE, not a rewrite** — `scripts/test.sh`
**1322 green** and `scripts/validate.sh` **654 measurements green and unmoved**, with
`tick_clock.loft`, `tick_bank.loft` and `tick_timer.loft` gone from `src/` and the
camera's ease gone from `render_camera.loft`.  ⚠ **It lives in `loft-libs-game`**
(project owner, 2026-08-17: *"something like this is needed for every game that is
built"*), which is that chunk's own stated remit — *runtime game services … consumed by
every game*.  ⚠⚠ **And the chunk's README had been advertising the slot since
bootstrap**: its `time/` row reads *"frame counter, dt, scheduling"* while `time` shipped
as date arithmetic, so the per-frame clock was a planned package that never landed.
`fixstep` fills it, and the stale row is corrected in the same change.

⚠ **What L6 shipped**: `fixstep 0.1.0` — `TickClock`, `Bank`, `Timer` and `approach`,
zero dependencies, 13 tests, **one per door**, each carrying the `@FIX-0NN` tag its
function cites.  The example half of the gate is REAL: run against the library the
citation checker reports **28 citations, 0 faults**, and it caught its own first
version — a tag in a section header is separated from its `fn` by a blank line, which
breaks the binding, so **all thirteen were dangling while looking correct**.

⚠⚠ **Two loft gaps found and FILED — and the first diagnosis of one was wrong, which
the repro is what caught** ([loft#963](https://github.com/loft-lang/loft/issues/963),
[loft#964](https://github.com/loft-lang/loft/issues/964)).  From inside dryopea the
first looked exactly like *"`loft test` ignores `--lib`"*, and this document said so
for one commit.  **It is false**: `loft test --lib lib/` resolves a package fine.  What
breaks it is **declaring that package as a path dependency** — four cases, rows 1 and 3
differing only by a `[dependencies]` block:

| | `--lib lib/` | no flag |
|---|---|---|
| **no dep declared** | **ok** | FAILED |
| **path dep declared** | FAILED | FAILED |

So the declaration is strictly worse than saying nothing, and it is the trap
`loft-libs-world/hex_draw/loft.toml` already documents for REGISTRY deps reaching path
deps too.  ⚠ **The conclusion survived its own refutation**: the in-tree shape still
could not run the suite, so `lib/fixstep` was still the wrong home — but for a reason
one `[dependencies]` line long rather than a broken test runner.  ⚠ The second gap
stands as first read: `loft test --native-wasm` is accepted, ignored, and runs the
interpreter, so that target is UNVERIFIED rather than passing.  ⚠ Repro:
`loft_repros/path_dep_suppresses_lib_search/`.  ⚠ The parity gate stands at **interpreter 13/13 and
`--native` 13/13, identical**; the package claims no target it has not run.

⚠⚠ **AND 0.1.0's OWN CI WAS RED ON ARRIVAL, on a gate this phase ARMED.**  The
chunk's unified CI runs `LOFT_DENY_WARNINGS=1` unless a package carries
`.allow_warnings`; `clock_step` does not read its `clk`, so the package failed its
first CI run.  ⚠ The warning is **correct**, which is what made the fix a decision:
not reading the clock IS the contract (a count must not spend a live session's
remainder), so touching `clk` to quiet it would break the contract and
`.allow_warnings` would switch the gate off for every future warning too.  **0.1.1
underscores the parameter at the one site that means it** and says so in the header.
⚠ It never surfaced in dryopea because `scripts/test.sh` shows warnings without
denying them — *the consumer's runner was the weaker gate, and the library's own CI
is what found it.*

⚠ **PUBLISHED** 2026-08-17: `fixstep 0.1.0` is in the registry
(`loft-lang/registry@4144a06`, index re-signed and the trust gate green), released at
`loft-libs-game@fixstep-v0.1.0`, sha256 `70cc1fed…`, 28 294 bytes — and **0.1.1**
(`registry@2f9eba1`, sha256 `eb46a3bd…`, 29 353 bytes) is what dryopea pins today.  ⚠⚠ **The proof is
that the DEV COPY WAS REMOVED FIRST**: `~/.loft/lib/fixstep` shadows the registry
([loft#667]), so dryopea's 1322 + 654 are green against the published *tarball* rather
than against the working tree it was built from.  ⚠ The CDN served a stale index for
~1 h afterwards (`20:25` against the pushed `22:03`), which the runbook names in advance
and which must not be read as a failed publish.

⚠ **What L6 still has NOT done**: the moros half of § Cross-repo coordination, where
`cam_approach` is the LINEAR form and nothing consumes the package.  That is another
repo's change and stays a separate, reversible step.

⚠⚠ **L5's headline is that an eased follow camera does not REMOVE a lattice
mover's jump — it MOVES it, off the world and onto the mover, and the jump
gets six times bigger doing so** (`@M035`, `@X085`).  Measured on screen over
one drive, as the worst single-frame move in pixels: a raw follow camera
leaves the mover at **0.0 px** and throws **14.9 px** at the ground; the
shipped eased camera leaves the ground at **1.3 px** and throws **96.1 px**
at the mover.  ⚠ **The one object in the frame an eased follow camera cannot
smooth is the object it is following**, and `@M023` could not see it because
it measured the camera's own target rather than anything drawn.

⚠⚠ **And the answer to § Open questions 6 is *neither*: alpha is what the
target ease was standing in for.**  Drawing the mover between its two hexes
takes that 96.1 px to **14.1 px and no further**, because `camera_rig_step`
eases toward the mover's HEX and a step-function target keeps a seventh of
the jump alive.  A camera that follows the **DRAWN** point reads **0.0 px** on
the mover and **0.329 px** on the ground — *smoother than the eased camera on
the ground as well*.  ⚠ So alpha and the ease are not complements to be added
in different places; applying one without the other is what leaves the
residual.

⚠⚠ **And the policy the textbook warns about is the EXACT one.**  Priced with
no camera in it at all, against the continuous ideal: `lerp(prev, cur, alpha)`
draws the tick that has ALREADY happened and is **2.598 m behind for ever**
(one whole step, four times the trail the ease already has), while
`cur + alpha·(cur − prev)` is exact to **9.5e-16 m** — because a lattice
mover's hex at a tick boundary *is* its true position at that instant.  ⚠ Its
price is a whole step's jump at every CHANGE of velocity (**2.598 m** starting,
**2.533 m** on the frame the player lets go), which is a different axis rather
than a smaller number.  **Three prices on three axes, decided by how long the
step is — so `clock_alpha` ships and no policy does.**

⚠⚠ **L5's gate is about THIS DOCUMENT'S OWN GATE for the FOURTH phase
running, and this time it is off by ONE.**  The row asks that the mover drawn
at alpha *"moves on **>200 of 240 frames**"*; measured, interpolating moves on
**exactly 200**.  ⚠ The missing frames are not noise: three of the four
policies read a PREVIOUS position and a mover that has not stepped yet has
none, so for the first forty frames every policy draws the hex.  **A threshold
within one frame of its subject is a threshold decided by whether the fixture
starts warm.**  ⚠ `@M031` was a gate that could not SEE, `@M033` one whose
control agreed for the wrong reason, `@M034` one that could not FAIL — and
this one is off by one.  Four phases, four different ways for a stated gate to
be weaker than the phase that ran it.

⚠ **What L5 shipped**: `clock_alpha` and `play_alpha` — the number, and the
game's door onto it — plus `tests/26_l5_the_alpha.loft`.  **No policy, and no
change to the camera**, which is § What this plan does NOT build's newest row:
nothing in dryopea draws an entity, so a `prev` position per drawn thing would
be a field nobody reads.  ⚠ **L5 is NOT cut** — the row's own escape clause is
*if alpha adds nothing over the ease* and it adds the only thing that can
smooth the mover at all.

⚠⚠ **L4's finding is about THIS DOCUMENT'S OWN GATE, for the third phase
running** (`@M034`) — and this one is the hardest of the three to notice,
because a vacuous gate PASSES.  The L4 row asks that *"a 1 Hz clock driven by a
30 Hz clock's ticks equals one driven from the wall"*.  It cannot fail: 1 Hz is
exactly thirty 30 Hz steps, so every slow boundary falls ON a fast one and there
is nothing for a driven clock to lag by, for **any** implementation that hands
on whole fast steps.  Measured, the two counts agree after every one of 600
frames.  ⚠ The property the row was reaching for is visible only when the steps
do NOT divide — a 700 000-unit clock driven by a 300 000-unit one is behind at
**3 of 21** frames, **0** ahead, never more than **1** behind, level again at
the common multiple — and that pair is what shipped beside it.

⚠⚠ **And the cap's stated negative control passes for the wrong
implementation.**  The row says *equal tick counts would mean the cap did
nothing*; the version a driver actually writes by mistake — clamp the answer,
keep the backlog — answers *fewer* ticks on the stalled frame too, and then
pays the stall off over the frames that follow, running the simulation behind
the wall for ever.  Over one stream with a 20 s stall in it a dropping cap of 4
plays **4** ticks and a deferring one plays **24** and still owes 6.  The
deferring driver is reproduced in `tests/26_l4` as the control, and it is what
makes the cap's gate able to fail at all (`@X083`).

⚠ **What L4 shipped**: `clock_advance_capped`, `clock_pump`, `clock_set_rate`
and `clock_drive` — four doors over the ONE accumulator, none of them a default
and **none of them consumed by dryopea**, which is the phase (§ FLEXIBLE).  The
rate is a RATIONAL applied at one private site to every DURATION door and to no
COUNT door (`@X084`), and a defaulted `0 / 0` reads as UNSCALED — this plan's
**third** answer to [loft#914], and the first where the silent default is
*the behaviour you already had* rather than a hazard to design around.

⚠⚠ **L3 measured the timer family before converting it, and the reading
INVERTS § 2** (`@M033`, `@D004`).  § 2 counted the three 1e-9 nudges as the
brittleness.  Swept at seven tick lengths through the shipped code, **all three
guarded timers hold their duration at every one of them** — helper recovery
90 / 120 / 180 / 300 / 600 / 1200 / 1800, tower rebuild 30 / 40 / 60 / 100 /
200 / 400 / 600, boost 3 / 4 / 6 / 10 / 20 / 40 / 60 — while the two that never
got a guard do not: the **wave lull** reads 23 / 30 / 45 / **76** / **151** /
300 / **451** and the **pre-walk window** 8 / 10 / **16** / 25 / **51** /
**101** / **151**.  ⚠ Both are right at the shipped 667 ms, which is why
nothing had seen them.  ⚠⚠ **That is `@D003`'s shape in the other family —
*the site that never got a guard at all*** — and two of the three guarded
timers count DOWN exactly as the broken pair does, so the DIRECTION this plan
was written around is not the discriminator.  A guard is.

⚠⚠ **And the second L3 finding is about THIS DOCUMENT'S OWN GATE.**  The L3
row below asks for *"a `Timer` counting UP to 20.0 s and one counting DOWN
from it both fire on the same tick"*.  Measured at the shipped tick over six
exact-multiple durations, float UP against float DOWN against the true count —
4 s **7/7/6**, 10 s **16/16/15**, 20 s **30/31/30**, 30 s **45/45/45**, 40 s
**61/61/60**, 60 s **91/91/90** — the two directions disagree at exactly ONE of
six and **agree while both being a tick long at FOUR**.  So the gate as worded
would have read *agreement* at four times as many cases as it caught.
***Two agreeing instruments are not a control; the TRUE count is.***

⚠ **What L3 shipped**: `src/tick_timer.loft` — one one-shot type, `{spent,
total}` in integer base units, both readings off one number.  All five timers
converted (`recover`, `repair`, `boost`, `cool`, `stand`, `lull`), all three
epsilons **deleted**, and `@D004` filed and closed in the phase that found it.
⚠⚠ **A `Timer` MAY hold its `total` where a `Bank` may not hold its `whole`,
and it is the same rule ([loft#914]) reaching opposite conclusions** (`@X082`):
a defaulted `whole` of 0 is a mover that never moves, a defaulted `total` of 0
is an UNARMED timer — which is exactly what every `0.0` seconds field it
replaces already meant.  ⚠ The `Timer`-as-`Bank` refutation L2 deferred here
was RUN and the boundary held: a one-shot built on `bank_gain` fires a second
time with nobody re-arming it, and its residue leaks into the next arming — a
5.0 s cooldown costs 8 ticks the first time and **7** the second.

⚠ **The seam did not CLOSE, it changed hands.**  `tick_clock.loft` predicted
L3 would delete `clock_seconds_from_units`.  What is left after it is not
simulation: `.keys` **authoring** (a person writes seconds, exactly where
`bank_fraction` sits one family over) and the camera's **ease**, which
§ The invariant already puts outside.  `clock_units_from_seconds` joined it as
the other half.

⚠⚠ **And § 2's count was SEVEN and there are EIGHT.**  The tower's CHARGE
accumulates float seconds and `wave_fire` subtracts one whole interval per
shot, carrying the remainder for ever — `bank_gain` written by hand, on the
BANK side of the boundary, still holding `TOWER_CHARGE_EPSILON`.  ⚠ It is not
a rename: a tower may only release a shot it is ALLOWED to fire, so the held
count and the carry have to come apart (`tower_hold` caps a capacitor at one
interval).  Left deliberately, and PINNED by
`tests/26_l3::test_the_tower_charge_is_still_a_hand_rolled_bank` so the
follow-up is a decision rather than a rediscovery.

⚠⚠ **L2 closed `@D003` and deleted both mover epsilons**, and the thing
worth carrying out of it is what the `Bank` deliberately does NOT hold
(`@X080`).  The RATE arrives per call, because `@X061` makes a rate a
property of a mover's CONDITION; and `whole` — how much banked progress
makes one hex — is a PARAMETER rather than a field, because loft
defaults an omitted struct field silently ([loft#914]) and dryopea
builds `Enemy` from partial literals in a dozen places.  ⚠ A `Bank`
carrying its own scale would default to zero in every one of them and
silently freeze that mover; a `Bank` carrying only the CARRY defaults to
a fresh bank, which is exactly right.  **The price is one extra argument
at three call sites inside `src/`, and no test ever writes it.**

⚠⚠ **And the reciprocal form was refused on arithmetic, not on taste.**
Storing *base units per whole unit* would have made `Bank` into
`clock_advance` with a variable step and needed no rate scale at all —
but `3 000 000 / 2.25` is 1 333 333.33, and `@M013`'s speed sweep
already includes 2.25 hex/s.  A rate scaled UP is exact for every number
`numbers.json` authors; a rate inverted is not.  *That asymmetry is why
`Bank` is not `TickClock` wearing a different name.*

⚠⚠ **L1 shipped the clock, and this document's own recommendation was the
thing it had to refute** (`@M031`, `@X079`).  § Open questions 2 recommended a
**666 667 µs** step — *"today's tick to within 1e-6 s"*, which *"moves
nothing"*.  Measured: the **654 scenario measurements do not budge** and
**seventeen tests fail**.  `23_k2a`'s carry stops being 0.0 to the bit,
`3.0 * TICK_SECONDS` stops being exactly 2, and two frame accumulators land a
tick out — because `0.666667` and `0.6666666666666666` differ in the seventh
digit and a dozen assertions are pinned to the exactness of the second.
⚠ **A step chosen to LOOK like the tick is not the tick.**

⚠⚠ **The answer is a finer base unit: 1/3 of a microsecond**, 3 000 000 a
second, because it is the COARSEST unit in which 2/3 of a second is a whole
number — today's step is exactly **2 000 000** of them, and `TICK_SECONDS`
derived from it is **bit-identical** to the `1.0 / 1.5` it replaced.  So there
is one definition of the tick and no residue to document.  ⚠ The unit is the
CONSUMER's choice and not the library's: a game at 128 Hz cannot use
3 000 000 either, so `clock_new` takes a step and counts whatever the caller
counts.  What the library promises is the IDENTITY, never a unit.

⚠ **The reusable half is which gate was blind.**  The 654 measurements are
integer tick COUNTS, and a 5e-7 relative shift in the tick's LENGTH moves none
of them; the unit tests pinned to bit-exactness caught it at once.  *A
measurement's resolution is not its authority* — and this is the second time
in two phases that the corpus's biggest gate was the one that could not see
the thing (`@M030`'s three accidents).

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
| **L1** ✅ | `clock_advance(clk, n × step_us) == clock_step(clk, n)` for **all** of 1..100000, against today's **602 of 1000** | an integer accumulator has no rounding to carry | ⚠ The float path must be KEPT as the control, or the gate proves only that integers equal integers.  ⚠ `tests/19_p1:97` asserts the 602 — its premise changes, and deciding what it becomes is part of L1, not after it |
| ↳ **measured** | **0 of 100 000 disagree**, and the float control disagrees at every one of the seven counts `19_p1` names by hand.  ⚠ The two sweeps are different LENGTHS on purpose: the float path spends a step per iteration, so sweeping it to 100 000 is 5 × 10⁹ subtractions — **the old arithmetic was quadratic as well as inexact**, which nothing had noticed because no frame ever delivered an hour.  (Confirmed by accident: a falsification that put the float body back inside `clock_advance` hung the file.) | `@M031`, `@X079` | ⚠⚠ The control is in the SAME function as the claim, asking both paths the same `n` — so the file cannot pass by both halves being empty, and it fires when `float_ticks_for` is stubbed to be correct.  ⚠ `tests/19_p1:97`'s 602 is **unchanged and now load-bearing**: `TICK_SECONDS` is bit-identical, so it stands exactly where § Open questions 3 recommended, as the control that proves the integer path is doing something |
| **L2** ✅ | every one of the **1268 tests and 654 measurements** (1269 after this phase splits one) green with `ENEMY_PROGRESS_EPSILON` and `HELPER_PROGRESS_EPSILON` **deleted** (not zeroed — removed) | exact arithmetic needs no guard | ⚠⚠ `@M017` says zeroing the float epsilon today turns the suite RED, so a green integer run is the proof.  ⚠ **And the one-shot timers must be tried as banks and must break** — if they do not, this document's family boundary is wrong and that is the finding |
| ↳ **measured** | **654 measurements green and UNMOVED, 1269 tests green, both epsilons deleted.**  `@M030`'s sweep now reads **180 × 7** for the player and **360 × 7** boosting.  ⚠ Behaviour at the shipped tick is bit-identical by construction — 3.0 hex/s over a 2 000 000-unit step is exactly 2 hexes with nothing carried — so the change is entirely at timesteps nobody ships yet, which is why the biggest gate has nothing to see for the *second* phase running | `@M032`, `@X080`, `@X081` | ⚠ The negative control was already in the tree and it FIRED: L0's four vehicle functions asserted the wrong numbers as a tripwire, and every one went RED on this phase before going green on the true 180 / 360.  ⚠⚠ **The `Timer`-as-`Bank` refutation this row asked for was NOT run and is deferred to L3** — the one-shot timers were left on float seconds behind one named seam (`clock_seconds_from_units`), so the family boundary is *asserted* here and *tested* there.  Recorded rather than quietly dropped |
| **L3** ✅ | a `Timer` counting UP to 20.0 s and one counting DOWN from it both fire on the **same** tick, with no epsilon in either | a one-shot duration is exact because it is an integer, not because it was nudged | ⚠⚠ `CLAUDE.md` § Timers and epsilons: *neither direction is safe*, measured — 20.0 s counting up lands exactly and counting down leaves a residue.  **Both directions or the gate is half a gate.**  ⚠ And this is where `Timer`-as-`Bank` is attempted and must break |
| ↳ **measured** | **this row's own expected result is a gate that could not fail**, and the phase's first measurement is what said so.  Over six exact-multiple durations at the shipped tick, float UP vs float DOWN vs true: **7/7/6, 16/16/15, 30/31/30, 45/45/45, 61/61/60, 91/91/90** — the two directions disagree at ONE of six and **agree while both being a tick long at FOUR**.  ⚠ The shipped gate therefore measures against the TRUE count and keeps the up/down pair only as the record.  ⚠⚠ The real defect was elsewhere and is `@D004`: the two timers with NO epsilon (`Enemy.stand`, `WaveSchedule.lull`) run a tick long at four and three of seven tick lengths, while all three GUARDED ones are exact at all seven — § 2 had the brittleness backwards.  **1285 tests green, 654 measurements green and unmoved, three epsilons deleted** | `@M033`, `@D004`, `@X082` | ⚠ The `Timer`-as-`Bank` refutation L2 deferred here was RUN and BROKE the bank twice, both silently: it fires a second time with nobody re-arming it, and a 5.0 s cooldown costs 8 ticks the first time and **7** the second.  ⚠ The negative control for the two converted rows is the pre-L3 float arithmetic reproduced beside them and asserted WRONG at seven of fourteen readings — so a green profile is a measurement rather than a restatement of `true_ticks` |
| **L4** ✅ | a capped driver and an uncapped one produce **different tick counts** and **identical worlds per tick**; a 1 Hz clock driven by a 30 Hz clock's ticks equals one driven from the wall | policy is the DRIVER's, arithmetic is the clock's | ⚠ Identical worlds per tick is the whole assertion — equal tick counts would mean the cap did nothing, and equal wall-clock outcomes would mean it compressed rather than dropped |
| ↳ **measured** | **both halves of this row are weaker than what the phase found.**  The composition clause **cannot fail**: 1 Hz is exactly thirty 30 Hz steps, so every slow boundary falls ON a fast one and the two counts agree after every one of 600 frames for any implementation that hands on whole steps.  The property it reaches for needs steps that do NOT divide — 700 000 driven by 300 000 is behind at **3 of 21** frames, **0** ahead, worst lag **1**, level at the common multiple.  ⚠ And the cap reads **4** ticks dropping against **24** deferring over one stream with a 20 s stall, with `state_diff` green against a counted control both ways and the two capped/uncapped worlds DIFFERING so the pair cannot hold vacuously.  ⚠ A wall minute at 1/3 speed is **30** ticks exact against **29** truncated.  **1305 tests green, 654 measurements green and unmoved** | `@M034`, `@X083`, `@X084` | ⚠⚠ **The stated control passes for the wrong implementation.**  *Equal tick counts would mean the cap did nothing* — but a DEFERRING cap answers fewer ticks on the stalled frame too and is still wrong, so the real control is the deferring driver reproduced beside the shipped one.  ⚠ *Equal wall-clock outcomes would mean it compressed* is unreachable rather than satisfied: the step is fixed and the door answers a COUNT, so nothing in the clock can compress — a driver that multiplied its dt could, which is `plans/22` § LOD's warning and not this file's |
| **L5** ✅ | `clock_alpha()` in `[0, 1)`, and the vehicle drawn at alpha moves on **>200 of 240 frames** un-eased | a fixed sim and a free frame rate meet at one number | ⚠ Alpha and the ease must be measured SEPARATELY, or a green reading is the ease's (`@M023` is the prior).  ⚠⚠ If alpha adds nothing over the ease, **L5 is cut** and that is a result |
| ↳ **measured** | **the alpha half holds and the frame count is off by ONE** — interpolating moves on **exactly 200** of 240, because a mover that has not stepped yet has no previous position and the first forty frames draw the hex under every policy.  ⚠⚠ And the phase's real finding is the one the row does not reach for: the eased camera does not REMOVE the mover's jump, it MOVES it — raw camera **0.0 px** mover / **14.9 px** ground against the shipped eased camera's **96.1 px** mover / **1.3 px** ground.  Alpha takes 96.1 to **14.1** and a camera following the DRAWN point takes it to **0.0** with **0.329 px** of ground.  ⚠ Priced with no camera at all: interpolate **2.598 m** of lag (one whole step) against extrapolate's **9.5e-16 m**, and extrapolate pays **2.598 / 2.533 m** in one frame at the start and the stop.  **1322 tests green, 654 measurements green and unmoved** | `@M035`, `@X085` | ⚠ The row's separation held and earned its keep — the policy sweep has no camera in it, so interpolation's *2.598 m* is the drawn position against the continuous ideal and not the ease's.  ⚠⚠ **And the screen 2x2 is its own control**: each camera is smooth for exactly one of the two points, so no row can be green by the instrument reading nothing — which is what `@M022`'s *can this gate produce a non-trivial reading at all?* asks for.  ⚠ One further control was needed and was not foreseen: measuring the truth against `HEX_FLAT_TO_FLAT` (seven digits) rather than against `lat_to_metres` (exact) charged extrapolation with **1.3 µm** of error that was the CONSTANT's — *a reference rounded to seven digits cannot certify a policy that is exact* |
| **L6** | dryopea's 1268 + 654 and moros's world digests unchanged across the extraction, and **every door in § A DOOR PER USE CASE has a test named for its case that the docs link to** | a library is a move, not a rewrite — and a door nobody can find is a door a consumer rebuilds | ⚠ Byte-identical digests on BOTH sides; a consumer that only compiles has verified nothing.  ⚠⚠ **And the example gate needs both halves**: a test with no link is invisible, a link to prose is a snippet that rots.  The refutation is a door whose "example" is not a compiled test |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **L0** — the instrument: is any mover tick-length independent? | S | `tests/26_l0_the_timestep_sweep.loft` — one scenario at three tick lengths.  ⚠ **Expected RED on arrival**; that is the point.  File `@D003` | **Done** 2026-08-17 — 13 tests, **seven** tick lengths not three, `@M030` + `@D003` filed.  ⚠ The four vehicle functions assert today's WRONG numbers as L2's tripwire; the suite stays green and L2 must break them |
| **L1** — the clock, in integer base units | M | `tests/26_l1_the_clock.loft` — `advance(n × step) == step(n)` over 1..100000, float path kept as control.  `scripts/validate.sh` 654 unchanged | **Done** 2026-08-17 — `src/tick_clock.loft`, 13 tests.  ⚠ **Not µs**: the recommended 666 667 µs step moves 17 tests (`@M031`), so the base unit is 1/3 µs and `TICK_SECONDS` is derived bit-identical (`@X079`).  ⚠ `main.loft` now hands integer µs down; the float door rounds |
| **L2** — `Bank`: a rate in whole units; both mover epsilons deleted | M | `scripts/test.sh` + `scripts/validate.sh` with `ENEMY_PROGRESS_EPSILON` and `HELPER_PROGRESS_EPSILON` **removed**.  ⚠ And the vehicle gains the bank it never had | **Done** 2026-08-17 — `src/tick_bank.loft`; `@D003` closed, both epsilons deleted, all three movers take INTEGER base units.  ⚠ `@X080`: a `Bank` holds the carry alone — the rate is `@X061`'s and `whole` is a parameter, because a nested struct's silent zero-default would freeze a mover.  ⚠ `@X081`: `vehicle_hexes_per_tick` is a CEILING that spends nothing, because `play_steer_reach` asks once per FRAME |
| **L3** — `Timer`: one-shot, UP and DOWN, and the family boundary | S | `tests/26_l3_the_timers.loft` — both directions on one target, plus the `Timer`-as-`Bank` refutation | **Done** 2026-08-17 — `src/tick_timer.loft`, 16 tests; all five one-shot timers converted and `HELPER_TIMER_EPSILON` / `TOWER_REPAIR_EPSILON` / `VEHICLE_TIMER_EPSILON` deleted.  ⚠ `@D004` found AND closed in the phase: the two timers that never had a guard were the broken ones.  ⚠ `@X082`: a `Timer` holds its `total` where a `Bank` may not hold its `whole` — same [loft#914] rule, opposite conclusion.  ⚠ § 2's count was seven and there are EIGHT — the tower's CHARGE is a hand-rolled bank, pinned rather than converted |
| **L4** — the policies dryopea does NOT need: cap, rate, composition | S | `tests/26_l4_the_policies.loft` — capped vs uncapped, and a nested clock | **Done** 2026-08-17 — four doors on `src/tick_clock.loft` (`clock_advance_capped` / `clock_pump` / `clock_set_rate` / `clock_drive`), 20 tests, and dryopea consumes none of them.  ⚠ `@M034`: **this row's composition clause cannot fail** — the pair it names is commensurate — and its cap control passes for a DEFERRING cap, which is the mistake a driver actually writes.  ⚠ `@X083`: a policy is a door, and the cap DROPS.  ⚠ `@X084`: the rate is a rational, exempt from the count door, and its defaulted `0 / 0` is the third [loft#914] answer in this plan |
| **L5** — alpha, or the finding that the ease already covers it | S | `tests/26_l5_the_alpha.loft` — frames moved, alpha and ease measured apart.  ⚠ May be CUT | **Done** 2026-08-17 — `clock_alpha` + `play_alpha`, 17 tests, and **no policy**: three of them measured, all three priced, none shipped.  ⚠ **NOT cut** — the ease is the one thing that cannot smooth the mover, because the mover is what it is chasing (`@M035`).  ⚠ `@X085`: the alpha is the clock's and the policy is the renderer's, and the two have to be applied in the SAME place or a seventh of the fault survives.  ⚠ The row's own frame count is off by one, for a reason that is in the arithmetic |
| **L6** — extract; a door per use case, each with the test that IS its example | M | both suites, both digests, and one named test per door that the docs link to | **Done (dryopea half)** 2026-08-17 — `fixstep 0.1.0` in `loft-libs-game`, 13 tests, 13 doors, 28 citations 0 faults.  ⚠ dryopea 1322 + 654 **unmoved**, which is the *a move, not a rewrite* half of the gate.  ⚠ **moros's digests are NOT done** — that repo still holds the linear `cam_approach` and consumes nothing; it is the remaining half and it is another repo's change.  ⚠ Publishing deferred deliberately |

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

**No retirement of the ease**, and after L5 that is a measured position
rather than a reservation: the ease still owns the azimuth (eight discrete
WASD headings), the boom (quantised to hex steps) and the deliberate trail
that makes a boost read as speed.  What L5 measured is that it never owned
the mover.

**No INTERPOLATION POLICY, and no change to what the camera follows** — L5's
newest row.  Three policies are measured and priced (`@M035`) and none is
built, because nothing in dryopea draws an entity: a previous position per
drawn thing would be a field nobody reads, which is the mistake
`play.loft`'s own `PlayState.cam` comment records from the other side (*a
field nobody reads is a decision validated by nothing*).  ⚠ The trigger is
[`plans/20`](../20-entity-art/README.md), and it inherits **two** changes
rather than one: what the mover is drawn at, and what the camera looks at.
Doing the first alone leaves a seventh of the jump.

**No re-derivation of `CAMERA_EASE_RATE`.**  L5 measured that its own
justification — a steady trail of `v / k = 0.649 m`, *"under half a hex"* —
assumes a CONTINUOUS mover, and against dryopea's the real trail is
**2.395 m**, 3.7x the figure.  The constant is not changed: it is also what
sizes the azimuth's 180° reversal, and the trail becomes the derived number
the moment a drawn mover is continuous.  ⚠ Pinned by
`tests/26_l5::test_the_ease_rate_was_derived_for_a_continuous_mover` so the
re-derivation is a decision rather than a rediscovery.

**No conversion of the tower's CHARGE** — the eighth *do-not-lose-a-fraction*
site, which § 2 never counted.  It is a hand-rolled `bank_gain` and belongs to
the `Bank` family, but a tower may only release a shot it is ALLOWED to fire,
so the held count and the carry have to come apart before `tick_bank.loft` can
own it.  ⚠ The trigger is a phase that touches `wave_fire`;
`tests/26_l3::test_the_tower_charge_is_still_a_hand_rolled_bank` is the pin.

## Cross-repo coordination

⚠ **There is no timing library today** — `time` (registry 0.3.0) is calendar
arithmetic and does not overlap.  So this is a NEW library, which makes
ownership a decision rather than a lookup: `CLAUDE.md` § Loft consumer
relationship says libraries are owned by their first-class projects and
dryopea may ADD to existing ones, and neither clause covers creating one.

Done means: dryopea's **1268 tests + 654 measurements** green, and moros's
world digests **byte-identical** for `house.keys`, `deck.keys` and
`cellar.keys`.  A library change is not done when one consumer compiles.

⚠ L0-L5 land entirely inside dryopea and are useful with no library at all.
**L6 is the only phase that needs the cross-repo decision**, which is why it
is last.

## Open questions

1. ✅ **Where does the library live, and what is it called?**  **`fixstep`, in
   `loft-libs-game`** (project owner, 2026-08-17: *"something like this is needed
   for every game that is built"*).  ⚠ The name recommendation held for a second
   reason found while checking: **moros already has a `tick` module**
   (`hex_editor/src/tick.loft`, the `Walker`), so `tick` would have read as the
   same thing while being a body rather than a clock — on top of `ticks()` being a
   loft builtin.  ⚠⚠ **A standalone repo was considered and refused on the owner's
   argument**: the surveyed evidence found only one other duplicated candidate
   (`approach`, which ships INSIDE the package), so a themed repo looked like
   over-building — but *every game needs this* is a stronger admission test than
   *two games happen to have written it*, and `loft-libs-game` already exists for
   exactly that class.  ⚠ `moros_map` was floated as a second tenant and is NOT
   one: it is a world model and belongs beside `hex_world` in `loft-libs-world`.
2. ✅ **What is the chosen step?**  ⚠⚠ **ANSWERED, and the recommendation
   above was WRONG** — 666 667 µs moves **seventeen tests** (`@M031`), because
   `0.666667` is not `0.6666666666666666` and a dozen assertions are pinned to
   the exactness of the second.  **The base unit is 1/3 of a microsecond**
   (3 000 000 a second) and the step is **2 000 000** of them, which is the
   coarsest unit in which 2/3 of a second is whole — so `TICK_SECONDS` derived
   from it is bit-identical and there is ONE definition (`@X079`).  ⚠ The
   original note stands on its other half: the tempting round number (500 000,
   a 2 Hz tick) re-prices every one of the 654 measurements and belongs to
   `plans/22`.
3. ✅ **Where does the seconds seam end up?**  ⚠ `tick_clock.loft` predicted
   L3 would DELETE `clock_seconds_from_units`.  It did not close — it **changed
   hands**.  After L3 no simulation reads seconds; what is left is `.keys`
   **authoring** (a person writes seconds, which is `bank_fraction`'s position
   one family over) and the camera's **ease**, which § The invariant already
   puts outside.  `clock_units_from_seconds` joined it as the other half.

4. ✅ **What happens to `tests/19_p1`?**  **Kept, unchanged, and now
   load-bearing** — exactly as recommended.  `TICK_SECONDS` is bit-identical
   after L1, so its 602/1000 still measures what it measured, and it is the
   control that proves the integer path is doing something rather than
   agreeing with itself.  ⚠ `tests/26_l1` does not lean on it from a distance:
   it asks BOTH paths the same `n` **in one function**, so the file cannot go
   green by both halves being empty.
5. **Does a saved game persist `banked`?**  § The four games predicts the RPG
   case does not fit without an answer.  Not L-anything's today — dryopea
   saves a map, never a run — but the first phase that saves a run inherits
   it.
6. ✅ **Is alpha a complement to the ease or a replacement?**  ⚠⚠
   ***Neither*, and the permitted third answer turned out to be the right
   one** (`@M035`).  Alpha is what the target ease was **standing in for**:
   the ease cannot smooth the mover at all (it moves the mover's jump from
   the world onto it, 14.9 px of ground becoming 96.1 px of mover), and alpha
   applied to the mover alone leaves 14.1 px behind because the camera is
   still easing toward a step function.  A camera following the DRAWN point
   reads 0.0 px on the mover and beats the eased camera on the ground too.
   ⚠ So they are not two smoothings to add in two places — **applying one
   without the other is what leaves the residual**, and the ease's remaining
   jobs are the azimuth, the boom and the deliberate trail
   (`docs/RENDERER.md` § R2b).

## See also

- [`plans/22`](../22-the-field-cache/README.md) — the shorter tick, which
  this makes safe and does not take.
- [`plans/19`](../19-the-interactive-loop/README.md) — the two doors, and the
  measurement this plan retires.
- [`plans/23`](../23-the-small-robots/README.md) — `@X058`, which released
  the constraint that made the tick a hex.
- [`plans/21`](../21-the-renderer/README.md) — the ease, and `@M023`.
