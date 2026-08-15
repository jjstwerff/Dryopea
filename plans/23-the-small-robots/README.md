<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 23 — The small robots: four roles, one AI

**Value:** `G` · **Effort:** `M`

## Status

**K0 + K1 + K2a + K2b shipped** (2026-08-15).  K3 is next, and it needs
no new code.

⚠⚠ **The scout is FASTER and the miner is SLOWER, and the phase cost
one lookup.**  `enemy_speed(kind)` answers 2.5 hex/s for a scout, 1.0
for a miner and the regular's 1.5 for everybody else; `enemy_tick` hands
it to the bank K2a built and nothing else moved — no mover, no target,
no field.  MEASURED, nine hexes of identical corridor: **6 / 9 / 14
ticks** (`@M016`).  ⚠ A miner stands STILL on one tick in three and a
scout takes TWO hexes on two in three, which is what a rate looks like
once a tick is no longer a hex.

⚠⚠ **The guard `@M014` measured as unreachable now FIRES, and that was
a REASON for the numbers rather than a consequence of them.**  `@M013`
listed the speeds at which `ENEMY_PROGRESS_EPSILON` is worth a whole
hex; 2.25 and 3.0 hex/s would both have read as *"quite a bit faster"*
and both hide the guard exactly as 1.5 does, so picking one would have
left the whole roster unable to see its own rounding (`@X063`).  2.5
does not, and it takes only **three ticks** to say so: three of
`2.5 × (1.0/1.5)` sum to 4.999999999999999, and the fifth hex exists
only because the epsilon releases it.  MEASURED (`@M017`) — zero the
constant and **7 of 1149 tests fail and `scripts/validate.sh` goes
red**, where the identical change at K2a left everything green.  ⚠ Only
three of the seven are assertions written to look for it; the other four
are ordinary behaviour — a round trip, a gap, a bank total and the
`.keys` file.

⚠ **The phase's own prediction about `12_b0` was WRONG, and the way it
was wrong is the useful part.**  K2a wrote that K2b *"is what makes
`test_a_tick_advances_an_enemy_exactly_one_hex` go red"*.  It did not,
and could not: that test is written about the REGULAR, whose speed is
exactly what K2b held.  What the ticks-are-hexes claim lost is its
SCOPE — it is now a statement about one class rather than about the
engine.  ⚠ The pin that did have to break was K1's
`test_speed_is_still_the_same_for_every_class`, and it is kept
INVERTED (`test_a_scout_now_outruns_a_miner`) rather than deleted,
because a pin and its inversion are one measurement and a deleted pin
says nothing.

⚠ **K2a's OTHER prediction was exactly right, and it paid within one
suite run.**  *"K2b is the phase where a captured scenario starts
carrying a remainder, and the first thing that happens is `compare.loft`
going red — which is the signal to give `place` its progress, not to
loosen the comparison."*  `tests/18_s2_the_round_trip.loft` failed
naming `enemies[0].progress: 0.33333333333333326 vs 0` on the first
full run after the constants changed.  The answer was a `banked <i>
<hexes>` verb (`stand`'s sibling, because `progress` is zero-neutral
too), a line in `emit_enemies`, and a field in `crop_state`'s literal —
where an omitted field takes its default SILENTLY (loft#914), so a crop
would have reset the very carry a cropped fixture exists to preserve.
⚠ **A tripwire written for a value that cannot occur is worth writing**:
this one fired on the phase it was written for and named the field.

⚠ **Two of `@M011`'s five breach clocks moved, and no rate did.**
20 / 35 / 50 / 96 / 456 → **23 / 35 / 50 / 96 / 454**.  A breach clock
counts from the tick the wave was PLACED, so it contains the walk as
well as the chewing, and the two that moved are exactly the two classes
whose speed moved.  ⚠ The three unmoved rows are what say the damage
rates are untouched — and the general lesson is that **a composite
clock re-baselines when either of its parts moves**, which is why
`@M016`'s pure arrival clocks exist beside it rather than instead of
it.

⚠ **The corpus is not the gate any more, and saying so is the
obligation.**  K2a's gate was *nothing moved*; K2b moves two numbers, so
what the corpus can still certify is the OTHER 569 measurements — the
suite went **1138 → 1149 tests** and the gate **30 → 31 scripts / 569 →
597 measurements**, where the +28 is exactly the new scenario's own
checks and the readings that changed are exactly the two K0 scenario
files' walking lines.  Their HP lines did not move, which is the
property worth reading off the pair: a class's speed decides when the
siege STARTS, its bite decides how the siege goes, and the two axes do
not contaminate each other.

⚠⚠ **SPEED IS NO LONGER THE TICK, and not one reading moved.**  An
enemy banks `speed × tick_seconds` and steps when a whole hex is due;
`enemy_tick` asks `enemy_bank` how many hexes this timestep owes it and
takes that many.  Every number is held at today's value, so the old path
and the new one agree bit for bit — **1128 → 1138 tests** (the ten new
ones are K2a's own) and **30 scripts / 569 measurements UNCHANGED**
(`@M015`).  `TICK_SECONDS = 1/ENEMY_SPEED_HEX_PER_SECOND` survives as
the expression that HOLDS the timestep at one regular's hex rather than
the one that forces it (`@X058`).

⚠⚠ **The phase's real finding is that its own guard cannot fire.**
`1.5 × (1.0/1.5)` is exactly 1.0 to the bit, so the carry is 0.0 for
ever and the accumulator behaves like the `+ 1` it replaced.  MEASURED
by setting `ENEMY_PROGRESS_EPSILON` to 0.0 and running both gates: **all
1128 pre-K2a tests and all 569 measurements stay green**, and only the
three assertions written to look for it fail (`@M014`).  That is
`17-T1`'s shape — *an epsilon whose removal leaves the suite green is a
guard that cannot fire* — arriving in the one phase whose whole gate is
*nothing moved*.

⚠⚠ **And 1.5 is one of the FEW speeds with that property, which is what
turns a curiosity into a trap.**  Swept over sixty ticks of the real
timestep, the epsilon is worth a whole hex at **1.0, 1.2, 1.8, 2.0 and
2.5** hex/s and worth nothing at 0.5, 0.75, 1.5, 2.25 and 3.0 —
so a class K2b hands 1.0 hex/s would silently lose a hex every forty,
and the corpus would report it as *a wave that arrives a tick late*
rather than as a rounding bug (`@M013`).  ⚠ It is worth a hex to a
**tenth-length tick** too, so the guard is [`plan 22`](../22-the-field-cache/README.md)'s
problem as much as K2b's.  ⚠ This is why `enemy_bank` takes its speed as
an ARGUMENT where `helper_bank` reads a constant (`@X060`): a bank that
read the constant could only ever be tested at the value that hides its
own guard.

⚠ **One asymmetry against the crew was decided rather than inherited**
(`@X059`): a hex the ground refuses is **spent, not re-banked**.
`helper_bank` does the opposite on purpose — a helper parked at a wall
keeps accumulating so its average stays a rate — but an enemy stopped by
a wall is BESIEGING, and storing the hexes would let a robot that chewed
a breach for ten ticks cross the base the moment it opens.

⚠ **`state_diff` learned `progress` as a TRIPWIRE, not because it can
differ today.**  The carry is exactly 0.0 after every tick at 1×, so
`emit.loft` has no verb for it and S2's round-trip is green without one.
K2b is the phase where a captured scenario starts carrying a remainder,
and the first thing that happens is `compare.loft` going red — which is
the signal to give `place` its progress, not to loosen the comparison.

⚠ **A wave has COMPOSITION, and the design decision was to DELETE this
plan's own negative control.**  `schedule 4 12` arms a list and
`compose 1 4 miner 8 scout` says what one wave of it is made of; the
roster comes out in that order, because K0 measured order to be worth
20x on a breach clock.  The suite went 1107 → **1128** and the gate is
**30 scripts / 569 measurements UNCHANGED** — the identity property
K1 needed, because a `vector<integer>` still means N waves of regulars
(`@X055`).

⚠ **§ Open questions 3 is answered and the answer removed a check.**  A
wave is a flat `vector<WavePart>` and its SIZE is SUMMED, so *the parts
sum to the count* is the definition rather than an invariant — the
control this plan named (*"a mix whose parts do not sum to the wave's
count is refused at parse"*) can only exist if the count is a second
fact stored beside the parts, which is the shape `carry.loft` refuses
in one line.  ⚠ **A control that cannot be WRITTEN is better than one
that cannot fail, and saying which is which is the obligation** —
`tests/23_k1_composition.loft` § The size is the sum asserts the
property the deleted check protected: composing twelve as `3 miner 2
scout` makes the wave FIVE, because there is no stored total to
disagree with.

⚠⚠ **K1's real cost was a loft heap-corruption bug, not the feature.**
Building the parts vector inline in `script_command` (~700 lines)
corrupts the interpreter heap *at compile time*, and the abort lands in
`tests/12_b1_rubble.loft` — a file that never says `compose`, never
reaches the branch and never mentions a schedule.  Bisected at
full-suite scale: **the whole data model without that branch is 1107
green**, so the nested `vector<Struct>` is innocent and the enclosing
function is the ingredient.  Filed as
[loft#935](https://github.com/loft-lang/loft/issues/935); the fix is to
give the parsing its own small functions.  ⚠ Two false leads were
measured and discarded — the trailing `u8` field (`integer` aborts
identically) and one particular inline expression (binding it merely
*moved* the abort to a different unrelated file).  ⚠ **A green suite is
not evidence that a violating call site is absent.**

⚠ **The four classes exist and a wall knows the difference** — four of a class
into the same sealed band breach at **20 / 35 / 50 / 96 / 456 ticks** (miner /
builder / robot / harvester / scout, `@M011`), which is **23× between the two
ends** from a mover that cannot tell them apart.  The suite went 1094 → 1107 and
the gate 28 scripts / 520 measurements → **30 / 569**, with **not one existing
reading moved** — because `robot` keeps its 1.0 HP/s and the four APPEND
(`@X054`).

⚠ **What K0 measured that the plan did not predict.**  The scout's first rate
was 0.2 HP/s and four of them breached at **231 ticks** — inside the 321 a real
base lasts (`@M005`), which is the opposite of *"no real weapon at all"*.  The
arithmetic had been done against the 100 HP BRACED figure while the siege chews
the 30 HP **end** (plan 12 B3), and 0.1 is the corrected number.  ⚠ It is
`@M012` because the shape recurs: **a rate priced against a wall's nominal HP is
priced against a hex nobody attacks.**

⚠ **And one negative control had to be redesigned before it could fail.**  The
obvious mixed-wave control — two miners, two scouts, must land between the two
pure waves — measured **20 ticks, exactly what four pure miners do**, because
the breach clock SATURATES past a handful of attackers.  Putting the scouts
FIRST in the roster is what makes it discriminating: a rate read once off the
roster reads 0.1 and needs ~460 ticks, where a per-enemy rate lets the miners
behind them chew at 3.0.  ⚠ A control that cannot fail is the thing
`plans/README.md` § A self-test is not validation refuses, and it took a
measurement to notice.

⚠ **This is item 1 of [`ROADMAP.md`](../ROADMAP.md) § The critical path** — the
cheapest gap in the whole design and the widest.  `spawn.loft` says it plainly:
*"the validation tier still emits only regulars"*, so **every wave is the same
wave**.  Until that changes, wave composition is a readout of one symbol
(`@X023`), no sortie can predict anything, and the intel layer exploration is
designed around has nothing to report.

⚠ **The roadmap's own estimate is HALF right, and this plan exists because of
the other half.**  *"One row each in `numbers.json` plus one branch in the
damage-to-wall lookup"* is exactly true of the wall-damage axis — see § Why K0
is as cheap as advertised.  But the design commits **two** axes, and the second
is SPEED: `DESIGN.md` § Speed must NOT be tied to the tick is a direct
instruction from the owner, and honouring it breaks a derivation the engine
rests on (`TICK_SECONDS = 1 / ENEMY_SPEED_HEX_PER_SECOND`, `spawn.loft:94`).
That is a whole phase with the corpus as its gate, not a row in a file.

## Goal

Four small-robot classes the game can emit, that differ in what they do to a
wall and in how fast they arrive — with no second mover, no second targeting
rule and no new code path.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § Small robots: four roles, one AI —
  the commitment (owner, 2026-08-13), and § Speed must NOT be tied to the tick.
- [`docs/ROBOT_ECONOMY.md`](../../docs/ROBOT_ECONOMY.md) § — *"composition is a
  readout"*; the roles are economic, and this plan is what makes the readout
  read more than one symbol.
- `src/passable.loft` — `ENEMY_KIND_*`, `climb_limit`, `enemy_kind_name`.
- `src/damage.loft` — `enemy_max_hp`, `body_source`, `enemy_height`.
- `src/spawn.loft` — `ENEMY_DAMAGE_TO_WALL_PER_SECOND` (`:141`), `wave_damage`'s
  `wd_bite` (`:1164`), `spawn_wave`'s `kind` parameter, `Enemy`, `TICK_SECONDS`.
- `src/waves.loft` — `WaveFile` / `wave_list_default`, ⚠ **`vector<integer>`
  today**: a wave is a COUNT with no class in it.
- `src/helper.loft` — `helper_bank` (`:267`), the banked-progress pattern K2a
  copies, epsilon included.
- `src/script.loft` — the class is NAMED and never numbered, in two places
  (`wave` at `:1655`, `place` at `:1802`).

## ⚠ Why K0 is as cheap as advertised — the code was SHAPED for it

Every per-class question in the engine is already written as a lookup whose
comment says what this plan does.  Nothing here is a refactor:

| site | what it says today |
|---|---|
| `spawn.loft:123` | *"one rate for every class … when insects get a section they get a rate, and this becomes a lookup like `climb_limit`"* |
| `spawn.loft:129` | *"four enemy types cost one row each in `numbers.json` and one branch here, and **no new behaviour anywhere** — which is the point of the rule"* |
| `damage.loft:252` | `enemy_max_hp` — *"they get a row, and this becomes a table like `climb_limit`"* |
| `damage.loft:277` | `enemy_height` — *"the day a class gets its own section it gets a row here"* |
| `passable.loft:93` | *"new variants APPEND — never reorder, the numbers are stored in enemy records"* |

⚠ **So the gate is not "does it compile", it is "does a wall know the
difference"** — and a scenario is what says so.

## The two axes, and what each one costs

| axis | design | mechanism | cost |
|---|---|---|---|
| **damage to a wall** | *"a miner cuts rock for a living; a scout has no real weapon at all"* | one branch in `wave_damage`'s rate | **K0** — a row each |
| **speed** | *"the scout is quite a bit faster than the others"* | banked progress per enemy | **K2** — a phase, gated on the corpus |

⚠ **A third axis arrives free and is not a third mechanism.**  `loot_rate` keys
on the rubble SOURCE (`wallet.loft:185`) and `body_source(kind)` already maps
class → source (`damage.loft:268`), so a class whose body is a distinct source
is worth distinct salvage **through two lookups that already compose**.  That is
the obvious fill for `DESIGN.md`'s one stated hole — *"the harvester is the one
role without a stated mechanical distinction yet … a hauler's obvious axis is
what it CARRIES, which would make it the richest salvage on the field"* — and
§ Open questions 1 is whether to take it.

## Invariant gate

| phase | concrete expected result | invariant it pins | negative control |
|---|---|---|---|
| **K0** | a 100 HP braced wall under 4 miners at 3 HP/s breaks at a measurably earlier tick than under 4 scouts at 0.2 HP/s — and the scouts do **not** break it at all within the scenario | *the class reaches the wall* — a per-class rate is READ, not defaulted | ⚠ `place 0 3 hauler` (an unknown name) must be **refused**, never silently a robot — `script.loft`'s existing rule, extended |
| **K1** | ✅ a wave authored `8 miner` arrives as 8 enemies of kind miner, counted at the marker | *composition is conserved from the list to the roster* | ⚠ **the named control was DELETED** — *a mix whose parts do not sum to the wave's count* cannot be built, because the count is not stored (`@X055`).  What replaced it: composing 12 as `3 miner 2 scout` gives a wave of **five**; an unknown class, `vehicle`, a wave index off the end, an odd token count and an empty composition are each refused; and a composed list must survive **emit → replay → `state_diff` identical** |
| **K2a** | ✅ **the whole corpus is unchanged** — 1128 tests, 30 scripts / 569 measurements, every arrival tick identical, with banking in the mover and every class still at 1.5 hex/s | *banked movement at 1× is the identity* | ⚠ **the named control was necessary and NOT sufficient**, and measuring it is the phase's finding.  Banking a hair under a whole hex and requiring no step is built (§ The guard can fire, both directions) — but at 1.5 hex/s the tick arithmetic is EXACT, so the epsilon is unreachable from any scenario and its removal leaves 1128 tests + 569 measurements green (`@M014`).  What makes the guard gateable is banking at a speed the constant does not take (`@M013`), which is why the speed is an argument |
| **K2b** | ✅ nine hexes of one corridor in **6 / 9 / 14** ticks — scout / robot / miner, the ratio of 2.5 / 1.5 / 1.0 within ±1 tick (`@M016`) | *speed is a rate, not a tick count* | ⚠ **the named control was already paid for and is not what the phase risked.**  *A class at 0.0 hex/s must never advance and must not divide by zero* was K2a's `test_a_zero_timestep_moves_nothing`, because the bank has no division in it at all.  What K2b risked is a speed read ONCE for the roster — `wave_damage`'s hoisted bite, one axis over — and no single-class scenario can see it: the control is a scout and a miner in ONE roster whose gap must OPEN from three hexes to nine.  ⚠ The second is that harvester and builder did NOT move, which a lookup answering 2.5 for every small robot would pass everything else |
| **K3** | three waves of equal SIZE and different composition give three different clocks | *composition is legible in the outcome* | — (a measurement phase) |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **K0** — the four classes as DATA | S | `tests/23_k0_the_classes.loft` (11 tests) + `a-wall-against-a-miner.keys` / `a-wall-against-a-scout.keys` — the same 15 HP stub, one word apart, rubble in one run and 12.8 HP in the other | ✅ **Shipped** |
| **K1** — a wave has COMPOSITION | M | `tests/23_k1_composition.loft` (21 tests) — the roster counted BY KIND matches the list, in the ORDER written; the default list still plays IDENTICALLY; and a composed list round-trips through `emit_keys` | ✅ **Shipped** |
| **K2a** — speed is BANKED, and nothing moves | M | ⚠ **the corpus is the gate**: 1128 pre-K2a tests green + 569 measurements unchanged, with the coupling broken — plus `tests/23_k2a_banked_movement.loft` (10 tests) for the three things the corpus CANNOT see: the guard, the speeds that need it, and the timestep being free | ✅ **Shipped** |
| **K2b** — the scout is FASTER | S | `tests/23_k2b_the_scout.loft` (11 tests) + `three-speeds-one-corridor.keys` — three classes, one corridor, one tick loop, 15 / 9 / 6 hexes after nine ticks.  ⚠ K1's `test_speed_is_still_the_same_for_every_class` broke as planned and is kept INVERTED; `12_b0`'s tick-is-a-hex test did NOT break, because it is written about the regular | ✅ **Shipped** |
| **K3** — what composition is WORTH | S | three equal-size waves, three compositions, three measured clocks — the repo's standard closing measurement.  ⚠ `compose` is what authors them, so this is a `.keys` phase with no new code.  ⚠ Since K2b the clock contains BOTH axes: a composed wave differs in when it arrives as well as in what it does when it gets there | Ready |

⚠ **K2 is cut in two on purpose, and the seam is where the safety is**
(`plans/README.md` § What makes a step SAFE).  K2a changes the *mechanism* with
every number held at today's value, so the OLD path and the NEW one produce
bit-identical results and the comparison is 1094 tests + 520 measurements — the
strongest gate in the repo.  K2b then moves ONE number against a corpus that has
already certified the mechanism.  Fold them together and a moved measurement has
two possible causes.

## What this plan does NOT build

- **A shorter tick.**  K2a breaks the *coupling* between the tick and the enemy
  speed; it does not change the timestep.  Shortening it shrinks the per-tick
  budget in direct proportion and is [`plan 22`](../22-the-field-cache/README.md)'s
  trigger — ⚠ *"the one nobody would look for"*.  A separate decision.
- **Speed per CONDITION.**  *"A damaged robot moves slower"* is the design's
  second source of variety and the one that makes speed a running quantity.  K2a
  makes it possible (progress is banked per enemy); nothing here reads `taken`.
- **The boss.**  A 2×2 footprint is a different question from a class row.
- **Insects.**  `ENEMY_KIND_INSECT` exists and stays as it is; this plan is the
  four SMALL ROBOTS, which are all `climb_limit` 2.0.

## Open questions

1. ✅ **Does the harvester take the salvage axis?**  **YES** — owner,
   2026-08-15: *"richest salvage is fine, we will eventually design more robots
   but these are the basics"*.  So the hole `DESIGN.md` § Small robots left open
   is filled the way that doc floats it — *what it CARRIES* — and it costs one
   rubble source plus one row, because `body_source` + `loot_rate` already
   compose.  ⚠ **And the second half of that answer is a constraint on the
   shape**: these four are the BASICS and more classes are coming, so a role
   that needs anything but a row has broken the rule before the roster is even
   full.
2. **Do the four classes need four `numbers.json` sections, or one section with
   four rows?**  Today there is one `enemy_regular`.  Four sections repeat
   eleven fields each to vary two; one table keyed by role varies what differs.
   **Decided in K0**, and `docs/NUMBERS.md` owns the answer.
3. ✅ **Does a wave row carry a mix, or does the LIST carry rows per class?**
   **A MIX** — a flat `vector<WavePart>` keyed by wave index, size SUMMED
   (`@X055`).  The economy this list is a placeholder for sends convoys, not
   single-class batches, and K0 already measured that a mix's ORDER is worth
   20x on a breach clock — so a class per wave would have thrown away the
   axis the roles exist for.
   ⚠ **The 6-field cap turned out not to constrain it, because the FILE does
   not move** (`@X057`): nothing in `src/` loads `WaveFile`, and the shape it
   would need is the `vector<Struct>` cast that hangs.  Composition is
   authored in the `.keys` vocabulary — `schedule` arms, `compose` fills
   (`@X056`).
