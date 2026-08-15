<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 23 — The small robots: four roles, one AI

**Value:** `G` · **Effort:** `M`

## Status

**K0 + K1 shipped** (2026-08-15).  K2a is next.

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
| **K2a** | **the whole corpus is unchanged** — 1094 tests, 520 measurements, every arrival tick identical, with banking in the mover and every class still at 1.5 hex/s | *banked movement at 1× is the identity* | ⚠ an epsilon whose removal leaves the suite green is a guard that cannot fire (`17-T1`) — bank a hair under a whole hex and require no step |
| **K2b** | a scout wave crosses a measured corridor in the ratio of its speed to a robot's, ±1 tick | *speed is a rate, not a tick count* | a class at 0.0 hex/s must never advance and must not divide by zero |
| **K3** | three waves of equal SIZE and different composition give three different clocks | *composition is legible in the outcome* | — (a measurement phase) |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **K0** — the four classes as DATA | S | `tests/23_k0_the_classes.loft` (11 tests) + `a-wall-against-a-miner.keys` / `a-wall-against-a-scout.keys` — the same 15 HP stub, one word apart, rubble in one run and 12.8 HP in the other | ✅ **Shipped** |
| **K1** — a wave has COMPOSITION | M | `tests/23_k1_composition.loft` (21 tests) — the roster counted BY KIND matches the list, in the ORDER written; the default list still plays IDENTICALLY; and a composed list round-trips through `emit_keys` | ✅ **Shipped** |
| **K2a** — speed is BANKED, and nothing moves | M | ⚠ **the corpus is the gate**: `scripts/test.sh` 1128 green + `scripts/validate.sh` 569 measurements unchanged, with the coupling broken | Ready |
| **K2b** — the scout is FASTER | S | a two-class corridor scenario; arrival ticks in the speed ratio.  ⚠ `tests/23_k1_composition.loft` § test_speed_is_still_the_same_for_every_class is the "not yet" pin it has to break | Blocked on K2a |
| **K3** — what composition is WORTH | S | three equal-size waves, three compositions, three measured clocks — the repo's standard closing measurement.  ⚠ `compose` is what authors them, so this is a `.keys` phase with no new code | Blocked on K2b |

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
