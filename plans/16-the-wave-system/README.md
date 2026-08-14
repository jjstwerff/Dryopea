<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 16 — The wave system: a base that lasts long enough to play

**Value:** `G` · **Effort:** `M`

## Status

**W0 + W1 + W2 + W3 shipped** (2026-08-14).  W4 is next.  Suite **938
green**, gate **26 scripts / 478 measurements**.

⚠ **The whole plan is scaffolding.**  Project owner, 2026-08-14: *"it
will be gone before the first game ships as I want natural patterns
instead of waves/spawn points anyway"*, and *"when a wave begins should
not be an issue at all, only points away from the base should spawn"*.
`DESIGN.md` § Wave list already called the authored list a placeholder;
the **spawn-marker model goes with it**.  So this plan exists to make a
base long enough to MEASURE, and nothing in it is worth polishing —
when natural patterns arrive, expect `waves.loft` and `spawn.loft`'s
marker round-robin to be deleted rather than migrated.

### W3 — the provocation trigger: a run that starts itself

Until W3 something OUTSIDE the simulation started every wave: a `.keys`
line saying `wave 8`, or W1's `schedule` verb calling
`wave_schedule_begin` on the run's behalf.  A run had a list and no way
of its own to begin playing it.

`wave_provoke_step` is that way, and it goes in front of
`wave_schedule_step` at the top of the tick — two lines that read as one
statement: *has anybody poked a far spawn marker, and is a wave due*.

⚠ **Provocation is a POSITION, which is the whole of it.**  No key, no
menu, no toggle: you drive out there or you do not, and driving out
there means being fourteen hexes from home at the moment the list stands
up.  It is the shape `DESIGN.md` § 11 wants of everything the player
does, and a trigger the player PRESSED would hand out the same
information for nothing.

#### ⚠ Two thresholds, and the band between them is the phase

| distance from core | emits enemies | can be poked |
|---|---|---|
| < 10 (`SPAWN_DISABLE_RADIUS`) | no | no |
| 10, 11 | **yes** | no |
| ≥ 12 (`WAVE_1_PROVOCATION_HEXES`) | yes | yes |

The middle band is a **live spawn source that is safe to stand on**, and
it is the only reason the distance test is a rule rather than a
restatement of "is this marker active at all".  Both the test file and
`provoking-the-far-marker.keys` use a marker at 11 as the control, so a
trigger that fired on any spawn marker goes red on a marker that really
does send the wave.  ⚠ If anyone ever tunes the two numbers together the
phase becomes untestable — `test_the_two_thresholds_leave_a_band_
between_them` is what says so.

#### ⚠ The CREW pokes too, and that extends the design's letter

`DESIGN.md` § Wave-1 triggers says *"the **player** has driven onto"*.
Everywhere else in this engine the crew is the player's chassis doing
the player's job — `wave_blockers` puts both in one map, `salvage_at` is
one implementation for both — and exempting helpers would make the safe
way to scout a far marker *send somebody else*: a free look at exactly
the information W2's window charges eight ticks for.  ⚠ Recorded as a
deliberate divergence, beside the round-robin one in § What this plan
does NOT build, and it wants confirming when the plan closes.

A WRECK pokes nothing (`alive` is the wreck, plan 14 H3 — the same
filter `wave_blockers` applies), and neither does an **enemy**: the rule
asks the roster of VEHICLES and never "is anybody on this hex", because
a wave spawns ON its marker and stands there for eight ticks.  A hex
test would let a scripted wave 1 provoke wave 1.

#### ⚠ It fires once, and it is read at the TOP of the tick

Nothing stops a player parking on the marker, so `running` is the guard.
Re-arming would call `wave_schedule_begin` again, which zeroes the lull
— the fifteen seconds W1 counted — and the list would empty as fast as
the base could clear it.  The test watches the lull sit at 15.0 s for
ten ticks with the player still standing there.

And the trigger reads the world the tick STARTED with, exactly as the
fields, the occupancy and the move order do: a player arriving mid-tick
is seen by the NEXT one.  Measured as a relation rather than a tick
number — `fired == arrived + 1` — so it survives any change to the
vehicle's speed.

#### What it moved: nothing, and that is the assertion

Every clock in the suite is unchanged, because the trigger asks a
POSITION no pre-W3 scenario occupies — the furthest any of them drives
is six hexes and every spawn marker is at eleven or beyond.  ⚠ It is a
test rather than a hope: `test_a_run_with_nobody_on_a_marker_is_
untouched` ticks an armed base for 40 ticks with the player at the core
and asserts the list never starts.

⚠ **`schedule <counts…>` now ARMS rather than starts.**  The verb
authors the run's list and stops there, so a `.keys` scenario begins its
own waves by driving onto a marker exactly as a player does — a verb
that also started it would be the one shortcut letting every scenario in
the gate skip the rule.  It prints what it did, so a list nobody pokes
does not read as a broken schedule.

### W2 — the pre-walk window, and what it moved

A wave stands at its marker for **8 ticks** and steps on the 9th.  One
float on `Enemy`, written by `spawn_wave` so both spawners have it, and
spent by `wave_stand` at the end of the tick.

⚠ **Eight, not seven, and the reason is a rule worth keeping.**  5.0 s
is 7.5 ticks, so it has to round.  A banked timer spans `ceil(T/dt)`
ticks if its reader asks BEFORE the spend and one fewer if it asks
after — the two forms differ by exactly one tick, always, and they
coincide only where `T` divides `dt` exactly.  **That is precisely the
case plan 15 C0's epsilon is about**, so the two questions are
complementary and every banked timer has exactly one of them live:

| timer | T / dt | live question |
|---|---|---|
| helper recovery 60.0 s | 90 | the EPSILON (C0) |
| pre-walk window 5.0 s | 7.5 | this ORDERING |

Spending at the END of the tick — where `helper_recover_tick` spends —
gives `ceil` and keeps the design's promise: the window is a guarantee
to the PLAYER, so a fractional tick rounds up.

⚠ **A standing enemy does nothing but stand**: it does not move, it
attacks nothing (`enemy_target` answers its own hex) and nobody is
blocking it.  What it does NOT get is immunity — *"stand VISIBLE"* is
the design's own word, and making it untargetable would be a new
special case in a design that avoids them.

### ⚠⚠ And that is what inverted plan 12 B7

**The window relocates a tower's first kills to the SPAWN MARKER.**  A
wave stands stacked for 8 ticks inside a 15-hex tower range, so the
first bodies pile 1.5 m out there — on a hex that leads nowhere —
instead of at the foot of the wall where they would have been a ramp.
Measured, one wall and one tower:

| base | before W2 | after W2 |
|---|---|---|
| undefended | 61 | **69** |
| sealed wall | 104 | **112** |
| wall + tower | 95 | **128** |

So **a tower is worth +16 ticks where it used to cost 9**, and B7's
headline — *a tower CUTS the clock, because its bodies ramp over the
wall it was defending* — is now conditional on where the kills land
rather than on there being a tower.  ⚠ The mechanic is not deleted:
`a-base-on-two-fronts` still falls to a ramp, because 1.0 m of bodies
is EXACTLY a 3.0 m wall less a 2.0 m climb, so two dead robots still
get the third one over.  It got thin, and it moved a row north.

⚠ **Every untowered clock moved by exactly +8**, which is the control
that says the rest is the window and not the arithmetic.

⚠ **What this cost the older measurements.**  Plan 14 H2's crew
scenarios lose most of their spread (77/214/242 → 123/135/138), because
the crew's value came from clearing ramps that now largely do not form;
plan 13 V2's ramp-versus-no-ramp pair can no longer see the crew at all
(both bases end with the same wall HP), so the crew's remaining 12
ticks are the WALLET rather than the terrain.  Plan 15 C3's six-tick
errand **survives unchanged** (93 vs 87).  Each plan's `## Status`
carries its own corrected numbers.

**W1 made waves arrive on their own.**  `src/waves.loft` holds the
authored list and a schedule that walks it; `wave_tick` asks it first,
before anything surveys the roster.  A defended base now plays wave 1,
clears it, waits out the lull and gets wave 2 with no line of script in
between — `schedule 5 8` and the `waves` band are the verbs.

⚠ **The gate is the GAP between two events, not the arrival of the
second wave.**  Two wrong schedules both put wave 2 on the ground: a
pure timer (which lands it on a base still fighting wave 1) and a clear
with no lull (which deletes the fifteen seconds the player is meant to
spend repairing).  So the test counts the ticks between the clear and
the spawn — **23**, because 15.0 s is 22.5 ticks — and a 400-tick run
on an undefended base asserts that a schedule ignoring the clear would
have emptied a five-wave list by then.

⚠ **An undefended base still never sees wave 2**, which is W0's finding
as an assertion: it cannot kill, so the field never clears, so the lull
never starts, and the run ends with the wallet rather than with the
list.

### ⚠ A loft interpreter SIGABRT, filed as [loft#908](https://github.com/loft-lang/loft/issues/908)

Loading `waves.json` crashed the interpreter — `BUG (#306): refused to
free the stack store … a stack-record ref was treated as an owned heap
store`, then a double free.  Reduced to twelve lines
(`loft_repros/missing_file_struct_return.loft`): **a function that both
reads a MISSING file's content and returns a struct**.  An existing file
is fine, and so is taking the text as a parameter.

⚠ **Interpreter only — the native backend is correct**, which is the
worst direction: both gates run interpreted, so it is a crash the suite
hits and a shipped build would not.

⚠ **It is the same `#306` guard as loft#867, which was fixed on
2026-08-12 and still passes** — so this is a residual neighbouring
shape on the missing-file path rather than a regression, which is what
the issue comment tells the maintainer.

⚠ **And the workaround dryopea already had did NOT apply.**  `CLAUDE.md`
§ Loft language gotchas records the if-else expression form as the fix
for `load_map_or_empty`'s early-return trouble; here both forms crash.
What works is splitting the read from the build — `wave_file_text`
answers `text`, `wave_file_parse` takes it and answers the struct — so
no single function does both.

## Goal

Waves arrive on their own, in the order the design authored, with a
lull between them — so a base is **seven waves long** instead of one.

⚠ **This is the trigger [plan 15](../15-the-carry-model/README.md) C3
named.**  A 60 s helper recovery is priced against `numbers.json`
§ wave_system's seven waves and 15 s lulls; dryopea plays ONE wave,
authored a line at a time by a `.keys` script, so a base ends when the
first wave gets in and a retrieved crew member never comes back.  C3
measured that as 85/79/79 ticks and refused to fix it by shortening the
recovery — this plan is the fix it pointed at.

⚠ **And it is bigger than that one measurement.**  Every clock plans
12, 13, 14 and 15 measured — 61/104/95, 95→145, 77/214/242, 85/79 —
was measured **during wave 1**.  The base the design prices its numbers
against has never been played.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § Wave list (validation
  placeholder), § Wave-1 triggers — walls or provocation, § Pre-walk
  visibility — the scramble decision window, § No wave HUD, § Wave 7
  cleared → free scramble.
- [`examples/waves.json`](../../examples/waves.json) — the authored
  list, and [`examples/numbers.json`](../../examples/numbers.json)
  § `wave_system`.  **This plan adds no new tunable without a row in
  one of those files.**

Source files it touches: `src/spawn.loft` (the schedule and the tick),
`src/script.loft` (the verbs) and a loader for the wave list.

## W0 — the probe (2026-08-14)

No code, three measurements, and the first one decides whether the
design's inter-wave rule is implementable at all.

### ⚠ 1. An undefended base never clears its wave — it dies first

`numbers.json` § wave_system.inter_wave_delay is documented as *"Wall-
clock seconds between the last enemy dying and the next wave's
spawn"*, so the schedule advances on a **clear**.  Measured, on one
band, playing a wave of five to its end:

| base | wave cleared | base fell | alive at the end |
|---|---|---|---|
| undefended | **never** | tick 74 | **5** |
| wall + 1 tower | tick **165** | never | 0 |
| wall + 2 towers | tick 161 | never | 0 |
| wall + 2 towers, wave of 8 | tick 23 | never | 0 |

**So "the last enemy dying" is reachable exactly when the base can
kill**, and a base that cannot clear wave 1 simply loses — the wallet
empties while five robots stand on the core.  That is coherent rather
than a gap: the schedule needs **no fallback timer**, because a wave
that never clears is a run that has already ended.

⚠ **And it makes the plan's own value concrete.**  A defended base is
still standing at tick 165 with the wave dead — where the same base
undefended is over at 74.  A 90-tick recovery has somewhere to live in
the first and nowhere in the second, which is exactly what C3 could not
express.

⚠ The wave-of-8 row clearing in 23 ticks where five took 161 is a
**line-of-sight** artefact, not a balance one: the probe seeds enemies
across a small block of hexes, and a `wall` at 3 m hides distant
targets from a 6 m tower eye until they close (plan 12 B5b).  Recorded
because it looks like a finding and is not — W1's gate must seed a wave
through `spawn_wave` and its markers rather than by hand.

### ⚠ 2. Both wave timers are IMMUNE to the epsilon, and that is worth writing down

Plan 15 C0 found that a banked timer loses a tick exactly when its
duration divides the tick length exactly — 60.0 s is 90 ticks and a
bare `> 0.0` gives 91, while the 5.0 s boost cooldown at 7.5 ticks is
untouched.

Both of this plan's timers are the safe kind:

| timer | ticks | exact? |
|---|---|---|
| `inter_wave_delay` 15.0 s | 22.5 | no — immune |
| `pre_walk_visibility_interval` 5.0 s | 7.5 | no — immune |

⚠ **So an epsilon here would be cargo-cult**, and its absence is a
decision rather than an oversight.  Recorded so that a later reader
comparing this file with `helper.loft` does not "fix" it — and so that
anyone who changes either number to a whole number of seconds knows
they have just moved it into the dangerous class.

### 3. `waves.json` LOADS, so the wave list can be real content

`text as` a struct of one scalar and a `vector<integer>` reads the file
correctly — delay 15, seven waves, 5 through 80.  So the list does
**not** have to be hand-copied into a `.loft` constant the way every
value in `numbers.json` is (`CLAUDE.md` § Reading by goal).

⚠ The known exposure comes with it: `text as` on a `vector<Struct>`
silently answers 0 entries on the NATIVE backend, which is why
`load_palette` is broken there and why both gates run interpreted.
This is a `vector<integer>` and untested on native, exactly like
everything else dryopea loads.

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **W0** — the probe: is the design's inter-wave trigger reachable? | XS | the three tables above, measured against the shipped sim | **Done** |
| **W1** — the schedule: waves arrive on their own | S | `tests/16_w1_the_schedule.loft` — a defended base plays wave 1 then wave 2 with no script line between them, the lull is 23 ticks counted, and an undefended base still dies in wave 1 and never sees wave 2 | **Done** |
| **W2** — pre-walk visibility: 5 s standing at the marker | S | `tests/16_w2_the_window.loft` — a wave stands 8 ticks and steps on the 9th; the SAME base run twice, differing only in the window, arrives exactly 8 ticks apart | **Done** |
| **W3** — the provocation trigger | S | `tests/16_w3_the_provocation.loft` + `tests/scripts/provoking-the-far-marker.keys` — driving onto a marker 14 hexes out starts wave 1; standing on one at 11, which is a LIVE spawn source, never does | **Done** |
| **W4** — re-measure the base at its real length | S | a seven-wave scenario, and what a retrieval is worth on it — the number [plan 15](../15-the-carry-model/README.md) C3 could not produce | Planned |

### Why the order is this order

W1 first because every later phase is a modification of *when* a wave
starts, and there is no "when" until a schedule owns it.  W2 and W3
both change that moment and neither can be gated without it.

⚠ **W3 is the provocation trigger only, and the omission is named.**
`DESIGN.md` gives wave 1 two triggers and the other is *"the player has
built N walls"* — dryopea has no construction (plan 14 § What this plan
does NOT build: *"a construction SITE is a concept dryopea lacks"*), so
a wall trigger would have to count walls PAINTED in the editor, which
is authoring rather than play.  It arrives with construction.

W4 last because it is the measurement the whole plan exists to make
possible, and it needs all three.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **W0** ✓ | the tables above | the schedule advances on a CLEAR, and a wave that never clears is a run that already ended | — (W0 measures; W1 is its gate) |
| **W1** ✓ | a defended base plays waves 1..N unattended; an undefended one dies in wave 1 | the wave list is DATA and the schedule is one state machine over it | ✓ the lull is COUNTED at 23 ticks, so a no-lull schedule is red; ✓ 400 ticks on an unclearable wave sends exactly ONE wave, where a timer would have sent five |
| **W2** ✓ | 8 ticks standing, then a step | the window is the player's information, so it costs the enemies time rather than being cosmetic | ✓ the SAME base with the window and without arrives exactly 8 ticks apart — a cosmetic window leaves both clocks identical; ✓ a tower kills it where it stands, so the window is not a shield |
| **W3** ✓ | a marker at 14 starts wave 1; one at 11 never does, however long it is stood on | provocation is a POSITION, the rule § 11 wants | ✓ the control marker is a LIVE spawn source, so a trigger with no distance test of its own goes red on a marker that really sends the wave; ✓ an enemy on its own marker provokes nothing, so an occupancy test is red; ✓ the lull sits at 15.0 s under a parked player, so a trigger that re-arms is red |
| **W4** | the seven-wave clock, and what a retrieval is worth on it | a base long enough to express a 60 s recovery | a reading taken on a base that still dies in wave 1 has measured nothing new |

## What this plan does NOT build

**No wave HUD** — `DESIGN.md` § No wave HUD is uncompromising about
it: no wave number, no countdown, no banner.  The marker pulse and the
pre-walk window are the entire wave UI, and only the second of those is
a simulation fact.

**No economy-driven waves.**  § Wave list is explicit that the authored
list is a placeholder for waves becoming *output of* the robot economy.
That is a Tier D feature and it needs an economy.

**No wall trigger** (§ Why the order is this order), **no free-scramble
phase** (§ Wave 7 cleared needs a scramble, which dryopea has not
built), and **no random marker selection** — `spawn_wave` picks
round-robin so a scripted run repeats, and the design's *"random at
spawn time"* would make every measurement in the suite non-repeatable.
⚠ That last one is a deliberate divergence from `DESIGN.md`, not an
omission; it wants recording there when this plan closes.

⚠ **The second divergence is W3's crew rule** — § Wave-1 triggers says
the *player* drives onto the marker and the shipped rule accepts any
live vehicle of the cooperative, for the reasons in § W3.  Same
treatment: recorded here, and it wants writing into `DESIGN.md` with the
round-robin one when the plan closes.

## Open questions

1. **What if a wave can neither clear nor kill?**  Enemies with no
   route at all besiege what they cannot climb (plan 11 F7), so a base
   they cannot break and a wallet they cannot reach is a stalemate: the
   schedule waits for a clear that never comes.  W0 did not produce one
   — every walled base in it was broken into or cleared — so it is
   unmeasured rather than impossible.  *Recommendation: let W1's
   schedule stall honestly rather than adding a timeout, and find out
   whether a real map can reach the state at all.*
2. **Does the lull start on the clear, or on the last DEATH?**  They
   differ when the player kills the last enemy long after the rest —
   the design says *"between the last enemy dying and the next wave's
   spawn"*, which is the same thing given the clear is defined by
   deaths.  *Recommendation: one definition, `alive_count == 0`, and
   no second clock.*

## See also

- [`plans/15-the-carry-model`](../15-the-carry-model/README.md) — C3 is
  what named this plan, and W4 is the measurement it could not make.
- [`plans/12-combat-resolution`](../12-combat-resolution/README.md) —
  B7's clocks are the ones W4 re-measures at seven waves.
- [`plans/03-marker-layer-and-spawns`](../03-marker-layer-and-spawns/README.md)
  — `spawn_wave` and the spawn director this schedule drives.
