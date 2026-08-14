<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 16 — The wave system: a base that lasts long enough to play

**Value:** `G` · **Effort:** `M`

## Status

**W0 + W1 shipped** (2026-08-14).  W2 is next.  Suite **915 green**,
gate **25 scripts / 447 measurements**.

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
| **W2** — pre-walk visibility: 5 s standing at the marker | S | enemies exist and do not move for 7.5 ticks after a wave begins; the wave's arrival is that much later | Planned |
| **W3** — the provocation trigger | S | driving onto a far spawn marker starts wave 1; a near one never does | Planned |
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
| **W2** | a wave stands still for 7.5 ticks, then walks | the window is the player's information, so it costs the enemies time rather than being cosmetic | enemies that move on the tick they spawn have no window at all, and every arrival clock is 7.5 ticks early |
| **W3** | a far marker driven onto starts wave 1, a near one does not | provocation is a POSITION, the rule § 11 wants | a trigger with no distance test fires on the marker beside the core, which the design silences on purpose |
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
