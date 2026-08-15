<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 19 — The interactive loop: a game you can play

**Value:** `G` · **Effort:** `H`

## Status

**P0 + P1 done** (2026-08-15).  P2 is next.  Suite **1065** green, gate
28 scripts / **520 measurements unchanged**.

**P1 built the seam — and FALSIFIED P0 while doing it.**  P0 said an
accumulator reproduces `tick N` exactly, so one door taking SECONDS
could serve both callers.  It does not: `play_advance(n × TICK_SECONDS)`
is **one tick short for 602 of the first 1000 values of `n`**.  P0
measured n = 30, which is in the lucky 398.  § P1.

⚠ **What survives from P0 is narrower and is still the load-bearing
half**: the frame SIZE does not reach the simulation, because the
remainder CARRIES.  1200 frames of 1/60 s and one `tick 30` reach an
S0-identical state, measured over a real scenario.

So the game has ONE tick and **two ways to ask for it** — a COUNT
(`play_ticks`, what the 520 measurements are pinned to) and a DURATION
(`play_advance`, what a frame has).  The one-seam rule was never "one
entry point"; it is *one caller of `wave_tick`*, and `play_one_tick` is
it.

⚠ **dryopea still cannot be played.**  `src/main.loft` opens a window
and runs the EDITOR — P1 built the door the window will drive, and P3
is what opens it.

⚠ **dryopea cannot be played.**  `src/main.loft` opens a window and runs
the EDITOR — paint, markers, camera, save.  The game lives entirely
inside `script_run`: `WaveState` rides on `ScriptRun`, and
`editor_step.loft` does not mention it, because *"a session that is
being edited has no enemies in it"*.  So every wave, every tower, every
clock in this repo has been measured by a `.keys` file and never once
played by a person.

## Goal

The game runs in the window: waves arrive on their own clock, the player
drives, and the same seam a `.keys` script drives is the one the
keyboard drives.

## Why this is the highest-value thing left

Plan 17 T3 measured a base clearing the authored seven-wave list, and
plan 16 W4 measured what a retrieval is worth.  ⚠ **Every one of those
numbers describes a game nobody has played.**  `docs/NUMBERS.md` § Design
targets says so outright about the first of them — *"NOT gateable, and it
needs a PLAYER … an unattended base is a measuring instrument rather than
the game"*.

And [plan 18](../18-scenario-capture/README.md) is finished and half
idle: it can capture a situation, crop it and reduce it, but there is no
live session to capture FROM.  P5 is its payoff.

## Anchors

Implements, and does not restate:

- [`plans/05`](../05-validation-scenario/README.md) — the integration
  spec for the whole playable milestone (input scheme, camera, HUD, win
  and loss).  ⚠ This plan builds the LOOP that spec assumes; plan 05
  stays the integration document and is not superseded.
- [`plans/08`](../08-game-validation/README.md) § V0 — *EVERY action
  runs through ONE seam*, and [`plans/09`](../09-lattice-conversion/README.md)
  § I1 — the one key table, which is why P1 and P2 are shaped as they
  are.
- `src/main.loft` (the GL shell), `src/editor_step.loft` (the editor's
  seam), `src/script.loft` (the `tick` verb), `src/bindings.loft`.

### ⚠⚠ The rule this plan exists to keep: ONE seam, two callers

Plan 09 I1 records what happens without it: before the `.keys` runner
typed on the same key table the GL loop reads, *"a binding could be
wrong in the editor with all 14 scripts green"*.

The same trap is open here, one level up.  If the GL loop advances the
game itself, then **a script and a player play different games** — and
the 520 measurements in `scripts/validate.sh` would stop describing
what a person experiences, silently.

⚠⚠ **P1 sharpened this, and the sharpening matters.**  The rule is *one
caller of `wave_tick`*, not *one entry point*.  A script asks for a
COUNT and a frame asks for a DURATION, and those cannot be folded
together without corrupting the count (§ P1: 602 of 1000).  What must
not fork is the TICK, and `play_one_tick` is it.

⚠ **A frame's door takes SECONDS, not frames and not ticks.**  A frame
says *16 ms passed*, so it must bank elapsed time and decide for itself
how many whole ticks that is — which makes P0's question the one that
can kill the design for a window.  It survived; what did not survive is
using the same arithmetic to serve `tick N`.

## P0 — can real time reproduce `tick N`? (2026-08-15)

The tick is 2/3 s and a frame is not, so the loop must bank elapsed
seconds and spend whole ticks out of them — the pattern `helper.loft`,
`tower.loft` and `vehicle.loft` already use, arriving a fourth time.
Two things had to hold or the one-door design was wrong:

1. **`n × TICK_SECONDS` through the accumulator is exactly `n` ticks.**
   Otherwise every scripted measurement in the suite disagrees with the
   played game.
2. **The frame SIZE does not change the answer**, or the simulation is
   frame-rate dependent and no measurement means anything.

Yes to both, in every case, with an empty `state_diff` against the scripted run:

| how the same 30 ticks were delivered | differs from `tick 30` |
|---|---|
| one frame of 20 s | — |
| 30 frames of 2/3 s | — |
| **1200 frames of 1/60 s** | — |
| 600 frames of 1/30 s | — |
| 200 frames of 0.1 s | — |

⚠ **So the frame size does not reach the simulation**, which is the
property an interactive loop has to have and the one nobody had checked.

### ⚠⚠ A REPEATING accumulator is immune to the trap a ONE-SHOT timer suffers

This looks like the banked-timer question plan 15 C0 found and plan 17
T1 sharpened, and it is not — the distinction is worth keeping:

| | one-shot (`helper.recover`) | repeating (the frame accumulator) |
|---|---|---|
| the boundary | decides ONCE, for ever | decides every frame |
| an ulp short | costs a whole tick, permanently | defers one tick to the next frame |
| the residue | is lost | **carries** |

1200 frames of 1/60 s accumulate to 20.000000000000146 rather than
20.0 — and it makes no difference, because the excess is 1.5e-13
against a tick of 0.667 and the remainder is kept either way.

⚠ **So there is no epsilon here, and its absence is a DECISION.**
Measured with one at 1e-9 and the answer is identical.  Adding one would
be the cargo-cult plan 16 W0 refused, and the note is here so a reader
comparing this with `helper.loft` does not "fix" it.

### ⚠⚠ Where P0 over-generalised — read § P1 before quoting the table

The table above is true and its conclusion was too strong.  Every row
delivers **20.0 s**, and 20.0 s happens to be one of the elapsed times
that recovers its tick count exactly.  P0 concluded *`n × TICK_SECONDS`
through the accumulator is exactly `n` ticks* from a single `n`, and
that is false for 602 of the first 1000.  P1 measured the sweep.

⚠ The methodological cost is worth naming: the probe varied the FRAME
SIZE (five values) and held `n` fixed at one.  Both were free to vary,
and the one held fixed is where the defect was.

## P1 — the play session and its ONE door (2026-08-15)

`src/play.loft`.  `PlayState` (the roster, the clock, the banked
seconds), `play_ticks`, `play_advance`, `play_step`, `play_core` — and
`play_one_tick`, the only call to `wave_tick` in the repo.

### ⚠⚠ A COUNT is not a DURATION, and 602 of 1000 say so

The one-door design assumed `tick N` could be spelled
`play_advance(N × TICK_SECONDS)`.  Built that way, the gate went red at
once — and read as a GAME event rather than as a clock being a hair
off:

```
validate: FAILED — a-base-that-plays-its-list line 76: waves = 0, outside 1..1
```

⚠ **One tick short, at the end, is what a float defect looks like from
outside**: not a number slightly wrong, but a wave that never arrives.

Swept, the shape is not marginal:

| how `n` ticks were asked for | wrong for `n` in 1..1000 |
|---|---|
| `play_ticks(n)` | **0** |
| `play_advance(n × TICK_SECONDS)` | **602** — always one SHORT |

⚠ **And "make the product exact" is not the fix.**  `n = 12` gives a
product of exactly `8.0` and still comes back **11**: it is the
subtraction chain, not the multiplication.  Eleven subtractions of a
repeating fraction leave a remainder a hair under the twelfth.

⚠ **Nor is an epsilon.**  It moves WHICH 602 values are wrong, not how
many.  A count is exact because it is counted.

So the door split, and the split is the finding:

* `play_ticks(ps, s, n)` — a COUNT.  `tick` and `fall` ask this, and
  every one of the 520 gate measurements is pinned to it.
* `play_advance(ps, s, seconds)` — a DURATION.  A GL frame asks this,
  and it is the only one that banks a remainder.
* `play_step(ps, s, input, seconds)` — a FRAME: the editor seam, then
  the duration.  Both the window and every scripted frame call it.

⚠ **`play_ticks` does not touch `banked`**, so mixing the doors cannot
make a scripted tick steal part of a frame — a drift that would be
sub-tick and therefore invisible to the whole corpus.

### ⚠ Why a `tick` is not routed through `play_step` either

A script's `tick 30` has no keypress in it.  Through the frame door it
would have to fabricate a neutral `EditorInput`, and `editor_step` reads
a neutral frame as *the player let go*: it commits the in-flight paint
stroke and resets the edge history.  `tick` would silently release the
player's mouse.

### ⚠⚠ Open question 1, answered by the compiler: a nested struct is a COPY

The plan recommended a `PlayState` composing an `EditorState` and a
`WaveState`.  **The first half cannot be written.**  A struct stored in
a field of another struct is copied, not aliased — loft says so itself:

```
advice[avoidable-copy]: copy of Inner — `i` is still used after this point
```

So a `PlayState` built around the session the caller already holds would
fork the world in two: `script_run(s, …)` mutates `s`, the game reads
`ps.ed`, and nothing exists that could see them drift.  ⚠ A field read as
a PARAMETER *does* alias (measured, two levels deep), so owning the state
outright would work — but only by inverting `script_run`, whose entire
contract is that the caller keeps the session and asserts on it after.

**The answer: the world is PASSED, the game is OWNED.**  `PlayState` is
what a live session has that an edited one does not — a roster, a clock,
and the seconds banked toward the next tick.

### ⚠ Open question 3, answered: `ScriptRun.ticks` MOVED rather than staying

It is `run.play.ticks` now, and plan 12 B7's clock is unchanged in
meaning.  It could not stay a field beside a `WaveState`: `play_ticks`
takes a `PlayState`, so a run holding the pieces separately could not
reach the door at all without handing over copies — which is the
two-games defect the file exists to prevent.  ⚠ It is a count of TICKS,
not of seconds; the accumulator changes who decides WHEN a tick happens,
not what one IS.

### What the gate said

* `scripts/validate.sh` — **28 scripts, 520 measurements, unchanged.**
  This is the phase's real gate and it caught the one defect there was.
* `scripts/test.sh` — 1065 green (1051 + `tests/19_p1_the_seam.loft`).
* ⚠ `test_a_count_asked_for_in_seconds_comes_back_short` asserts the
  **wrong** answer on purpose: 602, by name at n = 10, 12, 40.  It is
  the guard that stops anyone folding the two doors back together, and
  if a loft release ever makes it exact the redness is good news.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **P0** ✓ | `tick 30` reproduced from 1, 30, 200, 600 and 1200 frames — every one an empty `state_diff` | ⚠ claimed *an accumulator is a REFINEMENT of `tick N`* — **too strong, corrected by P1**; what holds is that the frame SIZE does not reach the simulation | ✓ the falsifier was live for the frame size; ✗ **not for `n`** — five frame sizes over ONE tick count, and the defect was in the variable held fixed |
| **P1** ✓ | 520 gate measurements unchanged; suite 1065 | ONE caller of `wave_tick` — asked by COUNT or by DURATION, never one folded into the other | ⚠ **the control FIRED**: routing `tick` through the seconds door went red as `a-base-that-plays-its-list … waves = 0, outside 1..1`, a wave that never arrives rather than a clock a hair off |
| **P2** | a `.keys` script and a keypress produce the same action | one key table, the I1 shape | ✗ a play action wired straight into the GL loop is invisible to every script — the exact defect I1 was built to stop |
| **P3** | waves arrive in a window, on the wall clock | the loop owns the CLOCK and the seam owns the rules (`editor_step`'s existing split) | a frame-rate-dependent simulation: assert the same elapsed time gives the same state at two frame rates |
| **P4** | enemies, the vehicle and the crew are drawn | measured frames, not goldens | ⚠ a golden AGREES WITH A SHEAR, so the gate is `classify_world` pixel shares — a thing not drawn reads as zero |
| **P5** | a key writes the live situation as `.keys` | plan 18's emitter, driven from a session rather than a script | the emitted file must REPLAY to an S0-identical state |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **P0** — the probe: can real time reproduce `tick N`? | XS | the table in § P0, measured against the shipped sim over five frame sizes | **Done** |
| **P1** — the play session and its ONE door | M | `tests/19_p1_the_seam.loft` (14 fns) + the whole gate: `play.loft` owns the only `wave_tick` call, `tick` / `fall` / every scripted frame route through it, and **all 520 measurements are unchanged** | **Done** |
| **P2** — play actions in the ONE key table | S | `tests/19_p2_the_keys.loft` — drive / boost / take / drop become `EditorAction` rows, and a script pressing the key does what the verb does.  ⚠ They apply in `play_step`, not `editor_step`: the editor's seam has no roster | Open |
| **P3** — the loop: the game runs in the window | M | `tests/19_p3_the_clock.loft` for the accumulator + a human check that `make play` plays.  ⚠ The headless half is what CI can hold | Blocked on P2 |
| **P4** — drawing the game | M | `tests/19_p4_the_frame.loft` — `classify_world` pixel shares for enemies, the vehicle and the crew.  A `.keys` scenario with `snap` for human inspection | Blocked on P3 |
| **P5** — capture from a live session | S | `tests/19_p5_capture.loft` — a key writes the situation, and the file replays to an S0-identical state ([plan 18](../18-scenario-capture/README.md)) | Blocked on P3 |

### Why the order is this order

**P0 first** because it can kill the design for the price of a probe,
and because everything after it assumes one door taking seconds.

**P1 before P2** because a key with nothing to drive is a row in a table
that cannot fail — and P1's gate is the strongest in the repo: 520
measurements that already pin the game's behaviour exactly.  ⚠ A seam
that changed anything shows up in 28 scenarios at once.

**P3 after P2** so the window has actions to send.  ⚠ Its gate is split
deliberately: the ACCUMULATOR is testable headlessly and the window is
not, so the part CI can hold is separated from the part a person checks.

**P4 and P5 both after P3** and independent of each other.

## What this plan does NOT build

**No HUD, no camera work, no landing flow, no menus** — those are
[`plans/05`](../05-validation-scenario/README.md)'s integration spec,
and most of them need art and UI decisions this plan would be inventing.

**No building** — walls and towers are placed in the EDITOR today, and a
construction site is a concept dryopea lacks (plan 14 § What this plan
does NOT build).  The loop plays the base as authored.

**No scramble**, which is the run's ending and needs the carry model's
third consumer plus a next-base to carry into.

**No multiplayer, no save-mid-game.**  ⚠ Plan 18's emitter is the
nearest thing and it is deliberately a FIXTURE writer, not a save file —
a captured situation is a starting position, not a resumable game.

## Open questions

1. ~~**Where does `WaveState` live in a live session?**~~  **Answered by
   P1, and not the way the recommendation guessed.**  A `PlayState`
   cannot COMPOSE an `EditorState`: a struct in a field is a COPY, so
   the world would fork.  The world is PASSED and the game is OWNED —
   `PlayState { wave, banked, ticks }`, and `ScriptRun` holds one.
   § Open question 1, answered by the compiler.
2. **Does the editor become a MODE of the game, or the game a mode of
   the editor?**  Today `main.loft` is an editor that could gain a play
   mode; the shipped game is a game that has no editor.  *Recommendation:
   a mode flag in the shell for now and no decision baked into the seam,
   because the answer changes when a landing flow exists (plan 05) and
   nothing here should pre-empt it.*
3. ~~**What happens to `ScriptRun.ticks` when the clock is real?**~~
   **Answered by P1:** it is a count of TICKS, as recommended — and it
   MOVED, to `run.play.ticks`, because a run holding the wave and the
   clock as separate fields could not reach `play_ticks` without handing
   over copies of both.  § Open question 3, answered.
4. **Can a person actually play it?**  The one question no test answers.
   ⚠ `docs/NUMBERS.md` § Design targets has a target that has been
   ungateable since it was written — *"a single base session ≈ 15-25
   minutes"* — and P3 is the first time it could be checked at all.
   *Recommendation: check it by playing, and write the number down
   wherever it lands.*

## See also

- [`plans/05-validation-scenario`](../05-validation-scenario/README.md) —
  the integration spec this loop serves.
- [`plans/18-scenario-capture`](../18-scenario-capture/README.md) — built,
  and waiting for P5.
- [`plans/08-game-validation`](../08-game-validation/README.md) § V0 —
  the one-seam rule, one level down.
