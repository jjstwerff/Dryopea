<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 19 — The interactive loop: a game you can play

**Value:** `G` · **Effort:** `H`

## Status

**P0 done** (2026-08-15).  P1 is next.  No `src/` change yet; suite 1051
green, gate 28 scripts / 520 measurements.

**P0 tried to kill the design and could not.**  A real-time accumulator
reproduces `tick N` **exactly**, at every frame size tried — and the
reason is structural rather than lucky: the remainder CARRIES, so a tick
deferred by float error is taken on the next frame.  § P0.

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
what a person experiences, silently.  So the game gets one door,
`play_step`, and both callers go through it.

⚠ **The door takes SECONDS, not frames and not ticks.**  A script says
`tick 30`; a frame says *16 ms passed*.  One door can serve both only if
it takes elapsed time and decides for itself how many whole ticks that
is — which makes P0's question the one that can kill the design.

## P0 — can real time reproduce `tick N`? (2026-08-15)

Yes, in every case, with an empty `state_diff` against the scripted run:

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

## ⚠ What P0 had to settle before anything was built

**Can a real-time accumulator reproduce `tick N` exactly?**

The tick is 2/3 s and a frame is not.  So the loop must bank elapsed
seconds and spend whole ticks out of them — the pattern
`helper.loft`, `tower.loft` and `vehicle.loft` already use, arriving a
fourth time.  Two things must hold or the design is wrong:

1. **`n × TICK_SECONDS` through the accumulator must be exactly `n`
   ticks.**  If it is `n-1` or `n+1`, every scripted measurement in the
   suite disagrees with the played game.
2. **Variable frame times must not change the answer.**  Sixty frames of
   1/60 s and one frame of 1 s must leave the same state, or the
   simulation is frame-rate dependent and no measurement means anything.

⚠ And plan 17 T1's finding is live here: a banked timer's DIRECTION is
half the epsilon rule, and *neither counting up nor counting down is
safe* — it has to be measured for this pair.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **P0** | the tables the probe produces | an accumulator is a REFINEMENT of `tick N`: same input, same state | ⚠ if 30 frames of 2/3 s and one `tick 30` differ by a single field, the one-door design is wrong and the plan stops |
| **P1** | all 520 gate measurements unchanged | the game has ONE door and the script runner goes through it | the corpus IS the control — every scenario's exact clock pins the behaviour, so a seam that changed anything goes red in 28 files |
| **P2** | a `.keys` script and a keypress produce the same action | one key table, the I1 shape | ✗ a play action wired straight into the GL loop is invisible to every script — the exact defect I1 was built to stop |
| **P3** | waves arrive in a window, on the wall clock | the loop owns the CLOCK and the seam owns the rules (`editor_step`'s existing split) | a frame-rate-dependent simulation: assert the same elapsed time gives the same state at two frame rates |
| **P4** | enemies, the vehicle and the crew are drawn | measured frames, not goldens | ⚠ a golden AGREES WITH A SHEAR, so the gate is `classify_world` pixel shares — a thing not drawn reads as zero |
| **P5** | a key writes the live situation as `.keys` | plan 18's emitter, driven from a session rather than a script | the emitted file must REPLAY to an S0-identical state |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **P0** — the probe: can real time reproduce `tick N`? | XS | a probe, no `src/` change — feed `n × TICK_SECONDS` through an accumulator and compare the state with `tick n`, then vary the frame size.  ⚠ If they differ, the plan stops here | Open |
| **P1** — the play session and its ONE door | M | `tests/19_p1_the_seam.loft` + the whole gate: `play_step(ps, input, seconds)` exists, `script.loft`'s `tick` routes through it, and **all 520 measurements are unchanged** | Blocked on P0 |
| **P2** — play actions in the ONE key table | S | `tests/19_p2_the_keys.loft` — drive / boost / take / drop become `EditorAction` rows, and a script pressing the key does what the verb does | Blocked on P1 |
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

1. **Where does `WaveState` live in a live session?**  Not on
   `EditorState` — `CLAUDE.md` § Architecture is explicit that folding a
   roster into the seam *"would make `editor_step` answer for something
   no editor action touches"*.  *Recommendation: a `PlayState` that
   COMPOSES an `EditorState` and a `WaveState`, so the editor's seam
   keeps its scope and the play seam is a second, narrower door.  P1
   decides.*
2. **Does the editor become a MODE of the game, or the game a mode of
   the editor?**  Today `main.loft` is an editor that could gain a play
   mode; the shipped game is a game that has no editor.  *Recommendation:
   a mode flag in the shell for now and no decision baked into the seam,
   because the answer changes when a landing flow exists (plan 05) and
   nothing here should pre-empt it.*
3. **What happens to `ScriptRun.ticks` when the clock is real?**  It is
   the run's CLOCK and plan 12 B7's comparisons are built on it.
   *Recommendation: it stays a count of TICKS rather than seconds — the
   accumulator changes who decides when a tick happens, not what one
   is.*
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
