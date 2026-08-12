<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `08` — Game validation: scripted play, measured effects, pictures to look at

**Value:** `S` (silent failure) · **Effort:** `MH`

## Status

**Active — V0a shipped 2026-08-12; V0b is the next work.** Nothing in
dryopea validated the game as something that *runs*. The 189 tests in
`tests/` covered pure functions and static renders; every one of them called
a library function directly and none of them played the game.

V0a changes that for one action. `src/editor_step.loft` holds an
`EditorState` + `EditorInput` + a pure `editor_step`, all inside the
aggregator; `tests/08_v0a_editor_step.loft` drives ground-mode paint through
it headlessly (10 tests, suite 199 green). `src/main.loft` now owns exactly
one `EditorState` and routes paint through the seam — every other action
still runs its old inline path against the same fields, which is what keeps
the tree green mid-migration.

The evidence this is a silent-failure problem, not a nice-to-have, is from
2026-08-12:

- **`src/main.loft` — the entire game loop — is compiled by nothing.** It
  sits outside the `dryopea.loft` aggregator, so `scripts/test.sh` never
  parses it. Three narrowing-cast errors sat in it undetected until it was
  parse-checked by hand. The file that *is* the game was broken and every
  gate was green. *(V0a moves the first action's logic into the aggregator;
  the hole closes fully when V0b empties main.loft of everything but
  poll → step → render.)*
- **`graphics::fill_triangle` drew every hex as a cross** for at least two
  released versions. dryopea's goldens did go red — but the library's own
  tests stayed green, because each assertion sampled the apex column or the
  base row, which is exactly what the broken code still drew. A gate that
  samples only where the artefact is drawn cannot see the artefact.
- **`spawn.loft`'s wave engine has unit tests and no scenario.** Nothing
  anywhere asserts that painting a base, placing spawns, saving, reloading
  and running waves produces enemies that approach.

This plan builds the instrument that would have caught all three, modelled
on moros's scripted-play harness.

## Goal

A `.keys` script drives dryopea headlessly through a sequence of real editor
and runtime actions; the harness measures the effects with thresholds and
writes a PNG per step into `shots/` for human inspection, and
`scripts/validate.sh` fails on a measurement that moves out of band.

## Anchors

- **Reference design:** moros's `tools/script.mjs` + `tools/scripts/*.keys`,
  and its `camera-frame` Makefile target. This plan adopts moros's
  *vocabulary and discipline*; see § Relationship to moros for what is and
  is not shared code.
- **Docs:** [`docs/DESIGN.md`](../../docs/DESIGN.md),
  [`docs/GROUND_TYPES.md`](../../docs/GROUND_TYPES.md) (the 11-entry
  palette the classifier must separate).
- **Source touched:** `src/main.loft` (split), a new `src/editor_step.loft`,
  a new `src/script/` (runner + measurements), `scripts/validate.sh`,
  `Makefile`.
- **Neighbouring plans:** [`05-validation-scenario`](../05-validation-scenario/README.md)
  defines *what* scenario counts as playable; **08 builds the instrument
  that measures it**. 05 supplies the content, 08 supplies the gate — they
  must not both define the scenario.

## The instrument comes first

⚠ **This section is the point of the plan, and it is the part that is easy
to skip.**

moros learned this the expensive way: drawing a missing floor made its
camera gate *redder*, because the classifier put wall and floor in one
`masonry` bucket. The threshold over that bucket was meaningless until the
instrument could tell the two apart — **the fix was owed to the instrument
before the number over it meant anything.**

dryopea has the same hazard, worse: the palette is 11 ground types with
deliberate family resemblance — a water family and a land family, several
neighbours a few RGB steps apart (see
[`docs/GROUND_TYPES.md`](../../docs/GROUND_TYPES.md)). A frame classifier
that buckets by nearest palette colour will fuse sea with the other water
types and report a confident, meaningless number.

So **V2 proves separation before it asserts anything**, with negative
controls: a frame painted entirely in one ground type must read ≈1.0 for
that bucket and ≈0 for all ten others, for **every** entry in the palette.
Any pair that cannot be separated is either given a distinguishable proxy
colour for validation runs or is merged into one *named* bucket
deliberately — never fused by accident.

## Phases

Cut against [`plans/README.md`](../README.md) § What makes a step SAFE. The
**Shape** column names which of the three safe shapes each step uses —
a phase that cannot name one is a phase that has not been cut yet.

| Phase | Effort | Shape | Verify | Status |
|---|---|---|---|---|
| **V0a** — seam for ONE action (paint) | S | one site at a time | a test builds an `EditorInput` with paint set, calls `editor_step`, asserts the hex changed; the other 24 actions still run their old inline path, so the tree is green throughout | **Shipped** |
| **V0b** — move the remaining actions, in groups | M | one site at a time | per group: a test drives the action through `editor_step` and asserts its effect; `src/main.loft` shrinks to poll → step → render | Open |
| **V1a** — parse + run, no output | S | parallel run | a script of `at` / `key` / `step` reproduces the SAME `EditorState` as the equivalent direct `editor_step` calls — compared field by field | Blocked on V0b |
| **V1b** — `snap` writes a picture | XS | — | `shots/<name>.png` exists and is a non-trivial render (not the empty canvas) | Blocked on V1a |
| **V2p** — probe: is the palette separable AT ALL? | XS | a probe first | pairwise-classify the 11 palette colours; **the deliverable is the answer, not code.** If any pair fuses, V2's design changes before it is built | Blocked on V1b |
| **V2** — the measurement vocabulary | M | — | the per-entry separation controls of § The instrument comes first: a canvas painted entirely in one type reads ≈1.0 for it and ≈0 for the other ten, for every entry | Blocked on V2p |
| **V3** — the scenario scripts (one step each) | M | one site at a time | each script's own assertions; five scripts, five steps — a broken one goes red alone | Blocked on V2 |
| **V4** — wire it in | S | — | `make validate` goes red on an out-of-band measurement, and prints the number that moved | Blocked on V3 |

⚠ **Why V0 is split.** As one `M` phase it failed the upper bound: extracting
25 actions at once leaves a half-done state with nothing to compare against,
because the old path is fused to `gl_key_pressed` and cannot run headlessly
to be compared *to*. Split, each step keeps the untouched actions on their
old path — the two paths coexist and the tree is green at every moment.

⚠ **Why V1 is split, and why V1b is `XS` and not a step on its own.** V1a can
go red for a real reason: the script path and the direct-call path can
disagree. V1b cannot really — a file either exists or it does not — so it is
deliberately marked `XS` and rides on V1a rather than pretending to be an
independent gate.

⚠ **V2p's deliverable is an ANSWER, not code.** It exists to falsify the
frame-classifier design for the cost of a compile, before V2 is built on top
of it. If sea and the other water types cannot be told apart, V2 changes
shape — and finding that out after V2 ships is how the instrument ends up
being trusted when it should not be. This is the same shape as the probe that
diagnosed `fill_triangle`: two cases side by side, one compile, a decision.

### V0 — the input seam (V0a + V0b)

Today `main()` calls `gl_key_pressed(...)` **inline, interleaved with the
state mutation it causes**, for ~25 actions. There is no seam where a script
can inject input, and no way to run a frame without a GL window.

Extract an `EditorInput` snapshot covering **every** action (not just the
camera pan that `InputState` already models: paint, mode toggle, palette
select, marker place / rotate / cycle, undo, redo, save, reload, clear,
recentre), and a pure

```
editor_step(state: &EditorState, input: EditorInput) -> void
```

`src/main.loft` keeps only: create window, poll GL into an `EditorInput`,
call `editor_step`, render, swap. Everything else moves into
`src/editor_step.loft`, **inside the aggregator** — which closes the
"nothing compiles main.loft" hole as a side effect, and is reason enough to
do V0 even if the rest of the plan stalls.

⚠ **One table, two readers.** The key→action mapping is the single
duplication in this design: the GL poll and the script runner must agree on
it, or every script is a lie. Keep the mapping in one place
(`editor_step.loft`) and have both call it — moros carries the same warning
about `script.mjs` versus `editor.html` and has to state it because theirs
*is* duplicated. dryopea can avoid the duplication outright; do so.

### V1 — the script runner

A `.keys` interpreter in loft, headless — no GL window, no server, no
browser. dryopea's renderer is already a software `Canvas`, so a run is
just: build input → `editor_step` → `render_to_canvas` → measure / save_png.

Vocabulary, adapted from moros to a top-down hex editor:

```
# a comment
echo <text>            marker into the transcript
at <q> <r> [zoom]      put the camera on a hex — exact, repeatable
key <K>                one frame with K held; K names the same action the GL poll maps
hold <K> <n>           K held for n frames
step <n>               advance n frames with no input
hover <q> <r>          pointer over a hex (hover preview, ghost arrow)
click <q> <r>          press at a hex (paint / place, per current mode)
drag <q> <r> <q2> <r2> press, move, release — the paint-line path
snap <name>            PNG into shots/<name>.png + a state dump
```

⚠ **An unknown command is an error, not a skipped line.** A typo that
silently does nothing turns a passing run into a lie — this is V1's negative
control.

### V2 — the instrument

Measurement commands, each asserting inline so the transcript reads as the
verdict:

```
count painted <lo> <hi>      painted_count within band
count markers <lo> <hi>      marker_count within band
count alive <lo> <hi>        live enemies — the wave-clear sentinel
kind <q> <r> <name>          EXACT ground type at a hex (an exact invariant)
marker <q> <r> <kind> [dir]  EXACT marker identity at a hex
frame <bucket> <lo> <hi> …   share of canvas pixels per palette bucket
```

`frame` is the only *approximate* one, and it is the one that needs the
separation controls above. The rest are exact and should be preferred
wherever they can answer the question — a hex either is grass or it is not,
and no threshold is needed to say so.

### V3 — the scenario scripts

The runs worth having on day one, each its own `.keys` file:

1. **`cold-start`** — empty world, camera at origin, palette loaded.
   Catches the load path and the sea-default render.
2. **`paint-a-base`** — paint a wall ring with a grass interior, snap,
   assert exact kinds at the ring and interior, and a frame share showing
   both. Catches the renderer (this is what `fill_triangle` broke).
3. **`round-trip`** — paint, save, clear, reload; assert the reloaded world
   is *identical* — same painted count, same kind at every painted hex.
   Exact invariant, no thresholds.
4. **`markers-and-rotation`** — place spawns in several directions, rotate,
   cycle kind, undo, redo; assert exact marker identity throughout. Six 60°
   rotations must be the identity.
5. **`a-wave-approaches`** — place spawns + a target, run the wave engine
   for N ticks, assert enemies exist, that their distance to the target
   **decreases**, and that they are alive. This is the first thing in
   dryopea's history that asserts the *game* works rather than that a
   function returns.

### V4 — wire it in

`scripts/validate.sh` runs every script in `tests/scripts/`, prints each
measurement with its band, and exits non-zero on the first out-of-band
reading. A `make validate` target, and a line in
[`CLAUDE.md`](../../CLAUDE.md) § Key commands.

Keep it **separate from `scripts/test.sh`** — the unit suite must stay fast
and hermetic. Validation is a second gate, run deliberately.

## Invariant gate

| Phase | Concrete expected result | Invariant pinned | Negative control |
|---|---|---|---|
| **V0** | An `EditorInput` with no action set leaves `EditorState` unchanged | `editor_step` is pure: same (state, input) → same state | A no-op frame that mutates *anything* fails |
| **V1** | `key Q` where Q maps to nothing | the runner refuses unknown commands | An unknown command must ERROR, not be skipped |
| **V2** | A canvas painted entirely `grass` reads `grass ≈ 1.0`, all others `≈ 0` | the classifier separates all 11 palette entries | Two adjacent palette colours landing in one bucket fails the gate |
| **V3** | paint → save → clear → reload → identical world | save/load round-trip = identity | A map with an unknown ground type is *refused*, not silently painted as sea |
| **V3** | six `R` presses on a spawn marker | rotation by six 60° steps = identity | A seventh press must not read as the identity |

## Relationship to moros

**Shared: the vocabulary and the discipline.** `echo` / `at` / `key` /
`snap`, thresholds as bands, PNGs kept for inspection rather than
byte-compared, and the instrument-before-threshold rule.

**Not shared: the runner.** moros's is Node driving a socket-connected
editor server, because moros's renderer is a GL client it must photograph
through a browser. dryopea renders to a software `Canvas` in-process, so its
runner is loft and needs neither server nor browser. Copying `script.mjs`
would import an architecture dryopea does not have.

⚠ Per [`plans/README.md`](../README.md) § lightest workflow, **do not write
a dryopea-local version of a routine a library already provides.** That rule
binds here too: if the `hex_*` family or moros grows a reusable measurement
or script-runner package, adopt it instead of keeping this one. Revisit if
plan 07 gives dryopea a GL/3D path — a 3D dryopea may want moros's
photograph-through-a-client model after all.

## Relationship to the golden-image tests

They stay, and they keep a different job:

| | Golden images | Measured frames (this plan) |
|---|---|---|
| Compares | exact bytes | classified pixel shares, with bands |
| Catches | any pixel change | "the thing is not drawn", "the wrong thing dominates" |
| Breaks when | the renderer changes at all | the subject genuinely changes |
| Answers | *something changed* | *what is wrong* |

A re-style invalidates all 16 goldens at once and none of the measured
frames — that is the point of having both.

## Open questions

1. **Are `shots/` committed?** moros commits them as the inspection record.
   dryopea already commits `tests/golden/` for exact comparison, and binary
   churn is real. *Provisional: `shots/` is gitignored, written fresh each
   run; a shot referenced by a doc gets copied into `docs/`.* Decided in V1.
2. **Does the runner ever drive the real GL build?** Headless proves the
   logic, not the GL path — the window, the input polling and
   `gl_screenshot` stay unvalidated. *Provisional: no; V0's seam means the
   GL loop carries almost no logic.* Revisit if a GL-only bug escapes.
3. **Should plan identity move to GitHub issue numbers**, as moros does?
   Out of scope here, but this plan is the natural first one to file if the
   answer is yes. See [`plans/README.md`](../README.md) § Identity.
