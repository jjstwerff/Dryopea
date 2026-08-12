<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `08` — Game validation: scripted play, measured effects, pictures to look at

**Value:** `S` (silent failure) · **Effort:** `MH`

## Status

**Active — V0 through V3 shipped 2026-08-12; V4 is the next work.** Nothing in
dryopea validated the game as something that *runs*. The 189 tests in
`tests/` covered pure functions and static renders; every one of them called
a library function directly and none of them played the game.

V0 changes that for **every editor action**. `src/editor_step.loft` holds an
`EditorState` + `EditorInput` + `editor_step`, all inside the aggregator;
`src/main.loft` is now a GL shell that polls, steps and renders. 33 tests
across `tests/08_v0a_editor_step.loft` (paint) and
`tests/08_v0b_actions.loft` (the other five groups) drive the editor
headlessly.

V1a turns that seam into something a run can be **written down** in.
`src/script.loft` reads a `.keys` script and plays it, and 20 tests in
`tests/08_v1a_script.loft` hold it to the gate: the gate script and a
hand-written twin of the same run land on the same `EditorState`, compared
field by field.

V1b gives it **pictures**. `snap <name>` writes `<shots>/<name>.png` and a
state line into the transcript; 14 tests in `tests/08_v1b_snap.loft` hold
it to *its* gate — the file lands, and what is in it is a real render. Suite
272 green.

V2p is an **answer, not code** — § V2p. It falsified this plan's own
prediction about the palette, and found a different hazard in its place.

V2 turns that answer into the **instrument**: six measurement commands, a
classifier in `src/measure.loft`, and a wave for `count alive` to count.
24 tests in `tests/08_v2_measure.loft` carry the separation control for all
eleven entries plus the three checks that would go red if V2p's answer ever
stopped being true.

V3 is the five **scenarios** — `tests/scripts/*.keys`, each one a run
written down, each with its own assertions. `a-wave-approaches` is the first
thing in dryopea's history that asserts the *game* works rather than that a
function returns. Suite 306 green.

Four decisions the rest of the plan rests on — two from V0, one from each
half of V1:

- **Edge detection lives in the seam, not the caller.** `EditorInput`
  carries what is HELD this frame; `editor_step` compares against the
  remembered previous frame. That is what makes a scripted `hold Tab 5`
  toggle the mode once, the way holding Tab does for a player. Had the
  caller resolved edges, the script and the editor would be two different
  machines — and V1's whole premise is that they are one.
- **The caller keeps the clock.** Paint and pan rate limits arrive as
  `in_*_tick` booleans: the caller decides *when* a step is due, the seam
  decides *what* it does. `editor_step` never calls `ticks()`, which is what
  makes a run reproducible frame by frame.
- **The runner reaches the editor ONLY through `editor_step`.** It never
  assigns an `EditorState` field, not even the camera: `at 3 -2 4` *walks*
  the camera with pan and zoom frames rather than setting `s.cam`. One
  convenience assignment would have been a line shorter and would have made
  "the script and the editor are one machine" an aspiration instead of a
  fact — anything a script can do a player can do, because the script has no
  other door.
- **A shot is the editor's own frame.** The composition main.loft drew
  inline — world, hover preview, markers, ghost, picker, save indicator,
  mode badge — moved into `render_editor_frame` (`src/editor_view.loft`),
  and both the GL loop and `snap` ask for it. The alternative was a second
  renderer living in the harness, which would make every measured frame
  evidence about the harness. This is § One table, two readers applied to
  pixels. And `snap` changes nothing: a script must play the same way
  whether or not someone asked it for pictures.

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
  a new `src/editor_view.loft` (the frame both the GL loop and `snap`
  draw), a new `src/script.loft` (the runner; V2's measurements get their
  own file rather than growing this one), `tests/scripts/*.keys`,
  `scripts/validate.sh`, `Makefile`.
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

dryopea looked like it had the same hazard, worse: 11 ground types with
deliberate family resemblance — a water family and a land family (see
[`docs/GROUND_TYPES.md`](../../docs/GROUND_TYPES.md)). The prediction
written here was that a nearest-palette-colour classifier *will fuse sea
with the other water types and report a confident, meaningless number*.

**V2p went and measured it, and that prediction was wrong** — see § V2p.
The palette separates comfortably, and a world render contains nothing but
exact palette colours, so bucketing is a table lookup rather than a nearest-
colour guess. What the probe found instead was contamination from a source
nobody had written down: the editor's own HUD, which puts a floor under
every bucket that has nothing to do with what was painted.

Both halves are the same lesson. The rule survives the correction intact:
**the instrument is owed a proof before any number over it means
anything** — and the proof is worth building precisely because it answers
something other than the question you expected to ask.

So **V2 proves separation before it asserts anything**, with negative
controls: a frame painted entirely in one ground type must read 1.0 for
that bucket and 0 for all ten others, for **every** entry in the palette.
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
| **V0b** — move the remaining actions, in groups | M | one site at a time | per group: a test drives the action through `editor_step` and asserts its effect; `src/main.loft` shrinks to poll → step → render | **Shipped** (5 groups: tool state · camera · markers · history · disk) |
| **V1a** — parse + run, no output | S | parallel run | a script of `at` / `do` / `step` reproduces the SAME `EditorState` as the equivalent direct `editor_step` calls — compared field by field | **Shipped** |
| **V1b** — `snap` writes a picture | XS | — | `shots/<name>.png` exists and is a non-trivial render (not the empty canvas) | **Shipped** |
| **V2p** — probe: is the palette separable AT ALL? | XS | a probe first | pairwise-classify the 11 palette colours; **the deliverable is the answer, not code.** If any pair fuses, V2's design changes before it is built | **Shipped** — see § V2p. It separates; the HUD was the real hazard, and V2 measures the world layer |
| **V2** — the measurement vocabulary | M | — | the per-entry separation controls of § The instrument comes first: a canvas painted entirely in one type reads 1.0 for it and 0 for the other ten, for every entry — exactly, per V2p | **Shipped** |
| **V3** — the scenario scripts (one step each) | M | one site at a time | each script's own assertions; five scripts, five steps — a broken one goes red alone | **Shipped** |
| **V4** — wire it in | S | — | `make validate` goes red on an out-of-band measurement, and prints the number that moved | Open |

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

### V0 — the input seam (V0a + V0b) — **shipped**

`main()` used to call `gl_key_pressed(...)` **inline, interleaved with the
state mutation it caused**, for ~25 actions. There was no seam where a script
could inject input, and no way to run a frame without a GL window.

What shipped: `src/editor_step.loft`, **inside the aggregator** — which
closes the "nothing compiles main.loft" hole as a side effect — holding

```
editor_state_from(pw, mw, cam, picker) -> EditorState
editor_state_attach(s, save_path, marker_save_path)
editor_step(s: EditorState, input: EditorInput)
```

`src/main.loft` now keeps only: create window, poll GL into an
`EditorInput`, call `editor_step`, render, swap.

**Where the line fell.** Three things stayed with the caller, and each for a
reason V1 depends on:

| Stays with the caller | Why |
|---|---|
| Rate limits (paint, pan) — passed in as `in_*_tick` | the seam must not read a clock, or a run is not reproducible |
| Combo resolution (Ctrl+R reload vs plain R rotate) | which physical keys mean an action is a *keyboard* concern; a script names the action and never sees a modifier |
| Pixel → hex for the hover | a window concern; the input carries `(q, r)` |

Edge detection did **not** stay with the caller — see § Status.

⚠ **One table, two readers.** The key→action mapping is the single
duplication in this design: the GL poll and the script runner must agree on
it, or every script is a lie. moros carries the same warning about
`script.mjs` versus `editor.html` and has to state it because theirs *is*
duplicated.

V0 removes most of the exposure by making `EditorInput` a table of **named
actions** rather than of keys: `in_toggle_mode`, `in_undo`, `in_clear_all`.
The GL poll binds physical keys to those names in one flat block; a script
should address the names **directly** and never learn a key code. What
remains for V1a to settle is only the surface syntax — see § V1.

⚠ **Neutral must be the zero value of every `EditorInput` field.** A loft
struct literal that omits a field silently takes that field's default, so a
"no action" sentinel of `-1` turns every partially-built input into a real
action. This was not hypothetical: the first cut had `in_select_palette:
integer` with `-1` for none, which defaulted to `0` — palette entry 0 is sea,
painting sea erases, and five paint tests went red the moment the struct grew
its next field. Build inputs from `editor_input_empty()`, never as literals.

### V1 — the script runner — **shipped**

A `.keys` interpreter in loft, headless — no GL window, no server, no
browser. dryopea's renderer is already a software `Canvas`, so a run is
just: build input → `editor_step` → `render_to_canvas` → measure / save_png.

`src/script.loft` holds it: `script_run(s, source)` and
`script_run_file(s, path)`, both returning a `ScriptRun` that says whether
the run survived, which line killed it, and how many commands and frames it
got through.

Vocabulary as it shipped, adapted from moros to a top-down hex editor:

```
# a comment                 whole lines only
echo <text>                 marker into the transcript
at <q> <r> [zoom]           walk the camera to a hex — exact, repeatable
do <action>                 one frame held then released — a key TAP
hold <action> <n>           the action held for n frames — a key HELD DOWN
step <n>                    advance n frames with no action
palette <index>             select a palette entry
hover <q> <r>               pointer over a hex (hover preview, ghost arrow)
click <q> <r>               press at a hex (paint / place, per current mode)
drag <q> <r> <q2> <r2>      press, move, release — the paint-line path
snap <name>                 PNG into <shots>/<name>.png + a state line
```

`<shots>` is per-run: `shots/` by default, `tests/actual/` under the suite.
`snap` creates the directory and **checks what `save_png` answers** — it
returns false rather than raising, and an unchecked write there is how a
gate stays green over a picture that was never taken. The "state dump" is
one line in the transcript beside the file name (hex and marker counts,
camera, mode, tool, history) rather than a sidecar file: the transcript is
where someone reading a failed run is already looking, and V2's measurement
commands, not a `.txt` nothing parses, are what will make those numbers
assertable.

⚠ **An unknown command is an error, not a skipped line.** A typo that
silently does nothing turns a passing run into a lie — this is V1's negative
control, and it covers unknown actions, malformed numbers and wrong argument
counts too. A run stops at the first bad line rather than reporting a state
nobody asked for.

**V1a chose to name the ACTION, not the key.** `do toggle_mode`, not
`key Tab`. V0 had already left `EditorInput` as a table of named actions, so
naming keys would have reintroduced exactly the duplication § One table, two
readers warns about: a `"Tab"` → `in_toggle_mode` table in the runner beside
the GL poll's `KEY_TAB` → `in_toggle_mode`. With actions named, the runner's
`script_set_action` is the only mapping it has, the GL poll binds physical
keys straight to the same fields, and neither side holds a copy of the
other's table. A script reads as what the player *did*, and rebinding Tab
leaves every script correct. `do Tab` is a test — it must fail.

`hold <action> <n>` came for free, as predicted: the seam edge-detects, so
five held frames toggle the mode once and the runner does nothing to arrange
it. Level-triggered actions (pan, palette) repeat under the same command, for
the same reason and to the same effect a player sees.

`palette <index>` is its own command rather than an argument on `do`, because
`hold palette 5` would not say whether the 5 is the entry or the frame count.

### V2p — the answer

**Shipped 2026-08-12. No code shipped with it — the deliverable is this
section.** A throwaway probe under `loft test` measured four things and was
then deleted; V2 turns the findings into standing assertions.

**1. The palette separates comfortably. The prediction above was wrong.**
Pairwise RGB distances over all 11 entries: the closest pair is
`hill ↔ steep_rock` at d² = 3526 (d ≈ 59), then `hill ↔ wall` at d² = 5501.
Nothing else is under d² = 6000. The water family — the one the plan
expected to fuse — is the *best* separated group in the palette, because it
ramps hard in brightness (`#0a2c5e → #2a7ec0 → #6fbce8 → #e8f4fc`);
`sea ↔ water` alone is d ≈ 132. No pair needs a proxy colour, and no pair
needs merging into a named bucket.

**2. Better than separable: a world render needs no classifier at all.**
The rasteriser is hard-edged. Out of 691,200 pixels, the number that were
anything other than an *exact* palette colour was **zero** — for a canvas
painted entirely in one type, for all 11 types, and (the case that could
have falsified it) for a mixed world where every hex is a different type
from its neighbours, so that every hex boundary in the frame is a colour
boundary. There is no anti-aliasing and no alpha blend, so bucketing a world
render is an exact table lookup. `frame` is **not** an approximate
measurement.

**3. The thing `snap` writes is contaminated by its own HUD — and the
contamination cannot be driven to zero.** On an *empty* editor frame, every
one of the eleven buckets already reads non-zero, because the picker draws
one 8×8 swatch of **every** palette entry: 64 px = 0.0093 % per bucket, no
matter what is painted. On top of that the white outlines, badge border and
picker highlight (268 px) read as `waterfall`, and the mode badge plus save
indicator (201 px) read as `grass`. A further 341 px per frame are not
palette colours at all — badge `#80c060`, dirty indicator `#f0a020`, spawn
arrow `#ff3060`, target ring `#c02020` — and each still lands in a bucket
(grass, sand, wall, wall). A full editor frame painted entirely in one type
reads 0.9986 for it, never 1.0.

**The decision this forces on V2: measure the WORLD layer, not the shot.**
`frame` classifies `render_to_canvas(s.cam, s.pw, …)` — its own render of
the same state — rather than the composited picture `snap` writes. Three
things follow:

- counts are exact, so the bands exist for *geometry* (how much of the
  canvas a base covers), not to absorb classifier fuzz;
- "0 for the other ten" is literally 0, so the negative control of
  § The instrument comes first is assertable as written;
- the `fill_triangle` class of bug stays visible, because `render_to_canvas`
  **is** the path that draws the hexes in the shot — only the HUD
  compositing on top is excluded, and the HUD has its own tests already
  (`03_qol_polish`, `03_m2_toggle`).

The shot keeps its own job: a picture for a human. The measurement takes its
own render of the same state, so the two can never disagree about what is
there.

⚠ **Two changes would invalidate this and must re-run the probe:** the
renderer starting to anti-alias or alpha-blend hexes, and a twelfth palette
entry landing near an existing one. V2 therefore carries both as standing
tests — off-palette pixel count is 0, and the minimum pairwise distance
stays above a floor — rather than trusting a measurement taken once.

### V2 — the instrument — **shipped**

Measurement commands, each asserting inline so the transcript reads as the
verdict — a pass prints what it saw beside the band it wanted, because a
transcript that only speaks up on failure cannot be read afterwards as a
record of what was checked:

```
count painted <lo> <hi>        painted hexes within a band
count markers <lo> <hi>        placed markers within a band
count alive <lo> <hi>          live enemies — the wave-clear sentinel
kind <q> <r> <name>            EXACT ground type at a hex
marker <q> <r> <kind> [dir]    EXACT marker identity at a hex ('none' too)
frame <bucket> <lo> <hi>       share of WORLD pixels in a bucket
```

`src/measure.loft` holds the classifier; the commands live beside the rest
in `src/script.loft`, in their own dispatcher — the others CHANGE the
session, these only ask it questions, and one branch list doing both reads
as twenty unrelated cases.

**V2 also had to give the runner a wave.** `count alive` over a wave that
can never start is precisely the confident meaningless number this plan is
about: every `count alive 0 0` would pass, for the wrong reason. So the
runner gained `wave <n>` and `tick <n>`, and `WaveState` lives on
`ScriptRun` rather than on `EditorState` — a session being edited has no
enemies in it, and folding a roster into the seam would make `editor_step`
answer for something no editor action touches. A run has a pointer, a shots
directory and a wave; the editor has none of the three.

The core is **the target marker**, and exactly one is required: a wave has
to know what it walks towards, and "whichever the hash yielded first" is not
an answer a run can repeat. A wave that emits fewer enemies than asked for
fails the run rather than reporting a smaller number — usually it means no
spawn marker sits outside `close_spawn_disable_radius`.

**A distance measurement was deliberately left out of V2.** Scenario 5
asserts that enemies get *closer* to the target and nothing above could say
that — but the scenario had not yet said whether it wanted "closer than last
time" or "exactly here". V3 answered that and added `range <lo> <hi>`; see
§ V3.

`frame` was written down as the only *approximate* one. V2p found it is
not: a world render contains only exact palette colours, so the bucket
counts are exact and the band exists to say how much of the canvas the
subject covers, not to absorb classifier error. It still needs the
separation controls above — as a standing check that the exactness holds,
not as a calibration.

Two consequences of that exactness are built in. `frame` **stops the run**
if a single pixel is not a palette colour, rather than dropping it from the
denominator: a blending renderer invalidates every band a scenario carries
instead of nudging it. And the classifier is split — `classify_canvas`
counts a canvas someone else drew, `classify_world` renders and counts —
so that guard can be tested by handing it a pixel it must reject. A
classifier that only ever sees input it drew itself can only be tested on
input it agrees with.

`frame` costs about two seconds per call: it classifies all 691,200 pixels
of a real frame, and the share means *share of the canvas the player looks
at*, so shrinking the canvas to go faster would make the number stop
matching the picture beside it in `shots/`. The eleven-entry separation
sweep in the unit suite runs on a 96×96 canvas instead — what it proves does
not depend on the pixel count.

The rest are exact too, and should be preferred wherever they can answer the
question — a hex either is grass or it is not, and no threshold is needed to
say so. `frame` earns its place on the one question the others cannot
answer: *is the thing actually drawn?*

### V3 — the scenario scripts — **shipped**

The runs worth having on day one, each its own `.keys` file in
`tests/scripts/`:

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

`range <lo> <hi>` landed with this phase rather than with V2 — the span
from the core to the live enemies, nearest and farthest. Two `range` lines
with lower numbers are what "decreases" means, and each is exact; a
`closer` command would have hidden the numbers it compared. It refuses an
empty roster: a band over nothing passes for the wrong reason, which is the
failure this whole plan is about.

**The scripts assert; `tests/08_v3_scenarios.loft` makes each one go red
alone.** It adds the single thing a script cannot say about itself — how
many measurements it made — because a scenario whose `count` and `kind`
lines were deleted would still finish, still report `ok`, and still be
worthless. Every scenario is attached to a save path even though only
`round-trip` uses one, so a scenario that grows a `do save` cannot silently
have nowhere to put it.

**The claim in scenario 2, demonstrated.** With `render_to_canvas` altered
to draw no hexes at all, `paint-a-base` runs its `count painted` and all
nine `kind` assertions green and then dies on `frame wall = 0.000000,
outside 0.01..0.02` — and no other scenario moves. That is the
`fill_triangle` shape exactly: state correct, screen wrong, and only the
pixel measurement can tell.

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
| **V0** | A key held for five frames fires its action once | edge detection lives in the seam, not the caller | An action that fires per-frame under a held key fails |
| **V0** | A session with no `save_path` refuses to save AND to reload | disk is reached only through an attached path | An unguarded reload reads an absent file and wipes the world |
| **V1** | `do levitate`, `teleport 4 4`, `at 3 south`, `at 3` | the runner refuses what it does not understand | An unknown command, action, number or arity must ERROR, not be skipped |
| **V1** | `do Tab` | the vocabulary names actions, so no key table exists to drift | A key name that WORKS means a second table was built |
| **V1** | `snap` into a directory that cannot be made | a write that could not happen ends the run | `save_png` answers false; an unchecked answer leaves a green gate over a missing picture |
| **V1** | the same script with and without `snap` in it | a shot is a photograph — rendering does not mutate the session | A run that plays differently because someone asked for pictures |
| **V2** | A world canvas painted entirely `grass` reads `grass = 1.0`, all others `= 0` | the classifier separates all 11 palette entries | Two adjacent palette colours landing in one bucket fails the gate |
| **V2** | Every pixel of a MIXED world render is an exact palette colour | the rasteriser is hard-edged, so bucketing is a lookup and not a guess | One blended edge pixel means `frame` is approximate again — and the bands were sized for exact |
| **V2** | The minimum pairwise palette distance stays above a floor | a twelfth entry cannot quietly land on top of an existing one | A new palette colour inside the floor fails before any scenario does |
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

1. ~~**Are `shots/` committed?**~~ **Decided in V1b: no.** `shots/` is
   gitignored and written fresh every run; a shot a doc cites is copied into
   `docs/` deliberately. dryopea already commits `tests/golden/` for the
   exact-comparison job, and a second tree of binaries that nothing compares
   would be churn with no reader. What *is* committed is `tests/scripts/*.keys`
   — those are source. The suite writes its shots to `tests/actual/`, which
   `scripts/test.sh` already wipes between runs.
2. **Does the runner ever drive the real GL build?** Headless proves the
   logic, not the GL path — the window, the input polling and
   `gl_screenshot` stay unvalidated. *Provisional: no; V0's seam means the
   GL loop carries almost no logic.* Revisit if a GL-only bug escapes.
3. **Should plan identity move to GitHub issue numbers**, as moros does?
   Out of scope here, but this plan is the natural first one to file if the
   answer is yes. See [`plans/README.md`](../README.md) § Identity.
