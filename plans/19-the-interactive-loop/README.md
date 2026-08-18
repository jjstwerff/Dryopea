<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 19 — The interactive loop: a game you can play

**Value:** `G` · **Effort:** `H`

## Status

**P0 + P1 + P2 + P3 done** (2026-08-15); **P6 + P7 done** (2026-08-18).
P5 is what is left.  Suite **1405** green (101 files), gate 33 scripts /
**654 measurements unchanged**, GL gate **3 fixtures / 55 measurements**.

⚠⚠ **dryopea can be played AND SEEN.**  `make play`, pan to the base,
press **P**: the window stops being a map editor and becomes the game —
the ground as triangles, the roster as part-trees, through the session's
own eased follow camera.  Press P again and the editor comes back.

### P7 — the HUD, which is one number (2026-08-18)

`src/hud.loft` § The wallet + `src/play_view.loft` § The one number +
`tests/19_p7_the_hud.loft` (8 tests) + two cases in `src/gl_gate.loft`.

⚠⚠ **MOST OF THIS PHASE IS WHAT `docs/DESIGN.md` § HUD REFUSES**
(`@X097`).  Its entire numeric HUD is *"Wallet (points) — one corner
number.  The only number the player must see to make build decisions.
[…] That's the entire HUD.  No wave-number, no inter-wave countdown, no
minimap, no boost cooldown bar."*  So there is **no health bar, no tick
counter, no enemy count and no wave readout**, and adding one is a
design change rather than a feature.  Everything else the player needs
is diegetic and already built: the rotors show boost (`@X091`), the
canopy shows cargo (`@X090`), a tower's top shows whether it has one.
⚠ The tick and the enemy count stay CONSOLE lines — a developer's
readout, not the player's.

⚠⚠ **THE DIGITS ARE RECTANGLES, AND THAT IS FORCED.**  `graphics`
publishes `draw_text` and it is unusable twice over: it rasterises
through `#native rasterize_text_into`, which answers *"native function
not loaded"* under `loft test`, and it needs a FONT FILE this repo does
not have.  **A HUD nothing headless can draw is a HUD no test and no
`snap` can see.**  So `digit_segments` is a seven-segment table and
`render_digit` is seven `fill_rect`s.  ⚠ `picker.loft` reached the same
conclusion in plan 01 and its note is the older half of this one.

⚠⚠ **A WRONG DIGIT IS THE FAILURE NO PIXEL COUNT CAN SEE**, so the
table is gated by an **independent oracle**: the lit-segment counts
**6 2 5 5 4 5 6 3 7 6** are a property of the shapes and derived from
nothing in this repo.  ⚠ The counts alone cannot tell a `2` from a `3`
or a `5` from a `6` — each pair lights five — so every digit's drawn
pixels are also folded position-sensitively and asserted distinct.

⚠⚠ **AND A SMALL, PARTIALLY TRANSPARENT QUAD FAILS TWO WAYS, NEITHER
VISIBLE TO A TEST** (`loft test` cannot blend, and the software `snap`
composes no overlay).  A fill with alpha 255 puts an opaque slab over
the corner of the world — which looks deliberate; ink with alpha 0
draws nothing — which looks like a HUD nobody wired up.  Both are cases
in `gl_gate.loft` now and both FIRED against their own break: the
opaque fill read **0 of 3396** kept pixels, the alpha-less ink **0 of
1164** digits.  ⚠ The expectations are the overlay's OWN canvas rather
than magic numbers — the blit is 1:1 at native size, so the ink on
screen is exactly the ink in the canvas.

⚠ **Two findings the window had to say.**  The digits shipped at
14 x 24 and were legible only as a squint against 960 x 720, so they
are **24 x 40**: *a HUD is read at a glance or it is not read*, and no
headless gate can judge that.  And the shape checksum was `h * 131 + i`
unbounded — the overflow landed on exactly **0** for digit `0`, which
the guard above it read as *drew no pixels at all*.  ***A checksum that
can reach the value meaning "empty" is the wrong checksum***, which is
`mesh_crc`'s `@X065` one layer out.

⚠ **The cost is nothing**: a play frame with the overlay measures 5-6 ms
against 5 ms without, so it is built and uploaded EVERY frame rather
than cached — which is the honest shape when the wallet drains
continuously and a cache keyed on the number would miss almost nothing.

⚠ **Still open, and a design question rather than a defect**: the number
has one colour at every value.  A ramp toward red as the wallet nears
zero would add no HUD element and would make the run's ONE end state
(`wallet_broke`) legible — but `DESIGN.md` does not ask for it, so it is
not invented here.

### P6 — the window draws the game (2026-08-18)

`src/play_view.loft` + `tests/19_p6_the_window.loft` (5 tests), a case in
`src/gl_gate.loft`, and eleven lines in `main.loft`.  Plan 25 M3 gave the
ground a GL path and plan 20 A5 gave the roster one; **neither had a
caller with a clock on it**, and this is the composition that does.

⚠⚠ **THE GROUND MOVES WHILE THE GAME IS RUNNING, AND THE RENDERER WORKS
THAT OUT FOR ITSELF** (`@X095`).  `ground_gl.loft` was written for an
EDITOR's ground: paint a hex and re-bake the tiles it reaches.  A live
session changes the terrain for three other reasons — a body falls, a
wall breaks, the player clears a heap — and none of them goes near
`paint` from the caller's side.  ⚠ A dirty list on `HeightLayer` was the
obvious answer and is refused three ways: it would leak into
`state_diff` (*are two runs in the same state* — a dirty list is not
state), survive an `emit`, and grow for ever in the 1397 headless tests
that have no renderer.  ⚠⚠ So `MeshWatch` keeps a SNAPSHOT of the height
layer and diffs it, which is exact only because of one invariant:
**every terrain change a TICK can make moves the HEIGHT LAYER** — a body
raises its hex, a breach raises masonry BEFORE it repaints, salvage
lowers a pile.

⚠ **The invariant is asserted against the simulation, not quoted.**
`test_the_maintained_ground_equals_a_cold_rebuild` plays a real besieged
base and compares every tile the renderer maintained with a cold
rebuild — `11_f8`'s field-cache shape.  ⚠⚠ And the fixture's own check
earned its keep at once: the first version passed while **no wall ever
broke**, because the tower killed the robots first.  The equality held
over a base where only BODIES fell — so *the one case the diff cannot
see directly, a REPAINT, had never happened*.  One wall hex now starts
1 HP from spent.

⚠⚠ **THE TILE IS NOW 8x8, AND THAT IS PLAN 25 M4's DEFERRED DECISION
ARRIVING** (`@X096`, `@M041`).  M4 swept `MESH_CHUNK_SHIFT`, found 8x8
buys 8.6x on an edit for 12x the draw calls, and **changed nothing** —
*"that half cannot be measured from `loft test`"* — naming *the phase
that wires GL into `play_mode`* as its inheritor.  Measured in a real
window: **the draw cost does not move at all.**

| tile | tiles | one-hex terrain change | draw / frame |
|---|---|---|---|
| 32x32 | 4 | **54 ms** | 5 ms |
| 16x16 | 8 | 29 ms | 5 ms |
| **8x8** | 24 | **7 ms** | 5 ms |
| 32x32, world 4x the area | 8 | **112 ms** | 7 ms |
| **8x8**, world 4x the area | **96** | **7 ms** | **7 ms** |

⚠ Twelve times the draw calls costs nothing measurable, and at 8x8 the
edit cost is CONSTANT in the world's size where at 32x32 it grows with
it.  So a body falling went from a 3-7 frame hitch to under one frame.
⚠ **The cost was two test fixtures that encoded the number 32**, and
both are now derived from `mesh_chunk_span()` and pass at 8, 16 and 32.
`25_m2`'s coordinates are a TILE BOUNDARY rather than a literal;
`25_m4`'s two thresholds (`> 500` hexes' worth, a `> 10x` sparse/dense
ratio) were each a statement about the world they were measured in.

⚠⚠ **AND A TOTAL FAILURE NO GATE IN THE REPO COULD HAVE SEEN.**
`ground_gl_draw` and `entity_gl_draw` each turn depth testing and
back-face culling ON and neither turns them off — right for them, and
the editor's picture is a full-screen TEXTURE BLIT.  Measured: with the
two `gl_disable` lines removed from `play_view_draw`, the frame after a
play frame is **691 200 black pixels of 691 200**.  ⚠ A player toggling
P twice would see it and the GL gate could not, because each fixture
draws ONE frame and exits — so it is now a case of its own in
`gl_gate.loft`, and it FIRED against the broken version.  ⚠⚠ **Its own
first failure was its own bug**: it filled a canvas with
`GL_GATE_CLEAR`, a constant that carries no ALPHA because everything
that reads it masks to 24 bits, so the blend drew a transparent quad and
the check read black for a reason that had nothing to do with depth
testing.  ***A control that fires is not yet a control that fires for
the right reason.***

⚠ **The roster is uploaded on a TICK boundary and not per frame** — 10 ms
against a 5 ms frame, and every entity position is a lattice position
whose joint angles are functions of the tick's own time.  ⚠ It is the
first thing that has to go the day anything is drawn at `play_alpha`
(`@M035`), which is the same decision as the camera following the drawn
point.

⚠ **What P6 did NOT build**: no HUD (the wallet, the wave and the tick
are still console lines), no interpolation, and no editor in 3-D —
pressing P goes back to the software frame, where the picker, the hover
preview and all 654 of `validate.sh`'s measurements live.

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

Before this plan, every wave, tower and clock in the repo had been
measured by a `.keys` file and **never once played by a person** —
`WaveState` rode on `ScriptRun` and `editor_step.loft` did not mention
it, because *"a session that is being edited has no enemies in it"*.

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

## P2 — the play actions join the ONE key table (2026-08-15)

Six rows in `editor_actions()` — `drive_north` / `_south` / `_east` /
`_west` (WASD), `boost` (Shift), `carry` (E) — which is
`DESIGN.md` § The handful of keys' whole list bar the editor's own.
`EditorInput` grew the seven fields they resolve into and
`play.loft::play_actions` is what consumes them.

### ⚠⚠ WASD is the camera AND the vehicle, and the exclusion is the phase

`editor_actions()` already gave the camera pan W, A, S and D.  The
design gives movement the same four.  They are never both live:
`editor_input_from` gained a `playing` argument and fills the pan set
**or** the drive set, never both.

⚠ **Without that exclusion the gate dies in bulk**, and not subtly:
every `at` is a camera walk made of pan frames, so each one would also
drive whatever vehicle its scenario had parked.  `plans/19` § Open
questions 2 said the mode belongs to the shell and must not be baked
into the seam — a PARAMETER is the weakest form it can take, and
nothing remembers it between frames.

⚠ **And a script says which meaning it wants by ACTION NAME**, not by a
mode verb: `do pan_north` presses W as a camera key and `do
drive_north` presses the same W as a play key.  A `live` verb would
have made a line's meaning depend on a line somewhere above it, which
is exactly what `take` / `drop` refuse to do about one carry key.

### ⚠⚠ W means NORTH, and there is no direction table

dryopea is pointy-top odd-r: its six neighbours are E, SE, SW, W, NW,
NE, so **there is no due-north neighbour** and the four keys cannot be
four `lat_neighbour` directions.  Three ways out, two of which invent
something:

| | what it costs |
|---|---|
| a (key → lattice direction) table | W would mean NE, so holding "north" **drifts steadily east** — and it resurrects the constant direction table `lattice.loft` deliberately does not have |
| drive to the hovered hex | free — it is the existing `drive <q> <r>` verb — but `DESIGN.md` § 11 reserves the mouse for UI clicks and gives movement to WASD |
| **a heading in METRES** ✓ | one float round-trip per steering frame |

The keys name a compass heading, the heading is added to the vehicle's
metre position, and `lat_from_metres` names the hex.  ⚠ The arithmetic
is `lattice.loft`'s own — including the y-negation that makes the
compass true on dryopea's screen — so nothing new is invented and the
four keys reach all eight compass headings through their combinations.

⚠ **Measured, and it is exact**: fifteen ticks of held W on an open
field give `(0, 2) (0, 4) … (0, 30)` — **zero drift in `q`, every
tick**.  A NE mapping would have read `q = 15`.  E, S and W are equally
clean and a two-key diagonal steps `(1, 2)` a tick.

### ⚠ Three smaller decisions, each with a reason

**Boost is EDGE-triggered, though `DESIGN.md` calls it a held key.**
Held is how the 2 s lift *feels*; but `vehicle_boost` is a one-shot
that refuses while boosting or cooling, so a held Shift would silently
re-arm the instant the 5 s cooldown lapsed.  That is a second boost the
player never decided to spend, and § The opportunity-cost layer is the
whole reason spending one is a decision.

**Releasing a drive key STOPS the vehicle**, spelled *drive to where
you already are* so there is still exactly one way to set a
destination.  ⚠ The stop is guarded on `in_playing`, because without
that guard an EDIT frame would cancel a destination a script had set
with `drive <q> <r>` — silently, on the next camera pan.

**`PlayState` remembers its own previous frame** rather than reading
`EditorState.prev`: that one is written at the END of `editor_step`, so
a play action reading it would compare this frame against itself.

### ⚠ A loft finding: there is no vector-of-TUPLES literal

`for row in [("drive_north", 119), …]` fails with `fatal: cannot build
this record — its type never resolved`, pointing at the last element
rather than at the construct.  A one-line struct is the fix and reads
better anyway.  Nothing else in `src/` or `tests/` uses the shape, so it
had never come up — recorded in
[`docs/LOFT_GOTCHAS.md`](../../docs/LOFT_GOTCHAS.md) § Literals.

## P3 — the loop: the game runs on a wall clock (2026-08-15)

`main.loft` calls `play_step` and hands it how long the frame took.
**P** flips between editing the map and playing it; the crew appears at
the core; waves arrive because seconds passed.

The whole of the shell's input-and-time half is three lines:

```
playing = play_mode(ps);
ei      = editor_input_from(isrc, hovered, pan_due, paint_due, playing);
play_step(ps, s, ei, play_frame_seconds(ps, elapsed));
```

### ⚠⚠ The mode moved from the SHELL to the SESSION, and the reason is `#cwd`

§ Open questions 2 recommended a mode flag *in the shell*, and P2 built
the seam's half exactly that way — a per-frame parameter, nothing
remembered.  **That half stands and is untouched.**  What moved is where
the flag lives BETWEEN frames.

A local in `main()` cannot be reached by any test: an entry point
carries `#cwd`, `#cwd` cannot be `use`d, so `main.loft` is compiled by
nothing (`CLAUDE.md` § Relative paths).  A flag there would make the one
decision CI cannot see the one that decides whether the game runs.

So `PlayState.playing` holds it, and the shell reads it.  ⚠ The seam is
still parameterised, which was the load-bearing half: plan 05's landing
flow can set the flag instead of a key without touching
`editor_input_from` at all.

### ⚠⚠ The mode gates the CLOCK and never the seam

Two flags, one word, and folding them would have broken the corpus:

| | what it says | who reads it |
|---|---|---|
| `EditorInput.in_playing` | what the KEYS mean **this frame** — WASD pans or drives | `editor_input_from`, `play_actions` |
| `PlayState.playing` | does wall time reach the simulation **at all** | `play_frame_seconds`, and nothing else |

⚠ **A `play_step` that gated its SECONDS on either flag goes red at
once**, and the tests that catch it are P1's and P2's rather than
P3's: `test_a_frame_carries_both_the_input_and_the_time` hands a
NEUTRAL frame two ticks' worth of seconds, and
`test_an_edit_frame_leaves_a_scripted_destination_alone` hands an EDIT
frame six — both on sessions that have never been in play mode, because
a script's time is the script's business.  Time is the caller's;
`play_frame_seconds` is how the *window* spends it.

⚠ And the mode is a function rather than an `if` in the GL loop for the
same reason the flag is on the session: `tests/19_p3_the_clock.loft`
§ A session opens in the editor is what makes the rule visible to CI.

### ⚠ Time spent editing is DROPPED, not banked

The same decision `play_advance` already makes about a coreless world,
and it matters more here because `play_advance` has **no clamp**: a
minute of map-making banked and then released would arrive as ninety
ticks in one frame.  ⚠ The shell still measures every frame — including
the ones whose seconds it throws away — because skipping the update
while editing is what would create the burst it is avoiding.

### ⚠ P is a table row, and a REFUSED script action

It is on `editor_actions()` for the reason every key is (plan 09 I1): a
key code read privately in `main.loft` is a binding no gate can see.
`EditorInput.in_toggle_play` carries it, and the WINDOW is its
consumer — the third one, after `editor_step` and `play_actions`.

⚠ But `do toggle_play` is **refused** by the script runner, alongside
`do mod_ctrl` and `do palette_5`.  P2 decided a script says which
meaning it wants by ACTION NAME — `do pan_north` against `do
drive_north` — precisely so a line's meaning never depends on a line
above it, and a mode verb puts that dependency straight back.

⚠ It is also filled OUTSIDE `editor_input_from`'s play / edit split,
unlike the six play rows.  Fill it in one branch and there is no way out
of whichever mode filled it.

### ⚠ Three smaller decisions

**The crew appears at the CORE**, which is a stand-in for plan 05's
landing flow and deliberately the smallest one: the design lands the
player on a spot they pick with the mouse, and picking one is UI work
this plan excludes.  ⚠ `play_begin` refuses an occupied chassis, so
leaving play mode and coming back is a **pause** rather than a restart —
otherwise a glance at the map would cost the run its position, and
`play_begin` would be a second respawn path beside `wave_drop`'s.

**The mode flips at the END of the frame.**  The input was resolved
under the old mode — WASD already meant one of its two things before the
flip could run — so flipping first would make one frame's keys disagree
with that frame's mode.  ⚠ And it happens inside `play_step`, beside the
`ps.prev` write it depends on: a caller doing it outside would have to
save the previous frame first, and one that forgot would toggle on every
frame the key was held.

**Esc still saves and exits** rather than leaving play mode.
`DESIGN.md` gives Esc to *cancel / menu*, and a menu is not built.

### What the gate said

* `scripts/validate.sh` — **28 scripts, 520 measurements, unchanged.**
  P3 is additive: every scenario in the corpus is a run in EDIT mode,
  and `tests/19_p3_the_clock.loft` § A scripted run is never in play
  mode is that stated as an assertion rather than as a hope.
* `scripts/test.sh` — **1094 green** (1082 + 12).
* ⚠ **One control fired**: `tests/09_i1_bindings.loft` pins the table's
  ROW COUNT, and it went red naming the count.  That is the gate working
  — a key table that can grow silently is a key table nobody is reading.

### ⚠ What P3 does NOT do, and it is visible the moment you press P

**Nothing of the game is drawn.**  P4 draws the enemies, the vehicle and
the crew; until then the window is an editor with a live simulation
behind it, and the console echo (a line per tick: waves, enemies,
wallet, where the crew is) is the whole of what a player can see.

**The camera does not follow the vehicle** — `DESIGN.md` § 12 has it
locked and auto-reframing, which is camera work § What this plan does
NOT build leaves to plan 05.  Pan to the base before pressing P.

⚠ **A tick lands INSIDE one frame, so the window will hitch at 1.5 Hz.**
`play_advance` spends whole ticks in the calling frame, and `CLAUDE.md`
§ Cost puts a tick at ~125 ms on a radius-40 world with 80 enemies —
against a 16 ms frame.  Nothing here is wrong: the budget a tick is
*allowed* is 667 ms precisely because a tick is 667 ms of game time.
But it is spent in one frame rather than spread over forty, and P4 is
where that becomes visible.  ⚠ **Not measured in a window** — no display
here — so this is the arithmetic, not a reading.

⚠ **The window half was not run by the agent that built it** — this
environment has no display, so `make play` could not be launched.  What
is verified is the loop's whole decision surface headlessly (`§ The
window's frame` mirrors it line for line) plus a `--native-emit`
parse-check of the shell.  § Open questions 4 is the human check, and it
is still open.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **P0** ✓ | `tick 30` reproduced from 1, 30, 200, 600 and 1200 frames — every one an empty `state_diff` | ⚠ claimed *an accumulator is a REFINEMENT of `tick N`* — **too strong, corrected by P1**; what holds is that the frame SIZE does not reach the simulation | ✓ the falsifier was live for the frame size; ✗ **not for `n`** — five frame sizes over ONE tick count, and the defect was in the variable held fixed |
| **P1** ✓ | 520 gate measurements unchanged; suite 1065 | ONE caller of `wave_tick` — asked by COUNT or by DURATION, never one folded into the other | ⚠ **the control FIRED**: routing `tick` through the seconds door went red as `a-base-that-plays-its-list … waves = 0, outside 1..1`, a wave that never arrives rather than a clock a hair off |
| **P2** ✓ | six rows on the design's own keys; 520 measurements unchanged; suite 1082 | one key table, the I1 shape — and one key means ONE thing per frame | ⚠ **the control was nearly fatal**: WASD is shared with the camera pan, so a merged reading would have driven a parked vehicle on every `at` in 28 scenarios.  ✓ § A camera frame does not drive; ✓ § The editor seam is blind to the play fields |
| **P3** ✓ | 12 s of held-east frames drive the crew to a spawn marker and the wave list wakes — **no `tick` anywhere in the test**; 520 measurements unchanged; suite 1094 | the loop owns the CLOCK and nothing else — the mode gates the clock, never the seam | ✓ 60 fps against 12 fps over the same 12 s, through the KEYS, `state_diff` empty (⚠ stronger than P1's: the steering re-points every frame, so 60 fps had 5x the chances to disagree).  ⚠ **a control FIRED**: `09_i1_bindings`'s row count |
| **P4** | ⚠ **moved to [plan 20](../20-entity-art/README.md) A5** — the row is kept because its negative control is the one that survived the move intact | measured frames, not goldens | ⚠ a golden AGREES WITH A SHEAR, so the gate is `classify_world` pixel shares — a thing not drawn reads as zero |
| **P5** | a key writes the live situation as `.keys` | plan 18's emitter, driven from a session rather than a script | the emitted file must REPLAY to an S0-identical state |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **P0** — the probe: can real time reproduce `tick N`? | XS | the table in § P0, measured against the shipped sim over five frame sizes | **Done** |
| **P1** — the play session and its ONE door | M | `tests/19_p1_the_seam.loft` (14 fns) + the whole gate: `play.loft` owns the only `wave_tick` call, `tick` / `fall` / every scripted frame route through it, and **all 520 measurements are unchanged** | **Done** |
| **P2** — play actions in the ONE key table | S | `tests/19_p2_the_keys.loft` (17 fns) — WASD / Shift / E are `EditorAction` rows, a script pressing the key does what the verb does, and the 520 measurements are unchanged.  ⚠ They apply in `play_step`, not `editor_step`: the editor's seam has no roster | **Done** |
| **P3** — the loop: the game runs in the window | M | `tests/19_p3_the_clock.loft` (12 fns) — P starts the clock, a held P toggles once, the crew lands at the core, the mode decides whether WASD pans or drives, **a wave arrives on 12 s of wall clock with no `tick` in the test**, and 60 fps and 12 fps play one game.  ⚠ The headless half is what CI can hold; the window is § Open questions 4 | **Done** |
| **P4** — drawing the game | M | ⚠ **Superseded IN PART by [plan 20](../20-entity-art/README.md) A5** — that phase took *what an entity looks like and how it is gated* (a PART-TREE, and pixel counts against a GL frame).  ⚠⚠ The other half — *the WINDOW* — was always this plan's, and it is **P6** below | **Superseded** |
| **P5** — capture from a live session | S | `tests/19_p5_capture.loft` — a key writes the situation, and the file replays to an S0-identical state ([plan 18](../18-scenario-capture/README.md)) | **Next** |
| **P6** — the window draws the game | M | `tests/19_p6_the_window.loft` (5 fns) + `src/play_view.loft` + a case in `src/gl_gate.loft`.  ⚠⚠ Its headline gate is that the ground the renderer MAINTAINS equals a COLD rebuild after a real base is played, which is `11_f8`'s field-cache shape — and it holds because *every terrain change a tick can make moves the height layer*.  ⚠ It also settled plan 25 M4's deferred tile size on a measurement M4 could not make (`@X096`) and found a GL state leak that draws a **black window** on the second P press (`@M041`) | **Done** 2026-08-18 |
| **P7** — the HUD, which is one number | S | `tests/19_p7_the_hud.loft` (8 fns) + two cases in `src/gl_gate.loft`.  ⚠⚠ `DESIGN.md` § HUD is emphatic and most of the phase is what it REFUSES — one corner number, the wallet, and nothing else.  ⚠ The digits are RECTANGLES because `draw_text` is `#native` and needs a font file, so a text HUD would be one no test and no `snap` could see (`@X097`).  ⚠ Gated by an INDEPENDENT ORACLE (the lit-segment counts 6 2 5 5 4 5 6 3 7 6) and by two GL cases that each fired against their own break (`@M042`) | **Done** 2026-08-18 |

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

### ⚠ P4 moved out of this plan (2026-08-15)

The project owner asked for *detailed* entity art built the moros way — a
part-tree with moving parts, and PNGs for every mob and tower — which is
[`docs/PARTS.md`](../../docs/PARTS.md) and [`plan 20`](../20-entity-art/README.md).

⚠ **The gate did not change and the source of truth did.**  P4 would have drawn
primitives inline in `editor_view.loft`; plan 20 A5 blits a cache rendered from
part-trees.  The reason that matters is the reason `editor_view.loft` exists at
all — its own header refuses *"a second renderer that happens to live in the
test harness"* — and a per-entity shape drawn inline in the frame composition is
that, one layer down.

⚠ **So plan 19 is complete at P5**, not at P4.  P5 is unaffected: capturing a
live session writes `.keys` and photographs nothing.

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
   the editor?**  ⚠ **Still deliberately unanswered, and P3 kept it
   that way.**  Today `main.loft` is an editor that gained a play mode
   (P), which is the recommendation acted on and not the question
   settled — the shipped game will be a game that has no editor.  What
   P3 changed is only WHERE the flag lives: `PlayState.playing` rather
   than a local in `main()`, because an entry point carrying `#cwd` is
   compiled by nothing.  ⚠ The seam is still parameterised, so plan 05's
   landing flow sets the flag instead of a key and `editor_input_from`
   does not move.  § P3 § The mode moved from the SHELL to the SESSION.
3. ~~**What happens to `ScriptRun.ticks` when the clock is real?**~~
   **Answered by P1:** it is a count of TICKS, as recommended — and it
   MOVED, to `run.play.ticks`, because a run holding the wave and the
   clock as separate fields could not reach `play_ticks` without handing
   over copies of both.  § Open question 3, answered.
4. **Can a person actually play it?**  The one question no test answers,
   and **P3 made it askable without answering it** — the loop is built
   and green headlessly, but the environment that built it has no
   display, so `make play` has never been launched with the game in it.
   ⚠ `docs/NUMBERS.md` § Design targets has a target that has been
   ungateable since it was written — *"a single base session ≈ 15-25
   minutes"*.
   ⚠ **Ask it after P4**, not now: with nothing of the game drawn, what
   a person would be checking is a console log, and "is this playable"
   is not a question a log can answer.
   *Recommendation: check it by playing, and write the number down
   wherever it lands.*

## See also

- [`plans/05-validation-scenario`](../05-validation-scenario/README.md) —
  the integration spec this loop serves.
- [`plans/18-scenario-capture`](../18-scenario-capture/README.md) — built,
  and waiting for P5.
- [`plans/08-game-validation`](../08-game-validation/README.md) § V0 —
  the one-seam rule, one level down.
