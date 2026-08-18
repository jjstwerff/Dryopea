<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Problems

Known bugs, limitations, and dryopea-side workarounds.  Mirrors
the style of loft's `doc/claude/PROBLEMS.md` so a reader bouncing
between repos doesn't have to relearn the format.

**dryopea-internal only.**  Problems that need loft to fix
(language gaps, runtime bugs, stdlib holes) go in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md) instead — that
file is the outbound queue.  PROBLEMS.md is for issues whose fix
lives in *this* repo.

Each entry gets a stable identifier `@D<NNN>` (D for dryopea, so
references can't be confused with loft's `@P<NNN>` rows).  Once
allocated the number is never reused, even after a fix lands.

## Entry template

```markdown
### @D001 — <short title>

- **Status:** Open | Fixed <date> — only once it moves to § Fixed
- **Severity:** High | Med | Low
- **Found while:** <what dryopea was doing when this surfaced>
- **Repro:** <minimal steps, ideally a path to a script>
- **Expected:** <…>
- **Observed:** <…>
- **Workaround:** <…or "none yet">
- **Fix plan:** <one sentence; "deferred" is OK with a reason>
- **Test:** <path/to/regression, once a guard exists>
```

Severity tiers:
- **High** — data loss, silent corruption, crash on a common
  code path, blocks a plan's current phase.
- **Med** — wrong output / missing feature on a code path that's
  exercised but not yet load-bearing.
- **Low** — cosmetic, edge-case, future-only.

## Open

### @D002 — `cam.zoom` changes no pixel: the wheel moves a number no renderer reads

- **Status:** Open
- **Severity:** Med — the editor has a control that appears to work and
  does nothing; and it removes a lever [`docs/PARTS.md`](docs/PARTS.md)
  § What could kill this design was counting on.
- **Found while:** writing `docs/PARTS.md` (plan 20), checking whether
  "zoom in to see the detail" was an option if entity sprites read too
  small at `VIEW_PPM` 24.
- **Repro:** `grep -rn "\.zoom" src/` — `camera.loft` moves it on the
  wheel (`ZOOM_MIN..ZOOM_MAX`), `save.loft` persists it, `script.loft`
  walks the camera to it and prints it in the state line.  **No file
  under `src/` that draws anything reads it**: `render.loft` does not
  mention `zoom`, and `render_editor_frame` takes `VIEW_PPM` as a
  constant.
- **Expected:** scrolling the wheel changes how much world the frame
  shows.
- **Observed:** the number changes, is saved, is reported by `snap`'s
  state line — and the picture is identical at `z1` and at `z6`.
- **Workaround:** none needed today; nothing depends on it.  ⚠ But
  `tests/scripts/*.keys` walk the camera to a zoom and `at` asserts it
  arrives, so **the gate is green over a control that does nothing** —
  which is why this is Med rather than Low.
- **Fix plan:** deferred, and deliberately NOT a phase of plan 20 —
  that plan must not grow a second subject.  The fix is to derive the
  render scale from the camera (`ppm = VIEW_PPM / zoom`, or a table) in
  the ONE place `render_editor_frame` passes it, so the GL loop and
  `snap` cannot disagree.  ⚠ It rebaselines every golden taken at a
  zoom other than the default.
- **Test:** none yet.  The shape it wants: two `snap`s of one map at two
  zooms, and `classify_world` shares that DIFFER — a golden would
  agree with whatever it started drawing.

## Fixed

### @D003 — the player is the one mover that throws its remainder away, so a shorter tick freezes it

- **Status:** **FIXED** 2026-08-17, plan 26 L2 — `Vehicle` carries a
  `Bank` and `vehicle_bank` releases whole hexes exactly as
  `enemy_bank` and `helper_bank` do.  The sweep that measured it reads
  **180 at all seven tick lengths and 360 boosting**, and the four
  functions that recorded the wrong numbers went RED on the phase that
  changed them.
- **Severity:** was High — it was silent, it was in the shipped mover,
  and it was a **blocker for the shorter tick**
  ([`plans/22`](plans/22-the-field-cache/README.md) and `@X058`, which
  made the timestep a free choice).  Today's tick hid it entirely.
- **Found while:** plan 26 L0, building the instrument that asks whether
  any mover is tick-length independent.  The plan predicted the defect
  from a reading of the source (`plans/26` § 2, *seven sites re-assert
  "do not lose a fraction" and one of them omits it*); L0 measured it.
- **Repro:** `tests/26_l0_the_timestep_sweep.loft`, or by inspection —

  ```
  (vehicle_speed(v) * tick_seconds) as integer?    — src/vehicle.loft:304
  ```

  and `Vehicle` has no `progress` field to carry a remainder in.
  `enemy_bank` and `helper_bank` both keep theirs.
- **Expected:** a rate is a rate — the player covers
  `VEHICLE_SPEED_HEX_PER_SECOND` hexes a second whatever the timestep
  is, exactly as every enemy and every helper does.
- **Observed:** over one simulated minute, at 667 / 500 / 333 / 200 /
  100 / 50 / 33 ms ticks (`@M030`):

  | mover | hexes a minute | true |
  |---|---|---|
  | miner 1.0 / robot 1.5 / scout 2.5 / helper 2.5 | exact at all seven | — |
  | **vehicle 3.0 hex/s** | **180 / 120 / 180 / 0 / 0 / 0 / 0** | 180 |
  | **boosting 6.0 hex/s** | **360 / 360 / 360 / 300 / 0 / 0 / 0** | 360 |

  End to end, driving 40 hexes down a painted corridor with a whole
  minute to do it in: the player arrives at 667 and 500 ms and **never
  leaves its starting hex** at 200 and 100 ms, while a robot beside it
  arrives at all four.  ⚠ Two consequences worth naming separately:
  at 500 ms the player still moves, at **two thirds** of its documented
  speed, which would report as a feel complaint rather than as
  arithmetic; and at 200 ms a **boosting** player covers 300 hexes
  where a cruising one covers none, so boost is worth an unbounded
  multiple of its 2x rather than 2x.
- **Why no gate saw it — three accidents in a row:** both shipped
  vehicle speeds are exact at the shipped tick (`3.0 * (1/1.5)` is 2.0
  to the bit, `6.0 * (1/1.5)` is 4.0); the half-tick
  `tests/23_k2a::test_the_mover_agrees_at_half_the_timestep` sweeps is
  exact too; and the one shortened timestep in the repo that WOULD see
  it — `23_k2a`'s tenth-tick case, where `3.0 * 0.0667` truncates to
  zero — banks an **enemy**.  ⚠ That last is
  `tests/11_f8`'s markerless-world trap with the axis and the subject
  swapped: the right sweep over a roster with none of the broken thing
  in it.  ⚠ And `@M013` cannot reach it from any direction — it varies
  the SPEED, at one tick length, through a mover that carries.
- **Workaround:** none was needed.  `TICK_SECONDS` was 1/1.5 s and
  nothing shortened it, which is exactly why it was worth writing down
  before something did.
- **Fix:** [`plans/26`](plans/26-the-fixed-step/README.md) **L2**
  (2026-08-17) — `src/tick_bank.loft`, one exact integer accumulator
  shared by all three movers, over the integer clock L1 built.  ⚠ A
  ROUNDING is not the fix and L0 measured that too: `+ 0.5` in the old
  expression fires all four defect records while **overshooting** (240
  hexes a minute at 500 ms) and still zeroing at the short end.  Only a
  carry is a rate.  ⚠ Deliberately NOT fixed in L0: the phase is the
  instrument, and a phase that changed the thing it measured would have
  measured nothing (`@X064`'s rule, plan 23 K3).
- **What it took with it:** `ENEMY_PROGRESS_EPSILON` and
  `HELPER_PROGRESS_EPSILON`, both **deleted** rather than zeroed.  The
  float bank needed them (`@M017`: zeroing the first turned 7 of 1149
  tests red and `scripts/validate.sh` with them); the integer bank has
  no rounding to guard.
- **Test:** [`tests/26_l0_the_timestep_sweep.loft`](tests/26_l0_the_timestep_sweep.loft)
  — the same instrument, asserting the true numbers, with the wrong
  ones kept in its prose and its function names so the record survives
  § The vehicle does TOO — the four functions that asserted **the wrong
  numbers** as the defect's record and L2's tripwire.  ⚠ They were a
  "not yet" pin in `tests/23_k1`'s sense: the phase that fixed this had
  to turn every one of them RED, and it did.  Falsified on arrival too
  — a vehicle that rounds instead of truncating fires all four while
  overshooting, which is what said the fix had to be a bank.

### @D004 — the two one-shot timers that never got a guard run a tick long at a shorter tick

- **Status:** **FIXED** 2026-08-17, plan 26 L3 — `Enemy.stand` and
  `WaveSchedule.lull` are `Timer`s over integer base units
  (`src/tick_timer.loft`), and both now read their true length at all
  seven tick lengths.
- **Severity:** was Med — silent, in the shipped simulation, and a
  **blocker for the shorter tick** exactly as `@D003` was.  It was
  RIGHT at today's 667 ms, which is why nothing had seen it.
- **Found while:** plan 26 L3, sweeping the one-shot timers at seven
  tick lengths before converting any of them — the same instrument L0
  pointed at the movers, aimed at the other family.
- **Expected:** a duration is a duration.  A 5 s pre-walk window and a
  15 s lull last 5 s and 15 s whatever the timestep is.
- **Observed** (`@M033`), against a true 8 / 10 / 15 / 25 / 50 / 100 /
  150 and 23 / 30 / 45 / 75 / 150 / 300 / 450:

  | timer | guard | 667 | 500 | 333 | 200 | 100 | 50 | 33 ms |
  |---|---|---|---|---|---|---|---|---|
  | helper recovery 60 s | epsilon |  90 | 120 | 180 |  300 |  600 | 1200 | 1800 |
  | tower rebuild 20 s   | epsilon |  30 |  40 |  60 |  100 |  200 |  400 |  600 |
  | boost 2 s            | epsilon |   3 |   4 |   6 |   10 |   20 |   40 |   60 |
  | **wave lull 15 s**   | **none** |  23 |  30 |  45 | **76** | **151** |  300 | **451** |
  | **pre-walk 5 s**     | **none** |   8 |  10 | **16** |  25 | **51** | **101** | **151** |

- **⚠⚠ Why it is worth a number of its own:** `plans/26` § 2 counted the
  epsilons as the brittleness and these two rows as fine.  **The
  epsilons were doing real work at every tick length and the two sites
  that never got one are the broken ones** — which is `@D003`'s shape
  in the other family: *the site that never got a guard at all*.  ⚠ Two
  of the three guarded timers count DOWN, exactly as these do, so the
  DIRECTION is not what separates them.  A guard is.
- **Workaround:** none was needed — `TICK_SECONDS` was 2/3 s and both
  are exact there.
- **Fix:** [`plans/26`](plans/26-the-fixed-step/README.md) **L3** —
  `src/tick_timer.loft`, one one-shot type over integer base units, and
  `HELPER_TIMER_EPSILON` / `TOWER_REPAIR_EPSILON` /
  `VEHICLE_TIMER_EPSILON` **deleted** with it.
- **Test:** [`tests/26_l3_the_timers.loft`](tests/26_l3_the_timers.loft)
  § Every timer in the game holds its duration at seven tick lengths —
  five profiles, one assertion each, with the pre-L3 float arithmetic
  reproduced beside them and asserted WRONG at seven of fourteen
  readings, so a green profile is a measurement rather than a
  restatement of the arithmetic that produced it.

### @D001 — clearing `prev.in_mouse_left` mid-step MANUFACTURES the edge it means to suppress

- **Status:** **Fixed 2026-08-12** — all four writes deleted from
  `src/editor_step.loft`.  Landed as its own step BEFORE plan 09 I1
  rather than inside it, for the reason I0 gives: I1's gate is a
  parallel run of the old and new input paths, and this fix
  deliberately makes them differ.  A parallel run against the buggy
  seam either goes red for the right reason and gets waved through,
  or gets "fixed" by porting the bug.
- **Severity:** Med — a held button plus Tab / Ctrl+N places a marker
  the player did not ask for, and `dirty` is set so it can reach disk.
  Not High only because holding the left button while pressing a hotkey
  is an uncommon gesture.
- **Found while:** plan 09 phase I0, probing whether `input`'s edge model
  matches the seam's.  The four `s.prev.in_mouse_left = false` writes are
  the one thing `input` structurally cannot reproduce, so the probe asked
  what they do — and the answer is: not what their comment says.
- **Repro:** measured 2026-08-12, loft 2026.8.0, interpreted.

  Ground mode, left button held and painting, then Tab goes down with the
  button still held:

  ```
  B4  Tab with the button HELD  -> mode = 1, markers = 1
  B4b Tab with the button UP    -> mode = 1, markers = 0
  ```

  Marker mode, a marker already placed, button still held, then Ctrl+N:

  ```
  B5  Ctrl+N, button HELD -> before = 1, after clear = 1,
                             at hover(7,0) = true, at old(4,0) = false
  ```

  So "clear all" leaves the map carrying a **new** marker at the hover
  hex.  `marker_empty()` ran; the forged edge then placed one.
- **Expected:** a held button crossing a mode flip or a clear does
  nothing — exactly what
  [`src/editor_step.loft:328`](src/editor_step.loft) says the write is
  for ("so a held button does not bleed across the boundary into the
  other mode").
- **Observed:** the opposite.  Clearing `s.prev.in_mouse_left` is what
  makes `input.in_mouse_left && !s.prev.in_mouse_left` true at the marker
  branch (`:510`) on that same step — so the write **creates** the rising
  edge instead of dropping it.
- **Why it survived:** `s.prev` is overwritten wholesale by
  `input_copy(input)` at `:525`, so the write never reaches the next
  frame at all.  Its only reachable effect is on branches BELOW it in the
  same step, and the marker branch is the one that re-reads `prev`.  The
  stroke it was supposed to end is already ended by `s.painting = false`,
  set beside each of the four sites — so the write is dead for its stated
  purpose and live only for the harm.
- **Sites:** `src/editor_step.loft` `:331` (mode toggle), `:390`
  (reload), `:406` (clear all), `:418` (undo).  I0 measured `:331`
  and `:406` and *reasoned* about the other two.  The fix measured
  all four, and the reasoning was right both times:

  | site | reasoned | measured |
  |---|---|---|
  | `:331` toggle | harmful | **harmful** — red before, green after |
  | `:390` reload | "same shape, no mode guard" | **harmful** — red before, green after |
  | `:406` clear all | harmful | **harmful** — red before, green after |
  | `:418` undo | "only true after a ground-mode stroke" | **DEAD** — green before AND after |

  `:418` is dead rather than harmful because its guard
  (`!undo_entry_is_noop(s.stroke)`) implies ground mode, while the
  marker branch it would feed requires marker mode, and the two
  cannot hold at once: every route to marker mode runs the toggle,
  which commits the stroke and empties it on the way past.  Pressing
  Tab and Ctrl+Z on one frame does not reach it either — the toggle
  empties the stroke first, so what fires is `:331`.
- **Workaround:** none — don't hold the button while pressing a hotkey.
- **Fix:** all four writes deleted.  Nothing replaced them: the stroke
  each one claimed to end is already ended by the `s.painting = false`
  beside it.  `s.prev` is now read-only for the whole of a step and
  written once at the end, which is stated as an invariant in the
  file's § Held keys header — the chokepoint, so a future action that
  wants to end a gesture reaches for the gesture's own state.
- **Test:** [`tests/09_d001_the_forged_edge.loft`](tests/09_d001_the_forged_edge.loft)
  — one per site, plus the compound Tab+Ctrl+Z gesture that shows
  `:418` is unreachable, plus two controls (an ordinary press still
  places a marker; a held sweep still lays exactly one).  Three go
  RED against the pre-fix seam, which is what says they can see the
  bug.  Plan 08's existing edge tests cannot: every one of them
  releases the button first, and holding it across another action is
  the whole gesture.

### @D005 — half of every box in the catalogue was wound INWARDS

- **Status:** **FIXED** 2026-08-18, plan 20 A5 — `part_mesh.loft::emit_box`
  now picks a distinct in-plane pair per face, swapped for the three
  negative faces, so `u x v` is the OUTWARD axis on all six.
- **Severity:** was High — under the `GL_CULL_FACE` that
  `ground_gl.loft` turns on for the whole frame, those triangles draw
  **nothing**, so every entity in the game would have been drawn with
  half of every box missing.  Shipped in A2 and survived A3 and A4.
- **Found while:** plan 20 A5, writing the winding gate BEFORE wiring
  the entities into GL — `ground_mesh.loft` gates the ground's winding
  as DATA three phases before a pixel exists, and this is that habit
  applied to the parts.
- **Repro:** emit any catalogue part and cross-product each triangle's
  `(b - a) x (c - a)` against its stored normal.  **72 of 144** on the
  hover unit came back negative.
- **Expected:** every triangle winds counter-clockwise seen from
  outside, which is what `emit_quad`'s own comment claims.
- **Observed:** `emit_box` chose *two in-plane axes, right-handed about
  the outward one* — and used the **same pair for a face and its
  opposite**.  `y x z` is `+x`, which is right for the `+x` face and
  backwards for the `-x` one; likewise `z x x` for `±y` and `x x y` for
  `±z`.
- **⚠⚠ Why it is worth a number of its own:** it changed **no count, no
  vertex position, no normal, and no `mesh_crc`** — the quad's four
  corners are the same rectangle walked the other way, so even a fold
  over the triangle INDICES agrees.  Forty-two tests across three
  phases passed over it.  ⚠ And the obvious fix does not work and looks
  as if it does: scaling `u` by the outward SIGN leaves `u * us`
  unchanged, because `us` is derived from `u`.  The pair has to be
  SWAPPED.
- **Test:** [`tests/20_a5_the_frame.loft`](tests/20_a5_the_frame.loft)
  `test_every_triangle_winds_with_its_normal` — swept over every
  `catalogue_names()` entry, emitted TURNED and off the origin so a
  placement that mirrored a part would be caught too.  It goes RED
  against the pre-fix emitter naming 72 of 144.

## See also

- [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md) — problems
  whose fix is upstream in loft.
- [`plans/README.md`](plans/README.md) — plans (multi-phase work,
  not bugs).
