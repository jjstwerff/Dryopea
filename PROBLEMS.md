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

### @D003 — the player is the one mover that throws its remainder away, so a shorter tick freezes it

- **Status:** Open
- **Severity:** High — it is silent, it is in the shipped mover, and it
  is a **blocker for the shorter tick**
  ([`plans/22`](plans/22-the-field-cache/README.md) and `@X058`, which
  made the timestep a free choice).  Today's tick hides it entirely.
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
- **Workaround:** none needed today.  `TICK_SECONDS` is 1/1.5 s and
  nothing shortens it, which is exactly why this is worth writing down
  before something does.
- **Fix plan:** [`plans/26`](plans/26-the-fixed-step/README.md) **L2** —
  the vehicle gains the bank it never had, over the integer clock L1
  builds, so the fix is exact rather than nudged.  ⚠ Deliberately NOT
  fixed in L0: the phase is the instrument, and a phase that changed
  the thing it measured would have measured nothing (`@X064`'s rule,
  plan 23 K3).
- **Test:** [`tests/26_l0_the_timestep_sweep.loft`](tests/26_l0_the_timestep_sweep.loft)
  § The vehicle does NOT — four functions that assert **today's wrong
  numbers**, as the defect's record and L2's tripwire.  ⚠ They are a
  "not yet" pin in `tests/23_k1`'s sense: the phase that fixes this
  must turn every one of them RED, and the replacement is the
  assertion the banked-mover functions in the same file already make.
  Falsified on arrival — a vehicle that rounds instead of truncating
  fires all four, and each names its whole profile.

## Fixed

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

## See also

- [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md) — problems
  whose fix is upstream in loft.
- [`plans/README.md`](plans/README.md) — plans (multi-phase work,
  not bugs).
