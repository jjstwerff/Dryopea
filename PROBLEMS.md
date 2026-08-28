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

⚠ **Nothing is open today.**  `@D002` was closed by BACKLOG C7 and
`@D006` by BACKLOG C10, both on 2026-08-28; `@D007` and `@D008` were
found and fixed the same day they were found.

## Fixed

### @D008 — a mob that turns at an anchor MID-TICK has no field for the next leg, and `slip` swallows the hexes

- **Status:** **FIXED** 2026-08-28, `plans/30` R4 — `errand_fields` builds
  a field per **anchor of the ROW** instead of per **destination the mob
  is making for now**.
- **Severity:** Med — never shipped (no scenario has a routine), but it
  made a mob's phase drift from its own rule without any gate saying so,
  which is the class `plans/30` exists to make impossible.
- **Found while:** `plans/30` R4, putting a SCOUT into the R4 gate so the
  mover's finishing stop had something to be visible against.
- **Repro:** a `ROLE_GATHER` mob of `ENEMY_KIND_SCOUT` over open ground,
  driven through `wave_tick`.  It reads `slip = 1 200 000` (one hex) by
  tick 15 and roughly one hex per two rounds thereafter; over three
  minutes its phase is **twelve hexes** from where the rule puts it.
- **Expected:** a mob that reaches an anchor with hexes left in its bank
  turns and spends them on the next leg (`@X335`), losing nothing.
- **Observed:** `errand_fields` read `errand_destination(e, state.now)`
  once per mob at the top of the tick, so the vector held a field for
  the leg the mob STARTED on.  After `errand_arrive` flipped the bag
  mid-tick, `errand_hex` looked for a field toward the other anchor,
  found none, could not place the step — and `errand_step` charged the
  hex to `slip`.
- **Why nothing caught it:** ⚠⚠ **twice silent.**  (1) The shipped robot
  releases exactly one hex a tick, so it can never turn with hexes left
  — `@M014`'s class, and every carrier in the R1-R3 corpus was one.
  (2) ⚠⚠ **The conformance gate AGREES**: the rule is read at
  `now − slip`, so charging the lost hex to `slip` moves the rule down
  onto the body and `cycle_at(e, now) == (e.q, e.r)` stays exactly true.
  ***`slip` is a currency that can pay for a defect***, which is
  `@X337`'s *liveness is a second gate* arriving a second time.
- **Fix:** `src/errand.loft::errand_fields` — one field per
  `(anchor, climb)` over `row.empty` and `row.laden`, deduped.  ⚠ The
  cost was already budgeted: `@M072`'s *a mob asks for at most three
  anchors*.
- **Test:** `tests/30_rc_the_conformance.loft` — the liveness gate
  `test_nothing_blocked_it_so_nothing_slipped` gained a **scout**
  carrier (slip must be 0), and
  `test_a_roster_with_no_routine_is_the_game_it_always_was` pins the
  field count at **2 per routine**.  `tests/30_r4_home.loft` catches it
  from the other side: the scout's departure slides a whole round late.

### @D007 — a dropped BEACON round-trips as a WRECK: `emit_cargo` writes a kind the `object` verb cannot name

- **Status:** **FIXED** 2026-08-28, BACKLOG D2 — one line in
  `emit.loft::emit_cargo` and one in `script.loft`'s `object` verb.
- **Severity:** Med — it silently changed captured content.  `plans/18`
  § S2's round trip is a GATE, and a scenario that dropped a beacon
  replayed with a wreck in its place.
- **Found while:** BACKLOG D2, writing
  `tests/scripts/the-opening-two-hundred.keys` — **the first scenario in
  the corpus to leave a beacon on the ground.**  The round-trip sweep
  named it exactly: `cargo[0].kind: 2 vs 0`.
- **Repro:** buy a tower beacon at the core, drop it, `emit_keys` the
  situation, replay the emitted `.keys`.  The object comes back
  `CARGO_WRECK`.
- **Observed:** `emit_cargo` wrote `"wreck"` for anything that was not
  `CARGO_TOP`, so `CARGO_BEACON` was written as a wreck; and the
  `object` verb accepted only `wreck` and `top`, so even a correct
  spelling had nowhere to land.
- **⚠⚠ The class, and it is the third instance:** a value the WRITER can
  produce and the READER cannot name.  `cargo` had been writable and
  unreadable in the `raise` verb since plan 23 K0 and `spoil` arrived
  with BACKLOG C9 — both fixed there, in the same shape.  ⚠ **The writer
  and the reader are a PAIR**, and a kind added to one and not the other
  is invisible until a scenario happens to produce it.
- **⚠ Why it survived:** `CARGO_BEACON` shipped with plan 27 C4 and
  **nothing in the corpus had ever dropped one** — every beacon in every
  scenario was carried to a marker and planted.  The gate was correct
  and had no input that could fail it, which is `CLAUDE.md`'s *a gate
  whose reading is already saturated cannot see what you built* wearing
  the other face: a gate with no case that exercises the branch.
- **⚠⚠ AND THE GATE THAT SHOULD HAVE CAUGHT IT ASSERTED THE BUG.**
  `tests/18_s1b_the_vocabulary_is_total.loft` — the file whose whole
  claim is *the vocabulary is TOTAL* — contained
  `test_a_cargo_kind_is_named_and_a_typo_is_refused`, which fed it
  `object 7 0 beacon 0 ground` and required a REFUSAL, calling `beacon`
  *an unbuilt kind*.  It was not unbuilt: `emit_cargo` could already
  produce one.  ⚠ So the totality gate had encoded the single gap it
  exists to prevent, and it is now split into *every kind the writer
  emits is authorable* and *a typo is refused* — the claim its own file
  name makes.
- **Test:** `tests/18_s2_the_round_trip.loft` sweeps every scenario, and
  `the-opening-two-hundred.keys` is now the case that reaches this
  branch — so the guard is the corpus rather than a new assertion.

### @D006 — `walk_vehicle` is read by nothing, so the hovering movers cannot cross water

- **Status:** **FIXED** 2026-08-28, BACKLOG C10 — `passable.loft` gained
  `hex_hoverable` and `can_hover`, and `vehicle.loft::drive_along` — the
  shared chassis the player and every helper use — asks the second door.
- **Severity:** Medium, and it went up the day the player could BUILD
  water.
- **Found while:** BACKLOG C5, probing the claim the moat feature was
  designed around — *the depth is what stops the moat being free,
  because the crew HOVER and fall in*.  The probe falsified it, which is
  the whole reason it was written before the feature.
- **Observed:** `can_climb` refused a step whose *either* end failed
  `hex_walkable`, and `hex_walkable` answered `walk_ground` for every
  caller — so the vehicle was stopped by flat sea and by `steep_rock`
  exactly as a robot is.
- **⚠⚠ The probe was right about the CODE and the design was right about
  the GAME.**  With the column read, C5's headline comes back exactly:
  a hovering mover crosses flat sea for free, **drops into a trench for
  free** — a drop always is — and then owes the climb back out, which
  `CLIMB_VEHICLE` (0.4 m) does not have and
  `VEHICLE_BOOST_CLIMB_METRES` (3.0 m) does.  ⚠ So `docs/PLAYING.md`'s
  *boost is the only way out of a base you have sealed* is true of
  trenches again, and the palette's 0-1-3-8 is priced against the boost
  for the first time: `water` and `rapids` are trenches a boost leaves
  and a `waterfall` is a hole nothing gets out of.
- **⚠⚠ It was SMALLER than this entry predicted, and the reason is
  worth keeping.**  *A change to the ONE passability rule* — it is one
  rule with two doors over one implementation, because `can_climb` takes
  a CLIMB rather than a kind and the flow fields are built for ENEMIES
  only.  Nothing that walks moved: not a field, not a mover, not one of
  the 833 gate measurements.  ⚠ And `tests/11_f6`'s *give a class a
  second movement axis and this goes red* did NOT fire — it compares the
  regular against an unknown kind, both of which walk, so the pin never
  pointed at the hazard its comment named (`@M025`'s shape).
- **⚠ One palette edit, and it is not a tidying.**  `steep_rock` carried
  `walk_vehicle: true` on the strength of *the floating vehicle hovers
  above terrain*, and a 0.4 m clearance does not clear a cliff.  The
  flag was unearned because the cliff has **no HEIGHT to be stopped
  by** — `height_override` is null and the `slope` 40 that should raise
  it waits on plan 02 — so `walk_ground: false` was the only thing
  carrying *this is a cliff*.  Reading the column without that edit
  would have made a massif drivable, and
  [`maps/the_gap_03.keys`](maps/the_gap_03.keys) states in its own
  header that **the gap is the only way through for anybody, in either
  direction**.  ⚠ It flips back the day a cliff is tall.
- **Test:** [`tests/c10_the_hover.loft`](tests/c10_the_hover.loft) and
  [`tests/scripts/a-trench-you-fall-into.keys`](tests/scripts/a-trench-you-fall-into.keys)
  — falls in, is still there twenty ticks later, boosts out, and the
  crew who share the chassis and have no boost do not.  ⚠
  `tests/c5_the_moat.loft` `@DRY-129` was the test that PINNED this
  defect and is rewritten rather than deleted: it is the same drive
  reading the other answer.

### @D002 — `cam.zoom` changes no pixel: the wheel moves a number no renderer reads

- **Status:** **FIXED** 2026-08-28, BACKLOG C7 — `editor_view.loft`
  gained `view_ppm(cam)` (`VIEW_PPM / zoom`), and **`VIEW_PPM` itself is
  now PRIVATE**, which is the half that keeps it fixed: a test cannot
  stop the next caller reaching for a base scale that looks like the
  answer, and a constant it cannot NAME can.
- **Severity:** Med — the editor had a control that appeared to work and
  did nothing; and it removed a lever [`docs/PARTS.md`](docs/PARTS.md)
  § What could kill this design was counting on.
- **Found while:** writing `docs/PARTS.md` (plan 20), checking whether
  "zoom in to see the detail" was an option if entity sprites read too
  small at `VIEW_PPM` 24.
- **Repro:** `grep -rn "\.zoom" src/` — `camera.loft` moved it on the
  wheel (`ZOOM_MIN..ZOOM_MAX`), `save.loft` persisted it, `script.loft`
  walked the camera to it and printed it in the state line.  **No file
  under `src/` that drew anything read it**: `render.loft` did not
  mention `zoom`, and `render_editor_frame` took `VIEW_PPM` as a
  constant.
- **Expected:** scrolling the wheel changes how much world the frame
  shows.
- **Observed:** the number changed, was saved, was reported by `snap`'s
  state line — and the picture was identical at `z1` and at `z6`.
- **⚠⚠ What the fix turned out to be, and it was not one place.**  The
  fix plan said *derive the render scale in the ONE place
  `render_editor_frame` passes it, so the GL loop and `snap` cannot
  disagree*.  There are **four** paths, and the fourth is not a drawing
  at all: the GL loop's frame, `snap`'s frame, `classify_world` behind
  the `frame` measurement, and **`screen_to_hex`**, which inverts a
  pointer position back to a hex.  ⚠ Fixing only the drawing would have
  made every click land on the wrong hex at any zoom but 1 — a worse
  defect than the one being fixed — so the door is a function all four
  call rather than a derivation inside one of them.
- **⚠ And the rebaseline it warned about was an EMPTY SET.**  *It
  rebaselines every golden taken at a zoom other than the default* —
  measured: all 16 goldens in the tree are drawn at zoom 1
  (`camera_default()` or `camera_at(…, 1)`), so not one moved, and
  neither did any of the 827 gate measurements.  The only scenario at a
  non-default zoom is `v1b_snap.keys`, which takes pictures and asserts
  nothing.
- **Test:** [`tests/c7_the_zoom.loft`](tests/c7_the_zoom.loft) and
  [`tests/scripts/the-wheel-changes-the-view.keys`](tests/scripts/the-wheel-changes-the-view.keys)
  — exactly the shape this entry asked for: one map at three distances,
  read as `classify_world` SHARES rather than as an image, because a
  golden would agree with whatever it started drawing.  The grass patch
  covers **0.207 / 0.052 / 0.013** of the frame at z1 / z2 / z4 — a
  quarter each doubling, because a share is an AREA, which is the half
  a "they differ" reading cannot see.

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
