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

### @D001 — clearing `prev.in_mouse_left` mid-step MANUFACTURES the edge it means to suppress

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
  (reload), `:406` (clear all), `:418` (undo).  `:331` and `:406` are
  the two MEASURED above.  `:390` (reload) has the same shape and no
  mode guard, so it should behave like `:406`; `:418` (undo) sits
  inside `if !undo_entry_is_noop(s.stroke)`, which by inspection can
  only be true after a ground-mode stroke — **both of those are
  reasoned, not measured.**  I1's tests cover all four rather than
  trusting the reasoning.
- **Workaround:** none — don't hold the button while pressing a hotkey.
- **Fix plan:** delete all four writes.  Belongs to plan 09 phase **I1**,
  which rewrites this code anyway to take its input from `input`; folding
  it in there means the seam's edge handling changes once.  I0 deliberately
  changed no `src/`: it is a probe phase, and its deliverable is this
  measurement.
- **Test:** none yet — I1 adds one per site, asserting the marker count
  does NOT move.  Note that plan 08's existing edge tests cannot see this:
  none of them holds the button across another action.

## Fixed

*(none yet)*

## See also

- [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md) — problems
  whose fix is upstream in loft.
- [`plans/README.md`](plans/README.md) — plans (multi-phase work,
  not bugs).
