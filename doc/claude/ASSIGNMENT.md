# Stack Slot Assignment — Analysis, Plan, and Current Status

## Problem

Stack slot positions are currently assigned **during code generation** in `state.rs`, driven
by the order in which `Value::Set` nodes are encountered while walking the IR.  The allocator
is a simple bump-pointer: each variable gets the next free byte offset and keeps it for the
lifetime of the whole function.

Scope analysis (`scopes.rs`) uses *structural nesting* as a proxy for variable lifetime.
When a variable is re-assigned in a sibling or child scope, `copy_variable` creates a fresh
`Variable` entry so the two lifetimes get distinct positions.  But `copy_variable` is an
ad-hoc patch and does not cover every case.

---

## Confirmed bugs

### Bug 1 — `13-file.loft` (fixed 2026-03-10)

Two variables inside the buf-read block — `b` (Buffer, scope 12) and the local copy of
`f` (File alias for `__ref_8`, scope 5) — were both allocated at stack position 244.  `b`
was still live when `f`'s `PutRef` overwrote slot 244, orphaning `b`'s store.  On the next
`ConvRefFromNull`, the LIFO store allocator found `allocations[max].free == false` and
panicked with "Allocating a used store".

### Bug 2 — `t_4Code_define` slot conflict (investigated 2026-03-11, not yet fixed)

Inside the `define` function in `lib/code.loft`, the variable `res` (integer, 4 bytes) is
allocated at slot 66 — slot [66, 70).  In the else-branch of an if-statement inside a loop,
the code goes through a sequence that:

1. Pushes two temporary DbRefs onto the stack at positions [62,74) and [74,86),
2. Runs `CopyRecord` which consumes both (stack drops from 86 → 62),
3. Then runs `generate_set` for the new variable `_elm_1` (DbRef, 12 bytes), which
   calls `claim(_elm_1, stack.position=62, ...)`.

`_elm_1` ends up at slot [62,74), which overlaps `res` at [66,70) — the integer and
a DbRef share bytes [66,70).

**Why this is a runtime bug, not just a validation failure:** the first op inside
`generate_set(_elm_1)` is a `VarRef(self)` that pushes a 12-byte DbRef starting at
slot 62, physically overwriting `res`'s bytes [66,70) on the runtime stack.

`validate_slots` catches this at compile time (debug builds) and panics before execution.

---

## Root cause

The bump-pointer allocator in `generate_set` uses `stack.position` as the next free slot.
`stack.position` tracks the "logical top of the expression stack" during code generation —
it advances when values are pushed and recedes when operators consume arguments.

After a net-negative operator (one that consumes more bytes than it produces — e.g.
`CopyRecord` consumes 2×12 bytes and produces 0), `stack.position` drops below previously-
allocated variable slots. Those variables are **still alive** but their slots are now
"below the current stack top". The next `generate_set` call incorrectly claims the dropped
position for a new variable, overlapping the live ones.

The fundamental invariant that breaks:
> `stack.position` at the moment `claim(v, stack.position, …)` is called must be ≥ the
> top of all slots occupied by currently-live variables.

---

## Why the naive fix doesn't work

An obvious attempt is to advance `stack.position` to `min_var_position()` (the maximum
end-slot of all already-assigned variables) before every `claim` in `generate_set`.

**This breaks other tests** (confirmed by regression in `enums::polymorph`).

The reason: `stack.position` is not just a compile-time bookkeeping value — it is the
source of truth for **relative offset calculations** in all subsequent bytecode. Each
`VarRef`, `FreeStack`, and related op encodes the distance between the current
`stack.position` and the variable's `stack_pos`.  If you advance `stack.position` at
compile time without emitting bytecode that correspondingly advances the **runtime** stack
pointer (`State::stack_pos`), the two go out of sync.

Concretely: after a loop body, the runtime stack pointer is restored to `loop_pos` (by
`OpFreeStack`). If a post-loop variable is then claimed at `min_var_position() > loop_pos`,
all accesses to that variable compute offsets relative to the inflated compile-time
`stack.position` (e.g. 160) but the runtime pointer is at the true `loop_pos` (e.g. 128).
Every address is wrong by the gap, causing reads from incorrect memory and
`copy_nonoverlapping` panics in debug builds.

### What makes the two cases different

| Scenario | Runtime stack at claim time | Variable below stack.position | Live? | Safe? |
|---|---|---|---|---|
| `_elm_1` in `t_4Code_define` | 62 (after CopyRecord) | `res` at [66,70) | **Yes** | **No — real conflict** |
| `t` after loop in `polymorph` | 128 (loop_pos) | `v` at [144,156), `a` at [156,160) | **No** | Yes — safe slot reuse |

The correct fix must distinguish between these cases using **live interval** information.

---

## Correct fix strategy

The proper solution is to check live intervals at slot-assignment time.  There are two
viable options; see the **Recommendation** section at the end for which to do first.

---

### Option A — Full separation (right long-term architecture)

Compute all slot positions in a dedicated pass **before** code generation, using the live
intervals already produced by `compute_intervals`.  Then `generate_set` in `state.rs`
simply reads the pre-assigned `stack_pos` instead of calling `claim()`.

**The bridging problem** — the hard part of Option A.  When `generate_set(_elm_1)` is
reached, `stack.position` is 62 (after `CopyRecord` consumed its arguments) but the
pre-assigned slot is 70 (above `res`).  The code generator must advance the **runtime**
stack pointer from 62 to 70 before generating the value expression.  There is currently no
bytecode instruction that just bumps the runtime stack by N bytes without doing anything
else.  Three sub-options:

1. **New opcode `OpAdvanceStack(N)`** — clean, minimal addition; bumps `State::stack_pos`
   by N without writing anything.

2. **N/4 `OpConstInt(0)` padding pushes** — no new opcode needed; ugly but functionally
   correct.  The garbage values are never read (they fall below the variable's slot which
   starts at the advanced position).

3. **Scope-analysis pre-init (cleanest variant)** — extend `scopes.rs` to emit a
   `Set(v, Null)` for any DbRef/Reference variable `v` that is first assigned inside an
   if/else branch.  This fires `claim(v)` before the if-else, when `stack.position` is
   still safely high.  The branch assignment then becomes a *re*-assignment, which goes
   through the `set_var` path that copies the expression result into the pre-claimed slot
   — no bridging needed.  This is analogous to what `scopes.rs` already does for
   depend-typed variables.  It is the preferred sub-option because it requires no new
   opcodes and no changes to `state.rs`; all the work stays in `scopes.rs`.

**Pros:** architecturally correct; enables real slot reuse; sets the stage for removing
`copy_variable`; no per-call overhead in the code generator.

**Cons:** bigger change; sub-option 3 requires a careful audit of `scopes.rs` to ensure
all first-use-in-branch cases are covered.

---

### Option B — Live-interval guard in `generate_set` (targeted short-term fix)

Add a `codegen_seq: u32` counter to `State` that mirrors `compute_intervals`'s `seq`
counter, incremented at the **same IR nodes** in `generate`.  In `generate_set`, before
claiming a new slot, advance `stack.position` only past variables that are **still alive**
at the current sequence point:

```rust
// Only push stack.position above variables whose live interval has not yet ended.
let min_pos = stack.function.variables.iter()
    .filter(|v| v.stack_pos != u16::MAX && v.last_use >= self.codegen_seq)
    .map(|v| v.stack_pos + size(&v.type_def, &Context::Variable))
    .max()
    .unwrap_or(0);
if stack.position < min_pos {
    stack.position = min_pos;
}
```

This correctly skips the dead loop element `v` in the `polymorph` test (its `last_use` is
below the current seq when `t` is claimed) and correctly blocks the slot for the live `res`
in `t_4Code_define` (its `last_use` is well above the current seq when `_elm_1` is claimed).

**The maintenance hazard** — `codegen_seq` must increment at the exact same IR nodes as
`compute_intervals`'s `seq`, in the same traversal order.  `compute_intervals` increments
for `Var`, `Set` (after processing the value sub-tree), `Call` (once per argument list plus
one for the call itself), and for unrecognised leaf nodes.  Any divergence — a missed
increment, an extra one, a branch traversed in a different order — silently makes liveness
checks wrong.  It must be kept in sync with `compute_intervals` by convention and tested.

**Pros:** ~60 lines of change; no new opcodes; no restructuring; directly targeted; easy
to verify with `validate_slots`.

**Cons:** the seq counter is a maintenance hazard; does not enable slot reuse; does not
clean up the underlying architecture.

---

### Recommendation

**Do Option B now; follow up with Option A (sub-option 3) later.**

Option B directly unblocks the `last` and `dir` failing tests with a small, auditable
change.  Once `codegen_seq` is in place, `validate_slots` acts as an immediate regression
check — if the sequencing ever drifts, the check catches it.

Option A sub-option 3 (scope-analysis pre-init) is the correct long-term solution.  It
keeps the fix entirely in `scopes.rs`, requires no changes to `state.rs` or new opcodes,
and once it is in place the Option B guard in `generate_set` becomes redundant and can be
removed, leaving a clean architecture.

**Suggested order:**
1. Implement Option B — unblocks failing tests immediately.
2. Implement Option A sub-option 3 — extend `scopes.rs` to pre-init first-in-branch DbRef
   variables; at that point the Option B guard is redundant and can be deleted.
3. Remove `copy_variable` once the assignment pass (Step 3 below) is complete and
   `validate_slots` is green across all suite tests.

---

## Full separation plan (Option A — preferred long-term)

### Proposed data flow

```
Parser (two passes)
  └─ variables.rs: add_variable(), copy_variable()   ← names, types, scopes (unchanged)
       scope_analysis (scopes.rs)                    ← scope IDs, OpFreeText/OpFreeRef (unchanged)
            compute_intervals (variables.rs)         ← first_def/last_use per variable [DONE]
                 assignment pass (NEW — variables.rs)← assign stack_pos from intervals
                      [debug] validate_slots (NEW)   ← assert no overlapping live slots [DONE]
                           byte_code (state.rs)      ← reads stack_pos, does NOT call claim()
                                execute()
```

### Key concepts

**Live interval** — For variable `v`, `[first_def, last_use]` measured in instruction
sequence numbers (monotonically increasing counter assigned to each `Value` node).

- `first_def`: sequence number of the `Value::Set(v, …)` that first defines `v`.
- `last_use`:  sequence number of the last `Value::Var(v)` (or implicit `OpFreeText`/
  `OpFreeRef` use) for `v`.

**Overlapping lifetimes**: `u.first_def <= v.last_use && v.first_def <= u.last_use`.

### Implementation steps

#### Step 1 — `compute_intervals` (DONE — `src/variables.rs`)

`compute_intervals(val, function, free_text_nr, free_ref_nr, seq)` walks the IR in
execution order, recording `first_def` and `last_use` on each `Variable`.  Currently called
from `scopes::check` after the scope pass.

#### Step 2 — `validate_slots` (DONE — `src/variables.rs`)

`validate_slots(function, data, def_nr)` (debug-only) checks every variable pair for
simultaneous live-interval and slot overlap. Logs a full diagnostic (variable table + IR
dump) then panics. Uses the extracted `find_conflict(vars) → Option<(i, u_end, j, v_end)>`
helper for testability.

Unit tests in `src/variables.rs` (`#[cfg(test)]`) cover:
- Non-overlapping intervals (no conflict)
- Non-overlapping slots (no conflict)
- Integer inside wider DbRef slot (conflict detected)
- Various edge cases

Integration tests in `tests/slot_assign.rs` cover the three patterns:
- Long-lived int accumulator with loop DbRef element
- DbRef before loop + loop element (both DbRef-sized, distinct slots)
- Nested loops (outer/inner loop elements both alive simultaneously)

#### Step 3 — Assignment pass (TODO)

Add `assign_slots(vars: &mut [Variable], arguments_size: u16)` to `src/variables.rs`.

Algorithm (linear scan):

```
active: list of (interval_end, slot_start, slot_size)
free_primitive_slots: list of (slot_start, slot_size)

sort vars by first_def ascending
position = arguments_size   // next free byte after arguments

for each variable v in sorted order:
    // expire slots whose interval ended before v.first_def
    for each slot in active where interval_end < v.first_def:
        if slot is primitive type:
            free_primitive_slots.push(slot)
        active.remove(slot)

    if v is primitive type:
        if free_primitive_slots has compatible slot s:
            v.stack_pos = s.start
            free_primitive_slots.remove(s)
        else:
            v.stack_pos = position
            position += size(v.type_def, Context::Variable)
    else:
        // ref / text: always fresh slot, never reuse
        v.stack_pos = position
        position += size(v.type_def, Context::Variable)

    active.push((v.last_use, v.stack_pos, size(v)))
```

#### Step 4 — Remove `claim()` from `state.rs` (TODO)

After the assignment pass, `stack_pos` is pre-assigned. `generate_set` should read it
directly instead of calling `claim`.

**Important:** `stack.position` in `state.rs` (the compile-time stack pointer) still needs
to be advanced correctly so that relative offsets for all subsequent ops are right. The
cleanest approach: set `stack.position = max(stack.position, v.stack_pos + size(v))` after
reading the pre-assigned slot, rather than calling `claim`.

#### Step 5 — Remove `copy_variable` (deferred)

After Steps 3–4 are complete and `validate_slots` is green across all tests, `copy_variable`
can be removed. Variables re-used across sibling scopes will simply have non-overlapping
intervals and can safely share a slot (or not) based on the interval check.

---

## Current state (as of 2026-03-11)

| Step | Status |
|---|---|
| `compute_intervals` | **Done** (called from `scopes::check`) |
| `validate_slots` + `find_conflict` | **Done** (debug-only, panics with full diagnostics) |
| Unit tests for `find_conflict` | **Done** (`src/variables.rs` test module) |
| Integration tests for slot conflicts | **Done** (`tests/slot_assign.rs`, 3 tests) |
| Bug 2 (`t_4Code_define`) root cause identified | **Done** (see above) |
| Assignment pass | **TODO** |
| Remove `claim()` from `state.rs` | **TODO** |
| Remove `copy_variable` | **Deferred** |

The naive `min_var_position()` guard in `generate_set` was attempted but **reverted**
because it breaks the `polymorph` enums test (it advanced the compile-time `stack.position`
without a corresponding bytecode instruction to advance the runtime pointer, causing all
post-loop variable accesses to use wrong offsets).

---

## Invariants to preserve

- **Arguments** are allocated first (positions 0 … arguments_size−1) before any locals.
  The assignment pass must skip variables with `argument == true` (they already have
  `stack_pos` set by the existing argument-layout logic).
- **`OpFreeText` / `OpFreeRef` insertion** by `scopes.rs` must happen *before*
  `assign_slots()` runs, because those ops introduce implicit last-uses that must be
  visible to liveness.
- **Dependent types** (`Type::Text(deps)`, `Type::Reference(_, deps)`, etc.) carry
  borrow information; the assignment pass must not place a borrowed variable in a slot
  that is freed before the borrow ends.  For the initial implementation, treating all
  ref/text types as non-reusable is sufficient.
- The **runtime stack pointer** (`State::stack_pos`) is unaffected by the assignment pass;
  it still advances and retreats during `Value::Block` execution via `OpFreeStack`.
  The compile-time `Variable::stack_pos` only determines where within the frame a variable
  lives.
- After the assignment pass sets all `stack_pos` values, `state.rs::generate_set` must
  still advance `stack.position` past each variable's slot so that subsequent ops compute
  correct relative offsets. The simplest way: `stack.position = max(stack.position,
  var.stack_pos + var_size)` when processing a variable whose slot is already set.

---

## Files affected

| File | Change |
|------|--------|
| `src/variables.rs` | `first_def`/`last_use` on `Variable`; `compute_intervals()`; `find_conflict()`; `validate_slots()`; `min_var_position()` (helper); unit tests |
| `src/scopes.rs` | Calls `compute_intervals()` at end of `check()` |
| `src/state.rs` | Calls `validate_slots()` after `def_code()`; future: remove `claim()` |
| `src/stack.rs` | `Stack` struct; `position` field; `claim()` via `Function` |
| `tests/slot_assign.rs` | 3 integration tests for conflict patterns |
