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

### Option B — Live-interval guard in `generate_set` (ATTEMPTED — DOES NOT WORK)

**Status: attempted 2026-03-11, reverted.  Do not implement.**

The idea was: before calling `claim(v, stack.position, ...)` in `generate_set`, advance
`stack.position` past all variables that are still alive at `v`'s `first_def` point:

```rust
let my_first_def = stack.function.first_def(v);
if my_first_def != u32::MAX {
    let min_pos = stack.function.min_safe_claim_pos(my_first_def);
    if stack.position < min_pos {
        stack.position = min_pos;
    }
}
```

(Public helpers `first_def(v)` and `min_safe_claim_pos(seq)` were added to `Function` in
`variables.rs`.)

#### Why it fails: the bridging invariant

`stack.position` in `Stack` is not just a bookkeeping value — it is the **source of truth
for every `VarRef` offset** emitted during code generation.  `OpVarRef(var_pos)` at runtime
computes `address = State::stack_pos − var_pos`, where `var_pos = stack.position −
var.stack_pos` at compile time.  This is correct only when `stack.position == State::stack_pos`
at every code point — the **bridging invariant**.

When the guard fires (e.g., advancing `stack.position` from 62 to 70 to skip over `res`), no
bytecode is emitted.  The runtime `State::stack_pos` stays at 62.  Every subsequent `VarRef`
in the value expression for the new variable now uses an offset computed from 70, but the
runtime reads from `62 − offset` — which is 8 bytes too low.  For `self` at argument
position 0 that underflows to a garbage address.

#### Observed failures (2026-03-11)

Both `wrap::last` and `wrap::dir` crashed with **SIGSEGV** (invalid memory reference) during
execution.  `wrap::dir` also produced an index-out-of-bounds panic in `keys.rs`.  These tests
were previously failing only with a `validate_slots` panic at compile time; the guard changed
the failure mode from "compile-time panic" to "runtime memory corruption".

#### Conclusion

There is **no way to advance `stack.position` at compile time without simultaneously emitting
bytecode** that advances the runtime stack pointer to match.  Any guard-only approach
(without bridging bytecode) is fundamentally broken whenever the value expression reads
variables.

The only correct fixes are:
- **`OpAdvanceStack(N)` opcode** — emit a new instruction that bumps `State::stack_pos` by N.
- **Scope-analysis pre-init (Option A sub-3)** — hoist the `claim` to a safe point BEFORE the
  if/else, so the value expression is never generated while there is a divergence.

Option A sub-3 is preferred (no new opcode, all changes in `scopes.rs`).  See below.

---

### Recommendation

**Implement Option A sub-option 3 directly.**

Option B was attempted and failed.  Option A sub-3 is the correct solution: it moves the
slot claim to a structurally safe point so no divergence can arise, fixes both the
`validate_slots` panic and runtime correctness, and covers all cases including those where the
conflicting code path is actually executed.

---

### Option A sub-option 3 — Scope-analysis pre-init (PARTIALLY IMPLEMENTED — 2026-03-11)

#### Core idea

`scopes.rs` already pre-emits `Set(dep, Null)` for dependent-type variables before they are
used (see the `depend` block inside `scan` for `Value::Set`).  Extend the same mechanism to
**Reference/Vector/Text variables that are first assigned inside an if/else branch**: emit
`Set(v, Null)` *before* the `Value::If` node, at the enclosing scope where `stack.position`
is still safely above all live variable slots.

When codegen later reaches `Set(v, actual_value)` inside the branch, `v` is already claimed
(pos != u16::MAX).  The assignment takes the **re-assignment path** in `generate_set`, which
calls `set_var`.  `set_var` generates the value expression at the current (valid)
`stack.position` and copies the result into `v`'s pre-claimed slot via `OpPutRef`
— no bridging needed, no divergence possible.

#### Why the pre-init is always at a safe stack position

The pre-init fires before the `Value::If` node is entered.  At that point, the runtime stack
contains only variables claimed by enclosing scopes plus the result of any preceding
expressions.  The CopyRecord that would otherwise lower `stack.position` into the danger zone
lives *inside* the if/else branch — it has not run yet when the pre-init fires.  Therefore
`stack.position == State::stack_pos` and the `OpConvRefFromNull` (or `OpText`) emitted by the
pre-init writes at the correct address with no divergence.

#### What is implemented

##### `needs_pre_init` predicate (implemented in `src/scopes.rs`)

All Reference, Vector, Enum-ref, and Text types are included regardless of dep:

```rust
fn needs_pre_init(tp: &Type) -> bool {
    matches!(
        tp,
        Type::Text(_) | Type::Reference(_, _) | Type::Vector(_, _) | Type::Enum(_, true, _)
    )
}
```

Borrowed variables (non-empty `dep`) are included because they occupy 12 bytes on the stack
and can cause the same slot overlap as owned variables.  A separate `deps_ready` check in
`find_first_ref_vars` ensures we only pre-init a borrowed variable when all its deps are
already in `var_scope` (otherwise the `OpCreateStack` emitted for a borrowed-Null pre-init
would reference an uninitialised slot).

##### `find_first_ref_vars` helper on `Scopes` (implemented in `src/scopes.rs`)

Recursively walks a `Value` subtree and collects variables that:
- appear as the target of `Value::Set(v, ...)`,
- are not yet in `var_scope`, and
- satisfy `needs_pre_init` and `deps_ready`.

Recurses into nested `If` and `Block` but NOT `Loop`.

##### Modified `scan` arm for `Value::If` (implemented in `src/scopes.rs`)

Before scanning the branches, `find_first_ref_vars` collects pre-init candidates.
Their var_scope entries are inserted at the current scope, then the branches are scanned.
Pre-init `Set(v, Null)` nodes are prepended before the scanned if in a `Value::Insert`.

##### Bug fix in `generate_set` for borrowed references (implemented in `src/state.rs`)

The original `generate_set` code for a borrowed Reference/Vector with `value == Null` was:

```rust
stack.add_op("OpCreateStack", self);
self.code_add(dep[0]);  // BUG: dep[0] is a variable NUMBER, not a stack offset
```

`OpCreateStack(pos)` at runtime computes `result.pos = stack_cur.pos + State::stack_pos - pos`.
So `pos` must be `(stack.position before the op) - dep[0].stack_pos`, not the variable number.
Using the variable number (e.g. 0 for `self`) instead of the correct offset produced a
self-referential or garbage DbRef.

Fixed in both the Reference/Enum-ref branch and the Vector branch:

```rust
stack.add_op("OpCreateStack", self);
let dep_pos = stack.function.stack(dep[0]);
let before_stack = stack.position - size_of::<DbRef>() as u16;
self.code_add(before_stack - dep_pos);
```

This creates a DbRef pointing into the dep variable's stack slot, which is a valid null-state
for the borrowed variable and is immediately overwritten by `OpPutRef` in the re-assignment.

#### Current status (as of 2026-03-11): owned references pass, borrowed references fail at runtime

**`validate_slots` no longer panics** — the slot conflict for `t_4Code_define` is resolved
for the cases tested.  The `enums::polymorph` regression test still passes.

However, **borrowed-reference pre-inits cause a runtime crash** in the
`long_lived_int_and_copy_record_followed_by_ref` test
(`database.rs:1462`: index out of bounds, store_nr=8 on a garbage DbRef).

The failing test pattern:

```loft
fn process(b: Bag) -> integer {
    result = 0;
    if b.items[0].val > 0 {
        result = b.items[0].val;
    } else {
        b.items += [b.extra];     // ← CopyRecord inside else-branch
        last = b.items[b.items.len() - 1];   // ← borrowed ref, first assigned here
        result = last.val;
    };
    result
}
```

`last` is `Type::Reference(Item, dep=[b])`.  The pre-init emits `Set(last, Null)` before the
`if`, which generates `OpCreateStack(before_stack - b.stack_pos)` to initialise `last`'s
pre-claimed slot with a DbRef pointing to `b`'s stack position.  The actual assignment
(`last = b.items[...]`) is supposed to overwrite this via `OpPutRef`.

**Root cause (still under investigation):** The garbage `store_nr=8` in the DbRef reaching
`set_int` (backtrace: `fill.rs::set_int → database.rs::store_mut`) indicates that either:

1. `OpPutRef` is computing the wrong `var_pos` and writing the real DbRef to the wrong slot,
   leaving the pre-init DbRef in `last`'s slot when `last.val` is read; OR
2. The `OpCreateStack`-produced pre-init DbRef itself is somehow corrupting adjacent memory
   and being read as the result of `last.val`.

The key difference from `t_4Code_define` (which no longer panics): in `t_4Code_define`,
`_elm_1` is a borrowed ref that is only read inside the false-branch AFTER its actual
assignment via `OpPutRef`.  In the `long_lived` test, `result = last.val` reads `last` and
then writes to `result` (a different integer variable) via `set_int`, which is where the bad
DbRef surfaces.

#### Next investigation step

Print or inspect the IR and bytecode for `fn process` in the `long_lived` test (by enabling
debug output in the test harness or adding a temporary `validate_slots` assert) to confirm:

- Where `last` is claimed (pre-claimed slot position).
- That `OpPutRef` computes `var_pos = stack.position_after_generate - last.stack_pos` correctly
  at the point of the re-assignment inside the else-branch.
- Whether `OpVarRef` for `last` (in `result = last.val`) correctly addresses the pre-claimed
  slot.

If the slot positions and var_pos values are correct, the problem must be in the runtime
behaviour of `OpCreateStack` producing a DbRef that somehow aliases or overwrites adjacent
data before `OpPutRef` fires.

#### Owned-reference case (WORKING)

The `t_4Code_define` slot conflict is resolved: `validate_slots` no longer panics.
`_elm_1` and `_elm_2` (both borrowed refs to `self`) get correct pre-inits.
All four `slot_assign` non-ignored tests pass.  `enums::polymorph` still passes.

The `wrap::last` and `wrap::dir` tests still fail, but now with "Different definition of
Point." — a **pre-existing correctness bug** in `lib/parser.loft`'s handling of type
references in function signatures.  This bug was always present but hidden behind the
`validate_slots` panic.  It is a separate issue from the slot-assignment fix.

#### Files changed so far

| File | Change |
|---|---|
| `src/scopes.rs` | Added `needs_pre_init` free function |
| `src/scopes.rs` | Added `find_first_ref_vars` method on `Scopes` |
| `src/scopes.rs` | Modified `scan` arm for `Value::If` to emit pre-inits |
| `src/state.rs` | Fixed `OpCreateStack` offset for borrowed Reference and Vector types |
| `src/variables.rs` | Added `first_def()` and `min_safe_claim_pos()` helpers (from Option B attempt, unused) |

#### Testing checklist

```
cargo test --test wrap -- last dir          # still FAILS (Different definition of Point — separate bug)
cargo test --test enums -- polymorph        # PASSES ✓
cargo test --test slot_assign               # 4 pass, 1 ignored (long_lived still fails at runtime)
cargo test --test slot_assign -- --include-ignored  # long_lived FAILS (store_nr=8 crash)
```

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
| Integration tests for slot conflicts | **Done** (`tests/slot_assign.rs`, 5 tests; 1 still ignored) |
| Bug 2 (`t_4Code_define`) root cause identified | **Done** |
| Option B guard (compile-time only) | **Attempted, reverted** — breaks bridging invariant |
| Option A sub-3 pre-init for owned refs | **Done** — `validate_slots` no longer panics for `t_4Code_define` |
| Option A sub-3 pre-init for borrowed refs | **Partially done** — runtime crash still occurs (store_nr=8) |
| `OpCreateStack` offset bug in `state.rs` | **Fixed** — now emits correct `before_stack - dep_pos` |
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
