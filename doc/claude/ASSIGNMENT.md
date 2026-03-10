# Stack Slot Assignment — Separation Plan

## Problem

Stack slot positions are currently assigned **during code generation** in `state.rs`, driven
by the order in which `Value::Set` nodes are encountered while walking the IR.  The allocator
is a simple bump-pointer: each variable gets the next free byte offset and keeps it for the
lifetime of the whole function.

Scope analysis (`scopes.rs`) uses *structural nesting* as a proxy for variable lifetime.
When a variable is re-assigned in a sibling or child scope, `copy_variable` creates a fresh
`Variable` entry so the two lifetimes get distinct positions.  But `copy_variable` is an
ad-hoc patch and does not cover every case.

**Confirmed bug (2026-03-10):** In `tests/suite/13-file.loft`, two variables inside the
buf-read block — `b` (Buffer, scope 12) and the local copy of `f` (File alias for
`__ref_8`, scope 5) — are both allocated at stack position 244.  `b` is still live when
`f`'s `PutRef` overwrites slot 244, orphaning `b`'s store.  On the next
`ConvRefFromNull`, the LIFO store allocator finds `allocations[max].free == false` and
panics with "Allocating a used store".

The root cause is that the position-assignment logic never verifies that two live
variables do not share a slot.

---

## Goal

Separate stack slot assignment into its own post-parse phase so that:

1. The parser and scope analysis work with **symbolic variable IDs only** (no concrete
   byte offsets).
2. A dedicated **assignment pass** computes live intervals and assigns byte offsets, with
   a provable guarantee that no two overlapping live intervals share a slot.
3. A lightweight **validation pass** (debug-only) verifies the guarantee before execution.

---

## Current data flow

```
Parser (two passes)
  └─ variables.rs: add_variable(), copy_variable()   ← names, types, scopes
       scope_analysis (scopes.rs)                    ← scope IDs, OpFreeText/OpFreeRef
            byte_code (state.rs)                     ← claim() assigns stack_pos HERE
                 execute()
```

`claim()` in `variables.rs` is called from `state.rs` at the moment a `Value::Set` node
is first processed.  It assigns `stack_pos` and advances the position counter.  There is
no look-ahead and no liveness information.

---

## Proposed data flow

```
Parser (two passes)
  └─ variables.rs: add_variable(), copy_variable()   ← names, types, scopes (unchanged)
       scope_analysis (scopes.rs)                    ← scope IDs, OpFreeText/OpFreeRef (unchanged)
            liveness pass (NEW — variables.rs)       ← compute live intervals per variable
                 assignment pass (NEW — variables.rs)← assign stack_pos from intervals
                      [debug] validation pass (NEW)  ← assert no overlapping live slots
                           byte_code (state.rs)      ← reads stack_pos, does NOT call claim()
                                execute()
```

---

## Key concepts

### Live interval

For variable `v`, the **live interval** is `[first_def, last_use]` measured in
**instruction sequence numbers** — a monotonically increasing counter assigned to each
`Value` node in the flattened operator list of the function.

- `first_def`: the sequence number of the `Value::Set(v, …)` node that first defines `v`.
- `last_use`:  the sequence number of the last `Value::Var(v)` (or implicit use in
  `OpFreeText`/`OpFreeRef`) for `v`.

Two variables `u` and `v` have **overlapping lifetimes** iff their intervals intersect:
`u.first_def <= v.last_use && v.first_def <= u.last_use`.

### Slot compatibility

Two variables may share a slot if and only if:
1. Their live intervals do not overlap, AND
2. They have the same byte size (computed by `size(tp, Context::Variable)`), AND
3. Neither has type `Text` or a ref type (owning allocations that need cleanup via
   `OpFreeText`/`OpFreeRef`) — OR both do and the cleanup is correctly ordered.

For safety, the initial implementation should **never reuse slots for ref-typed or
text-typed variables**.  Primitive slots (integers, booleans, singles, floats) may be
reused freely.  This keeps the fix targeted at the class of bugs that break the LIFO
store allocator.

---

## Implementation steps

### Step 1 — Sequence-number annotation

**File:** `src/variables.rs`

Add a method `number_instructions(code: &[Value]) -> Vec<(u16, u16)>` that walks the
operator list of a function in execution order (same traversal order used by
`scopes::scan`) and assigns a monotonically increasing sequence number to each
`Value::Set` and `Value::Var` node.  Return a mapping `(var_nr, seq_nr)` for each
occurrence.

The traversal must follow actual control flow — in particular:
- Both branches of `Value::If` are traversed (a variable used in either branch is live
  from its definition until the *later* of the two branches' last uses).
- Loop bodies are traversed once (conservative: treat loop as a single pass for liveness;
  variables live at loop entry are kept live until loop exit).
- `Value::Block` is traversed in order.
- `Value::Insert` expansions are included.

### Step 2 — Live interval computation

**File:** `src/variables.rs`

Add `compute_intervals(code: &[Value], vars: &mut [Variable])`.

For each variable `v`:
- `v.first_def` = min sequence number of all `Value::Set(v, …)` nodes.
- `v.last_use`  = max sequence number of all `Value::Var(v)` nodes, plus any implicit
  uses introduced by `OpFreeText(v)` / `OpFreeRef(v)` inserted by scope analysis.

Store `first_def` and `last_use` as new fields on `Variable` (both `u32`, initialized to
`u32::MAX` / `0` respectively).  Arguments get `first_def = 0`.

### Step 3 — Assignment pass

**File:** `src/variables.rs`

Add `assign_slots(vars: &mut [Variable], arguments_size: u16)`.

Algorithm (linear scan, simplified):

```
active: list of (interval_end, slot_start, slot_size)   // slots currently in use
free_primitive_slots: list of (slot_start, slot_size)   // released primitive slots

sort vars by first_def ascending

position = arguments_size   // next free byte after arguments

for each variable v in sorted order:
    // expire slots whose interval ended before v.first_def
    for each slot in active where interval_end < v.first_def:
        if slot is primitive type:
            free_primitive_slots.push(slot)
        active.remove(slot)

    if v is primitive type:
        // try to reuse a released slot of matching size
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

    active.push((v.last_use, v.stack_pos, size(v.type_def, Context::Variable)))
```

This pass replaces all calls to `claim()` inside `state.rs`.  After this pass,
every variable has a valid `stack_pos`; `byte_code()` reads the value directly
instead of calling `claim()`.

### Step 4 — Remove `claim()` from `state.rs`

**File:** `src/state.rs`

In `byte_code()`, remove the `claim()` call inside the `Value::Set` branch.  Instead,
read `variables.stack(var_nr)` which now always returns the pre-assigned position.

The `stack_pos` field of `State` (the runtime stack pointer) still advances during
execution; the assignment pass only sets the compile-time `Variable::stack_pos`, which
is a fixed offset into the frame, not the runtime pointer.

Keep the existing `set_stack()` path that records positions back into definitions for
debugging — it now becomes a no-op (positions are already set).

### Step 5 — Validation pass (debug-only)

**File:** `src/variables.rs` or `src/scopes.rs`

Add `validate_slots(vars: &[Variable])` gated on `#[cfg(debug_assertions)]`.

```
for each pair (u, v) with u.stack_pos != u16::MAX && v.stack_pos != u16::MAX:
    u_end = u.stack_pos + size(u.type_def, Context::Variable)
    v_end = v.stack_pos + size(v.type_def, Context::Variable)
    slots_overlap = u.stack_pos < v_end && v.stack_pos < u_end
    intervals_overlap = u.first_def <= v.last_use && v.first_def <= u.last_use
    assert!(!(slots_overlap && intervals_overlap),
        "Variables {} and {} share slot [{}, {}) while both live [{}, {}] / [{}, {}]",
        u.name, v.name, ...)
```

Call this from `scopes::check()` after `assign_slots()` completes.  Any future
regression will be caught immediately with a clear diagnostic instead of a runtime
panic deep in the LIFO allocator.

### Step 6 — Remove `copy_variable` (deferred)

`copy_variable` was introduced specifically to work around lifetime-overlap bugs in the
old greedy allocator.  Once the interval-based assignment pass is in place,
`copy_variable` becomes redundant: two uses of the same logical name in different scopes
will simply have non-overlapping intervals and can safely share a slot (or not, depending
on the interval check).

Removing `copy_variable` should be done **after** the validation pass is green across all
suite tests, so that any residual issues surface as validation failures rather than
runtime panics.

---

## Files affected

| File | Change |
|------|--------|
| `src/variables.rs` | Add `first_def`, `last_use` fields to `Variable`; add `number_instructions()`, `compute_intervals()`, `assign_slots()`, `validate_slots()` |
| `src/scopes.rs` | Call `assign_slots()` + `validate_slots()` at end of `check()` |
| `src/state.rs` | Remove `claim()` call from `byte_code()`; read pre-assigned `stack_pos` |
| `src/data.rs` | No changes expected |
| `src/parser.rs` | No changes expected |

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
  that is freed before the borrow ends.  For the initial implementation, simply treating
  all ref/text types as non-reusable is sufficient.
- The **runtime stack pointer** (`State::stack_pos`) is unaffected; it still advances
  and retreats during `Value::Block` execution via `OpFreeStack`.  The compile-time
  `Variable::stack_pos` only determines where within the frame a variable lives.

---

## Immediate fix (before full plan is implemented)

To unblock the failing `13-file.loft` test now, the simplest correct fix is:

In `scopes.rs` `copy_variable` detection, add an additional trigger: when a
variable is assigned inside a block where **another live variable already occupies the
same would-be slot**, force a copy.  This is a targeted patch, not a structural fix,
and should be removed once Step 3 above is complete.

Alternatively, restructure `13-file.loft` so that `b` and `f` are in different blocks
(the buf read block already uses `b` and `f` sequentially enough that splitting them
into two blocks would avoid the conflict).
