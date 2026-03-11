# Stack Slot Assignment

## Unresolved Issues

### Issue 1 — Borrowed-reference pre-init causes runtime crash

**Test:** `slot_assign::long_lived_int_and_copy_record_followed_by_ref` (currently `#[ignore]`d)

**Symptom:** `database.rs:1462` — index out of bounds, `store_nr=8` on a garbage DbRef.
Backtrace: `fill::set_int → database::store_mut`.

**Pattern:**

```loft
fn process(b: Bag) -> integer {
    result = 0;
    if b.items[0].val > 0 {
        result = b.items[0].val;
    } else {
        b.items += [b.extra];
        last = b.items[b.items.len() - 1];   // ← borrowed ref, first assigned here
        result = last.val;
    };
    result
}
```

`last` is `Type::Reference(Item, dep=[b])`.  Proposal P1 pre-inits `last` before the `if`
with `OpCreateStack(before_stack - b.stack_pos)`, then the actual assignment inside the
else-branch should overwrite via `OpPutRef`.  The garbage `store_nr=8` reaching `set_int`
means either `OpPutRef` wrote to the wrong address (leaving the pre-init DbRef in `last`'s
slot) or the `OpCreateStack` DbRef corrupted adjacent memory.

**Next step:** Print or inspect the IR and bytecode for `fn process` to verify:
- `last`'s pre-claimed `stack_pos`
- `var_pos = stack.position_after_generate - last.stack_pos` at the `OpPutRef` site inside
  the else-branch
- Whether `OpVarRef` for `last` (in `result = last.val`) addresses the correct slot

**Solved by:** P1 (partially implemented; owned-ref case works, borrowed-ref case crashes)

---

### Issue 2 — "Different definition of Point." in wrap tests

**Tests:** `wrap::last`, `wrap::dir` — fail with "Different definition of Point."

**Symptom:** A struct named `Point` is registered twice with incompatible types when
`lib/parser.loft` processes function return type references after a struct definition.
This is a pre-existing correctness bug in the loft parser library, unrelated to slot
assignment.  It was previously hidden behind the `validate_slots` panic.

**Status:** Not yet analysed.  Needs its own investigation.

---

### Issue 3 — Full slot assignment pass not implemented

Steps 3 and 4 of Proposal P2 (the assignment pass and removal of `claim()`) have not been
implemented.  The current pre-init approach (P1) is a targeted fix; P2 is the correct
long-term architecture.

**Solved by:** P2

---

### Issue 4 — Dead Option-B helpers in `variables.rs`

`first_def(var_nr)` and `min_safe_claim_pos(check_seq)` were added to `Function` during
the failed Option B attempt.  They are unused now that Option B was reverted.

**Fix:** Delete them from `src/variables.rs`.

---

## Resolved Issues

### Bug 1 — `13-file.loft` overlapping allocations (fixed 2026-03-10)

Two variables (`b: Buffer` in scope 12, `f: File alias` in scope 5) were both allocated at
stack position 244.  `b` was still live when `f`'s `PutRef` overwrote slot 244, orphaning
`b`'s store.  The next `ConvRefFromNull` found `allocations[max].free == false` and panicked.

Fixed by ensuring distinct stack positions for variables with overlapping lifetimes.

---

### Bug 2 — `t_4Code_define` slot conflict for owned references (fixed by P1 for owned refs)

`_elm_1` (DbRef, 12 bytes) was claimed at slot 62 after a `CopyRecord` dropped
`stack.position` from 86 to 62, overlapping `res: integer` at [66, 70).  `validate_slots`
detected and panicked.

Fixed by P1: `scopes.rs` now emits `Set(_elm_1, Null)` before the `if`, pre-claiming the
slot at a safe position before the `CopyRecord` fires.  `validate_slots` no longer panics
for `t_4Code_define`.

---

### Bug 3 — `OpCreateStack` used variable number instead of stack offset (fixed 2026-03-11)

In `state.rs::generate_set`, the borrowed-Reference/Vector pre-init path emitted:

```rust
stack.add_op("OpCreateStack", self);
self.code_add(dep[0]);  // BUG: dep[0] is a variable index, not an offset
```

`OpCreateStack(pos)` at runtime computes `result.pos = stack_cur.pos + State::stack_pos - pos`,
so `pos` must be a relative offset, not a variable number.  Fixed to:

```rust
stack.add_op("OpCreateStack", self);
let dep_pos = stack.function.stack(dep[0]);
let before_stack = stack.position - size_of::<DbRef>() as u16;
self.code_add(before_stack - dep_pos);
```

Applied to both the Reference/Enum-ref branch and the Vector branch in `generate_set`.

---

## Proposals

### P1 — Scope-analysis pre-init (Option A sub-3)

**Solves:** Bug 2 (owned refs — done), Issue 1 (borrowed refs — incomplete)

**Status:** Partially implemented 2026-03-11.

#### Core idea

`scopes.rs` already pre-emits `Set(dep, Null)` for dependent-type variables before they are
used.  Extend this to Reference/Vector/Text variables first assigned inside an if/else
branch: emit `Set(v, Null)` *before* the `Value::If` node, while `stack.position` is still
safely above all live variable slots.

When codegen reaches `Set(v, actual_value)` inside the branch, `v` is already claimed
(`pos != u16::MAX`).  The assignment takes the re-assignment path in `generate_set`, which
calls `set_var`.  `set_var` generates the value at the current (valid) `stack.position` and
copies the result into `v`'s pre-claimed slot via `OpPutRef` — no bridging needed.

#### Why the pre-init is always at a safe stack position

The pre-init fires before the `Value::If` node is entered.  Any `CopyRecord` that would
lower `stack.position` into the danger zone lives *inside* the if/else branch — it has not
run yet.  Therefore `stack.position == State::stack_pos` and the pre-init's
`OpConvRefFromNull`/`OpText` writes at the correct address.

#### Implementation

**`needs_pre_init` predicate** (`src/scopes.rs`):

```rust
fn needs_pre_init(tp: &Type) -> bool {
    matches!(
        tp,
        Type::Text(_) | Type::Reference(_, _) | Type::Vector(_, _) | Type::Enum(_, true, _)
    )
}
```

All Reference, Vector, Enum-ref, and Text types are included regardless of `dep`, because
all occupy 12 bytes on the stack and can cause the same slot overlap.  A `deps_ready`
check in `find_first_ref_vars` gates borrowed variables on all deps already being in
`var_scope` (otherwise `OpCreateStack` would reference an uninitialised slot).

**`find_first_ref_vars` method** (`src/scopes.rs`, inside `impl Scopes`):

Recursively walks a `Value` subtree and collects variables that:
- appear as the target of `Value::Set(v, ...)`,
- are not yet in `var_scope`, and
- satisfy `needs_pre_init` and `deps_ready`.

Recurses into nested `If` and `Block` but NOT `Loop` (loop variables have per-iteration
scope management and must not be pre-inited at the enclosing scope).

**Modified `scan` arm for `Value::If`** (`src/scopes.rs`):

1. Call `find_first_ref_vars` on both branches to collect pre-init candidates.
2. Register each candidate in `var_scope` at the current scope.
3. Scan the if normally (branches see the candidates as already assigned).
4. Prepend `Set(v, Null/empty)` for each candidate before the scanned if in a
   `Value::Insert`.

**Pre-init `Set(v, Null)` codegen** (`src/state.rs::generate_set`):

- Owned Reference/Enum-ref → `OpConvRefFromNull` (pushes a null DbRef)
- Borrowed Reference/Enum-ref → `OpCreateStack(before_stack - dep_pos)`
- Owned Vector → `OpConvRefFromNull` + `OpDatabase` + `OpVarRef` + length init
- Borrowed Vector → `OpCreateStack(before_stack - dep_pos)`
- Text → `OpText` (pushes empty string)

#### Files changed

| File | Change |
|---|---|
| `src/scopes.rs` | `needs_pre_init` free function |
| `src/scopes.rs` | `find_first_ref_vars` method |
| `src/scopes.rs` | `scan` arm for `Value::If` emits pre-inits |
| `src/state.rs` | `OpCreateStack` offset fixed (Bug 3 above) |

#### Test results (2026-03-11)

```
cargo test --test enums -- polymorph                # PASSES ✓
cargo test --test slot_assign                       # 4 pass, 1 ignored
cargo test --test slot_assign -- --include-ignored  # long_lived FAILS (Issue 1)
cargo test --test wrap -- last dir                  # FAILS (Issue 2, separate bug)
```

---

### P2 — Full slot assignment pass (Option A)

**Solves:** Issue 3 (long-term correct architecture)

**Status:** Planned; Steps 1 and 2 done, Steps 3–5 not yet implemented.

#### Core idea

Compute all stack slot positions in a dedicated pass *before* code generation, using the
live intervals produced by `compute_intervals`.  Code generation in `state.rs` reads
pre-assigned `stack_pos` instead of calling `claim()`.

The bridging problem is avoided because slots are assigned globally — there is no longer a
moment where `stack.position` drops below a live variable's slot and then a new claim is
made at the wrong position.

#### Data flow

```
Parser (two passes)
  └─ variables.rs: add_variable(), copy_variable()   ← names, types, scopes (unchanged)
       scope_analysis (scopes.rs)                    ← scope IDs, OpFreeText/OpFreeRef
            compute_intervals (variables.rs)         ← first_def/last_use per variable [DONE]
                 assign_slots (variables.rs)         ← assign stack_pos from intervals [TODO]
                      [debug] validate_slots         ← assert no overlapping live slots [DONE]
                           byte_code (state.rs)      ← reads stack_pos, no claim() [TODO]
                                execute()
```

#### Step 1 — `compute_intervals` (DONE — `src/variables.rs`)

`compute_intervals(val, function, free_text_nr, free_ref_nr, seq)` walks the IR in
execution order, recording `first_def` and `last_use` on each `Variable`.  Called from
`scopes::check` after the scope pass.

**Key concepts:**

- `first_def`: sequence number of the `Value::Set(v, …)` that first defines `v`.
- `last_use`: sequence number of the last `Value::Var(v)` (or implicit `OpFreeText`/
  `OpFreeRef`) for `v`.
- Overlapping lifetimes: `u.first_def <= v.last_use && v.first_def <= u.last_use`.

#### Step 2 — `validate_slots` (DONE — `src/variables.rs`)

`validate_slots(function, data, def_nr)` (debug-only) checks every variable pair for
simultaneous live-interval overlap and slot overlap.  Logs a full diagnostic then panics.
Uses the extracted `find_conflict(vars)` helper for testability.

Unit tests in `src/variables.rs` cover non-overlapping intervals, non-overlapping slots,
integer inside wider DbRef slot, and edge cases.

Integration tests in `tests/slot_assign.rs` (5 tests; 1 still ignored — Issue 1).

#### Step 3 — `assign_slots` (TODO — `src/variables.rs`)

Add `assign_slots(vars: &mut [Variable], arguments_size: u16)`.

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

Skip variables with `argument == true` — they already have `stack_pos` set by argument
layout.  `OpFreeText`/`OpFreeRef` insertion by `scopes.rs` must happen before this pass
runs, so those implicit last-uses are visible to liveness.

#### Step 4 — Remove `claim()` from `state.rs` (TODO)

After the assignment pass, `generate_set` reads the pre-assigned `stack_pos` instead of
calling `claim`.  `stack.position` still needs advancing so subsequent offset computations
are correct:

```rust
// In generate_set, when pos != u16::MAX:
stack.position = stack.position.max(pos + var_size);
```

#### Step 5 — Remove `copy_variable` (deferred)

After Steps 3–4 are complete and `validate_slots` is green, `copy_variable` can be
removed.  Variables re-used across sibling scopes will simply have non-overlapping
intervals and can share or not share slots based on the interval check.

#### Invariants to preserve

- Arguments occupy positions 0 … arguments_size−1; skip them in `assign_slots`.
- `OpFreeText`/`OpFreeRef` insertion happens before `assign_slots` runs.
- The runtime stack pointer (`State::stack_pos`) is unchanged by the pass; it still
  advances and retreats during block execution via `OpFreeStack`.
- After `assign_slots`, `state.rs::generate_set` must advance `stack.position` past each
  variable's slot so subsequent ops compute correct relative offsets.

---

### P3 — Live-interval guard in `generate_set` (ABANDONED)

**Status:** Attempted 2026-03-11, reverted.  Do not implement.

The idea was to advance `stack.position` past all live variables before calling `claim()`:

```rust
let my_first_def = stack.function.first_def(v);
if my_first_def != u32::MAX {
    let min_pos = stack.function.min_safe_claim_pos(my_first_def);
    if stack.position < min_pos {
        stack.position = min_pos;
    }
}
```

#### Why it fails: the bridging invariant

`stack.position` is the source of truth for every `VarRef` offset emitted during code
generation.  `OpVarRef(var_pos)` at runtime computes `address = State::stack_pos − var_pos`,
where `var_pos = stack.position − var.stack_pos` at compile time.  This is correct only
when `stack.position == State::stack_pos` at every code point.

When the guard fires (advancing compile-time `stack.position` from 62 to 70 without emitting
bytecode), the runtime `State::stack_pos` stays at 62.  Every subsequent `VarRef` in the
value expression uses an offset computed from 70 but the runtime reads from
`62 − offset` — 8 bytes too low.  For `self` at argument position 0 this underflows to a
garbage address.

Observed failures: both `wrap::last` and `wrap::dir` crashed with SIGSEGV.  The guard
changed the failure mode from "compile-time panic" to "runtime memory corruption".

#### Conclusion

There is no way to advance `stack.position` at compile time without simultaneously emitting
bytecode that advances the runtime stack pointer.  P1 and P2 are the only correct fixes.

---

## Current status (2026-03-11)

| Step | Status |
|---|---|
| `compute_intervals` | **Done** |
| `validate_slots` + `find_conflict` | **Done** (debug-only) |
| Unit tests for `find_conflict` | **Done** (`src/variables.rs`) |
| Integration tests (`tests/slot_assign.rs`) | **Done** (5 tests; 1 ignored) |
| P1: pre-init for owned refs | **Done** — `validate_slots` no longer panics for `t_4Code_define` |
| P1: pre-init for borrowed refs | **Partial** — runtime crash (Issue 1) |
| Bug 3: `OpCreateStack` offset | **Fixed** |
| P2: `assign_slots` | **TODO** |
| P2: remove `claim()` | **TODO** |
| P2: remove `copy_variable` | **Deferred** |
| Issue 2: "Different definition of Point." | **Open** (separate bug) |
| Issue 4: dead Option-B helpers | **Open** (delete from `variables.rs`) |
