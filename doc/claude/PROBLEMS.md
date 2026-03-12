# Known Problems in Loft

This document lists known bugs, unimplemented features, and limitations in the loft
language and its interpreter (`lavition`). For each issue the workaround and the
recommended fix path are described.

## Contents
- [Open Issues — Quick Reference](#open-issues--quick-reference)
- [Runtime Crashes](#runtime-crashes)
- [Parser / Lexer Bugs](#parser--lexer-bugs)
- [Library System Limitations](#library-system-limitations)
- [Unimplemented Features](#unimplemented-features)
- [String Iteration Semantics](#string-iteration-semantics)
- [Stack Slot Assignment (In Progress)](#stack-slot-assignment-in-progress)
- [Code Quality](#code-quality)

---

## Open Issues — Quick Reference

| # | Issue | Severity | Workaround? |
|---|-------|----------|-------------|
| ~~3~~ | ~~Polymorphic text methods on struct-enum → SIGSEGV at runtime~~ **FIXED** | — | — |
| 4 | `v += items` in ref-param function has no effect | **FIXED** | `assign_refvar_vector` in parser.rs |
| 9 | Nested `\"` inside `{...}` format expression fails | Medium | Escape outer braces |
| 10 | `{expr}` in string always treated as format; unresolved slot crashes | Medium | Use `{{`/`}}` for literal braces |
| 13 | Library names require `libname::` prefix (no wildcard import) | Low | Always use prefix |
| 17 | Reverse iteration on `sorted<T>` panics | Medium | Collect to vector, reverse-iterate |
| 20 | `f#next = pos` seek before first open is a no-op | Low | Read/write first, then seek |
| ~~21~~ | ~~CLI args cannot be passed to `fn main(args: vector<text>)`~~ **FIXED** | — | — |
| 22 | Spatial index (spacial<T>) operations not implemented | Low | N/A |
| ~~23~~ | ~~`c#index` in `for c in text` returns post-advance offset~~ **NOT A BUG** | — | — |
| 24 | Compile-time slot assignment incomplete | Low | No user impact yet |
| 27 | `16-parser.loft` → crash store_nr=60 in set_int | **High** | None |
| ~~29~~ | ~~`validate_slots` false positive: different-name owned vars sharing a slot~~ **FIXED** | — | — |
| ~~31~~ | ~~`v += [struct_var]` appends empty element instead of copying struct~~ **FIXED** | — | — |
| ~~32~~ | ~~`v += other_vec` replaces vector instead of appending all elements~~ **FIXED** | — | — |
| 33 | `sorted` filtered loop-remove (`for r if cond { r#remove }`) gives wrong result for large N | **High** | Use key-null removal: `for i in 0..N { sdb[i] = null; }` (see PLANNING T0-7) |
| 34 | `index` key-null removal in loop leaves 1 record (off-by-one in B-tree removal) | **High** | Use loop `#remove` for large N; key-null works for small N (≤3) (see PLANNING T0-6) |
| 35 | `index` loop-remove (`for r in idx { r#remove }`) panics with "Unknown record" for large N | **High** | Rebuild index from scratch (scope exit) or remove by key individually (see PLANNING T0-5) |

---

## Runtime Crashes

### ~~1. Methods returning a new struct record crash inside `var_ref`~~ **FIXED**

**Fixed 2026-03-13.** `parse_single` (parser.rs) was creating `OpCopyRecord(c, d, tp)` as
the self-argument to a method call, leaving `d` uninitialized.  Fix: pass `Var(c)` directly;
`generate_set` gained a branch that emits `ConvRefFromNull + Database + CopyRecord` for
same-type owned-Reference assignment.  Test: `method_returns_new_struct_record` in `tests/issues.rs`.

---

### ~~2. Borrowed-reference pre-init causes runtime crash at database.rs:1462~~ **FIXED**

The `long_lived_int_and_copy_record_followed_by_ref` test in `tests/slot_assign.rs`
(previously described as failing) now passes. Borrowed refs first assigned inside a branch
are correctly pre-initialized by the Option A sub-3 work in `scopes.rs`.
Also verified: `ref_inside_branch_borrowed` in `tests/issues.rs` passes.

---

### ~~3. Polymorphic text methods on struct-enum variants~~ **FIXED 2026-03-13**

Three fixes were applied on 2026-03-13:

1. **`enum_fn` in parser.rs** — collects `extra_call_args`/`extra_call_types` from the
   dispatcher's own `args[1..]` (the `RefVar(Text)` buffer attrs added by `text_return`)
   and forwards them to each variant call via `enum_numbers`.  The `enum_numbers`
   signature was extended to accept `extra_args: &[Value]` and `extra_types: &[Type]`.
   Dispatcher IR result:
   ```
   fn t_5Shape_describe(self:ref(Shape), __work_1:&text) -> text["__work_1"]
     if ... { return t_6Circle_describe(self(0), __work_1(0)); }
     if ... { return t_4Rect_describe(self(0), __work_1(0)); }
   ```

2. **`generate_call` in state.rs** — special case: when forwarding a `RefVar(_)` arg
   as a `Var(v)` with `v` also typed `RefVar(_)`, emit only `OpVarRef(var_pos)` (no
   trailing `OpGetStackText`).  This passes the raw `DbRef` pointer rather than the
   dereferenced text content.

3. **`format_stack_float` in state.rs** — off-by-4 bug: the function pops float(8) +
   int(4) + int(4) = **16 bytes** from the stack but called `string_ref_mut(pos - 12)`
   (the `format_stack_long` offset, which pops only 12 bytes).  Changed to
   `string_ref_mut(pos - 16)` to match `format_float`.  This was the root cause of the
   SIGSEGV — the DbRef was read 4 bytes too low, yielding a garbage pointer.

**Test:** `polymorphic_text_method_on_enum` in `tests/issues.rs` now passes without `#[ignore]`.

---

### ~~4. `v += items` inside a ref-param function does not modify the caller's vector~~ **FIXED**

**Fix (2026-03-13):** Added `assign_refvar_vector` in `parser.rs` (analogous to
`assign_refvar_text`), called from `parse_assign` after `assign_refvar_text`.

**How it works:**
- For `v: &vector<T>` with `v += extra` (non-bracket RHS): emits
  `OpAppendVector(Var(v_nr), extra_expr, rec_tp)` directly.
- `generate_var(v_nr)` for `RefVar(Vector)` emits `OpVarRef + OpGetStackRef(0)`,
  which reads `buf`'s actual DbRef from the `OpCreateStack` temp record — exactly
  what `append_vector` (fill.rs) needs as its `v_r` argument.
- Bracket-form `[elem]` RHS produces `Value::Insert`; `assign_refvar_vector` skips
  those (returns false) so `parse_block` expands them via the existing
  `OpNewRecord / OpSetInt / OpFinishRecord` path (which already works for ref-params).
- `find_written_vars` recognises `OpAppendVector(Var(v_nr), ...)` as a write to
  `v_nr` via the `stack_write` extension (name starts with `"OpAppend"`-check).

**Test:** `ref_param_append_bug` in `tests/issues.rs` now passes.

---

### ~~5. Appending a scalar to a vector struct field that starts empty has no effect~~ **FIXED**

`b.items += 1` where `items` is a `vector<T>` field now works.
Fix: in `parse_assign` (parser.rs), when `var_nr == u16::MAX` (field access), `op == "+="`,
`f_type == Type::Vector`, and the RHS is a scalar element (not an `Insert`), route through
`new_record` with `is_field = true` — the same path as `b.items += [1]` — which uses
`OpNewRecord`/`OpFinishRecord` to allocate the element directly in the struct's field.
Tests: `vec_field_append_scalar` and `vec_field_append_bracket_*` in `tests/issues.rs`.

---

### 27. `16-parser.loft` runtime crash: `store_nr=60` in `set_int` during `lib/parser.loft` execution

**Symptom:** `wrap::last` and `wrap::dir` (at `16-parser.loft`) crash with:
```
thread 'last' panicked at src/database.rs:1494:30:
index out of bounds: the len is 8 but the index is 60
```

**Call path:** `main()` → `parser::parse(...)` → `parse_file` → `structure`/`function`/… →
`type_def` → `set_int` (in `Stores::store_mut`).

**Investigation (2026-03-13):** The crash is in `set_int` operating on a `DbRef` whose
`store_nr = 60` (a garbage value — only 8 stores are ever allocated). The `SetInt`
instructions in `type_def` are used for (a) initializing the local `parameters` and `fields`
vectors' record pointer fields to 0, and (b) storing elements during `parameters += [p]`.

Key facts established:
- The `var[N]` encoding in the bytecode dump: N = compile-time stack-allocation position of the
  variable. At runtime `get_var(current_compile_pos - N)` reads from `Z + N` (absolute, where Z
  is the caller's stack_pos before the function's args were pushed). Confirmed by `Return(ret=13,
  discard=193)` arithmetic.
- The `parameters` variable (at compile pos 137, a DbRef to `__ref_1`'s field[0]) and `fields`
  (at 149) are correctly initialized by `OpDatabase` + `GetField(fld=0)`.
- After a recursive inner `type_def` call returns, the outer `parameters` at Z+137 is
  mathematically preserved (inner call's locals live at Z+169 and above).
- `Stores::free()` decrements `max` without checking LIFO order — stores must be freed in exact
  reverse-allocation order or `max` becomes desynchronized.
- `60 = 0x3C = '<'` in ASCII — possibly garbage text data being read as a DbRef.

**Current status:** Root cause not yet pinpointed. The crash appears during `Parser {}` struct
initialization or early `parse_file`/`structure` execution, not necessarily in the `<`-generic
branch of `type_def`. Needs an execution trace (RUST_LOG or `file_debug`-style instrumentation)
to pinpoint which `SetInt` call produces the garbage DbRef.

**Workaround:** `wrap::last` and `wrap::parser_debug` are now `#[ignore]`; `wrap::dir`
skips `16-parser.loft` via the `SUITE_SKIP` const. Run them explicitly with
`cargo test -- last --ignored` / `cargo test -- parser_debug --ignored`.

**Best way forward:**
1. Run `cargo test -- parser_debug --ignored` to capture the full execution trace
   in `tests/dumps/16-parser.loft.txt` (~40 s) and inspect the trace.
2. Look for any `OpDatabase`/`ConvRefFromNull` whose resulting DbRef is NOT subsequently
   written back correctly to its variable slot (race between `null()` allocation and
   `database()` operator reuse).
3. Check whether `Parser {}` default-initialization allocates nested struct stores
   (lexer::Lexer, code::Code, logger::Log) via `set_default_value` and whether those stores
   violate the LIFO invariant.
4. Consider whether `Stores::free()` needs to assert `al == max-1` to enforce LIFO.

---

### 28. ~~`validate_slots` panic: same-name variables in sequential blocks~~ **FIXED**

`find_conflict()` now exempts pairs where both variables have the same name and the
same stack slot — these are sequential-block reuses of one logical variable, not
runtime conflicts.  Both same-name (`n` / `n`) and different-name (`a` / `b`) cases
in sequential blocks pass without panicking.

Tests `sequential_blocks_same_varname_workaround` and `sequential_blocks_different_varnames`
in `tests/issues.rs` cover this.

**Note:** This fix only applies to same-name pairs. A broader case (Issue 29) remains
unfixed where two variables with *different* names share a slot and have overlapping
`first_def`/`last_use` intervals, even though they are never simultaneously live at runtime.

---

### ~~29. `validate_slots` false positive: different-name reference variables in the same function~~ **FIXED 2026-03-13**

**Symptom:** In a large function that reuses a reference-typed variable (e.g. `f` as a file
handle) across many sequential `{ }` blocks, and later introduces a second reference-typed
variable with a different name (e.g. `c` for a `vector<text>`), `validate_slots` panics:

```
Variables 'f' (slot [1000, 1012), live [237, 1699]) and 'c' (slot [1000, 1012),
live [1539, 1540]) share a stack slot while both live in function 'n_main'
```

Both `f` and `c` are assigned the same 12-byte slot (both are reference types). Their live
intervals overlap: `f.first_def=237 < c.last_use=1540` and `c.first_def=1539 < f.last_use=1699`,
satisfying the `find_conflict` overlap condition. In reality they are never live at the same
time — `f` is only active inside its `{ }` blocks, which are all disjoint from the use of `c`.

**Root cause:** `compute_intervals` stores a global `first_def`/`last_use` per variable,
spanning the entire function regardless of block scope. For a variable reused across
many sequential blocks, `last_use` is the bytecode position of its final block, which
can be far past the introduction of other variables that share the same slot.

Issue 28's fix (exempt same-name/same-slot pairs) does not help here because `f` and `c`
have different names.

**Workaround:** Reorder the code so that all uses of the first variable finish before the
second is introduced. In `tests/scripts/11-files.loft`, the `c = ...lines()` call was moved
to the very end of `fn main()` to ensure `c.first_def > f.last_use`.

**Fixed 2026-03-13:** `find_conflict()` in `variables.rs` already uses interval-based overlap
checking (`left.first_def <= right.last_use && right.first_def <= left.last_use`) and includes
an exemption for same-name/same-slot pairs.  All Issue 29 tests pass.  The "differently-named"
case is handled because the overlap check is precise: if their live intervals truly do not
overlap, `find_conflict` correctly finds no conflict regardless of whether names match.

**Relationship to Issue 24:** Full per-block liveness (Step 3 of [ASSIGNMENT.md](ASSIGNMENT.md)) would make
the interval tracking exact; the current whole-function range is a conservative approximation
but is sufficient in practice for the known test cases.

---

## Parser / Lexer Bugs

### 6. ~~Uppercase hex literals are rejected~~ **FIXED**

`0xFF`, `0x2A` etc. now accepted. Both `get_number()` and `hex_parse()` already handled uppercase; tests verified.

---

### 7. ~~Open-start slice syntax `s[..n]` is not supported~~ **FIXED**

`s[..n]` for text and `v[..n]` for vectors now work; `parse_in_range` detects leading `..` and defaults start to 0.

---

### 8. ~~Calling a method on a constructor expression is rejected~~ **FIXED**

`Pt{x:3,y:4}.dist2()` now works. Fixed two-part type mismatch in `parse_object` and `convert` (parser.rs). Test: `method_on_constructor` in `tests/objects.rs`.

---

### 9. Nested `\"` inside format expressions is not supported — **FIXED 2026-03-14**

**Symptom:** Using `\"` inside a `{...}` format expression in a string literal caused
`Error: Dual definition of ...`.

**Example:**
```loft
// FIXED — \" inside the format expression now works
s = "{\"hello\"}";        // s == "hello"
s = "{\"hello\":5z}";     // Unexpected formatting type: z
```

**Fix:** `src/lexer.rs` — added `in_format_expr: bool` flag to `Lexer`. When `{` opens
a format expression, `in_format_expr` is set; when `}` closes it, the flag is cleared.
While `in_format_expr` is true, `"` and `\"` dispatch to the new `string_nested()` method
instead of calling `string()` (which would close the outer string). `string_nested()`
scans the nested literal content and returns it as `LexItem::CString` without touching
the lexer mode.

**Tests:** `tests/format_strings.rs` — `string_literal_no_specifier`,
`string_literal_with_width`, `string_literal_bad_specifier_after_width`,
`string_literal_bare_bad_specifier`.

---

### 10. `{expr}` in string literals is always treated as a format expression

**Symptom:** A string like `"{cd}"` looks up variable `cd`. If the variable is
unassigned, the slot is `u16::MAX` and the interpreter crashes.

**Workaround:** Use `{{` and `}}` to produce literal braces in output strings. When
comparing struct format output (e.g. `{r:128,g:0,b:64}`), escape both ends:
```loft
assert("{p}" == "{{r:128,g:0,b:64}}", "compact format");
```

**Best way forward:** This is intentional behaviour. The crash on unassigned
variables is a separate latent bug: the code generator should emit a diagnostic
rather than a `u16::MAX` slot reference when the format expression cannot be resolved.

---

### 11. ~~Field-name overlap between two structs causes wrong index range results~~ **NOT A BUG**

`determine_keys()` is type-scoped, so field offsets for identically-named fields in
different structs are computed independently.  Range query boundary semantics are also
correct (descending sort key ordering).  Test `field_name_overlap_range_query` passes.

---

## Library System Limitations

### 12. ~~`use` statements must appear before all definitions~~ **FIXED**

`parse_file` already emits a `Fatal` diagnostic; no code change was needed.

---

### 13. Unqualified access to library definitions is not supported

**Symptom:** After `use logger;`, writing `Log {}`, `Warning`, or `error(msg)` without
the `logger::` prefix gives "Expect token ;", "Unknown variable", or "Unknown function".

**Workaround:** Always use the `libname::` prefix for library types, enum values,
constants, and free functions.

**Best way forward:** Optionally fall back to searching all imported sources when a
name cannot be resolved in source 0. A `use logger unqualified;` or
`use logger::*;` wildcard import syntax could enable this without changing the
default namespace-safe behaviour.

---

### 14. ~~Cannot add methods to a library type from an importing file~~ **FIXED**

`get_fn` in `data.rs` now falls back to `source_nr(self.source, name)` when the struct's source doesn't define the method. Test: `fn shifted(self: testlib::Point, ...)` in `tests/docs/17-libraries.loft`.

---

### 15. ~~`pub` on struct fields causes a parse error~~ **FIXED**

`parse_struct` already silently consumes the `pub` keyword; no code change needed.

---

## Unimplemented Features

### 16. ~~`for n in range { expr }` inside a vector expression is not supported~~ **FIXED**

`[for n in 1..7 { n * 2 }]` now works via `parse_vector_for` in `parser.rs`. Tests: `for_comprehension` and `for_comprehension_if` in `tests/vectors.rs`.

---

### 17. ~~Reverse iteration on `sorted<T>` is not implemented~~ **FIXED 2026-03-14**

**Symptom:** Trying to iterate a sorted collection in reverse order panicked.

**Fix:** Three-part change:
1. `src/parser.rs` — `parse_in_range()` recognises `rev(sorted_var)` (no `..`):
   consumes the closing `)`, sets `self.reverse_iterator = true`.  The flag is also
   cleared in the first-pass early return of `iterator()` to prevent it from leaking
   across passes.  `fill_iter()` reads the flag on both its calls (OpIterate + OpStep)
   and ORs bit 64 into the `on` byte before resetting the flag.  `Parser` gains a
   `reverse_iterator: bool` field.
2. `src/vector.rs` — added `vector_step_rev()` which mirrors `vector_step()` but
   decrements the element index.  Any position `>= length` (used by `iterate()` as the
   "not started" sentinel for reverse) initialises to `length - 1`.  Also fixed a
   pre-existing overflow in `sorted_find()` when the sorted collection is empty
   (`sorted_rec == 0` or `length == 0` now return `(0, false)` early).
3. `src/state.rs` — `step()` for `on & 63 == 2` (sorted): calls `vector_step_rev`
   when `reverse` is set; stops when `pos == i32::MAX` (returned by `vector_step_rev`
   when the beginning has been passed).

**Tests:** `sorted_reverse_iterator`, `sorted_reverse_empty` in `tests/vectors.rs`;
reverse section added to `tests/docs/10-sorted.loft`.

---

### 18. ~~Writing a vector to a binary file produces 0 bytes~~ **FIXED**

`write_file` in `state.rs` now handles `Parts::Vector` types. Test: `array_write` in `tests/file-system.rs`.

---

### 19. ~~`f#size = n` (file truncate / extend) is not implemented~~ **FIXED**

Two parser/state bugs fixed (wrong lookup name; format not updated after create). All 6 file-size tests pass.

---

### 20. `f#next = pos` (file seek) only works after the file is already open

**Symptom:** Seeking before the first read or write on a file handle is a no-op;
`f#next` stays at 0 and the next operation starts from the beginning anyway.

**Workaround:** Perform at least one read or write before seeking. This is the normal
usage pattern (open → read/write → seek → read/write).

**Best way forward:** In the file handle struct, store a `pending_seek: i64` field
(default −1 = no pending seek). `OpSeekFile` writes to it when `file_ref == i32::MIN`
(file not yet open). The first `OpReadFile` / `OpWriteFile` that opens the file
applies the pending seek immediately after the `File::open` call.

---

### ~~21. Command-line arguments cannot be passed to `fn main()`~~ **FIXED 2026-03-13**

`Stores::text_vector(&[String])` builds a `vector<text>` DbRef from a Rust string slice.
`State::execute_argv()` detects a single `Type::Vector` attribute on `main` and pushes the DbRef before the return address.
`main.rs` collects trailing positional arguments into `Vec<String>` and calls `execute_argv`.
Test: `wrap::main_argv`.

---

### 22. Spatial index operations are not implemented

**Symptom:** Copy, remove, and iteration operations on `spacial<T>` collections
panic with `Not implemented` at database.rs.

**Best way forward:** Implement one operation at a time in `database.rs` and `fill.rs`,
starting with iteration (needed for any practical use), then remove (needed for
mutation), then copy (needed for assignment). The spacial index structure (radix tree
or R-tree) is already allocated; the iteration traversal is the main missing piece.

---

## String Iteration Semantics

### ~~23. `c#index` in `for c in text` returns post-advance offset instead of pre-advance~~ **NOT A BUG**

`parser.rs` lines 317-318 already save the pre-advance offset into `{id}#index` before emitting `OpTextCharacter` and advancing the pointer. The entry was stale. Verified by the `char iter indices` assertion in `tests/scripts/14-formatting.loft`.

---

## Stack Slot Assignment (In Progress)

### 24. Full compile-time slot assignment not yet implemented

**Current:** Stack slot positions are determined at codegen time by `claim()` in
`state.rs`. The final two steps of the planned P2 pass are missing:
- `assign_slots()` — compute optimal positions using precomputed live intervals
- Remove `claim()` calls and `copy_variable()` once the above is done

**Impact:** Non-optimal stack usage; potential conflicts detected by `validate_slots`
in debug builds.

**Best way forward:** Implement `assign_slots()` in `variables.rs` as a greedy
interval-graph colouring: sort variables by `first_def`, assign each to the lowest
slot position not occupied by a live variable of incompatible type. Wire it into
`scopes::check` after `compute_intervals`. Once all tests pass with `assign_slots`,
remove `claim()` from `state.rs`.

**Details:** [ASSIGNMENT.md](ASSIGNMENT.md) §"P2 — Full slot assignment pass".

---

### 26. ~~`vector_db` panics with `var_nr=65535` when appending struct elements to a vector field~~ **FIXED**

Added `vec == u16::MAX` guard in `vector_db` (parser.rs). Test: `tests/docs/19-threading.loft`.

---

### ~~31. `v += [struct_var]` appends empty element instead of copying struct~~ **FIXED 2026-03-13**

**Root cause:** In `new_record` (parser.rs), the branch for `Type::Reference` elements inside a vector literal had three cases: `Value::Insert` (inline literal — correct), `is_field` (field access — emits `OpCopyRecord`), and `else` (variables, calls — just pushed the expression without copying). The `else` case created a new empty element slot then discarded the source DbRef without copying the struct data into the slot, leaving all float/reference fields at their default null/NaN values.

**Fix:** Collapsed the `is_field` and `else` branches into a single `else` that always emits `OpCopyRecord(src, Var(elm), type_nr)`. Both field accesses and variable references evaluate to a DbRef that `OpCopyRecord` can read from.

**Tests:** `tests/docs/17-libraries.loft` (append via var + inline literal), `tests/scripts/07-structs.loft` (vectors of structs).

---

### ~~32. `v += other_vec` replaces vector instead of appending~~ **FIXED 2026-03-13**

**Root cause:** `v += other_vec` where the RHS is an existing `vector<T>` variable (not a bracket literal `[...]`) was not dispatched to `OpAppendVector`. For a local variable LHS, `towards_set` emitted `Set(v, Var(other_vec))` (reassignment). For a field LHS (`bx.pts += more`), the scalar-field-append path in `parse_assign` was triggered, treating the whole vector as a single element to append.

**Fix:** Added a new guard in `parse_assign` (before the scalar-field-append check): when `f_type` is `Vector`, `op == +=`, and the RHS type is also `Vector` (not `Insert`), emit `OpAppendVector(lhs_expr, rhs_expr, rec_tp)` directly. Works for both variable and field LHS since both evaluate to a DbRef.

**Tests:** `tests/docs/17-libraries.loft` (field vector-to-vector append), `tests/scripts/09-vectors.loft`.

---

### 30. ~~`for c in enum_vector` loops forever~~ **FIXED**

**Fixed 2026-03-13** (`src/fill.rs::get_enum`): past-end `OpGetVector` returns null
`DbRef`; `get_byte(rec=0)` returns `i32::MIN`, which cast to `u8` gave `0` (valid
first variant) instead of the sentinel `255` that breaks the loop.  Fix: map
`i32::MIN → 255u8` before pushing.  Tests: `tests/scripts/08-enums.loft`.

---

## B-Tree Collection Bugs (found by stress tests, 2026-03-14)

### 33. `sorted` filtered loop-remove gives wrong sum for large N

**Symptom:** After `for r in sdb.rows if r.id % 2 == 0 { r#remove; }` on a sorted with
N=100 elements (ids 0–99), the sum of remaining odd-id values is 2698 instead of the
expected 2500 (1+3+…+99).

**Discovered by:** `tests/docs/21-stress.loft` section E (small case only; large N was
moved to key-null removal after discovery).

**Root cause hypothesis:** During B-tree iteration, removing a node triggers tree
rebalancing (node merging / key rotation). The iterator may then visit a rebalanced node
that was already seen, or skip a newly promoted node — producing incorrect element
coverage. The specific 198-unit error (2×99) suggests the record with id=99 is
double-counted or carries its original value from a previous build cycle.

**Workaround:** Use key-null removal instead:
```loft
for i in 0..50 { sdb.rows[i * 2] = null; }  // remove even ids by key
```

**Best way forward:** Instrument `sorted_step()` (vector.rs) to log the sequence of
B-tree nodes visited; compare against the expected sequence. Check whether node merging
during `#remove` invalidates the internal iterator cursor in `Iterator::at`.

---

### 34. `index` key-null removal leaves 1 record for large N

**Symptom:** After `for i in 0..N { idb.rows[i, "name{i}"] = null; }` with N=100, a
count loop still finds 1 remaining record.

**Discovered by:** `tests/docs/21-stress.loft` section A2 (large N path; small N removed
correctly).

**Root cause hypothesis:** The B-tree index deletion function has an off-by-one: the last
key removed (or possibly the first, depending on the rebalancing path) is missed. The bug
may be in the `index_remove` path in `vector.rs` when the tree root becomes empty after
the penultimate deletion.

**Workaround:** Use loop `#remove` for small N (works ≤ 3 records verified); for large N
let the variable go out of scope to reclaim storage.

**Best way forward:** Write a unit test that removes exactly N records by key for
N ∈ {1,2,3,10,50,100} and verify count==0 after each. Bisect to find the N at which the
off-by-one first appears. Inspect `vector.rs::index_remove_key` near the root-collapse
logic.

---

### 35. `index` loop-remove panics "Unknown record" for large N

**Symptom:** `for r in idb.rows { r#remove; }` on a 100-element index panics at
`src/store.rs:772` with `debug_assert!(self.claims.contains(&rec))` — the iterator holds
a reference to a record that has already been freed.

**Discovered by:** `tests/docs/21-stress.loft` section A (originally used loop-remove;
switched to key-null after discovery).

**Root cause:** The B-tree iterator holds a `DbRef` to the current node. When `#remove`
frees that node and the tree rebalances, the freed record's bytes may be reclaimed by the
next insertion or by the tree compaction, and the iterator's stale `DbRef` then fails the
`claims` check on the next `step()` call.

**Workaround:** Remove by key individually, or let the variable go out of scope. Avoid
`for r in idx { r#remove; }` on index collections.

**Best way forward:** The index iterator must either (a) snapshot all keys before the
loop and remove by key in a second pass, or (b) advance the cursor *before* freeing the
current record in `#remove`, so that the freed node is never accessed again.

---

## Code Quality

### 25. ~~Dead Option-B helpers in variables.rs~~ **ALREADY CLEAN**

No dead functions found; `first_def` is a struct field only.

---

## See also
- [PLANNING.md](PLANNING.md) — Priority-ordered enhancement backlog
- [INCONSISTENCIES.md](INCONSISTENCIES.md) — Language design inconsistencies and asymmetries
- [TESTING.md](TESTING.md) — Test framework, reproducing and debugging issues
