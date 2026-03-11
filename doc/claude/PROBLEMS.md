# Known Problems in Loft

This document lists known bugs, unimplemented features, and limitations in the loft
language and its interpreter (`lavition`). For each issue the workaround and the
recommended fix path are described.

---

## Runtime Crashes

### 1. Methods returning a new struct record crash at database.rs:1462

**Symptom:** Calling a method whose return type is a struct (creates a new record)
crashes with `index out of bounds` at `src/database.rs:1458`.

**Example:**
```loft
struct Color { r: integer not null }
fn double(self: Color) -> Color { Color { r: self.r * 2 } }
fn main() {
    c = Color { r: 3 };
    d = c.double();   // ← crashes
    assert(d.r == 6, "...");
}
```

**Workaround:** Inline the computation; avoid methods that return a new record.

**Best way forward:** Investigate `OpCreateRecord` / `new_record` in `state.rs`.
The DbRef returned from a method call has an invalid `store_nr` because the new
record is allocated relative to the callee's frame, not the caller's. Fix: allocate
the return record in the caller's scope (pass a pre-allocated slot as an out-param),
or copy the record into the caller's store after the call returns.

---

### 2. Borrowed-reference pre-init causes runtime crash at database.rs:1462

**Symptom:** A reference variable first assigned inside a branch (borrowed from a
vector element via `v[i]`) gets a garbage `store_nr=8` DbRef at runtime and crashes.

**Status:** Owned references are correctly pre-initialized (Option A sub-3, 2026-03-11).
Borrowed references still crash.

**Test:** `tests/slot_assign.rs::ref_inside_branch_borrowed` (marked `#[ignore]`).

**Details:** `doc/claude/ASSIGNMENT.md` §Issue 1.

**Best way forward:** Extend `needs_pre_init` in `scopes.rs` to emit `Set(v, Null)`
for borrowed refs as well. The current guard `deps_ready` in `find_first_ref_vars`
skips borrowed refs; relax it so a `Null` pre-init is emitted for variables of type
`RefVar` (stack-borrow) that are first assigned inside a branch. The pre-init does
not need to allocate storage — setting the DbRef to the null sentinel suffices.

---

### 3. Polymorphic text methods on struct-enum variants overflow state.rs:2084

**Symptom:** When multiple variants of a struct-enum each define a text-returning
method that accesses fields via format strings, running any of them causes an integer
subtract overflow or segfault.

**Example:**
```loft
enum Shape {
    Circle { radius: float },
    Rect   { width: float, height: float }
}
fn describe(self: Circle) -> text { "circle r={self.radius}" }
fn describe(self: Rect)   -> text { "rect {self.width}x{self.height}" }
```

**Workaround:** Avoid polymorphic text methods that use format strings on struct-enum
variants. Return integers or use global functions instead.

**Best way forward:** The overflow at `state.rs:2084` is a stack-offset underflow
during polymorphic dispatch. Each variant generates text-slot annotations relative
to its own frame size, but the dispatch trampoline shares a single frame size.
Fix: make the text-slot offset table per-variant during bytecode generation in
`interpreter.rs`, or pad all variant frames to the same size before emitting the
dispatch table.

---

### 4. `v += items` inside a ref-param function does not modify the caller's vector

**Symptom:** Appending to a `&vector<T>` parameter inside a function has no visible
effect on the caller's variable after the call returns.

**Example:**
```loft
fn fill(v: &vector<Item>, extra: vector<Item>) { v += extra; }
fn main() {
    buf = [Item { name: "a", value: 1 }];
    fill(buf, [Item { name: "b", value: 2 }]);
    assert(len(buf) == 2, "...");  // ← fails; len is still 1
}
```

**Workaround:** Field mutation via ref-param (`v[i].field = x`) works correctly.
For append, pass the result back as a return value or handle it in the caller.

**Best way forward:** `v += extra` inside a ref-param currently creates a new
allocation that is not linked back to the caller. Two options: (A) after the call
returns, emit an `OpCopyVector` in the callee epilogue to copy the modified vector
back into the caller's slot (similar to how ref-structs are handled); (B) pass a
`DbRef` pointer to the caller's vector slot so in-place append writes directly
into the shared allocation.

---

### 5. Appending a scalar to a vector struct field that starts empty has no effect

**Symptom:** `b.items += scalar` on a newly constructed struct variable does nothing
visible. The field stays empty.

**Example:**
```loft
struct Box { items: vector<i32> }
fn main() {
    b = Box {};
    b.items += 1;   // ← silently no-ops
    assert(len(b.items) == 1, "...");  // fails: len=0
}
```
Initialising via a struct literal (`Box { items: [1, 2] }`) or appending a whole
struct element to a `vector<Struct>` field works correctly.

**Workaround:** Initialise the vector in the struct literal, or accumulate into a
local vector first and assign it as a field.

**Best way forward:** When `+=` is emitted on a struct field whose current value is
a null vector, `OpVectorAppend` (or equivalent) must first allocate the vector in
the store and write the new DbRef back into the struct's field slot before appending.
The bug is that the append writes to a temporary copy of the null DbRef without
persisting the new allocation to the parent record.

---

## Parser / Lexer Bugs

### 6. Uppercase hex literals are rejected

**Symptom:** `0x2A` causes `Error: Problem parsing hex number`. Only lowercase hex
digits (`0x2a`) are accepted.

**Workaround:** Always use lowercase hex (`0x2a`, `0xff`, `0xdeadbeef`).

**Best way forward:** One-line fix in `src/lexer.rs`: change the hex-digit
character test from a manual `'a'..='f'` range to `c.is_ascii_hexdigit()` (which
accepts `A–F` as well) and normalise the digit to lowercase before accumulating
the value.

---

### 7. Open-start slice syntax `s[..n]` is not supported

**Symptom:** `s[..2]` gives `Error: Invalid index on string`.

**Workaround:** Use `s[0..2]` (explicit start index).

**Best way forward:** In `parse_index` (parser.rs), when the token directly after
`[` is `..` or `..=`, treat the missing start as the integer literal `0`. This is a
small lookahead change with no ambiguity.

---

### 8. Calling a method directly on a constructor expression is rejected

**Symptom:** `MyStruct { ... }.method()` gives `Error: MyStruct should be MyStruct
on call to method`.

**Workaround:** Assign to an intermediate variable first:
```loft
tmp = MyStruct { ... };
result = tmp.method();
```

**Best way forward:** In `parse_expr` (parser.rs), after a struct-literal is parsed
into a `Value`, allow chaining a `.method()` call immediately. The parser currently
expects either `=` or `;` after a struct-literal expression; adding a `.` branch
that delegates to `parse_method` would fix this. Requires ensuring the temporary
record lifetime is extended to cover the method call.

---

### 9. Nested `\"` inside format expressions is not supported

**Symptom:** Using `\"` inside a `{...}` format expression in a string literal causes
`Error: Dual definition of ...`.

**Example:**
```loft
// FAILS — \" inside the format expression
assert("{o:j}" == "{\"key\":1}", "...");
```

**Workaround:** Escape outer braces so the RHS is a plain string, not a format expression:
```loft
assert("{o:j}" == "{{\"key\":1}}", "...");
```

**Best way forward:** The lexer's string-scanning state machine needs a nested-string
mode: when it encounters `{` inside a format string, track brace depth; when inside
a nested string literal (after `"`), treat `\"` as an escape rather than as
terminating the outer format string.

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

### 11. Field-name overlap between two structs causes wrong index range results

**Symptom:** When two structs in the same file share a field name at different
positions, sorted/index range iteration returns results from the wrong struct type.

**Example:** `SortElm { key: text, value: integer }` and
`IdxElm { nr: integer, key: text }` both have a field named `key`; range iteration
on `IdxElm` may treat elements as `SortElm` and give wrong values or "Unknown field"
errors.

**Workaround:** Use unique field names across all structs in the same file, or split
conflicting structs into separate files.

**Best way forward:** Field lookup in `parse_field` (parser.rs) must be type-scoped.
When the left-hand side type is known, resolve the field name only within that
struct's definition rather than searching all definitions globally. The fix involves
threading the owning-type `d_nr` through `field()` in `data.rs` and filtering by it.

---

## Library System Limitations

### 12. `use` statements must appear before all definitions

**Symptom:** Placing `use libname;` after any function or struct definition gives
`Fatal: Syntax error`. There is no diagnostic message explaining the constraint.

**Workaround:** Put all `use` statements at the very top of the file.

**Best way forward:** Either relax `parse_file` in parser.rs to process `use`
statements at any point during the first pass (interleaved with other definitions),
or emit a clear diagnostic: "use statements must appear before all definitions".
The first option is more user-friendly; the second is a smaller change.

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

### 14. Cannot add methods to a library type from an importing file

**Symptom:** Writing `fn extend(self: testlib::Point)` — a method whose receiver
uses the `::` namespace separator — gives `Fatal: Syntax error`. Using just
`fn extend(self: Point)` gives `Error: Undefined type Point`.

**Workaround:** Add all methods inside the library file itself.

**Best way forward:** In `parse_fn_args` (parser.rs), when parsing the type of the
first (`self`) parameter, call `parse_type` the same way it is called for regular
parameters — `parse_type` already handles `::` qualified names. Currently the self
parameter type is parsed by a different, simpler path that does not support `::`.

---

### 15. `pub` on struct fields causes a parse error

**Symptom:** `pub struct Foo { pub x: i32 }` crashes the parser with
"Expect attribute" on the field line. `pub struct` and `pub fn` at the top level
are silently accepted but have no effect (there is no visibility system).

**Workaround:** Omit `pub` from all field declarations and from top-level items
(it has no effect anyway).

**Best way forward:** Two-part fix: (A) in the struct-field parser, consume an
optional leading `pub` token and discard it (forward-compatible); (B) design a
visibility system where `pub` marks items as public (default: private to the
library) so that callers cannot accidentally access internal fields. Part A is
a quick fix; Part B requires a broader design decision.

---

## Unimplemented Features

### 16. `for n in range { expr }` inside a vector expression is not supported

**Symptom:** Writing `[for n in 1..5 { n * 2 }]` as a standalone vector expression
gives `Error: For inside a vector is not yet implemented` (parser.rs:3179).

**Workaround:** `{for n in 1..5 { n * 2 }}` works inside a format string. Outside
a format string, build the vector with an explicit loop and `+=`.

**Best way forward:** In `parse_vector` (parser.rs), when the first token after `[`
is `for`, emit a vector comprehension: allocate a temporary vector, generate a
`for` loop that appends each expression result, and return the temporary as the
`[]` value. The parser already has all the pieces; this is a wiring change.

---

### 17. Reverse iteration on `sorted<T>` is not implemented

**Symptom:** Trying to iterate a sorted collection in reverse order panics at
state.rs:1074 (`Not implemented`).

**Workaround:** Collect into a plain vector and reverse-iterate the vector.

**Best way forward:** The sorted collection is a binary search tree. Reverse
in-order traversal (right subtree → node → left subtree) is symmetric to the
existing forward traversal. Implement `iter_sorted_rev` in `fill.rs` / `database.rs`
by swapping the left/right child pointers in the traversal stack, and wire it up
through `parse_for` when the `rev()` wrapper is detected.

---

### 18. Writing a vector to a binary file produces 0 bytes

**Symptom:** `f += [1.1f, 2.2f]` on a binary file silently writes nothing;
`f#size` remains 0. Only scalar types (integer, long, single, float) can be written.

**Workaround:** Loop over elements and write each one individually:
```loft
for v in my_vector { f += v; }
```

**Best way forward:** In `write_to_file` (state.rs / fill.rs), handle the
`Value::Vector` / `DbRef` case by iterating elements and calling the scalar write
path for each. Element type size is already available from `Stores::type_size()`.

---

### 19. `f#size = n` (file truncate / extend) is not implemented

**Symptom:** Assigning to `f#size` to truncate or extend a file has no effect.
The setter (`set_file_size`) is defined in the parser but not in the interpreter.

**Workaround:** None currently. Write the exact content you want; delete and recreate
if truncation is needed.

**Best way forward:** Add `OpSetFileSize` to `fill.rs` using Rust's `File::set_len(n)`.
The opcode already has a stub in the parser; only the fill.rs implementation is missing.

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

### 21. Command-line arguments cannot be passed to `fn main()`

**Symptom:** `fn main(args: vector<text>)` is not supported; any arguments are ignored.

**Status:** Two separate TODOs in state.rs (lines 2493, 2585).

**Best way forward:** After CLI argument parsing in `main.rs`, collect remaining
positional arguments into a `Vec<String>`. In `State::execute`, if the `main`
function definition has a `vector<text>` parameter, push a loft vector onto the
stack before entering the function body. The vector layout matches what `OpCreateVector`
produces; each element is a text DbRef created with `code_add_str`.

---

### 22. Spatial index operations are not implemented

**Symptom:** Copy, remove, and iteration operations on `spacial<T>` collections
panic with `Not implemented` at database.rs.

**Best way forward:** Implement one operation at a time in `database.rs` and `fill.rs`,
starting with iteration (needed for any practical use), then remove (needed for
mutation), then copy (needed for assignment). The spacial index structure (radix tree
or R-tree) is already allocated; the iteration traversal is the main missing piece.

---

## String Iteration Semantics (In Progress)

### 23. `c#index` in `for c in text` returns post-advance offset instead of pre-advance

**Current (buggy):** `c#index` returns the byte offset *after* the current character.

**Planned semantics:**
- `c#index` = byte offset of the *start* of the current character
- `c#next` = byte offset *after* the current character

**Status:** Call-sites in `default/02_images.loft` and tests in `tests/strings.rs` and
`tests/suite/02-text.loft` have already been updated to the new semantics. The Rust
implementation in `src/parser.rs` (`parse_for` / `iter_for` / `iterator`) still needs
to be updated: create two variables per text loop — `{id}#index` (pre-advance, saved
per iteration) and `{id}#next` (loop driver) — and pass both to `iterator`.

See `PLAN.md` and `doc/claude/LOFT.md` for the full plan.

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

**Details:** `doc/claude/ASSIGNMENT.md` §"P2 — Full slot assignment pass".

---

## Code Quality

### 25. Dead Option-B helpers in variables.rs

Two functions (`first_def`, `min_safe_claim_pos`) were added during a failed Option-B
attempt and are no longer used. Remove them.

---

## Reference

| # | Category | Severity | Effort |
|---|----------|----------|--------|
| 1 | Runtime crash — method returns struct | High | Medium |
| 2 | Runtime crash — borrowed-ref pre-init | High | Small |
| 3 | Runtime crash — polymorphic text methods | High | Medium |
| 4 | Runtime crash — ref-param vector append | High | Medium |
| 5 | Runtime crash — scalar vec field append | High | Small |
| 6 | Parser bug — uppercase hex | Low | Trivial |
| 7 | Parser bug — open-start slice | Low | Trivial |
| 8 | Parser bug — method on constructor | Medium | Small |
| 9 | Parser bug — nested `\"` in format expr | Medium | Medium |
| 10 | Parser silent — unresolved format expr slot | Medium | Small |
| 11 | Parser bug — field-name overlap | Medium | Medium |
| 12 | Library — use placement | Low | Trivial |
| 13 | Library — unqualified access | Low | Small |
| 14 | Library — self param with `::` type | Medium | Small |
| 15 | Library — pub on fields fails | Low | Trivial |
| 16 | Unimplemented — for comprehension in `[]` | Medium | Small |
| 17 | Unimplemented — reverse sorted iteration | Medium | Small |
| 18 | Unimplemented — vector write to file | Low | Small |
| 19 | Unimplemented — file truncation | Low | Trivial |
| 20 | Unimplemented — seek before open | Low | Small |
| 21 | Unimplemented — CLI args to main | Medium | Small |
| 22 | Unimplemented — spatial index ops | Low | Large |
| 23 | In progress — string iter `#index` semantics | Medium | Small |
| 24 | In progress — compile-time slot assignment | Low | Medium |
| 25 | Code quality — dead functions | Low | Trivial |
