// Copyright (c) 2026 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

# Enhancement Planning

## Goals

Loft / lavition aims to be:

1. **Correct** — programs produce the right answer or a clear error, never silent wrong results.
2. **Prototype-friendly** — a new developer should be able to express an idea in loft with minimal
   ceremony: imports that don't require prefixing every name, functions that can be passed and
   called like values, concise pattern matching, and a runtime that reports errors clearly and
   exits with a meaningful code.
3. **Performant at scale** — allocation, collection lookups, and parallel execution should stay
   efficient as data grows.
4. **Architecturally clean** — the compiler and interpreter internals should be free of technical
   debt that makes future features hard to add.

The items below are ordered by tier: things that break programs come first, then language-quality
and prototype-friction items, then architectural work.

Sources: [PROBLEMS.md](PROBLEMS.md) · [INCONSISTENCIES.md](INCONSISTENCIES.md) · [ASSIGNMENT.md](ASSIGNMENT.md) · [THREADING.md](THREADING.md) · [LOGGER.md](LOGGER.md)

---

## Contents
- [Tier 0 — Crashes / Silent Wrong Results](#tier-0--crashes--silent-wrong-results)
- [Tier 1 — Language Quality & Consistency](#tier-1--language-quality--consistency)
- [Tier 2 — Prototype-Friendly Features](#tier-2--prototype-friendly-features)
- [Tier 3 — Architectural / Future Work](#tier-3--architectural--future-work)
- [Quick Reference](#quick-reference)
- [Completed Work](#completed-work)

---

## Tier 0 — Crashes / Silent Wrong Results

### T0-5  Index loop-remove panics "Unknown record" (use-after-free in B-tree iterator)
**Sources:** PROBLEMS #35; found by `tests/scripts/16-stress.loft`
**Severity:** High — `for r in index_var { r#remove; }` panics in debug builds; silently
corrupts memory in release builds for collections with more than ~3 elements.
**Symptom:** `debug_assert!(self.claims.contains(&rec))` at `src/store.rs:772` fires because
the iterator holds a stale `DbRef` to a node that was freed and reused during B-tree rebalancing.
**Workaround:** Remove by key individually (`idx[k1, k2] = null`) or let the variable go
out of scope to reclaim storage.
**Fix path:** The iterator cursor must be advanced *before* the current record is freed in
`#remove`, so the freed node is never dereferenced again.
**Effort:** Medium (vector.rs, state.rs)

---

### T0-6  Index key-null removal in a loop leaves 1 record (off-by-one in B-tree deletion)
**Sources:** PROBLEMS #34; found by `tests/scripts/16-stress.loft`
**Severity:** High — silent wrong result; no panic. After N key-null removals the collection
reports 0 elements by count but has 1 phantom record.
**Workaround:** For large N use loop `#remove` for small collections (≤ 3 verified correct);
for large N let the variable go out of scope.
**Fix path:** Bisect by N (binary search over {1..100}) to find the smallest N that misbehaves;
inspect `vector.rs::index_remove_key` root-collapse logic.
**Effort:** Medium (vector.rs)

---

### T0-7  Sorted filtered loop-remove gives wrong result for large N
**Sources:** PROBLEMS #33; found by `tests/scripts/16-stress.loft`
**Severity:** High — silent wrong sum; no panic. `for r in sdb if cond { r#remove; }` with
N=100 produces incorrect element coverage during B-tree rebalancing.
**Workaround:** Use key-null removal instead of filtered loop-remove for sorted collections.
**Fix path:** Instrument `sorted_step()` to log visited nodes and compare against expected
sequence; look for double-visits or skips caused by node merging in `#remove`.
**Effort:** Medium (vector.rs)

---

### T0-1  `16-parser.loft` crashes with `store_nr=60` in `set_int`
**Sources:** PROBLEMS #27
**Severity:** High — uncaught panic, test marked `#[ignore]`
**Symptom:** Running `tests/docs/16-parser.loft` panics with "Incorrect store" inside
`set_int`; `store_nr=60` is far outside the valid range.
**Root cause:** Unknown.  Previous investigation ruled out a simple stack-frame bug;
the corruption origin has not been isolated.
**Fix path:**
1. Enable `LOFT_DUMP=1` with a minimal reproducer derived from `16-parser.loft`.
2. Add `debug_assert!(db.store_nr < valid_limit)` at every `set_int` / `get_int` entry
   to catch the corruption closer to its source.
3. Bisect the test to find the smallest loft snippet that triggers the panic.
**Effort:** High (investigation-first)

---

## Tier 1 — Language Quality & Consistency

### ~~T1-1  Non-zero exit code on parse / runtime error~~ **DONE 2026-03-14**

`main.rs` now exits with code 1 on parse errors and on `had_fatal = true` (set by
`panic()` / `assert(false)` in production mode). Tests: `production_mode_panic_sets_had_fatal`,
`production_mode_assert_false_sets_had_fatal`, `production_mode_no_error_had_fatal_false`
in `tests/issues.rs`.

---

### T1-2  Wildcard and selective imports
**Sources:** INCONSISTENCIES #13; PROBLEMS #13
**Severity:** Medium — heavy prototyping use of a library requires `libname::` on every name
**Description:** The only import form is `use mylib;`, which requires prefixing every
reference with `mylib::`.  Two shorter forms would eliminate that friction:
```loft
use mylib::*;           // import all public names into current scope
use mylib::Point, add;  // import specific names
```
**Fix path:**
1. Parser: extend `parse_use` to handle `::*` and `::name { , name }`.
2. Typedef/first pass: when `::*` is seen, copy all definitions from the library's namespace
   into the current file's namespace with a shadowing check.
3. Error on collision if a wildcard-imported name conflicts with a local definition.
**Effort:** Medium (parser.rs, typedef.rs)

---

### T1-3  First-class callable function references
**Sources:** INCONSISTENCIES #15
**Severity:** Medium — `fn <name>` references exist but cannot be called or stored
**Description:** The `fn <name>` expression produces a compile-time-checked `Type::Function`
value.  Its only consumer today is `parallel_for`.  To support higher-order programming
(map, filter, callbacks), these references must be:
1. Callable: `f(args)` where `f: fn(T) -> R` dispatches via the stored definition number.
2. Storable: allowed as struct fields and vector elements.
3. Passable: accepted as a parameter of type `fn(T) -> R`.
**Fix path:**
1. Parser: in `parse_call`, if the callee is a `Var` of type `Function`, emit `OpCallRef`.
2. Bytecode: `OpCallRef` reads the def-nr from the stack and calls `execute_at`.
3. Type checker: allow `fn(T) -> R` as a field/param/variable type.
4. Calling convention: define how arguments are passed to a ref-called function (same
   stack layout as a direct call; the def-nr replaces the literal jump target).
**Effort:** Medium (parser.rs, state.rs, fill.rs)

---

### T1-4  Map / filter / reduce in the standard library
**Sources:** Prototype-friendly goal
**Severity:** Medium — users reaching for functional patterns have no stdlib support
**Description:** Provide three functions on `vector<T>`:
```loft
map(v: vector<T>, f: fn(T) -> U) -> vector<U>
filter(v: vector<T>, pred: fn(T) -> boolean) -> vector<T>
reduce(v: vector<T>, init: U, f: fn(U, T) -> U) -> U
```
These depend on T1-3 (callable fn refs).  Initial implementation can be in loft itself
once `fn(T)->R` parameters can be called at runtime.
**Fix path:** Implement in `default/01_code.loft` after T1-3 is complete.
**Effort:** Low (loft code only, depends on T1-3)

---

### T1-5  Plain enum methods
**Sources:** INCONSISTENCIES #6
**Severity:** Medium — plain enums (`enum Direction { North, East, South, West }`) cannot
have methods; only struct-enum variants support dispatch
**Description:** A `fn label(self: Direction) -> text` is currently a compile error.
Users must write a free function that chains `if` statements on the value.  Allowing
methods on plain enums would make the dispatch consistent with struct enums and enable
pattern-like ergonomics:
```loft
fn label(self: Direction) -> text {
    if self == North { "north" } else if self == South { "south" } else { "{self}" }
}
```
**Fix path:**
1. Parser: allow `fn f(self: PlainEnum) -> T` as a valid declaration.
2. First-pass typedef: register these as ordinary functions keyed on the enum type.
3. Dispatch: emit a regular function call (no polymorphic dispatch table needed since
   there is only one body per plain-enum type).
**Effort:** Medium (parser.rs, typedef.rs)

---

### T1-6  `match` expression for enum dispatch
**Sources:** Prototype-friendly goal
**Severity:** Medium — if/else chains on enum values are verbose; exhaustiveness is unchecked
**Description:** A `match` expression covers all variants with compiler-checked exhaustiveness:
```loft
result = match direction {
    North => "north"
    East  => "east"
    South => "south"
    West  => "west"
}
// Compiler error if a variant is missing and no wildcard `_ =>` is present.
```
For struct enums, each arm binds the variant's fields:
```loft
area = match shape {
    Circle { radius }     => PI * radius * radius
    Rect { width, height} => width * height
}
```
This subsumes the current polymorphic dispatch for struct enums while also covering
plain enums (where dispatch is today impossible without a chain of `if` comparisons).
**Fix path:**
1. Lexer/parser: parse `match expr { pattern => expr }` blocks.
2. IR: lower each arm to a `Value::Match` node; add `OpMatch` bytecode instruction.
3. Type checking: verify all variants are covered or a wildcard `_` arm is present.
4. Code gen: emit a jump table or if-chain depending on the enum kind.
**Effort:** High (lexer, parser, state.rs, fill.rs)

---

### ~~T1-7  Null-coalescing operator `??`~~ **DONE 2026-03-14**

`??` is implemented at the lowest precedence (below `||`), chains left-to-right, and
handles the bare-`null` LHS special case. Implementation: `"??"` token added to lexer.rs
TOKENS; handled in `handle_operator` in parser.rs — mutates `*code` and `*ctp` in place
and returns `None` (allowing chaining). `call_op("!=")` is used for the null check.
**Note:** complex LHS expressions are evaluated twice at runtime (V1 limitation).
Tests: 5 cases in `tests/expressions.rs` (`null_coerce_*`).

---

### ~~T1-8  For-loop mutation guard extended to field access~~ **DONE 2026-03-14**

Both cases now produce a compile error:
```loft
for e in v         { v += [x]; }        // ERROR: variable
for e in db.items  { db.items += [x]; } // ERROR: field access (new)
```
Added `Function::set_coll_value` + `is_iterated_value` in variables.rs. Saved
`orig_coll_expr` before vector temp-copy substitution in `parse_for`; restored via
`set_coll_value` after `iterator()`. Two-branch check in `parse_assign`.
Tests: `for_loop_mutation_guard_*` in `tests/issues.rs`.

---

### T1-9  Document and test loop attributes on sorted / index / hash
**Sources:** INCONSISTENCIES #2
**Severity:** Low — which of `#first`, `#count`, `#index`, `#remove` work on keyed collections
is undocumented and partially untested
**Description:** `vector` supports the full set of loop attributes.  The keyed collections
(`sorted`, `index`, `hash`) may support `#first` and `#count` but this is not verified or
documented.  `#remove` definitely does not work on keyed collections.
**Fix path:**
1. Write a small loft test for each attribute × each collection type.
2. Implement any that are feasible (e.g. `#first` / `#count` on `sorted`).
3. Document the matrix in LOFT.md and emit a compile error for unsupported combinations.
**Effort:** Low–Medium (fill.rs, state.rs, LOFT.md)

---

## Tier 2 — Prototype-Friendly Features

### T2-1  Lambda / anonymous function expressions
**Sources:** Prototype-friendly goal; depends on T1-3 (callable fn refs)
**Severity:** Medium — without lambdas, `map` / `filter` require a named top-level function
for every single-use transform, which is verbose for prototyping
**Description:** Allow inline function literals at the expression level:
```loft
doubled = map(items, fn(x: integer) -> integer { x * 2 });
evens   = filter(items, fn(x: integer) -> boolean { x % 2 == 0 });
```
An anonymous function expression produces a `Type::Function` value, exactly like `fn <name>`,
but the body is compiled inline.  No closure capture is required initially (captured variables
can be added in a follow-up).
**Fix path:**
1. Parser: recognise `fn '(' params ')' '->' type block` as an expression.
2. Compilation: synthesise a unique def-nr, compile the body as a top-level function.
3. Runtime: the resulting value is the def-nr — identical to a named `fn <name>` ref.
**Effort:** Medium–High (parser.rs, state.rs)

---

### T2-2  REPL / interactive mode
**Sources:** Prototype-friendly goal
**Severity:** Low–Medium — a REPL dramatically reduces iteration time when exploring data
or testing small snippets
**Description:** Running `lavition` with no arguments (or `lavition --repl`) enters an
interactive session where each line or block is parsed, compiled, and executed immediately.
State accumulates across lines (variables and type definitions persist).
```
$ lavition
> x = 42
> "{x * 2}"
84
> struct Point { x: float, y: float }
> p = Point { x: 1.0, y: 2.0 }
> p.x + p.y
3.0
```
**Fix path:**
1. Implement an incremental `Parser` mode that accepts a single statement and returns when
   complete (tracking open braces to handle multi-line blocks).
2. Maintain a persistent `State` and `Stores` across iterations.
3. Print expression results automatically (non-void expressions print their value).
4. On parse error, discard the failed line and continue the session.
**Effort:** High (main.rs, parser.rs, new repl.rs)

---

### T2-3  `const` unification — compile-time local constants
**Sources:** INCONSISTENCIES #1
**Severity:** Low — `const c = expr` today is only a debug-build runtime lock; developers
expect compile-time immutability
**Description:** Make `const c = expr` a compile-time constant: any write to `c` is a
type error regardless of build mode.  This unifies the meaning of `const` across all
contexts (parameter modifier and local declaration) and removes the confusing
build-mode difference.
**Fix path:**
1. In the parser, when `const` is on a local assignment, mark the variable with
   `Variable.const_param = true` (same flag used for parameters).
2. Remove the runtime `store.lock()` path for local `const` (only needed if debug-mode
   enforcement is desired as a redundant check).
**Effort:** Low (parser.rs, scopes.rs)

---

### T2-4  Separate `file#exists` from `file#format`
**Sources:** INCONSISTENCIES #16
**Severity:** Low — `NotExists` is a file-absence signal masquerading as a file mode
**Description:** Replace the `Format.NotExists` sentinel with a dedicated `f#exists: boolean`
attribute, so `f#format` only contains meaningful modes (`TextFile`, `LittleEndian`,
`BigEndian`, `Directory`):
```loft
f = file("path");
if !f#exists { println("file not found"); return; }
if f#format == TextFile { ... }
```
**Fix path:**
1. Add a `#exists` attribute handler in `parser.rs` (alongside `#format`).
2. Emit `OpGetExists` which returns `rec != 0` / format != NotExists.
3. Keep `Format.NotExists` for backwards compatibility but document it as deprecated.
**Effort:** Low (parser.rs, external.rs)

---

## Tier 3 — Architectural / Future Work

### T3-1  `assign_slots()` pass + remove `claim()` from code generation
**Sources:** [ASSIGNMENT.md](ASSIGNMENT.md) Issues 3+4, Steps 3+4
**Description:** Replace the runtime slot-claim strategy with a compile-time
`assign_slots()` pre-pass that assigns stack offsets without any runtime overhead.
Once done, `claim()` calls in `byte_code()` can be removed.
**Effort:** High (state.rs, variables.rs, scopes.rs)

---

### T3-2  Parallel workers: extra arguments and text/reference return types
**Sources:** [THREADING.md](THREADING.md) (deferred items)
**Description:** Current limitation: all worker state must live in the input vector;
returning text or references is unsupported.
**Fix path:**
1. Extra args: synthesise an IR-level wrapper function that captures the extra args as
   closure variables and passes them alongside the element.
2. Text/reference returns: merge worker-local stores back into the main `Stores` after all
   threads join.
**Effort:** High (parser.rs, parallel.rs, store.rs)

---

### T3-3  Logger: production mode, source injection, hot-reload
**Sources:** [LOGGER.md](LOGGER.md)
**Description:**
- Production panic handler writes structured log entry instead of aborting.
- Source-location metadata injected at compile time into assert/log calls.
- Hot-reload of log-level config without restarting the interpreter.
**Effort:** Medium–High (logger.rs, parser.rs, state.rs)

---

### T3-4  Optional Cargo features
**Sources:** OPTIONAL_FEATURES.md
**Description:** Gate subsystems behind `cfg` features: `png` (image support), `gendoc`
(HTML documentation generation), `parallel` (threading), `logging` (logger), `mmap`
(memory-mapped storage).  Remove `rand_core` / `rand_pcg` dead dependencies.
**Effort:** Medium (Cargo.toml, conditional compilation in store.rs, external.rs, main.rs)

---

### T3-5  Spatial index operations
**Sources:** PROBLEMS #22
**Description:** `spacial<T>` collection type is declared in the parser but its insert /
lookup / iteration operations are not implemented.
**Effort:** High (new index type in database.rs and vector.rs)

---

### T3-6  Closure capture for lambda expressions
**Sources:** Depends on T2-1
**Description:** T2-1 defines anonymous functions without variable capture.  Full closures
require the compiler to identify captured variables, allocate a closure record, and pass
it as a hidden argument to the lambda body.  This is a significant IR and bytecode change.
**Effort:** Very High (parser.rs, state.rs, scopes.rs, store.rs)

---

## Quick Reference

| ID   | Title                                                   | Tier | Effort   | Source                     |
|------|---------------------------------------------------------|------|----------|----------------------------|
| T0-1 | 16-parser.loft crash store_nr=60                        | 0    | High     | PROBLEMS #27               |
| T0-5 | Index loop-remove panics (use-after-free in iterator)   | 0    | Medium   | PROBLEMS #35               |
| T0-6 | Index key-null removal leaves 1 record (off-by-one)     | 0    | Medium   | PROBLEMS #34               |
| T0-7 | Sorted filtered loop-remove silent wrong result         | 0    | Medium   | PROBLEMS #33               |
| ~~T1-1~~ | ~~Non-zero exit code on error~~ **DONE**           | —    | —        | DONE 2026-03-14            |
| T1-2 | Wildcard and selective imports                          | 1    | Medium   | INCON #13, PROBLEMS #13    |
| T1-3 | Callable fn-ref variables                               | 1    | Medium   | INCON #15                  |
| T1-4 | map / filter / reduce in stdlib                         | 1    | Low*     | Prototype goal             |
| T1-5 | Plain enum methods                                      | 1    | Medium   | INCON #6                   |
| T1-6 | match expression with exhaustiveness                    | 1    | High     | Prototype goal             |
| ~~T1-7~~ | ~~Null-coalescing operator `??`~~ **DONE**         | —    | —        | DONE 2026-03-14            |
| ~~T1-8~~ | ~~For-loop mutation guard for field access~~ **DONE** | — | —     | DONE 2026-03-14            |
| T1-9 | Document / implement loop attrs on sorted/hash/index    | 1    | Low–Med  | INCON #2                   |
| T2-1 | Lambda / anonymous function expressions                 | 2    | Med–High | Prototype goal             |
| T2-2 | REPL / interactive mode                                 | 2    | High     | Prototype goal             |
| T2-3 | const unification (compile-time local constants)        | 2    | Low      | INCON #1                   |
| T2-4 | Separate `file#exists` from `file#format`               | 2    | Low      | INCON #16                  |
| T3-1 | assign_slots() pass + remove claim() codegen            | 3    | High     | ASSIGNMENT Issue 3         |
| T3-2 | Parallel workers: extra args + text/ref returns         | 3    | High     | THREADING deferred         |
| T3-3 | Logger: production mode, source injection               | 3    | Med–High | [LOGGER.md](LOGGER.md)     |
| T3-4 | Optional Cargo features                                 | 3    | Medium   | OPTIONAL_FEATURES.md       |
| T3-5 | Spatial index operations                                | 3    | High     | PROBLEMS #22               |
| T3-6 | Closure capture for lambdas                             | 3    | Very High| Depends on T2-1            |

_* T1-4 depends on T1-3 (callable fn refs)._

---

## Completed Work

All items listed here are fully fixed and tested.

| ID    | Title                                              | Fixed      |
|-------|----------------------------------------------------|------------|
| T0-2  | Borrowed-ref pre-init incomplete (runtime crash)   | 2026-03-13 |
| T0-3  | Polymorphic text methods on struct-enum → SIGSEGV  | 2026-03-13 |
| T0-4  | Vector append in ref-param function silently lost  | 2026-03-13 |
| T0-5  | Direct call to unimplemented enum variant panics   | 2026-03-13 |
| T1-1  | Non-zero exit code on parse / runtime error        | 2026-03-14 |
| T1-7  | Null-coalescing operator `??`                      | 2026-03-14 |
| T1-8  | For-loop mutation guard extended to field access   | 2026-03-14 |
| T1-x  | Reverse iteration on sorted<T> panics              | 2026-03-14 |
| T1-x  | CLI args not accessible in fn main                 | 2026-03-13 |
| T1-x  | Zero-pad format places pad before sign             | 2026-03-13 |
| T1-x  | validate_slots false positive (different-name vars)| 2026-03-13 |
| T1-x  | Enum comparison operators undocumented             | 2026-03-13 |
| T1-x  | Bitwise ^, |, & treat zero as null (XOR null bug)  | 2026-03-13 |
| T1-x  | Missing polymorphic method panics compiler         | 2026-03-13 |
| T1-x  | for c in enum_vector loops forever                 | 2026-03-13 |
| T2-x  | Worker clone full bytecode on every parallel_for   | 2026-03-14 |
| T2-x  | store.claim() is O(n) linear scan → LLRB tree      | 2026-03-14 |
| T3-x  | Dead Option-B helpers in variables.rs              | 2026-03-13 |

---

## See also
- [PROBLEMS.md](PROBLEMS.md) — Known bugs and workarounds
- [INCONSISTENCIES.md](INCONSISTENCIES.md) — Language design asymmetries and surprises
- [ASSIGNMENT.md](ASSIGNMENT.md) — Stack slot assignment status (T3-1 detail)
- [THREADING.md](THREADING.md) — Parallel for-loop design (T3-2 detail)
- [LOGGER.md](LOGGER.md) — Logger design (T3-3 detail)
