# Testing Framework

## Overview

The Dryopea test suite has two distinct layers:

1. **Interpreter tests** (`tests/*.rs`) — Rust integration tests that parse and run loft code through the full compiler pipeline, validating results, errors, and warnings at the interpreter level.
2. **Generated Rust tests** (`tests/generated/*.rs`) — self-contained Rust files emitted by the interpreter tests (debug builds only) that replay the same logic through the compiled code generator, validating the generated Rust output.

Both layers share a common structure: the interpreter tests drive everything, and the generated tests are a by-product of running them.

---

## Entry Points

### `tests/*.rs` — interpreter test files

Each file is a Cargo integration test (auto-discovered because it lives directly in `tests/`). The test files are:

| File | Contents |
|---|---|
| `expressions.rs` | Arithmetic, control flow, type conversions, loops |
| `enums.rs` | Enum definitions, polymorphism, JSON formatting |
| `strings.rs` | String operations, slicing, formatting |
| `objects.rs` | Struct creation, field access, methods, references |
| `vectors.rs` | Vector/sorted/index/hash operations |
| `formatting.rs` | Format specifiers for all types |
| `math.rs` | Math functions, constants, trigonometry |
| `sizes.rs` | `sizeof` expressions and struct layout |
| `data_structures.rs` | Combined data structure behaviour |
| `parse_errors.rs` | Tests that expect specific parse/type errors |
| `slot_assign.rs` | Stack-slot assignment correctness (no overlapping slots) |
| `log_config.rs` | Unit tests for the `LogConfig` debug-logging framework |
| `expressions_auto_convert.rs` | Auto-conversion edge cases (hand-written) |
| `wrap.rs` | Runs `.loft` files from `tests/suite/`; generates HTML docs |
| `testing.rs` | The framework itself; not a runnable test target |

Each file includes `mod testing;` which pulls in `tests/testing.rs` as a module.

---

## The Testing Framework (`tests/testing.rs`)

### Macros

```rust
code!("loft source code")   // parse and run a block of loft code
expr!("loft expression")    // shorthand: wraps the expression in a test() fn
```

Both macros call into `testing_code` / `testing_expr`, which construct a `Test` struct and capture the Rust function name via `stdext::function_name!()`. The function name is parsed to extract:

- **`self.name`** — the short function name (e.g. `define_enum`)
- **`self.file`** — the containing module name (e.g. `enums`)

These two strings determine where the generated test file is written.

### The `Test` struct

```rust
pub struct Test {
    name: String,         // short test name
    file: String,         // module / file name
    expr: String,         // loft expression to evaluate
    code: String,         // loft code block (may be empty)
    warnings: Vec<String>,
    errors: Vec<String>,
    fatal: Vec<String>,
    sizes: HashMap<String, u32>,
    result: Value,        // expected interpreter result
    tp: Type,             // expected type (when needed)
}
```

### Builder methods

Tests are configured with a fluent builder API before the `Test` is dropped:

| Method | Purpose |
|---|---|
| `.result(Value::...)` | Assert the `test()` function returns this value |
| `.tp(Type::...)` | Override the inferred result type (needed for booleans, enums) |
| `.expr("...")` | Set the loft expression (shorthand for a `test()` routine) |
| `.error("...")` | Expect a specific parse/type error (repeatable) |
| `.fatal("...")` | Expect a fatal parse error |
| `.warning("...")` | Expect a specific warning (repeatable) |

### Execution model — `Drop`

**All test logic runs inside `impl Drop for Test`.** There is no explicit `.run()` call; the test executes automatically when the `Test` value goes out of scope at the end of the `#[test]` function.

The `drop` implementation:

1. Constructs a `Parser` and loads the default library from `default/`.
2. Appends a synthesised `test()` function (see below) when `.expr()` or `.result()` was set.
3. Parses the combined loft source via `p.parse_str(...)`.
4. Validates struct sizes against any `.sizes` entries.
5. Runs `scopes::check` (scope/type analysis).
6. **Debug builds only:** calls `generate_code` (writes `tests/generated/`).
7. Calls `assert_diagnostics` — panics if the actual warnings/errors do not exactly match the expected set.
8. If parsing succeeded: runs `byte_code` + `state.execute("test", ...)`.
9. **Debug builds only:** logs bytecode and execution trace to `tests/code/<file>_<name>.txt`.

### Synthesised `test()` function

When `.expr("...")` and `.result(...)` are both set, the framework generates a loft snippet:

```loft
pub fn test() {
    test_value = { <expr> };
    assert(
        test_value == <result>,
        "Test failed {test_value} != <result>"
    );
}
```

When `.result()` is `Value::Null` with a non-unknown type (i.e. testing that the expression returns null), it generates:

```loft
pub fn test() {
    <expr>;
}
```

---

## Generated Test Files (`tests/generated/`)

Generated files are written only in **debug builds** (`#[cfg(debug_assertions)]`). They are produced inside `Test::generate_code`, called from `Drop::drop`.

### `tests/generated/default.rs`

Written on every test execution (overwritten each time). Contains the compiled Rust representation of the default library only — everything up to `start` (the definition count before the test's own loft code was parsed). This file has no `#[test]` function; it serves as a reference snapshot of the default-library schema.

### `tests/generated/<file>_<name>.rs`

Written only when a test has a non-null `.result` or a non-unknown `.tp` (i.e., tests that validate output). The file name is `<file>_<name>.rs` where `<file>` is the Rust module name and `<name>` is the test function name.

For example, the test:
```rust
// in tests/enums.rs
#[test]
fn define_enum() {
    code!("enum Code { ... }")
        .expr("...")
        .result(Value::str("..."));
}
```
produces `tests/generated/enums_define_enum.rs`.

### Structure of a generated file

```rust
#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::Stores;
use dryopea::keys::{DbRef, Str, Key, Content};
use dryopea::external;
use dryopea::external::*;
use dryopea::vector;

fn init(db: &mut Stores) {
    // Registers all types from the default library + the test's own types.
    // Enumerations via db.enumerate / db.value.
    // Structs via db.structure / db.field.
    // Ends with db.finish().
    ...
}

fn n_test(stores: &mut Stores) { ... }  // generated Rust translation of the test's loft code

// Additional generated functions for each loft function defined in the test.

#[test]
fn code_<name>() {
    let mut stores = Stores::new();
    init(&mut stores);
    n_test(&mut stores);
}
```

The `init` function reconstructs the full type schema — both default-library types and any types added by the test — so each generated file is a fully self-contained Rust integration test.

---

## Additional Output Files

### `tests/code/<file>_<name>.txt` (debug builds only)

Written by `Test::output_code`. The content is controlled by a `LogConfig` value
selected at test time (see [LogConfig — Debug Logging Framework](#logconfig--debug-logging-framework) below).

Default content (preset `full`):

- The raw loft source code for the test.
- All type definitions introduced by the test (types beyond those in the default library).
- IR (intermediate representation) for each non-default function.
- Bytecode disassembly with slot annotations (`var=name[slot]:type`).
- The execution trace with variable-name annotations on stack-access steps.

Set the `LOFT_LOG` environment variable before running tests to select a different preset.

These files are useful for debugging compiler output and are not committed.

---

## LogConfig — Debug Logging Framework

`src/log_config.rs` provides structured control over what appears in the
`tests/code/*.txt` files and in the interpreter's execution trace.

### Selecting a preset at test time

Set the `LOFT_LOG` environment variable before `cargo test`:

```bash
LOFT_LOG=minimal   cargo test --test expressions expr_add   # execution only
LOFT_LOG=static    cargo test --test objects                 # IR + bytecode, no execution
LOFT_LOG=ref_debug cargo test --test objects reference       # snapshots on Ref ops
LOFT_LOG=bridging  cargo test --test expressions             # bridging invariant warnings
LOFT_LOG=crash_tail:20 cargo test --test vectors             # last 20 execution lines
LOFT_LOG=fn:helper cargo test --test expressions             # one function only
LOFT_LOG=variables cargo test --test slot_assign             # variable table per function
```

| `LOFT_LOG` value | Preset | Description |
|---|---|---|
| `full` *(default)* | `LogConfig::full()` | IR + bytecode + execution, slot annotations |
| `static` | `LogConfig::static_only()` | IR + bytecode; no execution trace |
| `minimal` | `LogConfig::minimal()` | Execution for `test` only; no IR/bytecode |
| `ref_debug` | `LogConfig::ref_debug()` | Full + stack snapshots on Ref/CreateStack ops |
| `bridging` | `LogConfig::bridging()` | Execution + bridging-invariant check |
| `crash_tail` or `crash_tail:N` | `LogConfig::crash_tail(N)` | Last N execution lines; flushed on panic |
| `fn:<name>` | `LogConfig::function(name)` | Only the named function |
| `variables` | `LogConfig::variables()` | IR + bytecode + variable table per function (no execution) |

The `variables` preset appends a table after each function's bytecode showing every variable's
name, short type, scope number, stack-slot range `[start, end)`, and live interval `[first_def, last_use]`.
Arguments are marked with `arg`.  Variables that have no slot yet (`stack_pos == u16::MAX`) or
that were never defined still appear so the full picture is visible.  Example:

```
variables for myfile:fn n_find_max(nodes:vector<ref(Node)>) -> integer
  #    arg  name                 type           scope  slot         live
  ----------------------------------------------------------------------
  0    arg  nodes                vec<ref(382)>  0      [0, 12)      -
  1         best                 int            1      [16, 20)     [6, 32]
  2         _vector_1            vec<ref(382)>  2      [20, 32)     [8, 15]
  3         n#index              int            2      [32, 36)     [10, 17]
  4         n                    ref(382)       3      [36, 48)     [19, 28]
```

### `LogConfig` struct

```rust
pub struct LogConfig {
    /// Which phases to include in the output.
    pub phases: LogPhase,           // { ir: bool, bytecode: bool, execution: bool }

    /// Only log IR/bytecode/execution for functions whose name contains one
    /// of these strings.  None = all functions.
    pub show_functions: Option<Vec<String>>,

    /// Only include execution steps whose opcode name (without Op prefix)
    /// contains one of these strings.  None = all opcodes.
    pub trace_opcodes: Option<Vec<String>>,

    /// Keep only the last N lines of the execution trace.  On panic the
    /// buffer is flushed before re-raising.  None = unlimited.
    pub trace_tail: Option<usize>,

    /// Append var=name[slot]:type to bytecode and =varname to execution steps.
    pub annotate_slots: bool,

    /// Capture a stack snapshot after every opcode whose name contains one
    /// of these strings.  None = never snapshot.
    pub snapshot_opcodes: Option<Vec<String>>,

    /// Number of bytes to print per snapshot.
    pub snapshot_window: usize,

    /// Warn when runtime stack_pos deviates from compile-time expected value.
    pub check_bridging: bool,

    /// Print the variable table (name, type, scope, slot, live interval) after
    /// each function's bytecode.  Enabled by the `variables` preset.
    pub show_variables: bool,
}
```

### Building a custom config

```rust
use dryopea::log_config::{LogConfig, LogPhase};

let config = LogConfig {
    phases: LogPhase::execution_only(),
    trace_opcodes: Some(vec!["Call".to_string(), "Return".to_string()]),
    annotate_slots: true,
    ..LogConfig::full()
};
```

### Key implementation files

| File | Role |
|---|---|
| `src/log_config.rs` | `LogConfig`, `LogPhase`, `TailBuffer` definitions and presets |
| `src/interpreter.rs` | `show_code(writer, state, data, config)` — static IR + bytecode output |
| `src/state.rs` | `execute_log(log, name, config, data)` — execution trace with all filters |
| `src/state.rs` | `dump_code(f, d_nr, data, annotate_slots)` — per-function bytecode dump |
| `tests/testing.rs` | Creates config via `LogConfig::from_env()`, passes to `show_code` + `execute_log` |
| `tests/wrap.rs` | Same: `LogConfig::from_env()` for suite file tests |
| `tests/log_config.rs` | Unit tests covering all filters, presets, and pipeline integration |

### Notes for Claude

- `src/main.rs` re-declares `mod log_config;` because it re-includes all source modules
  directly rather than importing from the library crate.
- The bridging check (`check_bridging: true`) will always report a violation on the
  FIRST instruction of the root test function because `execute_log` places the sentinel
  return address at runtime position 4–7 while compile-time tracking starts at 0.
  This is a known harmless offset, not a real bug.
- `crash_tail` mode wraps the execution loop in `catch_unwind(AssertUnwindSafe(...))`;
  if a panic occurs the tail buffer is flushed to the log file before re-raising.

---

## `tests/suite/` — end-to-end loft files

Managed by `tests/wrap.rs`. These are standalone `.loft` programs that exercise the full compiler and interpreter pipeline. They are not connected to the `Test` builder API.

The `dir` test runs all files in alphabetical order and also regenerates HTML documentation in `doc/` from the source comments. The `last` test runs only the final file for fast iteration.

---

## File Layout Summary

```
tests/
  testing.rs              # Framework: Test struct, macros, Drop impl, generate_code
  expressions.rs          # Interpreter tests: expressions, control flow
  enums.rs                # Interpreter tests: enums
  strings.rs              # Interpreter tests: string operations
  objects.rs              # Interpreter tests: structs / methods
  vectors.rs              # Interpreter tests: vector / sorted / hash
  formatting.rs           # Interpreter tests: format specifiers
  math.rs                 # Interpreter tests: math functions
  sizes.rs                # Interpreter tests: struct sizes / sizeof
  data_structures.rs      # Interpreter tests: combined data structures
  parse_errors.rs         # Interpreter tests: expected parser errors
  expressions_auto_convert.rs  # Hand-written generated-style test (pre-generator)
  wrap.rs                 # Suite runner + HTML doc generator
  suite/
    01-keywords.loft ... 16-parser.loft   # End-to-end loft programs
  generated/
    default.rs            # Default-library schema snapshot (no #[test])
    <file>_<name>.rs      # One file per result-bearing interpreter test
  code/
    <file>_<name>.txt     # Bytecode + trace dumps (debug, not committed)
```

---

## Running the Tests

```bash
# Run all interpreter tests (generates tests/generated/ as a side effect):
cargo test

# Run a specific interpreter test file:
cargo test --test enums

# Run a specific test function:
cargo test --test enums define_enum

# Run only suite file tests:
cargo test --test wrap

# Full test cycle including generated tests (see Makefile):
make test
```

`make test` runs the `clippy` target first (which runs `cargo clippy`, `rustfmt`, and `cargo run --bin gendoc` to regenerate HTML docs), then:

1. Deletes all files in `tests/generated/` and `tests/result/`.
2. Runs `cargo test -- --nocapture --test-threads=1`, appending output to `result.txt`.

---

## Validating Generated Code — the `generated/` Workspace

Two directories:
- `tests/generated/` — ephemeral output from interpreter tests (158+ files, cleared by `make test`)
- `generated/tests/` — committed reviewed subset; standalone Cargo workspace with `dryopea = { path = ".." }`

| Target | Purpose |
|---|---|
| `make generate` | `meld tests/generated/ generated/tests/` — review and copy approved files into the committed corpus |
| `make gtest` | `cargo clippy --tests`, `rustfmt`, `cargo test` inside `generated/` — lint, format-check, and run all promoted tests |
| `make meld` | Compare `tests/generated/text.rs` and `fill.rs` against `src/` counterparts; open meld if they differ |

```
cargo test (debug)
  └─► tests/generated/*.rs   (158+ files, ephemeral)
        │
        ▼  make generate  (meld review)
        │
        ▼  generated/tests/*.rs  (committed, reviewed subset)
              │
              ▼  make gtest
                   clippy → rustfmt → cargo test  (inside generated/ workspace)
```

---

## Key Constraints

- **Generated tests are debug-only.** `generate_code` and `output_code` are guarded by `#[cfg(debug_assertions)]`. Release builds (`cargo test --release`) skip file generation entirely.
- **`default.rs` has no `#[test]` function** and is excluded from the second-pass Cargo registration.
- **`expressions_auto_convert.rs`** exists as a hand-written `tests/` file from before the generator existed; the corresponding generated file is skipped to avoid a Cargo name collision.
- **Test execution order within a file** is non-deterministic (Cargo runs tests in parallel by default). `make test` passes `--test-threads=1` to force sequential execution and capture output deterministically into `result.txt`.

---

## `tests/loft/` — standalone loft test suite

A second, independent test suite that runs `.loft` files directly through the `lavition` binary.
**No `tests/code/*.txt` files and no generated Rust code are produced** — the lavition binary never
writes debug output.

```
tests/loft/
  01-integers.loft    arithmetic, bitwise, null, type conversions
  02-floats.loft      float/single arithmetic, math functions, null (NaN)
  03-text.loft        concatenation, len, indexing, slicing, UTF-8 iteration, search
  04-booleans.loft    logical ops, short-circuit, null truthiness
  05-control-flow.loft  if/else, for loops, ranges, break, named break, loop metadata
  06-functions.loft   default args, reference params, early return, recursion
  07-structs.loft     constructors, methods, virtual fields, JSON/format, vectors of structs
  08-enums.loft       plain enums, struct-enum variants, polymorphic dispatch
  09-vectors.loft     literals, append, slice, iteration, removal, #index/#first/#count
  10-collections.loft sorted, index, hash — lookup, ordered iteration, range queries
  11-files.loft       text + binary file I/O (u8/u16/i32/long/single/float/text/vector),
                      seek, #size set, move/delete, path safety
  12-formatting.loft  format specifiers: integers, floats, booleans, text, long, single, vectors
```

Run with:

```bash
make loft-test          # build lavition (release) then run every file
./target/release/lavition tests/loft/07-structs.loft   # run one file
```

Each file has a `fn main()` that calls `assert(condition, message)` for every case.
A failing assert panics and prints the message, naming the failed test.

---

## Debugging failures in `tests/loft/`

### Strategy overview

When `make loft-test` reports a failure, work from the outside in:

1. **Run the failing file directly** — the panic message names the exact assert.
2. **Narrow to the failing assert** — comment out asserts below the first failure to isolate it.
3. **Print intermediate values** — add `print("{var}")` before the assert to see the actual value.
4. **Run via the Rust test framework** — convert the minimal case to `expr!(...)` in a `tests/*.rs`
   file; this enables `LOFT_LOG` debug output without modifying the source.
5. **Use the debug binary** — `cargo build --bin lavition` produces a binary with extra runtime
   checks; segfaults often produce clearer output or trigger a Rust panic instead.

### Failure types and fixes

#### Assert fires with wrong value

```
panicked at src/fill.rs:1772:5: my assert message
```

The message is whatever string was passed as the second argument to `assert()`.
Add `print("{actual}")` directly before the failing assert to see the actual value.
Common causes:
- Off-by-one in an expected range or loop count — trace manually.
- Floating-point rounding — use `round()` before comparing or widen the tolerance.
- Format output differs from expected — print both sides and compare byte-by-byte.

#### Segfault (no output)

```
Segmentation fault (core dumped)
```

The interpreter hit an unguarded memory access.  Run the debug binary for a Rust panic
instead of a silent crash:

```bash
cargo build --bin lavition          # debug build, slower but safer
./target/debug/lavition tests/loft/08-enums.loft
RUST_BACKTRACE=1 ./target/debug/lavition tests/loft/08-enums.loft
```

Common causes:
- Calling a feature that is not yet implemented (e.g. `enum_value as integer`,
  unimplemented stdlib method) — the interpreter falls through to an unreachable branch.
- Passing a wrong type where the runtime expects a specific layout (e.g. a struct-enum
  variant used as a plain enum).
- Remove the suspect line; if the segfault disappears, the line triggers the bug.

#### Parse error — "Dual definition of"

```
Dual definition of <name> at file.loft:line:col
```

A name is defined twice in the same scope.  Common triggers:

- **Nested format string with escaped quotes**: `"outer {\"inner\"}"` — the lexer may
  treat `\"` as ending the outer string, leaving a bare word that re-opens a definition.
  Fix: build the inner string in a variable first, then interpolate the variable.
  ```loft
  inner = "{c#index}:{c}";   // build separately
  result += inner;            // then use in format
  ```
- **Two struct definitions with the same field name in the same file**: when two structs
  both have a field called e.g. `key`, the parser may confuse their field numbers during
  type resolution.  Rename one field or split the structs into separate test files.
- **Re-declaring a function with identical parameter types**: loft allows overloading by
  type; identical signatures are an error.

#### Parse error — "Undefined type"

A type name appears before its `struct`/`enum` definition.  Move the definition above its
first use, or above any function that references it.

#### Wrong result from index range query

If a range query like `db.map[83..92, "Two"]` returns unexpected elements, the most likely
cause is a **field-offset conflict**: two structs defined in the same file share a field
name at different positions.  For example:

```loft
struct A { key: text }           // key is field 0
struct B { nr: integer, key: text }  // key is field 1
```

When both `sorted<A[-key]>` and `index<B[nr,-key]>` exist in the same file, the compiler
may resolve `key` to the wrong field number for one of the lookups.

Fix: use distinct field names, or place conflicting struct definitions in separate test files.

#### Wrong iteration order in sorted/index

Verify the sort direction: `-field` means **descending**, `field` means **ascending**.
A mismatch between the declared direction and the expected order is the most common mistake.
Trace the expected element sequence manually before writing the assert.
