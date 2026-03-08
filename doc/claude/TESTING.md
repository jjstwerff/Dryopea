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

Written by `Test::output_code`. Contains:

- The raw loft source code for the test.
- All type definitions introduced by the test (types beyond those in the default library).
- The full bytecode listing produced by `show_code`.
- The interpreter execution trace (when `execute_log` is used).

These files are useful for debugging compiler output and are not committed.

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

## Key Constraints

- **Generated tests are debug-only.** `generate_code` and `output_code` are guarded by `#[cfg(debug_assertions)]`. Release builds (`cargo test --release`) skip file generation entirely.
- **`default.rs` has no `#[test]` function** and is excluded from the second-pass Cargo registration.
- **`expressions_auto_convert.rs`** exists as a hand-written `tests/` file from before the generator existed; the corresponding generated file is skipped to avoid a Cargo name collision.
- **Test execution order within a file** is non-deterministic (Cargo runs tests in parallel by default). `make test` passes `--test-threads=1` to force sequential execution and capture output deterministically into `result.txt`.
