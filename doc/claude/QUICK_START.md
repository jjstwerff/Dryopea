# Quick Start — Dryopea / lavition Codebase

A compact orientation for starting analysis. Follow the links to the full doc files for detail.

---

## What this project is

**lavition** is a tree-walking interpreter for the **loft** programming language, written in Rust. Loft is a statically typed, expression-oriented language with struct/enum support, a store-based heap, and a standard library loaded from `default/*.loft`.

---

## Running the interpreter

```
cargo run --bin lavition -- myprogram.loft
cargo run --bin lavition -- --help
cargo run --bin gendoc         # regenerate doc/*.html
```

---

## Execution path (start to finish)

```
src/main.rs          CLI entry; loads default/ then user file
  └─ src/parser.rs   Two-pass recursive-descent parser → Value IR
       ├─ src/lexer.rs       Tokeniser
       ├─ src/typedef.rs     Type resolution + field offsets
       ├─ src/variables.rs   Per-function variable table
       └─ src/scopes.rs      Scope/lifetime analysis
  └─ src/interpreter.rs  Compiles IR → flat bytecode
  └─ src/state.rs        Executes bytecode
       └─ src/fill.rs     248 opcode implementations
```

---

## Key data structures

| Type | File | Purpose |
|---|---|---|
| `Value` (enum) | `src/data.rs` | IR tree node |
| `Type` (enum) | `src/data.rs` | Static type of a `Value` |
| `Data` | `src/data.rs` | Table of all named definitions |
| `State` | `src/state.rs` | Bytecode stream + runtime stack |
| `Stores` | `src/database.rs` | All stores + type schema |
| `Store` | `src/store.rs` | Raw word-addressed heap |
| `DbRef` | `src/keys.rs` | Universal pointer: (store_nr, rec, pos) |

---

## Null sentinels

| Type | Null value |
|---|---|
| `integer` | `i32::MIN` |
| `long` | `i64::MIN` |
| `float` | `f64::NAN` |
| `boolean` | n/a (always non-null) |
| references | `store_nr == 0 && rec == 0` |

---

## Important conventions

- User functions are stored as `"n_<name>"` (not bare `"<name>"`). `data.def_nr("foo")` returns `u32::MAX`; use `data.def_nr("n_foo")`.
- Operators have `OpCamelCase` loft names → `op_snake_case` Rust names in `fill.rs`.
- Native stdlib functions in `external.rs` use the naming scheme `n_<func>` (global) or `t_<LEN><Type>_<method>` (method; LEN = number of characters in the type name).
- `#rust "..."` annotations in `default/*.loft` supply the Rust implementation body for code generation.

---

## Default library load order

```
default/01_code.loft    — operators, math, text, collections
default/02_images.loft  — Image, Pixel, File, Format types
default/03_text.loft    — text utilities
```

---

## Full documentation index

| File | Topic |
|---|---|
| `COMPILER.md` | Lexer, parser, two-pass design, IR, type system, scope analysis, bytecode |
| `INTERMEDIATE.md` | Value/Type enums in detail; 248 bytecode operators; State layout |
| `DATABASE.md` | Store allocator, Stores schema, DbRef, vector/tree/hash/radix implementations |
| `STDLIB.md` | Standard library API reference |
| `INTERNALS.md` | calc.rs, stack.rs, create.rs, external.rs, text.rs, png_store.rs, main.rs, radix_tree.rs |
| `DOC.md` | HTML documentation generation (gendoc.rs + documentation.rs) |
| `TESTING.md` | Test framework + `LogConfig` debug-logging presets and `LOFT_LOG` env var |
| `LOFT.md` | Loft language reference |

## Debug logging — `LOFT_LOG` quick reference

Set this env var before `cargo test` to control what appears in `tests/code/*.txt`:

| Value | What you get |
|---|---|
| *(unset)* or `full` | IR + bytecode + execution, slot annotations (default) |
| `static` | IR + bytecode only — fastest for codegen debugging |
| `minimal` | Execution trace for `test` only — cleanest for runtime bugs |
| `ref_debug` | Full + stack snapshots after every Ref/CreateStack op |
| `bridging` | Execution + bridging-invariant warnings |
| `crash_tail:N` | Last N execution lines; flushed on panic |
| `fn:<name>` | Only the named function |

See `TESTING.md § LogConfig` and `src/log_config.rs` for the full API.
