# Internal Implementation Modules

This document covers source files that are part of the runtime and build infrastructure but are not directly part of the compiler pipeline or standard library API.

---

## Field Layout Calculator (`src/calc.rs`)

### `calculate_positions`

```rust
pub fn calculate_positions(
    fields: &[(u16, u8)],  // (field_size_bytes, alignment)
    sub: bool,             // true for EnumValue variants (reserves byte 0 for the discriminant)
    size: &mut u16,        // updated to the total record size in bytes
    alignment: &mut u8,    // updated to the largest alignment requirement seen
) -> Vec<u16>              // byte offset for each field, in field order
```

Computes the byte offset of each field in a struct or enum-variant record. The algorithm:

1. If `sub = true` (enum variant), reserves bytes 0–7 for the discriminant (position 0 is fixed at byte 0).
2. Iterates fields in descending alignment order (8 → 4 → 2 → 1). For each field it first tries to fit the field into an existing alignment gap; otherwise appends it at the current end of the record.
3. Maintains a `BTreeMap<pos, size>` of free gaps. When a gap is consumed exactly it is removed; when it is larger the back portion is taken and the remainder stays.
4. Updates `*size` and `*alignment` as fields are placed.

Called by `typedef::fill_database` during type schema registration to assign offsets before `database.finish()` seals the schema.

---

## Bytecode Generation Stack (`src/stack.rs`)

### `Loop`

```rust
pub struct Loop {
    start: u32,       // bytecode position of the loop's begin opcode
    stack: u16,       // stack depth at loop entry (used to compute free_stack amounts)
    breaks: Vec<u32>, // bytecode positions of pending goto instructions for break
}
```

One entry per active loop nesting level. `break` statements append their forward-jump addresses to `breaks`; `end_loop` patches them all to the position after the loop.

### `Stack<'a>`

```rust
pub struct Stack<'a> {
    pub position: u16,      // current operand stack depth in bytes
    pub data: &'a Data,     // read-only reference to all definitions
    pub function: Function, // variable table for the function being compiled
    pub def_nr: u32,        // definition number of the function being compiled
    pub logging: bool,
    loops: Vec<Loop>,
}
```

Tracks the bytecode operand stack depth throughout code emission. Updated after every operator call via `operator()`, which pops parameter bytes and pushes the return type bytes.

### Key Methods

| Method | Description |
|---|---|
| `new(function, data, def_nr, logging)` | Construct a fresh stack frame |
| `size_code(val) -> u16` | Compute how many stack bytes a `Value` node produces |
| `operator(d_nr)` | Update `position` for a call to operator `d_nr` (pop params, push return) |
| `add_op(name, state)` | Look up operator by name, emit its opcode byte, call `operator` |
| `add_loop(code_pos)` | Push a new `Loop` frame with the current bytecode position |
| `end_loop(state)` | Pop the `Loop` frame; patch all pending `break` gotos |
| `add_break(code_pos, loop_nr)` | Register a `break` goto in the `loop_nr`-th enclosing loop |
| `get_loop(loop_nr) -> u32` | Return the start bytecode position of the `loop_nr`-th enclosing loop |
| `loop_position(loop_nr) -> u16` | Return the stack depth at the start of the `loop_nr`-th loop |

---

## Rust Code Generator (`src/create.rs`)

These functions generate Rust source files from a compiled loft program. They are used during development to produce the checked-in `src/fill.rs` and `src/external.rs` from the `#rust "..."` annotations in the default library.

### `generate_lib`

```rust
pub fn generate_lib(data: &Data) -> std::io::Result<()>
```

Writes `tests/generated/text.rs`. This file contains:
- A `FUNCTIONS` constant: a list of `(&str, Call)` pairs for every default-library function that has a `#rust` body and is not an operator.
- An `init(state)` function that registers all functions into the interpreter's static function table.
- One Rust function per entry, with parameters popped from the stack via `stores.get::<T>(stack)` and the result pushed via `stores.put(stack, new_value)`.

Parameter names in `#rust` bodies are written as `@param_name`; `replace_attributes` substitutes these with `v_param_name` (and `.str()` for `Text` parameters).

### `generate_code`

```rust
pub fn generate_code(data: &Data) -> std::io::Result<()>
```

Writes `tests/generated/fill.rs`. This file contains:
- An `OPERATORS` constant: an array of `fn(&mut State)` function pointers for every operator in definition order.
- One Rust function per operator. The function name is derived from the `OpXxx` loft name: strip the leading `Op`, convert `CamelCase` to `snake_case` (e.g. `OpAddInt` → `add_int`). The special case `return` is renamed to `op_return` to avoid a Rust keyword conflict.
- Non-mutable parameters are read from inline bytecode (`s.code::<T>()`); mutable parameters are popped from the stack (`s.get_stack::<T>()`).

### Operator Name Conversion

The private `operator_name(operator)` function strips the `Op` prefix and converts CamelCase to snake_case:

```
OpAddInt      → add_int
OpConvLongFromInt → conv_long_from_int
OpReturn      → op_return   (special case: "return" is a Rust keyword)
```

---

## Native Function Registry (`src/external.rs`)

`external.rs` is the hand-written Rust implementation of standard library functions that cannot be expressed in loft itself (OS interaction, string operations, etc.).

### Function Naming Convention

The `FUNCTIONS` array pairs each loft definition name with its Rust implementation:

| Pattern | Meaning | Example |
|---|---|---|
| `n_<function>` | Global function with no receiver | `n_assert`, `n_arguments` |
| `t_<N><Type>_<method>` | Method on a type; `N` = length of the type name | `t_4text_starts_with` ("text" = 4 chars), `t_9character_is_lowercase` ("character" = 9 chars), `t_4File_write` ("File" = 4 chars) |

### `init`

```rust
pub fn init(state: &mut State)
```

Registers all entries from `FUNCTIONS` into the interpreter's static function table via `state.static_fn`. Called once at startup after bytecode generation.

### Implemented Functions

| Name | Loft API |
|---|---|
| `n_assert` | `assert(test, message)` |
| `n_panic` | `panic(message)` |
| `t_4File_write` | `file.write(v)` |
| `n_env_variables` | `env_variables()` |
| `n_env_variable` | `env_variable(name)` |
| `t_4text_starts_with` | `text.starts_with(value)` |
| `t_4text_ends_with` | `text.ends_with(value)` |
| `t_4text_trim` | `text.trim()` |
| `t_4text_trim_start` | `text.trim_start()` |
| `t_4text_trim_end` | `text.trim_end()` |
| `t_4text_find` | `text.find(value)` |
| `t_4text_rfind` | `text.rfind(value)` |
| `t_4text_contains` | `text.contains(value)` |
| `t_4text_replace` | `text.replace(value, with)` |
| `t_4text_to_lowercase` | `text.to_lowercase()` |
| `t_4text_to_uppercase` | `text.to_uppercase()` |
| `t_4text_is_lowercase` | `text.is_lowercase()` |
| `t_9character_is_lowercase` | `character.is_lowercase()` |
| `t_4text_is_uppercase` | `text.is_uppercase()` |
| `t_9character_is_uppercase` | `character.is_uppercase()` |
| `t_4text_is_numeric` | `text.is_numeric()` |
| `t_9character_is_numeric` | `character.is_numeric()` |
| `t_4text_is_alphanumeric` | `text.is_alphanumeric()` |
| `t_9character_is_alphanumeric` | `character.is_alphanumeric()` |
| `t_4text_is_alphabetic` | `text.is_alphabetic()` |
| `t_9character_is_alphabetic` | `character.is_alphabetic()` |
| `t_4text_is_whitespace` | `text.is_whitespace()` |
| `t_4text_is_control` | `text.is_control()` |
| `n_arguments` | `arguments()` |
| `n_directory` | `directory(v)` |
| `n_user_directory` | `user_directory(v)` |
| `n_program_directory` | `program_directory(v)` |

The `find` and `rfind` functions return `i32::MIN` (the null sentinel) when the substring is not found.

---

## Text and Formatting Utilities (`src/text.rs`)

Low-level string helpers used by `src/fill.rs` operator implementations and `src/external.rs`. All functions operate on UTF-8 byte slices directly; they adjust slice boundaries to avoid splitting multi-byte code points.

### String Slicing

| Function | Description |
|---|---|
| `text_character(val, from) -> char` | Return the character at byte position `from` (negative = from end). Backs up into a multi-byte sequence if needed. Returns `char::from(0)` when out of range. |
| `sub_text(val, from, till) -> &str` | Zero-copy substring from byte `from` to `till`. Negative indices are relative to end. `till = i32::MIN` means a single character. Adjusts both boundaries outward to respect UTF-8 character boundaries. |
| `fix_from(from, s) -> usize` | Resolve a possibly-negative `from` index and back it up to a UTF-8 character boundary. |
| `fix_till(till, from, s) -> usize` | Resolve a possibly-negative `till` index and advance it past any UTF-8 continuation bytes. |
| `to_char(val) -> char` | Convert an `i32` code point to `char` (unchecked; used internally for known-valid code points). |

### Text Formatting

| Function | Description |
|---|---|
| `format_text(s, val, width, dir, token)` | Append `val` to `s` padded to `width` characters with `token` byte. `dir < 0` = left-align, `dir > 0` = right-align, `dir == 0` = centre. Padding counts Unicode characters, not bytes. |
| `format_int(s, val, radix, width, token, plus, note)` | Format `i32` into `s`. `radix` is 2/8/10/16. `plus` prepends `+` for positive decimals. `note` prepends `0b`/`0o`/`0x`. Null sentinel `i32::MIN` is formatted as `"null"`. |
| `format_long(s, val, radix, width, token, plus, note)` | Same as `format_int` but for `i64`. |
| `format_float(s, val, width, precision)` | Format `f64` using Rust's `{:w$.p$}` format. |
| `format_single(s, val, width, precision)` | Format `f32` using Rust's `{:w$.p$}` format. |

### Null-Sentinel Arithmetic (`src/text.rs`)

The loft runtime represents null for integers as `i32::MIN` and for longs as `i64::MIN`. All arithmetic functions propagate null: if either operand is the null sentinel, the result is also null.

Functions follow the naming pattern `op_<operation>_<type>`:

| Group | Functions |
|---|---|
| Integer arithmetic | `op_add_int`, `op_min_int`, `op_mul_int`, `op_div_int`, `op_rem_int` |
| Integer bitwise | `op_logical_and_int`, `op_logical_or_int`, `op_exclusive_or_int`, `op_shift_left_int`, `op_shift_right_int` |
| Integer unary | `op_abs_int`, `op_min_single_int` |
| Long arithmetic | `op_add_long`, `op_min_long`, `op_mul_long`, `op_div_long`, `op_rem_long` |
| Long bitwise | `op_logical_and_long`, `op_logical_or_long`, `op_exclusive_or_long`, `op_shift_left_long`, `op_shift_right_long` |
| Long unary | `op_abs_long`, `op_min_single_long` |
| Integer conversions | `op_conv_long_from_int`, `op_conv_float_from_int`, `op_conv_single_from_int`, `op_conv_bool_from_int`, `op_conv_bool_from_character` |
| Long conversions | `op_conv_float_from_long`, `op_conv_bool_from_long` |
| Cast conversions | `op_cast_int_from_long`, `op_cast_int_from_single`, `op_cast_long_from_single`, `op_cast_int_from_float`, `op_cast_long_from_float` |

`NaN` is used as the null sentinel for floating-point values; conversion functions check `is_nan()` and return `i32::MIN` / `i64::MIN` accordingly.

---

## PNG Image Loading (`src/png_store.rs`)

### `read`

```rust
pub fn read(file_path: &str, store: &mut Store) -> std::io::Result<(u32, u32, u32)>
//                                                                   img_rec  width  height
```

Decodes a PNG file directly into a `Store` allocation:

1. Opens `file_path` with a buffered reader.
2. Creates a `png::Decoder` and reads the image info header.
3. Claims a store record large enough for the decoded pixel buffer (`output_buffer_size / 8 + 1` words).
4. Decodes the first frame into the store record's raw buffer via `store.buffer(img)`.
5. Returns `(img_rec, width, height)` — `img_rec` is the word offset of the record in the store and is used by the `Pixel` accessors in the interpreter.

Called by the `png()` stdlib function in the default library when a `File` is decoded to an `Image`.

---

## Radix Tree (`src/radix_tree.rs`)

**Status: partially implemented.** The insert, first, last, and find operations are functional; the iteration `next` method and the `remove` and `optimize` helpers are stubs. The module is gated with `#![allow(dead_code)]`.

The radix tree is the planned backing structure for the `Spacial` collection type (`Parts::Spacial` in `database.rs`). It provides a compact, bit-indexed spatial index over arbitrary record keys.

### Record Layout

A tree record in a `Store` has the following fixed-offset fields:

| Offset constant | Meaning |
|---|---|
| `RAD_TOP` (4) | Root node reference (positive = leaf record, negative = internal node index) |
| `RAD_SIZE` (8) | Number of records currently stored |
| `RAD_BITS` (12) | Word offset of the companion bits record |
| `RAD_FALSE` (16) | Start of the false-branch child array (8 bytes per node) |
| `RAD_TRUE` (20) | Start of the true-branch child array (8 bytes per node) |

A separate bits record holds one byte per internal node, encoding how many key bits that node skips (path compression).

### `RadixIter`

```rust
pub struct RadixIter {
    positions: [i32; 64], // traversal path; negative = came from false-branch
    depth: i32,           // current depth in positions[]
    rec: u32,             // current leaf record reference
}
```

Returned by `rtree_first`, `rtree_last`, and `rtree_find`. The last element of `positions` up to `depth-1` is the node path; `rec` is the found leaf.

### Public Functions

| Function | Description |
|---|---|
| `rtree_init(store, initial) -> u32` | Allocate and initialise a tree record + bits record; returns the tree record offset |
| `rtree_first(store, tree) -> RadixIter` | Iterator pointing at the lexicographically lowest record |
| `rtree_last(store, tree) -> RadixIter` | Iterator pointing at the highest record |
| `rtree_find(store, tree, key) -> RadixIter` | Iterator pointing at the lowest record matching the key predicate |
| `rtree_insert(store, tree, rec, key)` | Insert record `rec` into the tree using bit-by-bit key function |
| `rtree_validate(store, tree, key)` | Debug: verify element count (test-only) |
| `rtree_optimize(store, tree)` | Rebuild the tree into a fresh store (stub) |

### Key Encoding

The `key` parameter to `rtree_find` and `rtree_insert` is a closure/function `fn(bit: u32) -> bool` (or `Option<bool>` for comparison). Each call returns the value of bit `bit` of the key; `None` signals the key has ended. This allows any bit-decomposable key (integers, coordinates, hashes) without copying the key into a fixed-width buffer.

---

## CLI Binary (`src/main.rs`)

The `lavition` binary is the interpreter entry point. It is separate from the library crate (`src/lib.rs`) and not exposed via `pub`.

### Usage

```
lavition [option] [file]

Options:
  --version       Print version from Cargo.toml
  --path DIR      Override the project directory (default: auto-detected from executable path)
  -h, --help, -? Print usage help
```

### `main` flow

1. Parse command-line arguments. On `--version` or `--help`, print and exit.
2. Determine the project directory via `project_dir()`.
3. Construct a `Parser` and call `parse_dir(dir + "default", true, false)` to load the default library.
4. Call `parse(file_name, false)` to parse the user program.
5. If diagnostics are non-empty, print them and exit.
6. Call `scopes::check` on the parsed data.
7. Construct a `State` from the `Stores` schema.
8. Call `interpreter::byte_code` to compile IR to bytecode.
9. Call `state.execute("main", &data)` to run the program.

### `project_dir`

Auto-detects the project root from the path of the running executable:
- Strips the `lavition` binary name.
- Strips a `target/release/` or `target/debug/` suffix if present.
- The result is the project root, and the default library is expected at `<root>/default/`.
