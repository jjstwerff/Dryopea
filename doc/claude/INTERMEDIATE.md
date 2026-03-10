# Dryopea Intermediate Language (IR) Reference

## Overview

The compiler pipeline is:
```
.loft source -> Parser (Value tree IR) -> State.byte_code() -> bytecode Vec<u8> -> fill::OPERATORS[opcode](&mut State) at runtime
```

The intermediate representation is the `Value` enum tree defined in `src/data.rs`.
Bytecode generation is done in `src/state.rs` via `State`.
The 248 operator functions are in `src/fill.rs`.
Variable/scope tracking during parsing is in `src/variables.rs` via `Function`.

---

## Value Enum (IR Nodes) — `src/data.rs`

```rust
pub enum Value {
    Null,
    Line(u32),                         // Source line annotation
    Int(i32),                          // Integer literal
    Long(i64),                         // 64-bit integer literal
    Single(f32),                       // 32-bit float literal
    Float(f64),                        // 64-bit float literal
    Boolean(bool),                     // Boolean literal
    Enum(u8, u16),                     // Enum variant index + database type id
    Text(String),                      // Text literal
    Call(u32, Vec<Value>),             // Operator/function call: (op_nr, args)
    Block(Box<Block>),                 // Scoped statement sequence
    Insert(Vec<Value>),                // Inline statements (no new scope)
    Var(u16),                          // Read variable at stack position n
    Set(u16, Box<Value>),             // Write variable at stack position n
    Return(Box<Value>),                // Return from function
    Break(u16),                        // Break out of n-th enclosing loop
    Continue(u16),                     // Continue n-th enclosing loop
    If(Box<Value>, Box<Value>, Box<Value>), // cond, then-branch, else-branch
    Loop(Box<Block>),                  // Infinite loop (exit via Break)
    Drop(Box<Value>),                  // Evaluate and discard return value
    Iter(u16, Box<Value>, Box<Value>), // for-loop: (var_nr, init, step)
    Keys(Vec<Key>),                    // Key descriptor for sorted/hash/index
}
```

### Special Var(0)

`Var(0)` is used as a **placeholder** in struct field default expressions to mean
"the current record being initialized." It is replaced with the actual record reference
at object initialization time by `Parser::replace_record_ref()` in `src/parser.rs`.
In `$` expressions inside struct field defaults, `$` maps to `Value::Var(0)`.

### Block

```rust
pub struct Block {
    pub name: &'static str,   // Debug label
    pub operators: Vec<Value>, // Ordered IR nodes
    pub result: Type,          // Return type
    pub scope: u16,            // Scope nesting level
}
```

---

## Type Enum — `src/data.rs`

```rust
pub enum Type {
    Unknown(u32),         // Forward reference placeholder (linked type id or 0)
    Null,                 // No type / null literal
    Void,                 // Function return: no value
    Integer(i32, u32),    // Range-constrained integer (min, max)
    Boolean,
    Long,                 // i64
    Float,                // f64
    Single,               // f32
    Character,            // Single unicode codepoint
    Text(Vec<u16>),       // Text + dependency variable list (for lifetime tracking)
    Keys,                 // Key spec for collection types
    Enum(u32, bool, Vec<u16>),               // def_nr, is_ref, deps
    Reference(u32, Vec<u16>),                // struct def_nr + deps (nullable)
    RefVar(Box<Type>),                       // Mutable reference argument (&T)
    Vector(Box<Type>, Vec<u16>),             // Dynamic array + deps
    Sorted(u32, Vec<(String, bool)>, Vec<u16>), // Ordered set: def_nr, [(field, asc)]
    Index(u32, Vec<(String, bool)>, Vec<u16>),  // Index: def_nr, [(field, asc)]
    Spacial(u32, Vec<String>, Vec<u16>),        // Spatial index: def_nr, [fields]
    Hash(u32, Vec<String>, Vec<u16>),           // Hash table: def_nr, [fields]
    Routine(u32),                            // Dynamic routine reference
    Iterator(Box<Type>, Box<Type>),          // (yield_type, internal_state_type)
    Function(Vec<Type>, Box<Type>),          // Closure: arg types + return type
    Rewritten(Box<Type>),                    // After append rewrite (Text/structs)
}
```

### Integer Storage Size

`Integer(min, max)` is stored compactly based on range:
- range < 256  → 1 byte
- range < 65536 → 2 bytes
- otherwise    → 4 bytes

Nullable integers with range exactly 256 or 65536 also use 1 or 2 bytes respectively.

### Dependency Lists (`Vec<u16>`)

Types that hold heap-allocated data (`Text`, `Reference`, `Vector`, `Enum(ref)`, collections)
carry a `Vec<u16>` of **variable indices** that this value borrows storage from.

The dep list is a *borrow tracker*:
- **Empty dep** → the variable owns its allocation; `OpFreeRef` will be emitted when it goes out of scope.
- **Non-empty dep** → the variable borrows from another; `OpFreeRef` is suppressed; code generation emits `OpCreateStack(dep[0])` instead of `OpConvRefFromNull`/`OpDatabase`.
- If the **return type** of a block/function depends on variable `v` (`tp.depend().contains(&v)`), `OpFreeRef(v)` is also suppressed even when dep is empty.

`Type::depend()` extracts the full dep list, recursing through `RefVar`.
`Type::depending(on)` returns a copy of the type with `on` prepended.

`Type::RefVar(Box<Type>)` means "stack reference": a DbRef pointing into another variable's stack slot rather than an independently-owned record. Used for `&text` (alias) parameters; `depend()` delegates to the inner type.

---

## AST-Level Operators (Call node op_nr)

These are the operator names used in `Call(op_nr, args)` at the AST level,
as listed in `data.rs`:

```
OpAdd   OpMin   OpMul   OpDiv   OpRem   OpPow
OpNot   OpLand  OpLor   OpEor   OpSLeft OpSRight
OpEq    OpNe    OpLt    OpLe    OpGt    OpGe
OpAppend  OpConv  OpCast
```

The actual numeric `op_nr` values are resolved via the operator registry in
`src/parser.rs` during parsing.

---

## Bytecode State — `src/state.rs`

```rust
pub struct State {
    bytecode: Vec<u8>,          // Main bytecode stream
    text_code: Vec<u8>,         // String constant pool
    stack_cur: DbRef,           // Current stack frame (a DB record in store 1000)
    pub stack_pos: u32,         // Current stack pointer
    pub code_pos: u32,          // Current position in bytecode
    pub database: Stores,       // All data stores
    pub arguments: u16,         // Stack size of function arguments
    pub stack: HashMap<u32, u16>, // code_pos -> stack level (for scoping)
    pub vars: HashMap<u32, u16>,  // code_pos -> variable stack position
    pub calls: HashMap<u32, Vec<u32>>, // code_pos -> called def_nrs
    pub types: HashMap<u32, u16>,      // code_pos -> type id
    pub library: Vec<Call>,            // Extern Rust function table
    pub library_names: HashMap<String, u16>,
    text_positions: BTreeSet<u32>,   // debug: set of absolute positions of live Strings
    line_numbers: HashMap<u32, u32>,
}

pub type Call = fn(&mut Stores, &mut DbRef);
```

The stack is stored as a database record in store 1000.
`stack_pos` starts at 4 (offset past the record header).

### `text_positions` (debug-only `String` liveness tracker)

Text variables on the stack are stored as Rust `String` objects (`size_of::<String>() = 24 bytes`
on 64-bit). In debug builds, `State` tracks which stack positions hold live `String`s:

- `OpText` (`state.text()`) → inserts `stack_cur.pos + stack_pos` into `text_positions` before advancing `stack_pos`.
- `OpFreeText(pos)` (`state.free_text()`) → computes `stack_cur.pos + stack_pos - pos`, calls `shrink_to(0)` on the `String`, and removes the absolute position from `text_positions`. Asserts it was present (double-free detection).
- `OpFreeStack(value, discard)` (`state.free_stack()`) → after decrementing `stack_pos` by `discard`, asserts that `text_positions` has **no entries** in the discarded range. Violation = "Not freed texts" panic.

**Consequence**: every `String` allocated by `OpText` in a block must be freed by `OpFreeText` before the block's `OpFreeStack` runs — except the block's *return* variable, which must be allocated **outside** the block's stack range (i.e. at an enclosing scope) so its position falls below the discard range.

---

## Variable Tracking — `src/variables.rs`

```rust
pub struct Function {
    pub name: String,
    pub file: String,
    steps: Vec<u8>,          // Byte steps for each variable on stack
    unique: u16,             // Unique name counter
    current_loop: u16,       // Current loop nesting (MAX = top-level)
    loops: Vec<Iterator>,    // Active for-loop contexts
    variables: Vec<Variable>,
    work_text: u16,          // Work variable for text operations
    work_ref: u16,           // Work variable for ref operations
    work_texts: BTreeSet<u16>,
    work_refs: BTreeSet<u16>,
    names: HashMap<String, u16>,  // name -> last variable index
    pub done: bool,
    pub logging: bool,
}

pub struct Variable {
    name: String,
    type_def: Type,
    source: (u32, u32),   // (line, col) of declaration
    scope: u16,           // 0 = function arguments
    stack_pos: u16,       // Position on stack frame
    uses: u16,            // Reference count
    argument: bool,
    defined: bool,
}
```

Variables are referenced in IR by their `stack_pos` (`u16`).
Scope 0 is always function arguments.
The same variable name may have multiple `Variable` instances across scopes.

---

## 248 Bytecode Operators — `src/fill.rs`

Operators are indexed by their position in the `OPERATORS` array. The array is generated by `src/create.rs::generate_code` from the `#rust "..."` annotations on `Op`-prefixed definitions in the default library. Operator names in the array follow the convention `Op<CamelCase>` → `op_<snake_case>` (e.g. `OpAddInt` → `add_int`). The exception is `OpReturn` → `op_return` (to avoid the Rust keyword).

The index of each operator equals its `op_code` field in `Data::Definition`, which is emitted as a single byte in the bytecode stream and used as an index into `fill::OPERATORS` at runtime.

Categories:

### Control Flow (0–6)
`goto`, `goto_word`, `goto_false`, `goto_false_word`, `call`, `op_return`, `free_stack`

### Boolean (7–12)
`const_true`, `const_false`, `cast_text_from_bool`, `var_bool`, `put_bool`, `not`

### Integer (13–56)
- Constants: `const_int` (4-byte), `const_short` (2-byte), `const_tiny` (1-byte)
- Var/Put: `var_int`, `var_character`, `put_int`, `put_character`
- Conversions: `conv_int_from_null`, `conv_character_from_null`, `cast_int_from_text`, `cast_long_from_text`, `cast_single_from_text`, `cast_float_from_text`, `conv_long_from_int`, `conv_float_from_int`, `conv_single_from_int`, `conv_bool_from_int`
- Math: `abs_int`, `min_single_int`
- Arithmetic: `add_int`, `min_int`, `mul_int`, `div_int`, `rem_int`
- Bitwise: `land_int`, `lor_int`, `eor_int`, `s_left_int`, `s_right_int`
- Comparison: `eq_int`, `ne_int`, `lt_int`, `le_int`

### Long (57–81)
Similar structure to Integer but for `i64`.
Extra: `format_long`, `format_stack_long`

### Single / Float (82–151)
Similar arithmetic + math functions (trig, ceil/floor/round, sqrt, log, pow, pi, e).
Extra: `format_single`, `format_stack_single`, `format_float`, `format_stack_float`

### Text (152–175)
`var_text`, `arg_text`, `const_text`, `conv_text_from_null`, `length_text`, `length_character`, `conv_bool_from_text`, `text`, `append_text`, `get_text_sub`, `text_character`, `conv_bool_from_character`, `clear_text`, `free_text`, `eq_text`, `ne_text`, `lt_text`, `le_text`, `format_text`, `format_stack_text`, `append_character`, `text_compare`, `cast_character_from_int`, `conv_int_from_character`

### Enum (176–184)
`var_enum`, `const_enum`, `put_enum`, `conv_bool_from_enum`, `cast_text_from_enum`, `cast_enum_from_text`, `conv_int_from_enum`, `cast_enum_from_int`, `conv_enum_from_null`

### Database / Struct (185–215)
- Record ops: `database` (allocate), `format_database`, `format_stack_database`
- Ref: `conv_bool_from_ref`, `conv_ref_from_null`, `free_ref`, `var_ref`, `put_ref`, `eq_ref`, `ne_ref`, `get_ref`, `set_ref`
- Field access: `get_field`, `get_int`, `get_character`, `get_long`, `get_single`, `get_float`, `get_byte`, `get_enum`, `set_enum`, `get_short`, `get_text`, `set_int`, `set_character`, `set_long`, `set_single`, `set_float`, `set_byte`, `set_short`, `set_text`

### Vector (216–228)
`var_vector`, `length_vector`, `clear_vector`, `get_vector`, `vector_ref`, `cast_vector_from_text`, `remove_vector`, `insert_vector`, `new_record`, `finish_record`, `append_vector`, `get_record`, `validate`

### Collections (229–241)
`hash_add`, `hash_find`, `hash_remove`, `eq_bool`, `ne_bool`, `panic`, `print`, `iterate`, `step`, `remove`, `clear`, `append_copy`, `copy_record`

### Static / Stack (242–247)
`static_call`, `create_stack`, `get_stack_text`, `get_stack_ref`, `set_stack_ref`, `append_stack_text`, `append_stack_character`, `clear_stack_text`

---

## DbRef

Universal pointer `(store_nr: u16, rec: u32, pos: u32)` — see `DATABASE.md` for the full definition and key/compare API. Used for stack frames (store 1000), struct instances, and vector elements.

---

## Key Patterns

### Object Initialization Order

`object_init()` in `src/parser.rs` fills unspecified struct fields in **definition order**.
Fields provided in the object literal are set first; then for each missing field,
the stored default expression is emitted, with `Var(0)` replaced by the actual record ref.

### Struct Default Expressions

Stored in `Definition.attributes[n].value` as a `Value` tree.
The `$` token in field defaults maps to `Value::Var(0)` (the current record placeholder).
`Parser::replace_record_ref()` substitutes `Var(0)` → actual record `Value` recursively
over `Call`, `If`, `Block`, and leaf nodes.

### Iterator Protocol

`Iter(var_nr, init, step)`:
- `init` evaluates to the iterator state and stores it in `var_nr`
- `step` advances the iterator and yields the next value (or signals done)
- The outer `Loop(Block)` with `Break` exits when done
