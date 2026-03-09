# Compiler Pipeline

This document covers how loft source code is turned into executable bytecode: the lexer, the two-pass parser, the IR, type resolution, scope analysis, and bytecode generation.

---

## Pipeline overview

```
Source text (.loft)
       │
       ▼
  [ Lexer ]           src/lexer.rs
  tokenises chars into LexItem stream
       │
       ▼
  [ Parser — first pass ]     src/parser.rs
  defines all names; determines types; claims variables
  lenient: unknowns are allowed, deferred to pass 2
       │
       ▼
  [ typedef::actual_types ]   src/typedef.rs
  resolves all unknown types; fills Stores schema
       │
       ▼
  [ Parser — second pass ]    src/parser.rs
  generates IR (Value tree) with full type knowledge
       │
       ▼
  [ typedef::fill_all ]       src/typedef.rs
  finalises field positions in Stores
       │
       ▼
  [ enum_fn ]                 src/parser.rs
  synthesises polymorphic dispatch functions for enums
       │
       ▼
  [ scopes::check ]           src/scopes.rs
  assigns scope numbers to variables; inserts free/drop ops
       │
       ▼
  [ byte_code ]               src/interpreter.rs
  compiles IR Value trees → flat bytecode in State
       │
       ▼
  [ state.execute ]           src/state.rs
  runs bytecode
```

---

## Lexer (`src/lexer.rs`)

### Core types

```rust
pub enum LexItem {
    Integer(u32, bool),  // value, started_with_zero
    Long(u64),
    Float(f64),
    Single(f32),
    Token(String),       // keyword or punctuation
    Identifier(String),  // any non-keyword identifier
    CString(String),     // string literal content up to next { or "
    Character(u32),      // 'x' character constant
    None,                // end of input / end of line
}
```

`LexResult` bundles a `LexItem` with a `Position` (file, line, column).

### Token and keyword sets

Defined as static slices at the top of the file:

- **TOKENS** — punctuation and multi-character operators:
  `:`, `::`, `.`, `..`, `,`, `{`, `}`, `(`, `)`, `[`, `]`, `;`, `!`, `!=`, `+`, `+=`, `-`, `-=`, `*`, `*=`, `/`, `/=`, `%`, `%=`, `=`, `==`, `<`, `<=`, `>`, `>=`, `&`, `&&`, `|`, `||`, `->`, `=>`, `^`, `<<`, `>>`, `$`, `//`, `#`

- **KEYWORDS** — reserved words that are emitted as `Token`, not `Identifier`:
  `as`, `if`, `in`, `else`, `for`, `continue`, `break`, `return`, `true`, `false`, `null`, `struct`, `fn`, `type`, `enum`, `pub`, `and`, `or`, `use`

The lexer tries two-character tokens first (e.g. `!=` before `!`). Keywords are detected after the identifier is collected.

### Lexer modes

```rust
pub enum Mode {
    Code,        // normal code: skip whitespace and line endings
    Formatting,  // inside a format string after `{`: preserve spaces
}
```

Mode switches happen inside string scanning. When a `{` is encountered inside a string, the lexer switches to `Formatting` and returns the prefix as `CString`. The parser then reads a format expression. When `}` is encountered in `Formatting` mode, the lexer returns to scanning the rest of the string.

This allows inline format expressions like `"result: {value:>10}"` to be tokenised seamlessly.

### String literals and escape sequences

- `"..."` → `CString` for each segment between `{...}` format expressions.
- `\\`, `\"`, `\'`, `\t`, `\r`, `\n` are supported escape sequences.
- `{{` and `}}` inside strings are literal braces.

### Number literals

| Syntax | Result |
|---|---|
| `123` | `Integer(123, false)` |
| `0xaf` | `Integer(0xaf, false)` |
| `0b1010` | `Integer(10, false)` |
| `0o17` | `Integer(15, false)` |
| `123l` | `Long(123)` |
| `1.5` | `Float(1.5)` |
| `1.5f` | `Single(1.5)` |
| `1e2` | `Float(100.0)` |

Special case: `1..4` tokenises as `Integer(1)`, `Token("..")`, `Integer(4)` — the lexer uses a look-ahead to avoid consuming `..` as part of a float.

### Backtracking with `Link` / `revert`

The lexer supports arbitrary lookahead through a memory buffer:

```rust
let link = lexer.link();    // save current position; start buffering tokens
// ... try parsing something ...
lexer.revert(link);         // restore position; replay buffered tokens
```

`link()` increments a reference count. While any link is alive all consumed tokens are buffered. `Link` implements `Drop` to decrement the count; when the count reaches zero the buffer is discarded.

The parser uses this to speculatively attempt a parse path (e.g. checking whether an identifier is a type name or a variable) and backtrack on failure.

### Key lexer methods

| Method | Purpose |
|---|---|
| `cont()` | Advance to the next token (stored in `peek`) |
| `peek()` | Return the current token without advancing |
| `peek_token(s)` | Return true if current token equals `s` |
| `has_token(s)` | Consume and return true if current token equals `s` |
| `token(s)` | Consume expected token; emit error if not found |
| `has_identifier()` | Consume and return if current item is `Identifier` |
| `has_integer()` | Consume and return if current item is `Integer` |
| `has_cstring()` | Consume and return if current item is `CString` |
| `has_keyword(s)` | Consume if current item is `Identifier(s)` (local keyword) |
| `link()` / `revert(l)` | Save / restore lexer position |
| `switch(filename)` | Open a new file and restart |
| `parse_string(text, name)` | Switch to an in-memory string |

---

## Parser (`src/parser.rs`)

### `Parser` struct

```rust
pub struct Parser {
    pub data: Data,           // all definitions (functions, types, structs, enums)
    pub database: Stores,     // runtime type schema (field positions, sizes)
    pub lexer: Lexer,
    pub diagnostics: Diagnostics,
    first_pass: bool,         // true during first pass, false during second
    context: u32,             // definition number of the function being parsed
    vars: Function,           // variable table for the current function
    in_loop: bool,            // whether break/continue are valid
    default: bool,            // true when parsing the default/ library
    file: u32,
    line: u32,
}
```

### Two-pass design

Every source file is parsed **twice**:

**First pass** (`first_pass = true`):
- Registers all type, enum, struct, and function definitions.
- Assigns variable slots (but types may still be `Unknown`).
- Lenient: unknown types and unresolved names do not cause errors.
- Claims working text variables for string assembly expressions.
- Records which stores (via `database`) are mutated by each function.
- After the first pass, `typedef::actual_types` resolves unknown types and `typedef::fill_all` computes field offsets in `Stores`.

**Second pass** (`first_pass = false`):
- Generates the full `Value` IR tree for each function body.
- All type names, variable types, and function signatures must be known.
- Emits errors for type mismatches, unknown variables, and call failures.

The two-pass approach allows forward references — a struct or function can be used before it is defined.

### Entry points

```rust
// Parse a file (two full passes)
parser.parse("path/to/file.loft", is_default);

// Parse all .loft files in a directory, alphabetically
parser.parse_dir("default", true, debug);

// Parse from an in-memory string (used in tests)
parser.parse_str(text, "filename", logging);
```

`parse_dir` recurses into subdirectories and calls `scopes::check` after each file.

### `parse_file` — top-level loop

```rust
fn parse_file(&mut self) {
    // 1. Process `use` declarations first, switching the lexer to the
    //    included file and returning when it's done.
    while self.lexer.has_token("use") { ... }

    // 2. Parse top-level definitions in a loop:
    loop {
        self.lexer.has_token("pub");   // optional pub modifier
        if !parse_enum()
        && !parse_typedef()
        && !parse_function()
        && !parse_struct()
        && !parse_constant() { break; }
    }

    // 3. Resolve types and fill the Stores schema.
    typedef::actual_types(...);
    typedef::fill_all(...);
    database.finish();

    // 4. Synthesise polymorphic dispatch helpers.
    self.enum_fn();
}
```

### `use` resolution — `lib_path`

When `use foo;` is encountered, the parser looks for `foo.loft` in the following order:

1. `lib/foo.loft` (project-local library)
2. `foo.loft` (current directory)
3. `<current_dir>/lib/foo.loft`
4. `<base_dir>/lib/foo.loft` (when inside `tests/`)
5. Directories from the `LOFT_LIB` environment variable
6. `<current_dir>/foo.loft`
7. `<base_dir>/foo.loft`

### Operator precedence

Binary operators are parsed using a recursive-descent precedence climber. `OPERATORS` lists levels from lowest to highest precedence:

```rust
static OPERATORS: &[&[&str]] = &[
    &["||", "or"],                           // 0 — lowest
    &["&&", "and"],
    &["==", "!=", "<", "<=", ">", ">="],
    &["|"],
    &["^"],
    &["&"],
    &["<<", ">>"],
    &["-", "+"],
    &["*", "/", "%"],
    &["as"],                                 // 9 — highest
];
```

`parse_operators(precedence)` handles one level; it calls `parse_operators(precedence+1)` for the right operand. At the top of the recursion, `parse_part` handles postfix `.field` and `[index]` access, and `parse_single` handles atoms.

### `parse_single` — atom parsing

Handles the innermost syntactic unit:

| Token | Result |
|---|---|
| `!` / `-` | Unary not / negate |
| `(` expr `)` | Grouped expression |
| `{` block `}` | Inline block |
| `[` ... `]` | Vector literal |
| `if` | Inline if-expression |
| identifier | Variable, function call, type constructor, or method |
| `$` | Current record reference (inside struct field defaults) |
| integer / long / float / single | Literal |
| string | Format-string expression |
| character | Character literal as integer |
| `true` / `false` / `null` | Literal boolean / null |

### Function parsing — `parse_function`

```
'fn' name '(' [args] ')' ['->' return_type] ( ';' | '{' body '}' )
```

- First pass: registers the definition via `data.add_fn` or `data.add_op`.
- Second pass: looks up existing definition with `data.get_fn`, parses body, stores the code in `data.definitions[context].code`.
- Functions ending with `;` have no body (declaration of an external/built-in operation).
- After the body, `parse_rust` optionally reads `#rust "..."` annotations for the code generator.

**Important — internal function naming:**
- `add_fn` stores user-defined functions under the key `"n_<name>"` (e.g. `fn helper` → `"n_helper"`), not under `"helper"`.
- `add_op` (used only for default-library operators) stores under the plain name.
- `def_nr("helper")` therefore returns `u32::MAX` even if `fn helper` exists — the name `"helper"` is not in `def_names`.
- Consequence for type resolution: if a user writes `v: helper` (function name used as a type), `parse_type("helper")` sees `u32::MAX`, creates a `DefType::Unknown` entry for `"helper"` on the first pass, and `actual_types` emits "Undefined type helper" after the first pass. This is the correct/expected error path — no second-pass diagnostic needed.

### Struct parsing — `parse_struct`

```
'struct' Name '{' field* '}'
```

Each field: `name ':' type ['=' default] ['limit' min '..' max] [CHECK(...)]`

- Field types with `default(expr)` or `virtual(expr)` are handled via `parse_field_default`.
- `$` in a default expression is replaced by `Value::Var(0)` (the current record reference) at struct-init time.
- Trailing commas are allowed.

### Enum parsing — `parse_enum`

```
'enum' Name '{' variant* '}'
```

Two forms of variant:
- Plain: `Name` — a simple value.
- Struct-enum: `Name '{' field* '}'` — a variant with fields (polymorphic record).

After parsing, `enum_fn` synthesises dynamic dispatch wrappers so that functions defined on specific variants can be called polymorphically.

### Type parsing — `parse_type`

Converts a type identifier into a `Type` enum value. Handles:
- Built-in types: `integer`, `long`, `float`, `single`, `boolean`, `text`, `character`, `reference`
- Generic containers: `vector<T>`, `sorted<T[key]>`, `index<T[key]>`, `hash<T[key]>`
- User-defined structs and enums by name lookup
- `&T` reference types

### Type conversion and casting — `convert` and `cast`

Before emitting a binary operation or assignment, the parser checks if the actual type is compatible with the expected type:

1. **`convert`** — implicit, lossless conversion (e.g. widening an integer range, converting null, unwrapping a `RefVar`). Looks for `OpConv*` operators.
2. **`cast`** — explicit `as` conversion (e.g. text to enum, int to enum). Looks for `OpCast*` operators.
3. **`can_convert`** — pure check used for error reporting without code modification.

### String and format expression parsing — `parse_string`

When the lexer emits a `CString` followed by format mode:

```
"prefix {expr [:format_spec]} suffix"
```

The parser builds an `Insert` or append sequence:
- The prefix string literal is emitted.
- The format expression is parsed as a normal expression.
- A format specifier (width, radix, alignment, padding) is parsed by `string_states` and `get_radix`.
- The corresponding `OpFormat*` operator is called.
- The suffix string literal is emitted.
- The whole thing is assembled into text using append operations.

### Variable tracking — `Function` / `vars`

`self.vars` (a `Function` from `src/variables.rs`) tracks the variable table for the function being compiled:

- `create_var(name, type)` — allocates a new slot.
- `unique(prefix, type)` — allocates an anonymous working variable.
- `change_var_type(nr, type)` — updates the inferred type of a variable.
- `become_argument(nr)` — marks a slot as a function parameter.
- `work_texts()` — returns slots claimed for text assembly.
- `test_used(lexer, data)` — emits warnings for unused variables.

### `parse_assign` — assignment and mutating operators

```
expr [ '=' | '+=' | '-=' | '*=' | '%=' | '/=' ] expr
```

For a simple `=`:
- If the left side is a variable, the right side type is used to refine the variable's type (`change_var_type`).
- If the left side is a field, `set_field` emits the appropriate `OpSet*` call.

For `+=` on text: delegates to `assign_text` which manages the string-assembly working variable.

---

## IR — The `Value` tree (`src/data.rs`)

The parser produces a tree of `Value` nodes that represents a function body.

### `Value` enum

```rust
pub enum Value {
    Null,
    Line(u32),             // source line marker
    Int(i32),
    Long(i64),
    Float(f64),
    Single(f32),
    Boolean(bool),
    Text(String),
    Enum(u8, u16),         // (variant_index, database_type_id)

    Var(u16),              // read variable slot nr
    Set(u16, Box<Value>),  // write variable slot nr = expr

    Call(u32, Vec<Value>), // call definition nr with arguments

    Block(Box<Block>),     // { statements... ; result }
    Insert(Vec<Value>),    // inline block (no new scope)
    If(Box<Value>, Box<Value>, Box<Value>),  // if cond { then } else { else }
    Loop(Box<Block>),      // loop { ... }
    Break(u16),            // break n loops
    Continue(u16),         // continue n-th loop
    Return(Box<Value>),    // return expr
    Drop(Box<Value>),      // evaluate and discard result
    Iter(u16, Box<Value>, Box<Value>),  // iterator: (var, init, next)
    Keys(Vec<Key>),        // key specification for sorted/index/hash
}
```

`Block` wraps a `Vec<Value>` (statement list), the result `Type`, a `scope` number, and a name used in bytecode dumps.

### `v_block`, `v_set`, `v_if`, `v_loop` — IR constructors

Convenience functions used throughout the parser:

```rust
v_block(ops, result_type, name) → Value::Block(...)
v_set(var, expr)               → Value::Insert([Value::Set(var, expr)])
v_if(cond, then, else)         → Value::If(...)
```

### `Type` enum

Carries the static type of a `Value`. Key variants:

| Variant | Meaning |
|---|---|
| `Unknown(u32)` | Not yet resolved (first pass, or pending inference) |
| `Null` | The null/absent value |
| `Void` | No return value |
| `Integer(min, max)` | Bounded integer; min/max drive storage size (1/2/4 bytes) |
| `Boolean` | True/false |
| `Long` | 64-bit integer |
| `Float` | 64-bit float |
| `Single` | 32-bit float |
| `Character` | Unicode code point (stored as `Int`) |
| `Text(Vec<u16>)` | String; the `Vec<u16>` lists variables this text depends on |
| `Enum(def_nr, is_ref, deps)` | Enum type; `is_ref` true for struct-enum references |
| `Reference(def_nr, deps)` | Record reference (pointer into a Store) |
| `Vector(Box<Type>, deps)` | Dynamic array |
| `Sorted/Index/Hash/Spacial` | Keyed collections |
| `RefVar(Box<Type>)` | Stack reference (`&T` parameter) |
| `Iterator(result, state)` | Iterator type |
| `Rewritten(Box<Type>)` | Marker that text/vector append was rewritten |

The dependency lists (`deps: Vec<u16>`) track which variables a reference-typed value "depends on" for lifetime purposes, used by scope analysis.

### `DefType` — definition categories

```rust
pub enum DefType {
    Unknown,     // not yet resolved
    Function,    // normal function
    Dynamic,     // polymorphic dispatch wrapper
    Enum,        // enum type
    EnumValue,   // one variant of an enum
    Struct,      // struct type
    Vector,      // vector type definition
    Type,        // built-in type (integer, text, …)
    Constant,    // named constant
}
```

### `Data` — the definition table

`Data` holds `Vec<Definition>` for every named entity. A `Definition` stores:
- `name`, `def_type`, `returned` (return type for functions)
- `attributes: Vec<Attribute>` — fields (for structs/enums) or parameters (for functions)
- `code: Value` — the compiled IR body
- `variables: Function` — the variable table
- `known_type: u16` — the corresponding `Stores` database type id
- `rust: String` — optional hand-written Rust body for built-in ops

Key `Data` methods:

| Method | Purpose |
|---|---|
| `def_nr(name)` | Look up definition index by name |
| `find_fn(source, name, type)` | Find function by name and first-argument type |
| `add_fn / add_op` | Register a new function/operator in first pass |
| `get_fn` | Find existing function in second pass |
| `get_possible(prefix, lexer)` | Get all definitions whose name starts with prefix |
| `definitions()` | Current count of definitions |
| `def(nr)` | Borrow a definition by index |

---

## Type resolution (`src/typedef.rs`)

Called after each parse pass inside `parse_file`:

### `actual_types`

Iterates all definitions added since `start_def` and:
- Resolves `Unknown` types to their concrete forms (now that all names are registered).
- For each struct/enum, calls `fill_database` to register fields in `Stores`.
- Ensures that vector-of-struct types are registered in `Stores`.

### `fill_database`

For a struct or enum definition, calls `Stores` methods to build the runtime type schema:
- `db.structure(name, parent)` — creates a record type.
- `db.field(s, name, type_id)` — adds a field.
- `db.enumerate(name)` + `db.value(e, variant, ...)` — creates an enum type.
- Field sizes (1/2/4 bytes for integers; 4 for references/vectors; 8 for long/float) are determined by `Type::size`.

### `fill_all`

Calls `database.finish()` to compute final field byte offsets for all record types.

---

## Scope analysis (`src/scopes.rs`)

`scopes::check(data)` is called after parsing a file. It visits every function's IR tree and:

1. Assigns each variable declaration to a scope number (0 = function arguments).
2. Tracks which scopes are "open" at each point.
3. When a variable goes out of scope and its value depends on store memory (text, references), inserts `OpFreeText` / `OpFreeRef` calls.
4. Detects re-use of a variable name across scopes and remaps the second occurrence to a fresh slot.

The scope numbers are written back into `Function` (the variable table) and used by the bytecode generator to determine when to emit cleanup operations.

---

## Rust code generation (`src/generation.rs`)

`src/generation.rs` provides the `Output` struct and `rust_type` function used to transpile compiled loft programs to Rust source files. This is used only during development to regenerate `src/fill.rs` and `src/external.rs` from the `#rust "..."` annotations in the default library. It is not involved in the normal interpreter execution path.

### `Output<'a>`

```rust
pub struct Output<'a> {
    pub data: &'a Data,         // read-only view of all definitions
    pub stores: &'a Stores,     // runtime type schema
    pub counter: u32,           // unique label counter for generated identifiers
    pub def_nr: u32,            // definition number currently being emitted
    pub indent: u32,            // current indentation level
    pub declared: HashSet<u16>, // variable slots already declared in this function
}
```

Bundles the read-only compile-time data with the mutable emission state so that individual emit functions receive a single context argument.

### `rust_type(tp, context) -> String`

Maps a loft `Type` to the corresponding Rust type string. The `context` parameter controls the form:

| Context | Effect |
|---|---|
| `Context::Argument` | Stack/argument passing type (e.g. `Str` for text, `i32` for integer) |
| `Context::Variable` | Local variable type (e.g. `String` for text — owned heap allocation) |
| `Context::Reference` | Prefixes the argument type with `&` |

Integer types are mapped to `u8`/`u16`/`i8`/`i16`/`i32` based on the `Integer(min, max)` range. Reference, vector, and collection types all map to `DbRef`.

---

## Bytecode generation (`src/interpreter.rs`, `src/state.rs`)

`byte_code(state, data)` iterates all `Function` definitions (excluding operators) and calls `state.def_code(d_nr, data)` for each. This compiles the `Value` IR tree into a flat bytecode representation stored in `State`.

The bytecode is a compact encoding of the `Call`/`Set`/`If`/`Loop` IR nodes. It is optimised for fast interpretation rather than size.

`state.execute("main", data)` runs the named function.

`show_code(writer, state, data)` dumps both the IR tree and the bytecode for each user-defined function to a writer — used for the debug output in `tests/code/`.

---

## Default library (`default/*.loft`)

The default library is loaded before any user source. It is parsed with `default: true`, which:
- Allows `OpXxx`-prefixed names (operator definitions).
- Allows `#rust "..."` annotations that supply the Rust implementation string for the code generator (`src/generation.rs`).
- Registers all built-in types, operators, and standard functions in `Data` and `Stores`.

Files are loaded in alphabetical order:
- `01_code.loft` — all operators and standard functions
- `02_images.loft` — image, file, pixel types
- `03_text.loft` — text utility functions

---

## Naming conventions enforced by the parser

| Category | Convention | Enforcement |
|---|---|---|
| Functions / variables | `lower_case` | `is_lower()` |
| Types / structs / enums / enum values | `CamelCase` | `is_camel()` |
| Constants | `UPPER_CASE` | (noted but not enforced by `is_upper`) |
| Operator definitions | `OpXxx` prefix | `is_op()` |

Violations emit an `Error` diagnostic but do not abort compilation.

---

## Diagnostic system (`src/diagnostics.rs`)

All errors, warnings, and fatal messages flow through `Diagnostics`:

```
Warning  — informational; compilation continues
Error    — type/syntax error; second pass is skipped if errors found in first pass
Fatal    — parse cannot continue (e.g. unterminated string, syntax error)
```

Diagnostics are collected on the `Lexer` and merged into `Parser::diagnostics` after each parse call. The `diagnostic!` and `specific!` macros format messages with file/line/column from `self.lexer.pos()`.

---

## Source file summary

| File | Role |
|---|---|
| `src/lexer.rs` | Tokeniser; link/revert backtracking; string/format mode |
| `src/parser.rs` | Two-pass recursive-descent parser; IR generation |
| `src/data.rs` | `Value`, `Type`, `DefType`, `Data`, `Attribute` definitions |
| `src/typedef.rs` | Type resolution; `Stores` schema population |
| `src/scopes.rs` | Scope assignment; lifetime cleanup insertion |
| `src/variables.rs` | Per-function variable table (`Function`) |
| `src/interpreter.rs` | `byte_code` — IR → bytecode; `show_code` |
| `src/state.rs` | Bytecode executor |
| `src/diagnostics.rs` | Error/warning collection and formatting |
| `src/database.rs` | `Stores` — runtime type schema (field offsets, sizes) |
| `src/generation.rs` | Rust code generator — `Output` struct, `rust_type` mapping, emits `fill.rs` / `text.rs` |
| `src/calc.rs` | Field byte-offset calculator for struct/enum-variant layout |
| `src/stack.rs` | Bytecode-generation stack frame (`Stack`, `Loop`) |
| `src/create.rs` | Drives code generation: `generate_lib` and `generate_code` |
| `default/*.loft` | Built-in operators and standard library |
