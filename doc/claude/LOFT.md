# Loft Language Reference

Loft is a statically-typed, imperative scripting language used by the Dryopea project.
Source files use the `.loft` extension. The language compiles to an internal bytecode representation
and can emit Rust code for host integration.

---

## Naming Conventions (enforced by the parser)

| Construct              | Convention         | Examples               |
|------------------------|--------------------|------------------------|
| Functions, variables   | `lower_case`       | `my_fn`, `count`       |
| Types, structs, enums  | `CamelCase`        | `Terrain`, `Format`    |
| Enum values            | `CamelCase`        | `Text`, `FileName`     |
| Constants              | `UPPER_CASE`       | `PI`, `MAX_SIZE`       |
| Operator definitions   | `OpXxx` prefix     | `OpAdd`, `OpEqInt`     |

---

## Types

### Primitive types

| Type        | Description                                      |
|-------------|--------------------------------------------------|
| `boolean`   | `true` / `false`                                 |
| `integer`   | 32-bit signed integer (range can be constrained) |
| `long`      | 64-bit signed integer; literals end with `l`     |
| `float`     | 64-bit floating-point; literals contain a `.`    |
| `single`    | 32-bit float; literals end with `f`              |
| `character` | A single Unicode character                       |
| `text`      | A UTF-8 string; `len()` counts bytes             |

Any variable or field can hold a `null` (absent) value unless declared `not null`.

Integer ranges can be constrained with `limit`:
```
integer limit(-128, 127)   // fits in a byte
integer limit(0, 65535)    // fits in a short
```

The default library also defines convenient width-specific aliases:
```
u8    // integer limit(0, 255)
i8    // integer limit(-128, 127)
u16   // integer limit(0, 65535)
i16   // integer limit(-32768, 32767)
i32   // integer (explicit 32-bit)
```

### Composite types

| Type syntax                        | Description                                           |
|------------------------------------|-------------------------------------------------------|
| `vector<T>`                        | Dynamic array of `T`                                  |
| `hash<T[field1, field2]>`          | Hash-indexed collection of `T` on the given fields    |
| `index<T[field1, -field2]>`        | B-tree index (ascending/descending)                   |
| `sorted<T[field]>`                 | Sorted vector on the given fields                     |
| `reference<T>`                     | Reference (pointer) to a stored `T` record            |
| `iterator<T, I>`                   | Iterator yielding `T` using internal state `I`        |
| `fn(T1, T2) -> R`                  | First-class function type                             |

The key fields are declared **inside** the angle brackets with the element type.
A `-` prefix on a field name means descending order:
```
sorted<Elm[-key]>           // single key, descending
index<Elm[nr, -key]>        // two keys: nr ascending, key descending
hash<Count[c, t]>           // compound hash key
```

### Enum types

Simple enums (value types):
```
enum Format {
    Text,
    Number,
    FileName
}
```

Polymorphic enums (each variant has its own fields, stored as a record):
```
enum Shape {
    Circle { radius: float },
    Rectangle { width: float, height: float }
}
```

### Struct types

```
struct Argument {
    short: text,
    long: text,
    mandatory: boolean,
    description: text
}
```

Fields are declared as `name: type` with optional modifiers **after** the type:
- `limit(min, max)` — constrain an integer field to a range
- `not null` — disallow the null value (enables full integer range in storage)
- `default(expr)` — a stored default value; `= expr` is a shorthand for `default(expr)`
- `virtual(expr)` — a read-only computed field (evaluated at init, not stored)

In default expressions, `$` refers to the record being initialised:
```
struct Object {
    name_length: integer = len($.name),   // computed from another field
    name: text
}
```

Example with all modifiers:
```
struct Point {
    r: integer limit(0, 255) not null,
    g: integer limit(0, 255) not null,
    b: integer limit(0, 255) not null
}
```

---

## Declarations

### Functions

```
fn function_name(param: type, other: type = default_value) -> return_type {
    // body
}
```

- `pub` prefix makes a definition publicly visible (applies to functions, structs, and enums).
- Parameters with a `&` prefix are passed by mutable reference (in-out for any type).
  - **Enforced**: a `&` parameter that is never mutated (directly or transitively through a called function) is a **compile error**. Drop the `&` if the parameter is read-only.
- Parameters with `const` prevent mutation of that parameter inside the function body.
  - `const` is a compile-time check: any assignment to a `const` parameter is an **error**.
  - `& const T` is syntactically valid but unusual — it means "pass by reference, but don't write to it" (which is redundant; prefer plain `const T` passed by value for primitives).
  - `Attribute.constant/mutable` on function definitions are NOT set for `const` user-defined-function parameters (that would break bytecode generation). The check lives purely in `Variable.const_param`.
- Default parameter values are supported.
- Functions without a `->` clause return `void`.
- A function body ending in an expression (without `;`) returns that value.

External (Rust-implemented) functions are declared without a body, followed by `#rust "..."`:
```
pub fn starts_with(self: text, value: text) -> boolean;
#rust "@self.starts_with(@value)"
```

### Constants

```
PI = 3.14159265358979;
```

Constants must be `UPPER_CASE` and are defined at file scope.

### Types and type aliases

```
type MyInt = integer;
type Coord = integer limit(-32768, 32767);
```

In library/default files, `size(n)` specifies the storage size in bytes:
```
pub type u8 = integer limit(0, 255) size(1);
```

### Library imports

```
use arguments;
```

Searches for `arguments.loft` in `lib/`, the current directory, directories from the
`LOFT_LIB` environment variable, and relative to the current script.
`use` declarations must appear at the top of the file, before any other declarations.

---

## Operators

Listed by precedence (lowest to highest):

| Precedence | Operators                              | Notes                   |
|------------|----------------------------------------|-------------------------|
| 1 (lowest) | `\|\|`, `or`                           | logical OR              |
| 2          | `&&`, `and`                            | logical AND             |
| 3          | `==`, `!=`, `<`, `<=`, `>`, `>=`       | comparison              |
| 4          | `\|`                                   | bitwise OR              |
| 5          | `^`                                    | bitwise EOR             |
| 6          | `&`                                    | bitwise AND             |
| 7          | `<<`, `>>`                             | bit shift               |
| 8          | `-`, `+`                               | addition/subtraction    |
| 9          | `*`, `/`, `%`                          | multiplication/division |
| 10         | `as` (type cast/conversion)            |                         |

Unary operators: `!` (logical not), `-` (negation).

Assignment operators: `=`, `+=`, `-=`, `*=`, `/=`, `%=`.

### The `as` operator

Used for explicit type casts and conversions:
```
10l as integer      // long to integer
"json-text" as Program   // deserialize text as a struct
```

---

## Literals

| Kind         | Syntax examples                     |
|--------------|-------------------------------------|
| Integer      | `42`, `0xff`, `0b1010`, `0o17`      |
| Long         | `10l`, `42l`                        |
| Float        | `3.14`, `1.0`                       |
| Single       | `1.0f`, `0.5f`                      |
| Character    | `'a'`, `'😊'`                       |
| Boolean      | `true`, `false`                     |
| Null         | `null`                              |
| String       | `"hello world"`                     |

---

## String formatting

Strings support inline expressions and format specifiers using `{...}`:

```
"Value: {x}"             // embed variable
"Hex: {n:#x}"            // hexadecimal with 0x prefix
"Oct: {n:o}"             // octal
"Bin: {n:b}"             // binary
"Padded: {n:+4}"         // width 4, always show sign
"Zero-padded: {n:03}"    // width 3, zero-padded
"Float: {f:4.2}"         // width 4, 2 decimal places
"Left: {s:<5}"           // left-aligned width 5
"Right: {s:>5}"          // right-aligned
"Center: {s:^7}"         // center-aligned
"{x:j}"                  // JSON output
"{x:#}"                  // pretty-printed multi-line output
```

Escape `{` and `}` as `{{` and `}}`.

For-expressions can be used inside strings to produce formatted lists:
```
"values: {for x in 1..7 {x*2}:02}"   // produces [02,04,06,08,10,12]
```

---

## Control flow

### If / else if / else

```
if condition {
    // ...
} else if other {
    // ...
} else {
    // ...
}
```

`if` can be used as an expression when both branches produce a value:
```
result = if x > 0 { x } else { -x }
```

### For loops

```
for item in collection {
    // item is each element
}
```

Ranges:
```
for i in 1..10 { }        // 1 to 9 (exclusive end)
for i in 1..=10 { }       // 1 to 10 (inclusive end)
for i in 0.. { }          // open-ended (needs break)
```

Text iteration yields characters:
```
for c in some_text { }    // c: character
```

Filtered iteration:
```
for item in collection if item.active { }
```

Reverse iteration:
```
for i in rev(1..10) { }
```

Inside a loop, the iteration variable supports several attributes using `#`:

| Attribute    | Meaning                                                                              |
|--------------|--------------------------------------------------------------------------------------|
| `v#index`    | For **text** loops: byte offset of the **start** of the current character.           |
|              | For all other loops: 0-based position of the current element.                        |
| `v#next`     | For **text** loops only: byte offset immediately **after** the current character.    |
| `v#count`    | Number of iterations completed so far.                                               |
| `v#first`    | `true` for the first element only.                                                   |
| `v#remove`   | Remove the current element (only in filtered loops).                                 |

Text iteration example — `#index` and `#next` are always consistent: `c#next == c#index + len(c)`:
```
// "Hi 😊!": H@0..1, i@1..2, ' '@2..3, '😊'@3..7, '!'@7..8
for c in "Hi 😊!" {
    // c#index = start byte of current character
    // c#next  = first byte of the next character
}
```

`v#remove` is only valid inside `for ... if ...` loops:
```
for v in x if v % 3 != 0 {
    v#remove;
}
```

### Break and continue

```
break
continue
```

Only valid inside a loop.

### Return

```
return value
return           // for void functions
```

The last expression in a block (without a trailing `;`) is automatically returned.

---

## Variables

Variables are declared implicitly on first assignment. Their type is inferred:
```
x = 42
name = "hello"
items = [1, 2, 3]
```

Variables may be explicitly initialized from expressions:
```
data = configuration as Program
```

---

## Vectors

```
v = [1, 2, 3]               // create
v += [4]                    // append one element
v += [5, 6]                 // append multiple elements
for x in v { }             // iterate
v[i]                        // index (null if out of bounds)
v[2..-1]                    // slice (negative indices count from end)
v[start..end]               // slice range (end exclusive)
v[start..]                  // open-ended slice to end
[elem; 16]                  // repeat initializer: 16 copies of elem
```

To remove elements while iterating, use `v#remove` inside a filtered loop (see [For loops](#for-loops)).

---

## Structs and record initialization

Named form (recommended; type is explicit):
```
point = Point { x: 1.0, y: 2.0 }
```

Anonymous form (type is inferred from context):
```
point = { x: 1.0, y: 2.0 }
```

Fields not specified get their `default(...)` value, or the zero value for their type.
Nullable fields default to `null`.

Field access uses `.`:
```
point.x
arg.long.len()
```

---

## Methods and function calls

Functions whose first parameter is named `self` can be called with dot syntax:
```
text.starts_with("prefix")
text.to_uppercase()
```

Otherwise they are called as free functions:
```
len(collection)
round(PI * 1000.0)
```

---

## Assertions

```
assert(condition)
assert(condition, "message")
```

Panics at runtime if the condition is false.

---

## Polymorphism / dynamic dispatch

For struct-enum types, multiple functions may share the same name if each handles a
different variant as its `self` parameter. Loft generates a dispatch wrapper automatically:

```
enum Shape {
    Circle { radius: float },
    Rect { width: float, height: float }
}

fn area(self: Circle) -> float { PI * (self.radius ^ 2) }
fn area(self: Rect) -> float { self.width * self.height }

c = Circle { radius: 2.0 };
c.area()   // dispatches to the Circle overload
```

Note: ordinary (non-enum) function overloading by argument type is **not** supported —
two functions with the same name and different non-variant parameter types are a compile error.

---

## File structure

A loft file may contain (in any order):
- `use <library>;` imports (must appear at the top)
- `pub` / non-`pub` function definitions
- Struct definitions
- Enum definitions
- Type aliases
- Top-level constants

---

## External function annotations (`#rust`, `#iterator`)

Used only in default/library files to bind loft declarations to Rust implementations:

```
pub fn len(self: text) -> integer;
#rust "@self.len() as i32"

pub fn env_variables() -> iterator<EnvVar, integer>;
#iterator "stores.env_iter()" "stores.env_next(@0)"
```

---

## Operator definitions (internal)

Operators are defined as functions named `OpXxx` in default files and linked to
infix/prefix syntax by the parser. Examples: `OpAdd`, `OpEq`, `OpNot`, `OpConv`, `OpCast`.

---

## Shebang

Loft scripts support a Unix shebang line for direct execution:
```
#!/path/to/loft-interpreter
fn main() { ... }
```

---

## Summary of grammar (informal)

`use` declarations must appear before any other top-level declarations in a loft file.

```
file         ::= { use_decl } { top_level_decl }
use_decl     ::= 'use' identifier ';'
top_level    ::= [ 'pub' ] ( fn_decl | struct_decl | enum_decl | type_decl | constant )
fn_decl      ::= 'fn' ident '(' args ')' [ '->' type ] ( ';' | block )
struct_decl  ::= 'struct' CamelIdent '{' field { ',' field } [ ',' ] '}'
enum_decl    ::= 'enum' CamelIdent '{' variant { ',' variant } '}'
variant      ::= CamelIdent [ '{' field { ',' field } '}' ]
field        ::= ident ':' type { field_mod }
field_mod    ::= 'limit' '(' expr ',' expr ')'
               | 'not' 'null'
               | 'default' '(' expr ')' | '=' expr
               | 'virtual' '(' expr ')'
type_decl    ::= 'type' CamelIdent '=' type ';'
constant     ::= UPPER_IDENT '=' expr ';'
block        ::= '{' { stmt } '}'
stmt         ::= expr [ ';' ]
expr         ::= for_expr | 'continue' | 'break' | 'return' [ expr ]
               | assignment
assignment   ::= operators [ ( '=' | '+=' | '-=' | '*=' | '/=' | '%=' ) operators ]
operators    ::= single { '.' ident [ '(' args ')' ] | '[' index ']' | '#' ident }
               { op operators }
single       ::= '!' single | '-' single | '(' expr ')' | block | '[' vector_lit ']'
               | 'if' expr block [ 'else' ( single | block ) ]
               | 'for' ident 'in' range_expr [ 'if' expr ] block
               | CamelIdent [ '{' field_init { ',' field_init } '}' ]
               | ident | integer | long | float | single | string | character
               | 'true' | 'false' | 'null'
range_expr   ::= expr '..' [ '=' ] expr   // exclusive or inclusive end
               | expr '..'                 // open-ended
               | 'rev' '(' range_expr ')' // reverse
```

---

## Best Practices

### String comparisons containing `{` or `}`

All string literals in loft are format strings — any `{...}` is interpreted as a
format expression. When comparing formatted output against a string that contains
literal braces, escape both sides with `{{` and `}}`:

```loft
// WRONG — {r:128,g:0,b:64} tries to look up variable r with format spec 128,...
assert("{p}" == "{r:128,g:0,b:64}", "...");

// CORRECT — double braces produce literal { and }
assert("{p}" == "{{r:128,g:0,b:64}}", "...");
```

Similarly for JSON format output:
```loft
assert("{o:j}" == "{{\"key\":1}}", "json format");
```

### Hex literals — always lowercase

The lexer only accepts lowercase hex digits. `0xff` is valid; `0xFF` causes a parse error.

```loft
// CORRECT
x = 0xff;
y = 0xdeadbeef;

// WRONG — uppercase hex digits are rejected
z = 0xFF;
```

### String slicing — always provide both bounds

Open-start slices (`s[..n]`) are not supported. Always give an explicit start:

```loft
s = "ABCDE";
s[0..3]   // "ABC"  — CORRECT
s[..3]    // Error: Invalid index on string
s[2..]    // "CDE"  — open-end slices work fine
```

### Struct methods — do not call directly on a constructor expression

Calling a method directly on an inline constructor (`MyStruct { ... }.method()`)
is currently rejected with `"X should be X on call to method"`. Use an intermediate
variable:

```loft
// WRONG
result = MyPoint { x: 1, y: 2 }.translate(3, 4);

// CORRECT
tmp = MyPoint { x: 1, y: 2 };
result = tmp.translate(3, 4);
```

### Methods returning new struct records

Methods whose return type is a struct currently crash at `database.rs:1458`.
Until fixed, return a scalar (integer, float, text) or modify `self` in place:

```loft
// CRASHES — method returning a new Color record
fn doubled(self: Color) -> Color { Color { r: self.r * 2 } }

// WORKS — method returning an integer
fn brightness(self: Color) -> integer { self.r + self.g + self.b }

// WORKS — method mutating self in place
fn double(self: Color) { self.r *= 2; self.g *= 2; self.b *= 2; }
```

### Unique field names across all structs in one file

When two structs in the same file share a field name at different positions, the
compiler can confuse them when resolving collection key fields. This causes wrong
results or "Unknown field" errors in sorted/index range iteration.

Use distinct field names per struct, or isolate conflicting structs in separate files:

```loft
// RISKY — both structs have a 'key' field but at different positions
struct SortElm { key: text, value: integer }
struct IdxElm  { nr: integer, key: text, value: integer }

// SAFE — field names are unique
struct SortElm { s_key: text, s_value: integer }
struct IdxElm  { i_nr: integer, i_key: text, i_value: integer }
```

### Ref-param vector append

`v += items` inside a `&vector<T>` function parameter does **not** propagate back
to the caller. Only field-level mutations are visible after the call returns:

```loft
fn bad_append(v: &vector<Item>, extra: Item) {
    v += [extra];   // silently has no effect on the caller
}

fn ok_mutate(v: &vector<Item>, idx: integer, val: integer) {
    v[idx].value = val;   // works — field mutation via ref-param is visible
}
```

To append to a vector from a function, return the new vector as a return value:
```loft
fn with_extra(v: vector<Item>, extra: Item) -> vector<Item> {
    v += [extra];
    v
}
items = with_extra(items, new_item);
```

### Polymorphic text methods on struct-enum variants

Defining text-returning methods on multiple variants of a struct-enum where each
method uses format strings to access fields causes a state machine overflow at
runtime. Return integers or build text in the caller instead:

```loft
// CRASHES at runtime
enum Shape {
    Circle { radius: float },
    Rect   { width: float, height: float }
}
fn describe(self: Circle) -> text { "r={self.radius}" }      // ← triggers bug
fn describe(self: Rect)   -> text { "{self.width}x{self.height}" }

// SAFE — return scalar, build text outside
fn area(self: Circle) -> float { PI * self.radius * self.radius }
fn area(self: Rect)   -> float { self.width * self.height }
```

---

## Known Limitations

A complete list with workarounds is in `PROBLEMS.md` at the repository root.
The most commonly encountered limitations are summarised here.

### Parser / lexer

| Limitation | Workaround |
|---|---|
| Hex literals must be lowercase (`0xff`, not `0xFF`) | Always write hex in lowercase |
| Open-start slice `s[..n]` not supported | Use `s[0..n]` |
| `for expr { }` inside a vector literal not supported | Use `{for ... { }}` inside a format string, or build with a loop and `+=` |
| Method call directly on constructor expression rejected | Assign to a variable first |
| `\"` inside a `{...}` format expression causes parse error | Use `{{` / `}}` to produce literal braces so the content is not treated as a format expression |

### Runtime

| Limitation | Workaround |
|---|---|
| Method returning a new struct record crashes | Return scalars or mutate `self` in place |
| `v += items` in a ref-param function has no effect on caller | Return the modified vector, or use field mutation |
| Polymorphic text methods with format strings on struct-enum variants crash | Return integers; build text in the caller |
| Reverse iteration on `sorted<T>` not implemented | Collect into a `vector<T>` and use `rev(...)` |
| Writing a `vector<T>` to a binary file writes 0 bytes | Loop and write each element individually |
| `f#size = n` (file truncation) not implemented | Delete and recreate the file |

### Exit codes

`lavition` exits with code 0 even when a parse error occurs. To detect failures in
shell scripts, capture output and check for `Error:` or `panicked`:

```sh
out=$(lavition myfile.loft 2>&1)
if [ $? -ne 0 ] || echo "$out" | grep -q "^Error:\|panicked"; then
    echo "FAILED: $out"
fi
```
