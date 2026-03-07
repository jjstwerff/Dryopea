# Lav Language Reference

Lav is a statically-typed, imperative scripting language used by the Dryopea project.
Source files use the `.lav` extension. The language compiles to an internal bytecode representation
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
- Parameters with `const` are compile-time constants.
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

Searches for `arguments.lav` in `lib/`, the current directory, directories from the
`LAVITION_LIB` environment variable, and relative to the current script.
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
| Integer      | `42`, `0xFF`, `0b1010`, `0o17`      |
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

| Attribute    | Meaning                                             |
|--------------|-----------------------------------------------------|
| `v#index`    | 0-based position of the current element             |
| `v#count`    | number of iterations completed so far               |
| `v#first`    | `true` for the first element only                   |
| `v#remove`   | remove the current element (only in filtered loops) |

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
different variant as its `self` parameter. Lav generates a dispatch wrapper automatically:

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

A lav file may contain (in any order):
- `use <library>;` imports (must appear at the top)
- `pub` / non-`pub` function definitions
- Struct definitions
- Enum definitions
- Type aliases
- Top-level constants

---

## External function annotations (`#rust`, `#iterator`)

Used only in default/library files to bind lav declarations to Rust implementations:

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

Lav scripts support a Unix shebang line for direct execution:
```
#!/path/to/lav-interpreter
fn main() { ... }
```

---

## Summary of grammar (informal)

`use` declarations must appear before any other top-level declarations.

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
