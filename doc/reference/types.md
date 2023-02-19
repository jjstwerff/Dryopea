There are only a limited number of build-in types.
- integer: equivalent with i32 a signed 32 bits number, maximal 10 decimals.
- long: equivalent with i64 a number with maximal 20 decimals.
- single: equivalent with f32 a floating point number with 7 significant decimals
- float: equivalent with f64 a floating point number with 15 significant decimals
- boolean: true or false
- text: equivalent with String
- enum: a more limited enum, no sub structures can be defined
- struct: a more full-featured struct, can have restrictions on fields and sub-structures
- vector: equivalent with Vec
- sorted: a sorted vector with defined ordering fields
- hash: a hash table roughly equivalent with HashMap
- index: a red-black tree
- radix: a radix index, useful to determine near objects in a 2D or 3D space

Any of these types has a defined null value. On problematic expressions like x = y/0 this will result in x being null without a thrown exception.

On a struct field there can be additional restrictions that allow the language to potentially store a number more efficiently. All structures use a memory allocation where the parts of it will be kept relatively close to each other in memory. They can be exported and imported from json.

Several conversions between types will be applied automatically while others need to be specified. The code assumes that every text declaration is a format string with free expressions between given brackets.

## Type integer
- (1 + 3 * 8) % 10   =>   (1 + 24) % 10   =>   5
- 8 / (4 - 2 * 2)   =>   8 / 0   =>   null
- abs(4 - 8)   =>   4
- 3 >= 2   =>   true
- if 8 / 0 { "cool" } else { "problem" }   =>   "problem"
- c = 4; "count {c * 2:04}"   =>   "count 0008"
- "binary {42:b}"   =>   "binary 101010"

## Type long
- 1l + 1000   =>   1001l
- 2 * 20l as integer   =>   40

## Type single & float
- 1.2f + 30   =>   31.2f
- 2.0f / 3   =>   0.6666667
- 1.0 / 3   =>   0.3333333333333333
- 23.0f > 24   =>   false

## Type boolean
Other types placed into a boolean context will show if there is a value unequal to null.
- true && (2 / 0)   =>   false
- !(3.0 / 0.0)   =>   true

## Type text
- len("123")   =>   3
- x = "123"; x.clear(); x += "4"; x.len()   =>   1

## Type enum
- enum Val { Def, None }; x = None; "{x} {x >= Def}"   =>   "None true"

## Type struct

## Type vector

## Type sorted

## Type hash

## Type index

## Type radix
