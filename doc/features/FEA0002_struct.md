FEA0002_struct
==============

Summary
-------

State
-----
- **Complete** No
- **Implementation** Partial
- **Tested** Limited
- **Compatible** Not enough
- **Documentation** No
- **Release** Initial project release
- **Stable** No

Use-cases
---------

Design
------
Dual parser passes:
- Prevent code generation inside blocks on first pass (function calls can produce errors here)
- Already define the function header.
- Prevent defining structs/enum/type twice
- Test a calling a later defined function, struct, enum or type.

Inner structs
- Find links between structs: convert to Type::Inner when needed
- Remember the min/max sizes on structs due to child implementation
- Test a link towards structs and switch from Inner.
- Test big differences in min/max size to prevent Inner.
- Test struct sizes / alignments.

Structures with a variable number of fields via sub-structures.
Sub structures can have unique method implementations, but share the same type.
Default values on structure fields.
Restriction on numbered type fields.
Use these restrictions to create byte and short type fields.

Examples
--------

Consistency
-----------

Development
-----------
Construct objects on a struct type.

Output to own language notation

Duplicate field names

Allow restrictions on integer limits, not null

struct Data {
    val: integer limit 1..=100,
    byte: integer limit 0..256,
    signed: integer limit -127..128
}

logging of write failures, script hook to know about these failures

compare between equal and unequal types

assign to variable with other type

call function with other type

tests
- store
- mmap

json

size

alignment

constant

subtype
- validate subtype conversions
- validate subtype methods

null

min/max text

min/max number

import json

mutable

getters

setters

randomness

Comments
--------

Changelog
---------
