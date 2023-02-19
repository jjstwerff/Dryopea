Scripting
=========

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
Structures with a variable number of fields via sub-structures.
Sub structures can have their on unique method implementations as long as they share the same type.
Default values on structure fields.

Examples
--------

Consistency
-----------

Development
-----------
repair or comment out current tests

output to own language notation

duplicate field names

allow restrictions on integer limits, not null
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
