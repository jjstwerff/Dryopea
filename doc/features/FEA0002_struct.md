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
Fix first list functions:
- correct code generations

Default values on structure fields:
- Validate that defaults are actually written

Restriction on numbered type fields:
- remember restrictions
- Use these restrictions to create byte and short type fields.
- skip setting when outside these restriction

Reading records from json:
- Report problems on field restrictions or unknown enum values.
- Report problem on unknown field.

Structures with a variable number of fields via sub-structures:
- syntax to define them
- find the correct fields (both from parent and own)
- Remember the min/max sizes on structs due to child implementation.
- Test big differences in min/max size to prevent Inner.
- Sub structures can share fields but need the same type
- Reading fields not defined on a sub-structure is not automatically an error but returns undefined values
- Sub structures can have unique method implementations, but share the same type.

Reading from json:
- Report problem on not existing field.

Variable with super-struct pointing to a child:
- Determine that we try to read this from a super-struct.
- Function sizeof and alignment from reference.

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
