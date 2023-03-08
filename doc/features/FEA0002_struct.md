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
Depend on FEA0027_testing with actual rust code generation

Create list of tests:
- Class with setting & getting fields of different types
    Introduce separate store variables (and parameters) with references linking towards it
    Keep writable references inside the store
- Test multiple references
- Output to json-like format
- Import from json-like format
- Creation of linked records (export / import)
- Removal of linked record & keep references correct & null reference to removed record
- Validate that we do not reuse the removed record space, still as much space as possible
- Call function with linked arguments to the same store
- Call function with reference function return value
- Optimize store after removal & validate store sizes & validate references
- De-fragment store after removals & reuse of open space
- Call function with multiple read-only store references from mutable references
- Return value with a read-only reference

Introduce arrays on top of these structures.
Introduce indexes inside the top record.
Introduce indexes on other layers though they will be less efficient due to extra stored parent references.
Keep checking optimization routines on these structures.

Static database: game & world
Thread local database: actor & player

Analyze if we need a mutable reference in a routine:
- change something here
- call a routine that changes something

A readonly reference is more efficient. We do not have to use a registered one.
- Can we generalize this code? Values with the different references.
- Reference code to a field needs a constant and a version for the different field types & set or get
- Array operations need the constant of the field to alter this potentially.

Mutable Reference to separate record.
Mutable reference to array element.
- Request a registered element with a field reference & an index
Mutable slice from an array.
- A double array element with custom logic if the element itself is removed from the array.

Readonly Reference to separate record.
Readonly reference to array element.
- A field reference & an index number
Readonly slice from an array.
- A field reference & two index numbers.

Field references this is not an explicit reference:
- Constant of the field position in code or 0 on single field arrays.
- Record of array element with a type definition containing the field position and possibly validation logic.

Dynamic actions:
- resize of database
- resize of array
- insert/remove from an array

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
