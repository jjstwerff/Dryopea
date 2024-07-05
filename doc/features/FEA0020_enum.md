FEA0020_enum
============

Summary
-------
We want to allow for a similar enum type as rust.

However, the extra data on enumerate values introduces a whole range of language constructions like 'let',
'match' and '?'. So for simplicity reasons we currently do not allow those.
Also, we do not yet introduce name spacing for enum values.

State
-----
- **Complete** Yes
- **Implementation** Partial
- **Tested** Reasonable
- **Compatible** Yes
- **Documentation** Some
- **Release** Initial project release
- **Stable** No

Use-cases
---------
Clear names in code for commonly used values.
Eventually we can randomize the internal presentation of these values for sake of providing second party assets with
game projects. Those commonly have a licence that prohibits easy extraction of those asserts from the resulting game.

Design
------
Simple enum definitions without internal fields.

The null value of an enum is 255 or 65535.

Allow more enum values than 255 inside the database: switch to u16 instead of u8 internally.
Allow conversion from and to text from enum.
Allow conversion from and to integer from enum.
Possibly randomization of the enum values inside the binary database.

Examples
--------

Consistency
-----------

Development
-----------
Allow to randomize enumerate values:
- keep this transformation stored in the database.rs structure for enumerates
- only when explicitly requested as this has a slight performance impact
- keep the original defined ordering around (for conversions to and from numbers)
- internally write different values to and from the database: conversion on writing/reading

Comments
--------

Changelog
---------