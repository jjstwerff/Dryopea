Scripting
=========

Summary
-------
We want to allow for a similar enum type as rust.

However, the extra data on enumerate values introduces a whole range of language constructions like 'let',
'match' and '?'. So for simplicity reasons we currently do not allow those.
Also, we do not yet introduce name spacing for enum values.

State
-----
- **Complete** Yes
- **Implementation** Full
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

Examples
--------

Consistency
-----------

Development
-----------

Comments
--------

Changelog
---------