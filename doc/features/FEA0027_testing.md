FEA0027 Scripting
=================

Summary
-------
For now, we use rust tests for fine-grained tests of language features.
Eventually we need to test interacting with complex structures, exports and imports.
This will introduce longer programs with many steps.

To make this easier, we want to have a directory tree with code and corresponding text or data files. This code can contain their own asserts.

Then we need a setup that automatically tests all this code and allow to only run specific part of it.

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
Create assertion functions for our language.
A test that runs all of these tests.
A separate executable that can run all or a part of these tests. 

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
