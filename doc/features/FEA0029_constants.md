FEA0029 constants
=================

Summary
-------

Allow to define constants and allow static expressions inside them.
The actual data will be calculated.

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
Define the constant type in the first pass.
Define the constant content in the second pass.
We want to allow huge constant structures like maps, item, conversations and room descriptions.

Questions:
- Do we fill the data statically or lazy?
- Do we need a memory allocation for the complete data?

Introduce complete escaping on strings.

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
