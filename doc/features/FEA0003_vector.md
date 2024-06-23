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
Do not allow <> or [] on every type definition, only on structure types: vectors for now.

Examples
--------

Consistency
-----------

Development
-----------
Allow ".." in any vector context.
Allow ".." with only the end parameter, assume start at 0.
Allow for/if inside a vector definition.
Allow #first #index #last #continue on the loop variable.

json

iterators

import json

mutable

test that iterators still point to the correct position after a remove.
for now disallow multiple iterators on the same vector when an inner iterator removes a record
    we can detect this at runtime, but want to detect this too as compile time

Comments
--------

Changelog
---------