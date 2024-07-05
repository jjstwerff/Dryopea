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
Disallow multiple threads to mutate the same structure.
- threading code
  set of databases per thread
  create a new stores structure based on a subset of the main thread one
  set the linked stores to readonly & also in the main thread
  keep track of the number of started threads per database
  merge into main: set to mutable when all databases are back

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