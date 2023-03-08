FEA0028_interpreter
===================

Summary
-------
We eventually want a world-class interpreter in terms of speed.
So we need a very efficient internal notation and actions on this structure.

State
-----
- **Complete** Partially
- **Implementation** No
- **Tested** No
- **Compatible** Yes
- **Documentation** Some
- **Release** Initial project release
- **Stable** No

Use-cases
---------
We want to allow scripts to be written and executed in a browser.
So we compile towards binary web-assembly.
Eventually we will allow a websocket to a server to optimize the wasm code further.

Design
------
Validate changes to initial memory structure on adding (to) a constant string

More complete library:
- database handling functions:
  . new store
  . free store
  . append string
  . free string
  . append vector
  . insert vector
  . remove vector
  . free vector
- assert functions

Export with custom functions:
- add to known types
- add new function types, exports and code
- add constants to memory
- add data to the current data sections
- the only global $0 is at the end of the data, increase this with the same amount
- optimize the result
- export strings to javascript

Introduce and test more arithmetic functions
- Add math functions: sin, cos, tan, asin, acos, atan, pi, exp, log, pow

Testing:
- Run result via node.js, process.exit([exitcode]) (1=good, 2=panic, 3+=assert)

Out-of-scope
------------

Examples
--------

Consistency
-----------

Development
-----------

Out-of-scope
------------

Comments
--------

Changelog
---------
