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
Generate rust code into web-assembly.
- Allow to keep store around into other methods. Share memory.
- We can use a javascript defined array (though not change it's size)
- Javascript can request the size of the array from rust though.

We want to allow scripts to be written and executed in a browser.
So we compile towards binary web-assembly.
Eventually we will allow a websocket to a server to optimize the wasm code further.

Design
------
The optimizing of already written webassembly cannot be very good due to the extra stack implementation. We might want to optimize via written rust code instead and then generation truly optimized code. 

Text in a structure:
- an immutable array of u8
- all mutations happen on String's outside of structures
- validate that we always free these structures

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
- Start writing operators for all current tests
- Validate the removal of unused functions: test this automatically

Binary version of wasm:
- Parse current .wasm file sections
  https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec
- Parse current types
- Parse current defined functions, skip the actual content
- Parse current exports
- Start writing new wasm file = for now add all current data
- Validate this file with wasm2wat
- Allow to add new types = do nothing with them, just check the result
- Build wat token map to wasm numbers
  https://webassembly.github.io/spec/core/binary/instructions.html
- Allow to add new functions
- Allow to add new exports
- Allow to remove no-public exports

Future:
- Add math functions: sin, cos, tan, asin, acos, atan, pi, exp, log, pow
- Allow to define object content as data
- Allow to define arrays as data

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
