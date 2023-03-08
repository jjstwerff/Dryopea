FEA0028_interpreter
===================

Summary
-------
We eventually want a world-class interpreter in terms of speed.
So we need a very efficient internal notation and actions on this strucuture.

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
Export functions:
- routine to write full number to allow me to extend sections
- scan the name section, show all function names with types (needed for my analysis)
- the only global $0 is at the end of the data, increase this with the same amount
- write append functions for database strings
- introduce assert functions that call throw on problems
- add data to the current data sections
- allow to export database strings to javascript

Memory management:
- normal stores with malloc spaces
- we will need double pointers (db, ref) or a single u64
- a string and vector stores the actual length in the data
- a static string needs a position and a length
- correctly initialize the globals

Adding functions:
- test on known types & add more if needed
- fill type per function definition
- generate the code when we actually write the section
- update the section size write all bits, start at 0
- add to data in the same pass, calculate relative start of data

Integrate with database:
- references are given as two parameters to other functions
- how to keep the data stores around for rust version?

Prevent removing functions:
- add needed functions to exports
- remove force function from exports
- optimize the result

wasm-opt scriptlib_bg.wasm -Oz -o scriptlib.wasm
wasm-opt scriptlib_bg.wasm -O0 -S -o scriptlib.wat

Implement string handling from constant data

Generate result wasm file:
- Copy needed original parts, remember free memory position.
- Add claimed block

Remember details:
- Current types
- Current functions, the imports are not needed

Binary format:
- A reference is an u64 with store & position.
    For memory the position is enough, but for updates we need the store start.
    References need to still be valid after a move of a store.
- constants of different types
    Separate operator for Data appends.
    String is a position on a structure. This starts with the normal claimed length.
    We store the actual length here too. Before the raw u8 byte data.

- stack position to local space
- operators

Move arithmetic functions to imports?

Ordering:
- Needed types added with new functions.
- Find correct function: on internal generation of functions.

Create own new .wat file:
- Copy types, empty other sections
- Optimize result: test if we are correct & conversion to .wasm & optimizations
- Copy functions
- Write needed exports: current ones from functions & __wbindgen_*
- Find corresponding types for new functions
- Create function header for public functions (test for now, empty content)
- Add new types to current data
- Write new wat text with added types
- Added function headers
- Added exports for public functions (malloc & test)
- Allow calling operators = write parameters into locals where needed, replace names
- Allow adding new data = for now with constant strings, remember positions & length
- Each assert gets a unique number to allow for more in depth reporting
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
