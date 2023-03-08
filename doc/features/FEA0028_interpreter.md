FEA0028_interpreter
===================

Summary
-------
We eventually want a world-class interpreter in terms of speed.
So we need a very efficient internal notation and actions on this strucuture.

State
-----
- **Complete** No
- **Implementation** Full
- **Tested** Reasonable
- **Compatible** Yes
- **Documentation** Some
- **Release** Initial project release
- **Stable** No

Use-cases
---------
Especially inside the browser we need to be able to run for a long time without any further optimizations.
This project can later be exported and optimized outside this environment with LLVM.

Design
------
The code and the stack are arrays of bytes.
- 1: Nop reserved for eventual alignment
- 1: Byte byte
- 1: Short byte*2
- 1: Int byte*4
- 1: Single byte*4
- 1: Long byte*8
- 1: Float byte*8
- 1: Text byte:size, byte*size
- 1: Big byte*4:size, byte*4:position
- 1: Drop byte:positions to drop from the stack
- 4: Read1/2/4/8 byte*2:position
- 4: Write1/2/4/8 byte*2:position
- 1: Call byte*4:code
- 1: Return byte:positions drop the stack including arguments and return code
- 128: Operations byte*x:positions to read and write

Stack:
- A memory allocation that can grow

Stack positions:
- The number of steps back from stack top-8
- 0-7 adds to the stack

Routines:
- Define operators list from default definitions. Types are constants.
- Efficiently read/write multibyte values from stack and code.
- Calculate stack positions relative to the top. Stack can grow via expressions.
- Automatically switch to Read/Write when function stack needs grow.
- Function per type to report the return value of a test function.
- Function calls keep space for: return value, return address, arguments.
- Do not drop from the stack unless at the end of functions or blocks.
- Implement the operations as an array of function calls.
- Definitions hold the position inside this generated code array.

Export to other languages:
- Directly to LLVM byte code

Out-of-scope
------------
Alignment in generated code:
- We do not want much bigger code due to every multibyte constant.
- This mostly only impacts 64/4k byte boundaries.
- When we detect such a boundary we can insert NOP operations to prevent this.

Optimize value Read before calls:
- Before we call we need the values in place on the stack.
- Expressions can leave their results anywhere so we might immediately move them.

Any optimization:
- Optimization is for production systems, those will want to use LLVM for that

Stack shrinking on long-running processes

Multi-threading

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
