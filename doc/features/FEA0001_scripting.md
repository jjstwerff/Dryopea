Scripting
=========

Summary
-------
It should be possible to change the behavior of the game by scripting.
Even the rendering of the game will be dynamically determined by this script.
For example the material color and thickness of walls will be determined via scripts.

We strive for easy to write code that looks and feels a lot like rust. So it is mostly a subset of the rust syntax.

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

Design
------
We omit explicit references with & from the language, objects will always be passed by reference to methods.
Mutability is checked by the compiler, it doesn't have to be given by the programmer.
There is no 'let' keyword, variables can be defined without it.
A set of methods like len, abs, sin, cos may be used as a function too without explicitly declaring its type.

Examples
--------

Consistency
-----------

Development
-----------
We need to still clearly document the changes versus normal rust code.
- Possible types
- No references
- No lifetimes
- Where do we pass by reference
- Mutability
- Variable declarations
- Automatic conversions
- Simple functions 'len', 'abs', 'sin', 'cos' etc

Out-of-scope
------------
Conversion of types on second assign to variables.
Identifier names consisting of Unicode characters.

Comments
--------

Changelog
---------
