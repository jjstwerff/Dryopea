FEA0001_scripting
=================

Summary
-------
It should be possible to change the behavior of the game by scripting.
Even the rendering of the game will be dynamically determined by this script.
For example the material color and thickness of walls will be determined via scripts.

We strive for easy to write code that looks and feels a lot like rust. So it is mostly a subset of the rust syntax.

State
-----
- **Complete** Yes
- **Implementation** Partial
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
Mutability is checked by the compiler, it doesn't have to be explicitly given by the programmer.
There is no 'let' keyword, variables can be defined without it.
A set of methods like len, abs, sin, cos may be used as a function too without explicitly declaring their type.

Out-of-scope
------------
FEA0027 Introduce asserts in scripts for more clean tests and to allow efficient validation on projects.
FEA0019 Conversion of types on second assign to variables.
FEA0019 Identifier names consisting of Unicode characters.
FEA0029 More complete string escaping including \042 or \x22, \u0000

Examples
--------

Consistency
-----------

Development
-----------
Errors on variable scopes. Those who are used inside a for loop should not be present outside that loop anymore.

We need to clearly document the changes versus normal rust code.
- Possible types
- No references
- No explicit lifetimes
- Where do we pass by reference
- Mutability
- Variable declarations
- Automatic conversions
- Simple functions 'len', 'abs', 'sin', 'cos' etc

Comments
--------

Changelog
---------
