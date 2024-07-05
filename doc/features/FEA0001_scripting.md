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
move auto-generated test to the actual test
- where do we generate default.rs
- do not generate init() on default.rs
- generate public functions
- allow to clear texts (we need to provide a mutable pointer)
- null validation (iterators)
- null values for field defaults
- write out defaults per field on sub-object
- panic when encountering Null during code generation
- calls to multi-structure functions len() code generated via default 01_code.gcp
    validation of the different defined functions: print, assert, len, abs, clear, sin, cos

Errors on variable scopes. Those who are used inside a for loop should not be present outside that loop anymore.

Verify the parse documentation: the implementations have been shift around and sometimes slightly altered.
- Can we auto generate a separate document? (documentation & functions we call)
- Using the own language: reading directory, reading file, matching strings (without regular expressions)

Loop variable actions #index #continue #first #last #break #remove

Variable names on runtime & in generated code

We need to clearly document the changes versus normal rust code.
- Possible types
- No references
- No explicit lifetimes
- Where do we pass by reference
- Mutability
- Variable declarations
- Automatic conversions
- Simple functions 'len', 'abs', 'sin', 'cos' etc

- Generators from for loops that yield values
    crate yield_iter or next_gen
    https://lang-team.rust-lang.org/design_notes/general_coroutines.html
- Generators that can remove records/values from arrays

- iterator type: next, remove, yield, continue, break

- Detect mutability in code back to the function definition
- As a part of variable use, so some code generation can generate non-mutable let statements.

Reduce the necessary clippy warnings from generated code: see data::Data::output(
#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

Comments
--------

Changelog
---------
