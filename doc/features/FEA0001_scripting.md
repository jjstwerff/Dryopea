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
Changing a slice:
- Fall back to a copy = mutable version
- In the same database of a different one?
     The same is far more efficient on strings (copying on start) but less in cleaning up = just drop everything.
     But then we double our links to strings, so we need a reference counter to clear them again afterward.

Create rust version for them:
- Vector slice version: less copying of data
- Interpreter integration with only mutable strings
- Tests with code that could become more efficient
- analyze mutations to variable content / stores, each function argument gets a mutable boolean
- we can let the store mutable but each string variable on it needs to be immutable (perhaps even smaller scopes)

Document why we need two phases parsing:
- can we do a more limited second pass? Like filling in the details & verifying instead of full parsing.

Text use cases: first ones can be done without script solution
- parse commands from a message
    Starting with incomplete data? Can we pause this and resume later? We do not need to store all text.
    Directly into database or own parser? Directly can be the quickest.
- import/dump store from text / file
    Current implementation.
 
- write a text response to a command: code an example including the multiple sources & relevant string functions
    Different sources of data, conversions to text. Already write out parts that are finished.
    Sources: static & DbString
    There should be no intermediate strings here, just a single string to send out

General:
- sub slicing
- conversion into string or into db-string
- appending (possibly not after readonly analysis)
- multiple sources (static, String, DbString, iterator)
    Enum?

Strings:
- String (current)
- DbSlice (database ref without copying)
- [code] &'static str
- iterator with DbSlice
- [code] iterator with static var slice
- iterator with String var slice
- iterator with separate String parts

Solutions:
- increment as separate statement beside the loop.. over engineered?.. if & call statements are still bigger
- increment as last statement inside the loop: fickle
- current loop but with remembering index before increment (extra code & variable)
- implement them all in rust with an internal object and create a new Value to use it
    Vector iterator with peek
    String iterator with peek
    Number iterator

Code generation for iterators with closures, so they can still link to the local stack:
- next() -> value
- count() -> i32
- index() -> i32
- peek() -> value
- last() -> boolean
- yield(value) separate (into vector / string / file)

Implement this in a test situation.

Structure on the stack for the iterator: able to access it out of a called function, possible parts
- #position on the stack of the iterator data
- #number of the loop (from the start of the project = we do not handle recursive functions here)
- #count
- #index
- #cur_index    current position: after the current value
- #next_index
- #value        current value of the for variable
- #peek         next value of the for variable when there is a #last (#next_value == null)
- #into (vector, text, file, null (no yield/return))
- #size (for #into vector)
- routine to read the next matching value given the current fields

For statements:
- #continue (each loop gets a number unrelated to nesting, #continue with the corresponding number, break function)
- #break (#break with the corresponding number, break function)
- #first under constraints (analyze its use, new variable that counts, do not allow outside loop & in constraints)
- #count under if constraints, (analyze, new var (same as #first) possibly part of constraints for paging)
- version of a loop inside a vector & appending to an existing vector
- version of a loop inside a string
- a loop adding to a string
- a loop adding to a text file
- #yield (add a value to the outside vector / output string / file, possibly multiple times, in a sub-loop)
- #return (#yield & #break)

- #index (remember in variable when it was still correct)
- #last (#next before the loop in separate variables, loop = (move to final variables & #index, #next))
- #next (loop till constraints matches {(#last = move to final & #index & #counter),
    next-element, test->#break, increment})


let index = 0;
loop {.. everything inside is the code of the loop
    let var = value(index);
    if !test(var) {
        break.. stop the code
    }
    {
        code(var);
        continue; stop the code
        code2(var);
        break; stop the code
        code3(var);
    }
    index += 1;
    if break or return { break }
}
- 
- on next the index value is not yet increased
 
- iter#index on for in vector or string
    keep the internal as a variable & allow them to be requested
- random (of different types, can hold a range or vector)
- libraries: implement 'pub' and 'use' from separate directory (code/test/data/test-data) namespace non pub symbols
  images.gcp
    example png with test that reads it and checks the corresponding data
  terrain.gcp
  location.gcp
  actor.gcp
  menu.gcp
  meshes.gcp
  text_edit.gcp
  detection.gcp
- stand-alone scripts with arguments: optional data and tests
  optionally compile them to rust code
  arguments.gcp
--  optional parameter vector<text> on the main function
--  automatic documentation -v --version -? -h --help and on errors / missing arguments
--  tests and example data
- stand-alone tests: move already existing tests to this format

- iter#index iter#first iter#last iter#break iter#continue:
  don't allow these values to be changed or defined
  at first just implement these variants all the time, define the possible variables
  generate this code in the second pass when they are used (via existing usage validation code)
- initial documentation: the current code is functional and not optimized, so it should be possible for others to use:
    language constructions: fn, types, vector, enum, struct, sizeof, iterator, index, slice, as, hash, sorted, radix
    types, conversions, null, operators, math, library
    some tutorials
- string operations:
  to_lowercase, to_uppercase, contains, ends_with, starts_with, find, is_empty, lines, repeat, replace, rfind, split,
  trim, is_alphabetic, is_alphanumeric, is_digit, is_hexdigit, is_numeric, is_lowercase, is_uppercase, is_whitespace
- vector operations: is_empty, contains, find, rfind, sort
- math: trunc
- define logical inverse (!) on a integer & long
- allow constants: PI/E to mutate towards single version
- internally optimize SQRT(2) to constant, pow(nr, 2), log(nr, E) etc
- 'yield' statement in for loop context
- more flexible calling of functions on expressions: (4.0^5).log(2) should work, the function now only works on vars
- rules to make code more efficient functions (ln(x) instead of log(x, E) or exp(x) instead of x.pow(E)) & constants

Move auto-generated test to the actual test
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
- always include 'let' on the first use of a variable (see vectors.rs/empty_vector)

Errors on variable scopes. Those who are used inside a for loop should not be present outside that loop anymore.

Verify the parse documentation: the implementations have been shift around and sometimes slightly altered.
- Can we auto generate a separate document? (documentation & functions we call)
- Using the own language: reading directory, reading file, matching strings (without regular expressions)

Loop variable actions #last #remove

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

More efficient File / Dir and Png library functions:
- do not store a full directory to read files from a directory (allow a rust iterator to be used via a callback)
- do not automatically claim a new store on all operations (allow to add to a current store)
- clear already allocated objects on a failed call

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
