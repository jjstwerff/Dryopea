FEA0019 Function
================

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
All simple types and strings will be passed by value.
Structures are always passed by reference.

Default values on function arguments and named parameters in calls.
Limited template definitions, only on the first field.
Allow the attributes & code to use the given templates type names.
Automatic detection of methods via first 'self' field.

Possible conversion of second assign to a variable.

Allow unicode definition and variable names.

Allow functions to be passed as a parameter.

Out-of-scope
------------
FEA0031 Lambda calculus and closures into function parameters.

Examples
--------

Consistency
-----------

Development
-----------
Default values on function arguments.
Named arguments on calls.

Track mutations to structures inside functions.
- Disallow inserting/removing from structures inside a loop iterating over that structure outside iterator.remove().
- Disallow calling a function with a mutable reference argument inside a loop over the same structure.
  This function might mutate the structure that we are currently looping. A read only reference is fine though.

Comments
--------

Changelog
---------