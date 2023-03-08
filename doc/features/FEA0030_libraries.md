FEA0030_libraries
=================

Summary
-------
Internal libraries on projects should be encouraged.
It still should be as easy to setup as possible.

State
-----
- **Complete** No
- **Implementation** No
- **Tested** None
- **Compatible** Yes
- **Documentation** Some
- **Release** Initial project release
- **Stable** No

Use-cases
---------

Design
------
Better file types of scripts, do not confuse our Ide so much
- For now use gcp instead of rs

Multi-file setup:
- Files in a directory form the same library, they share symbols.
- Multiple directories are their own library, only 'pub' symbols are known outside it.
- A project can have internal libraries via subdirectories. Those are not exposed outside the project.
- Libraries have their own name-spacing. So symbols need <lib>::<symbol> to be reached unless via an explicit 'use'.
- External libraries need a use statement to be included in files.

Allow to link to javascript.
Allow to link to ansi C libraries.
Allow to link to rust libraries.

Out-of-scope
------------

Examples
--------

Consistency
-----------

Development
-----------
Test a directory setup with code.
Allow to simulate this setup with different text files.

Test a subdirectory as an internal library.
Public symbols and library name prefixes.
Use statements to include more symbols.
Use statements to include outside project directory libraries.
Library files for external libraries. Allow to link in C libraries and any rust library.

Comments
--------

Changelog
---------
