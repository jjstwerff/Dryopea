FEA0032 Database
================

Summary
-------
More general database features like: validation, compacting, efficiency analysis, json import

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
- Error: Inner Structs cannot be used to create a database object
- Database methods in the language.
  Still allow fields with the same name as the given functions on the database.
  db.validate() -> list of errors
  db.empty_space() -> list of empty space (deleted records)
  db.efficiency() -> distance to the ideal spot of items
  db.optimize() -> everything is moved to the ideal spot (single empty space, 100% efficiency)
  db.trim() -> remove the empty space at the end of the data (is there a real reason to do this?)
  db.merge(text) -> list of errors
  db.clear() -> remove all content (default field values in the main object)
  txt as [struct-type] -> convert text to the database & obj.errors() when not possible
  txt as vector<text> -> with v.errors()
  txt as [enum-type] -> null when unknown
  db.binary() -> get binary presentation of the data
  db.import(data) -> read binary presentation of the data
  define getters and setters on a struct with routines to handle them
  iterate a record into fields:
  fld.name()
  fld.mutable() -> bool
  fld as [type] to get to the data or null if no obvious conversion is possible
  fld.set(val) -> list of errors
- More clean database generated code without: {let db=$v1; ...}
- text only presents a slice from a store, then all functions should work on that with limited allocations
  performance tests against String/str implementation of rust, via generated code

- Record links
  routine to find path to record: can be both directly or a search
  into a list of fields -> record link is always an object with possibly sub objects
  routine for get a record from a record link object = step through the structure
  can we do that with the current database structure definition?

- Import of json with conversion routines on fields & records

Out-of-scope
------------

Examples
--------

Consistency
-----------

Development
-----------

Comments
--------

Changelog
---------