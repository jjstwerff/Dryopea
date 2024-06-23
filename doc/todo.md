PRIO:
- slightly better code generation:
    rust code in Definition.rust: replace parameters & introduce brackets around those
    optionally a fn main with test call and validation
    add a module declaration to each generated file
- create a runtime library to link against from generated code
- better readable inter log (correct variable & fn names)
- move TODO items to the features only keep the high priority topics here
    create priority list inside the feature documents & number those
- FEA0001_scripting error about variable scoping (for iterator variable outside for statement)
- FEA0004_map (heights) to mesh
- FEA0005_material
- FEA0006_wall
- FEA0022_vulcan: debug output of mesh, move through world, load new maps on demand
- FEA0012_pathing
- FEA0009_item
- FEA0013_sight
- FEA0016_bot: rules based on pathing & direction map & sight
- sorted list: slice.binary_search
- map_vector implementation: what do we currently parse with constant string {for??} fn iter_for(...)
- FEA0006_radix
- FEA0030_libraries
- FEA0023_web_gl
- hash
- index

TODO:
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
- loop variable actions #index #continue #first #last #break #remove
- variant structs
- Record links
    routine to find path to record: can be both directly or a search
    into a list of fields -> record link is always an object with possibly sub objects
    routine for get a record from a record link object = step through the structure
        can we do that with the current database structure definition?
- Remove object allocations on array remove
- Remove array field allocation on object remove
- Array of arrays
- Generators from for loops that yield values
  - crate yield_iter or next_gen
  - https://lang-team.rust-lang.org/design_notes/general_coroutines.html
- Generators that can remove records/values from arrays
- iterator type: next, remove, yield, continue, break
- Constant elements in a list with implicit object names
- Detect mutability in code back to the function definition
- Import of json with conversion routines on fields & records
- List slice with two references both mutable and readonly.
