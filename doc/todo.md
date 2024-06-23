TODO:
- Output of vector via the database.rs routine
- Validate vector of objects output
- Validate object with vector field as output
- Integrate with code to fill the database structure:
    OpFormatDatabase(ref, db_nr, pretty)
        object: complex -> field with object / array / text
        array: complex -> complex object member
        simple: no space after { or before } no newlines
        complex: no newline after { when top level, extra indent after {, newline after each field, lower indent after }
        complex array: newline after [, extra indent and newline after each , and before ] lower indent
     enum output -> database definition
     vector field output -> database definition with container
     stand-alone vector output -> separate text method to call
     multi-layout record output
- Save the low level database definition as json.
- Stand-alone tool to show the database content as json. Only a part of it via json path. Validating the structure & analyzing the empty space.
- Same tool can read the database content as json, possibly ignoring the differences.
  - Optimization of the binary data.
    Routine parser into the database:
          db = Object.parse(str)  or  db = Object(str)   or  db=Object(file)
          Where do the errors go?
          db is null      "{db:?}" might show errors
    https://github.com/serde-rs/json?tab=readme-ov-file
    read definition from json & dump to json
    the pre-defined types are a fixed mapping, the rest is derived from the data structure
    start with simple errors on base types
    move correct positions of fields to the database module
    unknown and missing fields
    default values on fields (simple typed for now, in the future with script integration)
    linking keys towards other records (even to those that currently do not exist)
    allow ignoring unknown fields
- Output of the database
    tests for the different types
        record
        base types: integer, long, single, float, boolean, text
        vector
        multiple typed structures
        output keys of records
    define record links
        routine to find path to record: can be both directly or a search
        into a list of fields -> record link is always an object with possibly sub objects
        routine for get a record from a record link object = step through the structure
            can we do that with the current database structure definition?
- Integrate database with inter = add type structure to struct Inter
    Fill database structure during parsing
    Link to structure in parser types
    Output routine with structure information, optional to always preset 'type' fields, optional quoted fields
    Input routine with structure information, allow optional 'type' fields even when not needed, optional quoted fields
- Simple Array test: create & read
- Simple Object test: create & read
- String of Array
- Mutate Object
- Mutate Array
- Array of Objects
- Remove object allocations on array remove
- Remove array field allocation on object remove
- Object with array field
- Import Object
- Import Array
- Iterate Array
- Iterate Object
- Remove Array element on Iterator
- Add Arrays
- Array of arrays
- Object relation field
- Import Object with relation
- Fix array tests
- Initial implementation of sorted list: slice.binary_search
- Test with closures sorting a list, short notation for this specific closure (only how to get the order fields)
- Generators from for loops that yield values
  - crate yield_iter or next_gen
  - https://lang-team.rust-lang.org/design_notes/general_coroutines.html
- Generators that can remove records/values from arrays
- Complex iterators
  - next, remove, yield, continue, break
- Validate multiple database code
  - disallow references in fields to other databases
- In language tokenizer
- In language parser
- In language data gatherer
  - for loops with yield
  - use of class fields
  - use of parameters
  - mutations to databases
  - alterations to arrays with life references
- Json output of records
- Json parsing of records
- Constant elements in a list with implicit object names
- Json output of list
- Json parsing of list
- Detect mutability in code back to the function definition
- Fix all current unit-tests & create more
- Allow multiple variant structs (parser)
- Test list with multiple variant records
- Test element with multiple variant records
- Test ordered list with fields in definition
- Test index
- Test radix
- Test hash
- Allow externally defined databases with slightly different internal structures to be used by a script
- Import of json with conversion routines on fields & records
- Json parsing of linked records
- Test deleted linked records (only with record types that need it)
- Test rebuilding store to remove deleted records
- List slice with two references both mutable and readonly.
- Allow to define multiple references to the same store (parser)
- Detect illegal calls to multiple reference functions

- Code
  . separate compiler main
        read all .gcp files in the indicated directory
        generate .rs files in the same directory (this will override current one)
  . generate enum types
  . generate field enums
  . record constructors, non-structure fields with null for default value.
        update indexes, enum when construction fails (with field that failed)
        duplicate key check
  . generate get/set/add/clear/insert/remove/iter methods on typed structures
        getters on the parent
        setters on the parent; return Enum (Ok, Invalid(nr), Undefined(nr))
        allow to remember unresolved keys to references inside the store, this is useful for json imports
        resolve unresolved keys when an object is inserted that matches this key
        remaining unresolved keys
        correct references to rebuild hash tables or changed lists
- create the outline code for the graphical routines (map arrays, map items/materials)

Roadmap:
- FEA0001_scripting (v)
- FEA0020_enum      (v)
- FEA0002_struct    (!)
- FEA0003_vector    (!)
- Move to feature branches and merge policies on main.
- FEA0004_map
- FEA0005_material
- FEA0006_wall
- FEA0009_item
- FEA0022_vulcan
- FEA0028_interpreter
- FEA0006_radix
- FEA0030_libraries
- FEA0023_web_gl
- Move feature development and documentation out of the tree as this is far more volatile in nature.
