TODO:
- Simple Array test: create & read
- Simple Object test: create & read
- String of Object
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
