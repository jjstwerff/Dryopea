Link to feature documents when possible. It is allowed to indicate that only a part of the feature is high priority.
Eventually features need to be "done" however that is never a short term goal.

Priorities:
- completeness:
  . strings: link to new implementation & test this, test slices, iterate slices
      for now just concentrate on the interpreter version
  . vectors: fix implementation, introduce slices
  . iterators: full features for the interpreter (#last #break #continue #first #index #count)
        a general iterator with code to implement 'next' (for hash/index/radix/list structures)
  . ordered
        store the code to compare between two elements
  . hash
        store the code to get the hash value of an element, equals on record position
  . index
        store the code for compare elements & compare with a given key
  . radix
        code to get nth-bit for each record
  . list (other way to present object references)
  . yield (add to data structure / file / iterator)
  . polymorphes records
- correctness:
  . move vector slices to full vectors on insert/remove record
  . prevent creating slices when the underlying string/vector gets modified
  . vector childs: copy together with child records
  . vector strings: copy vectors with string fields between stores
  . errors on changing to vectors with iterators: we might 'fix' some of those cases later
  . free stores running out of scope: leaked memory detection on all tests, also inside the stores

Promote the language online, with code snippets (also code snippets for problem situations)
- efficiency:
  . generate correct rust code
  . allocation prevention
  . unneeded actions in generated code like wrapping constant strings in a slice
  . optional double strings
  . prevent complex iterators where possible
  . introduce warning on big impact of polymorphes records on empty space inside vectors
- cleanup:
  . Reference counted strings, also when copying vectors. Otherwise, we just don't deallocate strings.
  . removing child records
  . compacting free space, empty space in stores detection during tests with a cutoff point to reduce clutter
  . prevent/cleanup overallocation
- reporting:
  . We can add logging optionally to debug a certain misbehaving routine without impacting the end product.
  . Logging during parsing of texts.
  . Logging of failing math like division by zero / tan(pi).
  . Logging of out of boundaries on types like integer overflow.
  . Use cases of insert/remove before or after an allocation? Or do we omit operations with logging.
  . Use cases of slices outside normal range? We currently silently fix these. Logging of this?
- fixes:
  . Corrections to iterators over vectors paired with changes to those vectors.

Additions:
- 'match' operator with string and vector unwinding, possibly about parser notation
- Safe and efficient threading support.
- Full coroutine support.
- Database migrations.

- FEA0001_scripting libraries and full operator/math in the language
- FEA0004_map (heights) to mesh
- FEA0001 code generation, validation of variable scope
- FEA0005_material
- FEA0006_wall
- FEA0022_vulcan: debug output of mesh, move through world, load new maps on demand
- FEA0012_pathing
- FEA0009_item
- FEA0013_sight
- FEA0016_bot: rules based on pathing & direction map & sight
- FEA0003_vector sorted map_vector
- FEA0006_radix
- FEA0030_libraries
- FEA0023_web_gl
- FEA0005_hash
- FEA0007_index
