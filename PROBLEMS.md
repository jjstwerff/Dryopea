# Known Compiler Problems

Findings from static analysis of the compiler source. Ordered by severity.

---

## Silently Wrong Behaviour

### P11 — `parser.rs:3421` — Enum field access via `.` is silently discarded

```rust
if let Type::Enum(_, _, _) = last_t.clone() {
    // TODO do something with enum fields
}
```

Writing `my_enum.some_field` on an enum value compiles without error but emits no code.
The expression evaluates to `null` silently. No diagnostic is emitted.

---

### P12 — `parser.rs:5050` — `sizeof(polymorphic_enum)` returns the wrong size

```rust
// TODO FEA0002 call function to get size or alignment of correct child
```

For a polymorphic enum, `sizeof` returns the base enum type's compile-time size, not the
actual runtime variant's size. No error or warning is emitted.

---

### P15 — `database.rs:2150` — Double-free via secondary index removal

```rust
// TODO prevent removing records twice via secondary structures
```

If a record belongs to both a primary vector and a secondary index, removing it through
the index and then through the vector (or vice versa) calls `remove_claims` twice on the
same sub-records. No guard exists. The second removal frees already-freed store records,
corrupting the allocator free-list.

---

### P16 — `vector.rs:13, 220-221` — Slice mutation aliases backing data; string ref-counting absent

Two related issues:

1. A slice (`a[i..j]`) shares backing bytes with the original vector. Mutating through
   the slice modifies the original in place without any copy-on-write. The TODO comment
   acknowledges that slices should be moved to a separate allocation on mutation.

2. String reference counts are not tracked. Removing a vector element that contains a
   `text` field does not invalidate or decrement any reference to the string's store
   record. If the store record is subsequently reclaimed, any surviving reference to the
   string becomes a dangling pointer.

---

## Summary Table

| ID  | File              | Line(s)   | Severity | Description                                      | Test |
|-----|-------------------|-----------|----------|--------------------------------------------------|------|
| P11 | parser.rs         | 3421      | Medium   | Enum field access silently produces null         | ✓    |
| P12 | parser.rs         | 5050      | Medium   | `sizeof` returns wrong size for polymorphic enum | ✓    |
| P15 | database.rs       | 2150      | Medium   | Double-free via secondary index removal          | —    |
| P16 | vector.rs         | 13,220-221| Medium   | Slice aliasing and missing string ref-counting   | —    |

---

## Test Coverage

### Covered bugs

**P11 — `enums.rs` — `enum_base_field_access`**

Defines `enum Val { A { n: integer }, B { n: integer } }` and a function
`fn get_n(v: Val) -> integer { v.n }`. When `v` has the base `Val` type the TODO block
at line 3421 emits no code, so `v.n` silently evaluates to null/zero instead of 42.
The test asserts `get_n(A { n: 42 }) == 42` and currently fails with the wrong value.

**P12 — `sizes.rs` — `sizeof_polymorphic_enum_variants_differ`**

Defines `enum Val { Small { n: u8 }, Large { n: long } }` and
`fn get_size(v: Val) -> integer { sizeof(v) }`. With the bug, both
`get_size(Small { n: 1 })` and `get_size(Large { n: 42l })` return the same
compile-time base-enum size. The test checks
`get_size(Small {...}) == get_size(Large {...})` and expects `0` (they differ); it
currently evaluates to `1` (same size), exposing the bug.

---

### Bugs without tests and why

**P15 — Double-free via secondary index removal**
There is no public lav syntax to remove a record directly through a `hash` or
`sorted` index. The only removal primitive is `vector.remove(i)` (`OpRemoveVector`),
which acts on the primary vector only. The double-free path therefore cannot be
reached from user lav code with the current language surface.

**P16 — Slice aliasing / missing string ref-counting**
A vector slice expression `v[i..j]` compiles to a `Type::Iterator` IR node, not a
stored `vector` value. It can be used inline (in `for` loops or format strings) but
cannot be assigned to a variable that is then mutated independently. Because there is
no way to hold a slice as a mutable lav variable, the copy-on-write alias cannot be
demonstrated from lav code.

