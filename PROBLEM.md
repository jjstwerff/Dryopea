# Problem: gen_enums_polymorph — Wrong s_pos from claim()

## Failing test
`gen_enums_polymorph::code_polymorph` panics at `src/store.rs:300`:
```
Unknown record 681224
```

## Backtrace (abbreviated)
```
Store::valid (store.rs:300)
Store::get_int (store.rs:395)
Store::get_str (store.rs:494)
t_4Text_add (enums_polymorph.rs:744)
t_5Value_add (enums_polymorph.rs:861)
n_test (enums_polymorph.rs:822)
code_polymorph (enums_polymorph.rs:873)
```

## What's happening

`t_4Text_add` reads the text field of a Text EnumValue:
```rust
store.get_str(store.get_int(db.rec, db.pos + 4) as u32)
```
The int at `pos+4` is `681224`, which is not a valid store record.

## Root cause traced

`681224` is the garbage/uninitialized bytes at the wrong offset.
The string was stored with `set_str("123")` which returns `s_pos`.

**Debug output confirms:**
```
DEBUG: elm1 store_nr=1 rec=5 pos=8 s_pos=8
```

`s_pos = 8` — but it should be `17`.

`claim()` is returning `8`, which is **inside the already-claimed vector record**
(rec=5 occupies positions 5–16 in the store). This should be impossible.

## Store state at time of set_str (store 1)

Expected state when `claim(2)` is called for the string:
- pos=1: claimed, size=4 (struct record, positions 1–4)
- pos=5: claimed, size=12 (outer vector array, positions 5–16)
- pos=17: free, size=83

`claim(2)` should return 17. Instead it returns 8.

## What has been verified

- `var__elm_1.store_nr = 1` — correct store used for `set_str` ✓
- `vec_rec = 5` — from debug output ✓
- The `claim()` algorithm as read should produce 17
- No extra allocations between `record_new` and `set_str`
- `null()` creates store 0 (separate from data store 1) ✓

## Hypothesis for next investigation

The discrepancy must come from an unexpected store state. The most likely
cause is in `database()`:

```rust
} else {
    self.allocations[self.max as usize].init();  // ← reinit when reusing
}
```

If a store is reinitialized at an unexpected time, that would reset the
free-space tracking and corrupt claim positions. Specifically, after
`null()` creates store 0 and `alloc_record(23)` creates store 1,
something might be calling `database()` again with `max < allocations.len()`,
triggering a re-`init()` of store 1 while it already contains live data.

## File state

The generated test file currently has temporary debug `eprintln!` statements
at lines 783 and 813–816 of:
  `tests/generated/enums_polymorph.rs`

These must be removed (or will be regenerated) before the fix is confirmed.
