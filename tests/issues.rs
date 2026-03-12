// Copyright (c) 2026 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Minimal reproducing tests for known open issues in the loft runtime.
//! Each test isolates exactly the bug pattern described in doc/claude/PROBLEMS.md.
//! Broken tests are marked #[ignore] so they are tracked but do not break CI.

extern crate dryopea;

mod testing;

use dryopea::interpreter::byte_code;
use dryopea::logger::{Logger, RuntimeLogConfig};
use dryopea::parser::Parser;
use dryopea::scopes;
use dryopea::state::State;
use std::sync::{Arc, Mutex};

// ── Issue 3 ──────────────────────────────────────────────────────────────────
// Polymorphic text methods on struct-enum variants → stack overflow at state.rs:2070.
// `text_return` adds RefVar(Text) attributes to variant functions in the second pass,
// but enum_fn only runs in the first pass, so the Dynamic dispatch IR still calls
// with only [Var(0)] despite each variant now needing extra text-buffer arguments.

#[test]
fn polymorphic_text_method_on_enum() {
    code!(
        "enum Shape {
    Circle { radius: float },
    Rect   { width: float, height: float }
}
fn describe(self: Circle) -> text { \"circle r={self.radius}\" }
fn describe(self: Rect)   -> text { \"rect {self.width}x{self.height}\" }
fn test() {
    c = Circle { radius: 3.0 };
    assert(c.describe() == \"circle r=3\", \"got: {c.describe()}\");
}"
    );
}

// ── Issue 5 ──────────────────────────────────────────────────────────────────
// Scalar `+=` on an empty (null) vector struct field has no effect.
// Expected: the scalar is appended and len == 1.

/// `b.items += [1]` (bracket form) on a null field — this is the WORKING baseline.
/// The bracket form goes through parse_vector with is_field=true and uses
/// OpNewRecord / OpFinishRecord to allocate the element in place.
#[test]
fn vec_field_append_bracket_scalar_works() {
    code!(
        "struct Box { items: vector<integer> }
fn test() {
    b = Box {};
    b.items += [1];
    assert(len(b.items) == 1, \"len after += [1]: {len(b.items)}\");
    assert(b.items[0] == 1,   \"value after += [1]: {b.items[0]}\");
}"
    );
}

/// `b.items += [3, 5]` on a null field — multiple elements with bracket form.
#[test]
fn vec_field_append_bracket_multi_works() {
    code!(
        "struct Box { items: vector<integer> }
fn test() {
    b = Box {};
    b.items += [3, 5];
    assert(len(b.items) == 2, \"len: {len(b.items)}\");
    assert(b.items[0] == 3, \"[0]: {b.items[0]}\");
    assert(b.items[1] == 5, \"[1]: {b.items[1]}\");
}"
    );
}

/// `b.items += 1` (bare scalar, no brackets) on a null field — FIXED.
/// Parser now routes through new_record so the field is allocated in place.
/// Was tracked as Issue 5 in doc/claude/PROBLEMS.md.
#[test]
fn vec_field_append_scalar() {
    code!(
        "struct Box { items: vector<integer> }
fn test() {
    b = Box {};
    b.items += 1;
    assert(len(b.items) == 1, \"len after += 1: {len(b.items)}\");
    assert(b.items[0] == 1,   \"value after += 1: {b.items[0]}\");
}"
    );
}

// ── Issue 1 ──────────────────────────────────────────────────────────────────
// A method whose return type is a NEW struct record crashes at database.rs:1494
// because the DbRef returned by the method has a garbage store_nr.

/// Minimal reproducer: `fn double(self: Color) -> Color { Color { r: self.r * 2 } }`
/// Calling `c.double()` crashes with "index out of bounds: the len is N but index is M".
/// Tracked as Issue 1 in doc/claude/PROBLEMS.md.
#[test]
fn method_returns_new_struct_record() {
    code!(
        "struct Color { r: integer not null }
fn double(self: Color) -> Color { Color { r: self.r * 2 } }
fn test() {
    c = Color { r: 3 };
    d = c.double();
    assert(d.r == 6, \"d.r after double: {d.r}\");
}"
    );
}

// ── Issue 2 ──────────────────────────────────────────────────────────────────
// A borrowed reference first assigned inside a branch gets a garbage store_nr=8
// DbRef at runtime, crashing at database.rs:1462.
// Owned references are correctly pre-initialized (Option A sub-3); borrowed refs are not.

/// Borrowed ref first assigned INSIDE an `if` branch — FIXED.
/// Was tracked as Issue 2 in doc/claude/PROBLEMS.md; now passes after
/// the Option A sub-3 pre-init work in scopes.rs.
#[test]
fn ref_inside_branch_borrowed() {
    code!(
        "struct Item { val: integer }
fn test() {
    items = [Item { val: 10 }, Item { val: 20 }];
    result = 0;
    if items[0].val > 0 {
        r = items[0];
        result = r.val;
    };
    assert(result == 10, \"result: {result}\");
}"
    );
}

// ── Issue 4 ──────────────────────────────────────────────────────────────────
// `v += items` inside a function that takes `v` as a `&vector<T>` ref-param
// has no visible effect on the caller's variable after the call returns.

/// Baseline: field mutation through a ref-param WORKS (e.g. `v[0].val = x`).
#[test]
fn ref_param_field_mutation_works() {
    code!(
        "struct Item { val: integer }
fn set_first(v: &vector<Item>, x: integer) { v[0].val = x; }
fn test() {
    buf = [Item { val: 1 }];
    set_first(buf, 42);
    assert(buf[0].val == 42, \"buf[0].val: {buf[0].val}\");
}"
    );
}

/// Bug: `v += extra` via ref-param leaves the caller's vector unchanged.
/// Tracked as Issue 4 in doc/claude/PROBLEMS.md.
#[test]
fn ref_param_append_bug() {
    code!(
        "struct Item { name: text, value: integer }
fn fill(v: &vector<Item>, extra: vector<Item>) { v += extra; }
fn test() {
    buf = [Item { name: \"a\", value: 1 }];
    fill(buf, [Item { name: \"b\", value: 2 }]);
    assert(len(buf) == 2, \"len after fill: {len(buf)}\");
    assert(buf[1].value == 2, \"buf[1].value: {buf[1].value}\");
}"
    );
}

// ── Issue 11 ─────────────────────────────────────────────────────────────────
// Field-name overlap between two structs in the same file must NOT cause wrong
// field offsets in key lookups or tree traversal.
//
// Investigation: `determine_keys()` is type-scoped, so IdxElm.key is correctly
// resolved at offset 4 (after nr:integer), not at offset 0 (SortElm.key's position).
// Key lookups and full iteration both pass; Issue 11 was already fixed or never existed.
//
// Range-query note: `[10..20, "B"]` iterates everything up to but not including
// the element at (nr=20, key="B") in the descending ordering.  Since "C">"B"
// alphabetically and the key is sorted descending, (20,C) appears BEFORE (20,B) in
// the tree and IS therefore included → sum = 200+100+300 = 600.

/// Two structs share a field name `key` at different offsets:
/// `SortElm { key: text, value: integer }` (key is field 0, offset 0)
/// `IdxElm  { nr: integer, key: text, value: integer }` (key is field 1, offset 4)
/// Key lookups and iteration on `IdxElm` must use key's offset in IdxElm (4),
/// not in SortElm (0).  Confirmed working — field offsets are type-scoped.
#[test]
fn field_name_overlap_range_query() {
    code!(
        "struct SortElm { key: text, value: integer }
struct IdxElm  { nr: integer, key: text, value: integer }
struct Db {
    srt: sorted<SortElm[-key]>,
    idx: index<IdxElm[nr, -key]>
}
fn test() {
    db = Db {
        srt: [
            SortElm { key: \"One\",   value: 1 },
            SortElm { key: \"Two\",   value: 2 },
            SortElm { key: \"Three\", value: 3 }
        ],
        idx: [
            IdxElm { nr: 10, key: \"A\", value: 100 },
            IdxElm { nr: 10, key: \"B\", value: 200 },
            IdxElm { nr: 20, key: \"C\", value: 300 }
        ]
    };
    // Direct key lookup in sorted (must find correct field offset for SortElm)
    srt_val = db.srt[\"Two\"].value;
    assert(srt_val == 2, \"srt lookup: {srt_val}\");
    // Direct key lookup in index (must find correct field offsets for IdxElm)
    idx_val = db.idx[10, \"B\"].value;
    assert(idx_val == 200, \"idx lookup: {idx_val}\");
    // Range: [10..20, \"B\"] = up to (not including) the element at (nr=20, key=B).
    // In descending key order: (20,C) comes before (20,B), so it IS in range.
    // Correct sum = 200 + 100 + 300 = 600.
    sum = 0;
    for e in db.idx[10..20, \"B\"] { sum += e.value };
    assert(sum == 600, \"range sum: {sum}\");
}"
    );
}

// ── Issue 28 ─────────────────────────────────────────────────────────────────
// validate_slots could panic in debug builds when the same variable name is reused
// in sequential `{ }` blocks in the same function (both get the same slot but
// different live-interval entries).  Fixed: find_conflict() exempts same-name+same-slot pairs.

/// Same variable name in sequential blocks — the core Issue 28 case (fixed).
#[test]
fn sequential_blocks_same_varname_workaround() {
    code!(
        "fn test() {
    total = 0;
    { n = 1; total += n; }
    { n = 2; total += n; }
    { n = 3; total += n; }
    assert(total == 6, \"total: {total}\");
}"
    );
}

/// Different variable names in sequential blocks — validate_slots must not panic.
/// Each block is fully self-contained; variables don't escape their block.
#[test]
fn sequential_blocks_different_varnames() {
    code!(
        "fn test() {
    total = 0;
    { a = 10; total += a; }
    { b = 20; total += b; }
    assert(total == 30, \"total: {total}\");
}"
    );
}

// ── Issue 29 ─────────────────────────────────────────────────────────────────
// validate_slots false positive: two differently-named owned (Reference) variables
// that share a slot but have non-overlapping actual live ranges trigger a conflict
// because compute_intervals gives the first variable a last_use that reaches past
// the second variable's first_def.

/// Two differently-named struct variables in sequential blocks — each in its own
/// `{ }` scope so their lifetimes don't overlap.  validate_slots must not panic.
#[test]
fn sequential_blocks_different_ref_varnames() {
    code!(
        "struct Rec { x: integer }
fn test() {
    total = 0;
    { a = Rec { x: 10 }; total += a.x; }
    { b = Rec { x: 20 }; total += b.x; }
    assert(total == 30, \"total: {total}\");
}"
    );
}

/// The real issue 29 pattern: same variable name `f` reused across many sequential
/// blocks; a differently-named reference variable `c` is introduced between some of
/// those blocks.  validate_slots must not panic (c.first_def may fall between two
/// of f's live ranges, which are separate Variable entries sharing the same slot).
#[test]
fn issue_29_reused_refname_with_later_different_var() {
    code!(
        "struct Rec { x: integer }
fn test() {
    total = 0;
    { f = Rec { x: 1 }; total += f.x; }
    { f = Rec { x: 2 }; total += f.x; }
    c = Rec { x: 99 };
    { f = Rec { x: 3 }; total += f.x; }
    total += c.x;
    assert(total == 6 + 99, \"total: {total}\");
}"
    );
}

// ── T1-1: Non-zero exit code on runtime error (production mode) ───────────────
// In normal mode a failing assert/panic aborts via Rust panic!().
// In production mode (--production flag) the error is logged and execution
// continues — main.rs must exit(1) via had_fatal.  These tests verify that
// `Stores::had_fatal` is set correctly so the binary-level exit code is right.

/// Helper: compile loft code and return a State ready for execution.
fn compile_for_production(code: &str) -> (State, dryopea::data::Data) {
    let mut p = Parser::new();
    p.parse_dir("default", true, false).unwrap();
    p.parse_str(code, "t1_1_test", false);
    assert!(
        p.diagnostics.lines().is_empty(),
        "Parse errors: {:?}",
        p.diagnostics.lines()
    );
    scopes::check(&mut p.data);
    let mut state = State::new(p.database);
    byte_code(&mut state, &mut p.data);
    (state, p.data)
}

/// Attach a production-mode logger (writes to /dev/null) to a State.
fn attach_production_logger(state: &mut State) {
    let config = RuntimeLogConfig {
        log_path: std::path::PathBuf::from("/dev/null"),
        production: true,
        ..Default::default()
    };
    let lg = Logger::new(config, None);
    state.database.logger = Some(Arc::new(Mutex::new(lg)));
}

/// No error: had_fatal stays false.
#[test]
fn production_mode_no_error_had_fatal_false() {
    let (mut state, data) = compile_for_production("fn test() { assert(1 == 1, \"ok\"); }");
    attach_production_logger(&mut state);
    state.execute("test", &data);
    assert!(!state.database.had_fatal, "had_fatal must stay false when assert passes");
}

/// panic() in production mode: had_fatal becomes true, execution does NOT abort.
#[test]
fn production_mode_panic_sets_had_fatal() {
    let (mut state, data) = compile_for_production("fn test() { panic(\"deliberate\"); }");
    attach_production_logger(&mut state);
    state.execute("test", &data);
    assert!(state.database.had_fatal, "had_fatal must be true after panic() in production mode");
}

/// assert(false, ...) in production mode: had_fatal becomes true.
#[test]
fn production_mode_assert_false_sets_had_fatal() {
    let (mut state, data) =
        compile_for_production("fn test() { assert(1 == 2, \"mismatch\"); }");
    attach_production_logger(&mut state);
    state.execute("test", &data);
    assert!(
        state.database.had_fatal,
        "had_fatal must be true after assert(false) in production mode"
    );
}

// ── T1-8: For-loop mutation guard extended to field access ────────────────────
// Appending to a collection that is actively being iterated can cause infinite
// loops (vector) or structural corruption (sorted/index).  The guard that
// catches `items += [x]` must also fire for `db.items += [x]`.

/// Direct variable form: existing guard must still work.
#[test]
fn for_loop_mutation_guard_simple_var() {
    code!(
        "fn test() {
    items = [1, 2, 3];
    for e in items { items += [e]; }
}"
    )
    .error(
        "Cannot add elements to 'items' while it is being iterated — \
use a separate collection or add after the loop \
at for_loop_mutation_guard_simple_var:3:32",
    );
}

/// Field-access form: `db.items += [x]` inside `for e in db.items { ... }`.
#[test]
fn for_loop_mutation_guard_field_access() {
    code!(
        "struct Container { items: vector<integer> }
fn test() {
    db = Container { items: [1, 2, 3] };
    for e in db.items { db.items += [e]; }
}"
    )
    .error(
        "Cannot add elements to a collection while it is being iterated — \
use a separate collection or add after the loop \
at for_loop_mutation_guard_field_access:4:38",
    );
}

/// Safe: appending to a DIFFERENT field than the one being iterated is allowed.
#[test]
fn for_loop_mutation_guard_different_field_ok() {
    code!(
        "struct Container { src: vector<integer>, dst: vector<integer> }
fn test() {
    db = Container { src: [1, 2, 3], dst: [] };
    for e in db.src { db.dst += [e * 2]; };
    assert(len(db.dst) == 3, \"len: {len(db.dst)}\");
    assert(db.dst[0] == 2, \"dst[0]: {db.dst[0]}\");
}"
    );
}
