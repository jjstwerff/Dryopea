// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Tests for immutability checks:
//! - `&` parameter that is never modified → error
//! - `&` parameter that IS modified → ok
//! - `const` parameter that is modified → error
//! - `const` parameter that is only read → ok
//! - Transitive mutation via a called function counts as modified → ok (no error)

extern crate dryopea;

mod testing;

/// A `&` parameter that is actually modified: no compile error.
#[test]
fn ref_param_is_modified() {
    code!(
        "fn increment(a: &integer) { a += 1 }
fn test() { x = 5; increment(x); assert(x == 6, \"x={x}\") }"
    )
    .result(dryopea::data::Value::Null);
}

/// A `&` parameter that is never modified in the body: error.
#[test]
fn ref_param_never_modified() {
    code!(
        "fn read_only(a: &integer) -> integer { a }
fn test() {}"
    )
    .error(
        "Parameter 'a' has & but is never modified; remove the & at ref_param_never_modified:1:39",
    );
}

/// A `const` parameter that is only read: no error.
#[test]
fn const_param_read_only() {
    code!(
        "fn double(a: const integer) -> integer { a * 2 }
fn test() { assert(double(5) == 10, \"double\") }"
    )
    .result(dryopea::data::Value::Null);
}

/// A `const` parameter that is mutated: error.
/// A warning is also emitted because `a` is never READ (only assigned).
#[test]
fn const_param_mutated() {
    code!(
        "fn bad(a: const integer) { a = 42 }
fn test() {}"
    )
    .error("Cannot modify const parameter 'a' at const_param_mutated:1:36")
    .warning("Parameter a is never read at const_param_mutated:1:27");
}

/// A `& const` parameter (const reference): mutation is an error.
#[test]
fn const_ref_param_mutated() {
    code!(
        "fn bad(a: & const integer) { a = 42 }
fn test() {}"
    )
    .error("Cannot modify const parameter 'a' at const_ref_param_mutated:1:38");
}

/// A `&` parameter mutated through a called function is detected as modified (no error).
#[test]
fn ref_param_mutated_via_call() {
    code!(
        "fn add_one(v: &integer) { v += 1 }
fn wrapper(a: &integer) { add_one(a) }
fn test() {}"
    );
}
