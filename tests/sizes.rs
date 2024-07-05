// Copyright (c) 2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn expr_integer() {
    expr!("a = 1; sizeof(1+2+3) + sizeof(integer) + 10 * sizeof(a) + 100 * alignment(integer)")
        .result(Value::Int(448));
}

#[test]
fn expr_float() {
    expr!("a = 1.1; sizeof(float) + 10 * sizeof(a) + 100 * alignment(float)")
        .result(Value::Int(888));
}

#[test]
fn expr_enum() {
    code!("enum E {V1, V2, V3}")
        .expr("sizeof(E) + 10 * sizeof(V1) + 100 * alignment(E)")
        .result(Value::Int(111));
}

#[test]
fn expr_struct() {
    code!(
        "struct S {a: integer, b: long, c: E}
enum E {V1, V2}"
    )
    .expr("sizeof(S) + 100 * alignment(S)")
    .result(Value::Int(817));
}

#[test]
fn hash_member() {
    code!(
        "struct S {a: integer, b: long, c: integer}
struct Main { s:hash<S>[b] }"
    )
    .expr(
        "m = Main {};
sizeof(S) + 100 * sizeof(Main) + 1000 * alignment(S)",
    )
    .result(Value::Int(8820));
}

/*
#[test]
fn index_member() {
    // Structure S will be a RB tree member so it is +9 size.
    // So it gains a left: reference, right: reference and black: boolean field.
    code!(
        "struct S {a: integer, b: long, c: integer};
struct Main { s: index<S>[a, c desc] };"
    )
    .expr("m = Main {}; sizeof(S) + 100 * sizeof(Main) + 1000 * alignment(S)")
    .result(Value::Int(8829));
}
*/

#[test]
fn reference_field() {
    // S is now a stand alone object.
    // The vector holds references to S, the same as biggest.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S>, biggest: reference<S> };"
    )
    .expr(
        "m = Main{};
sizeof(S) + 100 * sizeof(Main) + 10000 * alignment(S) + 100000 * sizeof(vector<S>)",
    )
    .result(Value::Int(1642416));
}

#[test]
fn copy_field() {
    // S is now an Inner object that is the exact size of its fields.
    // biggest is a inner object that increases the size of Main.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S>, biggest: S };"
    )
    .expr(
        "m = Main{};
sizeof(S) + 100 * sizeof(Main) + 10000 * alignment(S) + 100000 * sizeof(vector<S>)",
    )
    .result(Value::Int(1242012));
}

#[test]
fn vector_size() {
    // S is now an Inner object that is the exact size of its fields.
    // biggest is a inner object that increases the size of Main.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S> };"
    )
    .expr(
        "m = Main{};
sizeof(S) + 100 * sizeof(Main) + 10000 * alignment(S) + 100000 * sizeof(vector<S>)",
    )
    .result(Value::Int(1240812));
}
