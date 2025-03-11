// Copyright (c) 2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn expr_integer() {
    expr!("a = 1; sizeof(1+2+3) + sizeof(integer) + 10 * sizeof(a)").result(Value::Int(48));
}

#[test]
fn expr_float() {
    expr!("a = 1.1; sizeof(float) + 10 * sizeof(a)").result(Value::Int(88));
}

#[test]
fn expr_enum() {
    code!("enum En {V1, V2, V3}")
        .expr("sizeof(En) + 10 * sizeof(V1)")
        .result(Value::Int(11));
}

#[test]
fn expr_struct() {
    code!(
        "struct S {a: integer, b: long, c: En}
enum En {V1, V2}"
    )
    .expr("sizeof(S)")
    .result(Value::Int(17));
}

#[test]
fn hash_member() {
    code!(
        "struct S {a: integer, b: long, c: integer}
struct Main { s:hash<S[b]> }"
    )
    .expr("sizeof(S) + 100 * sizeof(Main)")
    .result(Value::Int(820));
}

#[test]
fn index_member() {
    // Structure S will be a RB tree member so it is +9 size.
    // So it gains a left: reference, right: reference and black: boolean field.
    code!(
        "struct S {a: integer, b: long, c: integer};
struct Main { s: index<S[a, -c]> };"
    )
    .expr("m = Main {}; sizeof(S) + 100 * sizeof(m)")
    .result(Value::Int(829));
}

#[test]
fn reference_field() {
    // S is now a stand-alone object.
    // The vector holds references to S, the same as biggest.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S>, biggest: reference<S> };"
    )
    .expr("sizeof(S) + 100 * sizeof(Main) + 10000 * sizeof(vector<S>)")
    .result(Value::Int(122012));
}

/*
TODO needs initialisation of fields instead of Main.biggest as reference.
#[test]
fn copy_field() {
    // S is now an Inner object that is the exact size of its fields.
    // biggest is an inner object that increases the size of Main.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S>, biggest: S };"
    )
    .expr(
        "m = Main{};
sizeof(S) + 100 * sizeof(Main) + 10000 * sizeof(m) + 100000 * sizeof(vector<S>)",
    )
    .result(Value::Int(1242012));
}
*/

#[test]
fn vector_size() {
    // S is now an Inner object that is the exact size of its fields.
    // biggest is an inner object that increases the size of Main.
    code!(
        "struct S {a: integer, b: integer, c:integer};
struct Main { s: vector<S> };"
    )
    .expr(
        "m = Main{};
sizeof(S) + 100 * sizeof(Main) + 10000 * sizeof(m) + 100000 * sizeof(vector<S>)",
    )
    .result(Value::Int(1280812));
}
