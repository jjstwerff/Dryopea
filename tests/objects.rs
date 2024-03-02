// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Testing framework
extern crate dryopea;

mod testing;

use dryopea::data::Value;
#[test]
fn define_object() {
    code!(
        "struct Object{first: integer, second: text, third: boolean}
fn obj() -> boolean {
  o = Object {first: 1234, second: \"data\", third: true};
  o.first-12 == 1222 && len(o.second) == 4 && o.third
}"
    )
    .expr("if obj() {1} else {0}")
    .result(Value::Int(1));
}

#[test]
fn print_object() {
    code!(
        "struct Object{a: integer, bb: text, ccc: boolean}
fn obj() -> Object { Object {a: 12, bb: \"hi\", ccc: false} }"
    )
    .expr("o = obj(); f=o.a; \"{f}\"")
    .result(Value::Text("{a: 12, bb: \"hi\", ccc: false}".to_string()));
}
