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
    .expr("o = obj(); \"{o} pretty {o:#}\"")
    .result(Value::str(
        "{a:12,bb:\"hi\",ccc:false} pretty { a: 12,\n  bb: \"hi\",\n  ccc: false\n}",
    ));
}

#[test]
fn special_fields() {
    code!(
        "enum Gender { Male, Female, Fluid }
struct Object{a: vector<integer>, b: Gender}
fn sum(o: Object) -> integer {
  r = 0;
  for v in o.a { r += v; };
  r
}"
    )
    .expr("  o = Object {a: [1,4,3], b: Fluid};\n  o.a += [sum(o)];\n  \"{o}\"")
    .result(Value::str("{a:[1,4,3,8],b:Fluid}"));
}
