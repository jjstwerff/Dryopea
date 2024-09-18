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
fn obj() -> Object { Object {a: 12, bb: \"hi\", ccc: false } }"
    )
    .expr("o = obj(); \"{o} pretty {o:#}\"")
    .result(Value::str(
        "{a:12,bb:\"hi\",ccc:false} pretty { a: 12, bb: \"hi\", ccc: false }",
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

// TODO A vector with only enum value
// TODO Vector with record including an enum

#[test]
fn duplicate() {
    code!(
        "struct Point {
   r: integer,
   g: integer,
   r: integer
}"
    )
    .error("Error: field `r` is already declared at duplicate:4:6");
}

#[test]
fn colours() {
    code!("struct Point {
  r: integer limit(0, 255) not null,
  g: integer limit(0, 255) not null,
  b: integer limit(0, 255) not null
}

fn value(self: Point) -> integer {
  self.r * 0x10000 + self.g * 0x100 + self.b
}"
    )
    .expr("  points = [ Point { r:128, b:128 }, Point { b:255 } ];\n  \"size:{sizeof(Point)} purple:{points[0]} value:{points[0].value():x} blue:{points[1]}\"")
    .result(Value::str("size:3 purple:{r:128,g:0,b:128} value:800080 blue:{r:0,g:0,b:255}"));
}

#[test]
fn restrictions() {
    code!(
        "struct Data {
  byte: integer limit(0, 255) not null,
  val: integer limit(1, 256) check(val > byte),
  signed: integer limit(-127, 127) default(1)
}

fn calc(self: Data) -> integer {
  self.val * 65536 + self.byte * 256 + self.signed
}
"
    )
    .expr("1")
    .result(Value::Int(1));
}
