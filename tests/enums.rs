// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Testing framework
extern crate dryopea;

use dryopea::data::Value;

mod testing;

#[allow(unused)]
#[test]
fn enum_field() {
    code!(
        "enum Content {
    Long { v: long },
    Float { v: float },
    Single { v: single },
    Text { v: text }
};

struct Container {
    name: text,
    content: Content,
    list: vector<Content>
}

fn fill() -> Container {
    Container {
        name: \"testing\",
        content: Single { v: 1234.56f },
        list: [
            Long { v: 9876543210l },
            Text { v: \"An example sentence of text\" },
            Float { v: 3.141592653589793 }
        ]
    }
}"
    )
    .expr("c = fill(); \"Container: {c:#}\"")
    .result(Value::str(
        "Container: { name: \"testing\",
  content: Single { v: 1234.56 },
  list: [ Long { v: 9876543210 }, Text { v: \"An example sentence of text\" }, Float { v: 3.141592653589793 } ]
}",
    ));
}

#[test]
fn define_enum() {
    code!(
        "enum Code {
    Null,
    Line { line: i32 },
    Integer { i_value: i32 },
    Enum { e_value: u8, tp: u16 },
    Boolean { b_value: boolean },
    Float { f_value: float },
    Text { t_value: text },
    Call { function: text, parameters: u8 }, // we use polish notation, values should be done
    Block { name: text, tp: u16, size: u16 }, // the size indicated the end of the block
    Loop { name: text, tp: u16, size: u16 },
    Continue { loops: u8 }, // the loops with 0: current, 1:first parent, etc
    Break { loops: u8 },
    Return, // return the given parameter or void
    Set,
    Var,
    If,
    Drop}"
    )
    .expr("v = \"Call {{ function: \\\"foo\\\", parameters: 2 }}\" as Code; \"{v}\"")
    .result(Value::str("Call {function:\"foo\",parameters:2}"));
}

#[test]
fn general_json() {
    code!(
        "
enum Value {
    Null,
    Integer { i_value: i32 },
    Boolean { b_value: boolean },
    Float { f_value: float },
    Text { t_value: text },
    Object { fields: vector<Pair> },
    Array { content: vector<Value> }
}

struct Pair {
    field: u16,
    value: Value
}

struct Field {
    field: u16,
    name: text
}

struct Json {
    key_fields: vector<Field>,
    key_hash: hash<Field[name]>,
    data: Value
}
"
    )
    .expr(
        "v = \"{{ data: Integer {{ i_value: 12 }} }}\" as Json;
    i = v.data;
    assert(i == Integer, \"Compare\");
    w = \"Text {{ t_value: \\\"Something\\\" }}\" as Value;
    v.data = w;
    \"{v} & {i}\"",
    )
    .result(Value::str(
        "{key_fields:[],data:Text {t_value:\"Something\"}} & Text {t_value:\"Something\"}",
    ));
}

#[test]
fn call_enum() {
    code!(
        "enum Value {
    Integer { i_value: i32 },
    Text { t_value: text },
    Array { content: vector<i32> }
}

fn add(self: Integer) -> i32 {
    self.i_value
}

fn add(self: Text) -> i32 {
    self.t_value as i32
}

fn add(self: Array) -> i32 {
    n = 0;
    for v in self.content {
        n += v;
    }
    n
}
"
    )
    .expr(
        "
t = Text { t_value:\"123\" };
i = Integer { i_value: 101 };
a = Array { content: [1,2,3,4] };
t.add() + i.add() + a.add()",
    )
    .result(Value::Int(234));
}

#[test]
fn polymorph() {
    code!(
        "enum Value {
    Integer { i_value: i32 },
    Text { t_value: text },
    Array { content: vector<i32> }
}

fn add(self: Integer) -> i32 {
    self.i_value
}

fn add(self: Text) -> i32 {
    self.t_value as i32
}

fn add(self: Array) -> i32 {
    n = 0;
    for v in self.content {
        n += v;
    }
    n
}
"
    )
    .expr(
        "l = [ Text { t_value:\"123\" }, Integer { i_value: 101 }, Array { content: [1,2,3,4] }];
c = 0;
for v in l {
    a = v.add();
    if a { c += a; }
}
\"{l}:{c}\"",
    )
    .result(Value::str(
        "[Text {t_value:\"123\"},Integer {i_value:101},Array {content:[1,2,3,4]}]:234",
    ));
}

#[test]
fn types() {
    code!(
        "enum Value {
    S { data: sorted<Sort[nr]> },
    I { data: index<Ind[nr]> },
    H { data: hash<Elm[name]> }
}

struct Sort {
    nr: i32,
    d: Value
}

struct Ind {
    nr: i32,
    d: Value
}

struct Elm {
    name: text,
    d: Value
}
"
    )
    .expr("1")
    .result(Value::Int(1));
}
