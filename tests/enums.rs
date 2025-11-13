// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Testing framework
extern crate dryopea;

use dryopea::data::Value;

mod testing;

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
    .expr("null")
    .result(Value::Null);
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
    .expr("null")
    .result(Value::Null);
}
