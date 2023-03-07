// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;
mod testing;

use dryopea::data::{Type, Value};

#[test]
fn expr_integer() {
    expr!("1").result(Value::Int(1)).tp(Type::Integer);
}

#[test]
fn expr_add() {
    expr!("1 + 2").result(Value::Int(3));
}

#[test]
fn expr_multiply() {
    expr!("1 + 2 * 3").result(Value::Int(7));
}

#[test]
fn expr_brackets() {
    expr!("(1 + 2)").result(Value::Int(3));
}

#[test]
fn expr_multi_brackets() {
    expr!("(1 + 2) * 3").result(Value::Int(9));
}

#[test]
fn expr_add_null() {
    expr!("1 + null").tp(Type::Integer);
}

#[test]
fn expr_float() {
    expr!("1.1").result(Value::Float(1.1));
}

#[test]
fn expr_str() {
    expr!("\"1.1\"").result(Value::Text("1.1".to_string()));
}

#[test]
fn expr_lower() {
    expr!("2 * 2 < 3").result(Value::Int(0)).tp(Type::Boolean);
}

#[test]
fn expr_prio_lt() {
    expr!("2 + 1 <= 3").result(Value::Int(1)).tp(Type::Boolean);
}

#[test]
fn expr_inline_if() {
    expr!("if 2 + 2 > 3 {\"A\"} else {\"B\"}").result(Value::Text("A".to_string()));
}

#[test]
fn expr_variable() {
    expr!("a = 1; a").result(Value::Int(1));
}

#[test]
fn expr_variables() {
    expr!("a = 1; b = 12; 0 + b + a").result(Value::Int(13));
}

#[test]
fn expr_zero_divide() {
    expr!("2 / (3 - 2 - 1)").tp(Type::Integer);
}

#[test]
fn expr_long() {
    expr!("a = -2l; 3l != abs(a)")
        .result(Value::Int(1))
        .tp(Type::Boolean);
}

#[test]
fn for_loop() {
    expr!("b = 0; for a in 0..5 { b+=a }; b").result(Value::Int(10));
}

#[test]
fn continue_loop() {
    expr!("b = 0; for a in 0..10 { if a == 2 {continue} if a > 5 {return b} b += a }; b")
        .result(Value::Int(13));
}

#[test]
fn text_length() {
    expr!("t = \"some\"; t+=\"thing\"; t.len() + len(t)").result(Value::Int(18));
}

#[test]
fn auto_convert() {
    expr!("10l * 2").result(Value::Long(20)).tp(Type::Long);
}

#[test]
fn call_routine() {
    code!("fn routine(a: integer) -> integer {if a > 4 {return a+1} else {return 1}; 2}")
        .expr("routine(5) + routine(2)")
        .result(Value::Int(7));
}

#[test]
fn call_void() {
    code!("fn routine(a: integer) {return;}")
        .expr("routine(5)")
        .warning("Parameter a is never read in call_void line 1:25");
}
