// Copyright (c) 2021-2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::{Type, Value};

const INTEGER: Type = Type::Integer(i32::MIN + 1, i32::MAX as u32);

#[test]
fn aa_print() {
    code!("fn test() { OpPrint(\"hi!\"); }");
}

#[test]
fn expr_integer() {
    expr!("1").result(Value::Int(1)).tp(INTEGER);
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
    expr!("1 + null").tp(INTEGER);
}

#[test]
fn expr_float() {
    expr!("1.1").result(Value::Float(1.1));
}

#[test]
fn expr_str() {
    expr!("\"1.1\"").result(Value::str("1.1"));
}

#[test]
fn expr_lower() {
    expr!("2 * 2 < 3").result(Value::Boolean(false));
}

#[test]
fn expr_prio_lt() {
    expr!("2 + 1 <= 3").result(Value::Boolean(true));
}

#[test]
fn expr_inline_if() {
    expr!("if 2 + 2 > 3 {\"A\"} else {\"B\"}").result(Value::str("A"));
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
    expr!("2 / (3 - 2 - 1)").tp(INTEGER);
}

#[test]
fn expr_long() {
    expr!("a = -2l; 3l != abs(a)").result(Value::Boolean(true));
}

#[test]
fn mutating_operators() {
    expr!("a = 12; a -= 6; a *= 3; a /= 2; a += 1; a")
        .result(Value::Int(10))
        .tp(INTEGER);
}

#[test]
fn for_loop() {
    expr!("b = 0; for a in 0..5 { b+=a }; b").result(Value::Int(10));
}

#[test]
fn for_long() {
    expr!("b = 0l; for a in 10l..=20l { b+=a }; b").result(Value::Long(165));
}

#[test]
fn reverse_loop() {
    expr!("b = 0; for a in rev(1..=6) { b=b*10+a }; b").result(Value::Int(654321));
}

#[test]
fn reverse() {
    expr!("b = 0; for a in rev(0..6) { b=b*10+a }; b").result(Value::Int(543210));
}

#[test]
fn extended_for() {
    expr!("b = 0; for a in 0..=5 { b+=a }; b").result(Value::Int(15));
}

#[test]
fn continue_loop() {
    code!("fn routine() -> integer {b = 0; for a in 0..10 { if a == 2 {continue} if a > 5 {return b} b += a }; b}")
    .expr("routine()").result(Value::Int(13));
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
        .warning("Parameter a is never read at call_void:1:25");
}

#[test]
fn compare() {
    code!("enum T{A, C, B}\nfn count(v: T) -> integer { if v > C { 2 } else { 1 } }")
        .expr("count(A) + count(B) + count(B)")
        .result(Value::Int(5));
}

#[test]
fn convert_to_long() {
    expr!("123 as long + 2").result(Value::Long(125));
}

#[test]
fn convert_text() {
    expr!("\"123\" as long + 2").result(Value::Long(125));
}

#[test]
fn convert_to_int() {
    expr!("123 as integer + 2").result(Value::Int(125));
}

#[test]
fn convert_text_to_int() {
    expr!("\"123\" as integer + 2").result(Value::Int(125));
}

#[test]
fn boolean_named() {
    expr!("123 and (12 or false)").result(Value::Boolean(true));
}

#[test]
fn boolean_symbols() {
    expr!("123 || (12 && false)").result(Value::Boolean(true));
}

#[test]
fn logical() {
    expr!("(1 << 8) - 3 & 127").result(Value::Int(125));
}

#[test]
fn to_enum() {
    code!("enum Number { One, Two, Three, Four }")
        .expr("\"Two\" as Number < \"Four\" as Number")
        .result(Value::Boolean(true));
}

#[test]
fn int_to_enum() {
    code!("enum Number { One, Two, Three, Four }")
        .expr("1 as Number < 3 as Number")
        .result(Value::Boolean(true));
}

#[test]
fn null_enum() {
    // TODO this is the current situation, and not what we eventually want.
    code!("enum Number { One, Two, Three, Four }")
        .expr("null < 3 as Number")
        .result(Value::Boolean(false));
}

#[test]
fn inner_loop() {
    expr!(
        "
s = \"\";
for i in 0..10 {
    for j in 0..10 {
        if j > i {
            i#continue
        }
        s += \"{i}{j},\";
        if len(s) > 100 {
            i#break
        }
    }
}
s
"
    )
    .result(Value::str("00,10,11,20,21,22,30,31,32,33,40,41,42,43,44,50,51,52,53,54,55,60,61,62,63,64,65,66,70,71,72,73,74,75,"));
}

#[test]
fn recursion() {
    code!(
        "fn first(s: State, c: integer) -> integer {
	if s == Start {
		s = Ongoing
	} else if c > 10 {
		s = Halt
	}
	second(s, c)
}

fn second(s: State, c: integer) -> integer {
	if s != Halt {
		first(s, c + 1)
	} else {
		1 + c
	}
}

enum State {
	Start,
	Ongoing,
	Halt
}"
    )
    .expr("first(Start, 0)")
    .result(Value::Int(12));
}
