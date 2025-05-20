// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;
mod testing;

use dryopea::data::Value;

#[test]
fn format_escaping() {
    expr!("\"ab{{cd}}e\"").result(Value::str("ab{cd}e"));
}

#[test]
fn format_expression() {
    expr!("\"ab{1+2+32:#x}c{12:o}d{391}e{12:+4}f{1:03}g{42:b}h\"").result(Value::str(&format!(
        "ab{:#x}c{:o}d{}e{:+4}f{:03}g{:b}h",
        35, 12, 391, 12, 1, 42
    )));
}

#[test]
fn format_boolean() {
    expr!("\"1{true:^7}2\"").result(Value::str(&format!("1{:^7}2", true)));
}

#[test]
fn format_text() {
    expr!("vr=\"abc\"; \"1{vr:<2+3}2{vr}3{vr:6}4{vr:>7}\"").result(Value::str(&format!(
        "1{:<5}2abc3{:6}4{:>7}",
        "abc", "abc", "abc"
    )));
}

#[test]
fn format_float() {
    expr!("\"a{1.2:4.2}b{1.34}c{1.4:5}d{334.1:.2}e\"").result(Value::str(&format!(
        "a{:4.2}b{}c{:5}d{:.2}e",
        1.2, 1.34, 1.4, 334.1
    )));
}

#[test]
fn format_long() {
    expr!("\"a{1l+1:+4}b{12l as integer}c {2l * (4l % 6l) >= 8} d\"")
        .result(Value::str(&format!("a{:+4}b12c true d", 2)));
}

#[test]
fn format_single() {
    expr!("\"a{0.1f + 2 * 1.0f}b\"").result(Value::str(&format!("a{}b", 2.1f32)));
}

#[test]
fn format_range() {
    expr!("\"a{for x in 1..7 {x*2}:02}b\"").result(Value::str("a[02,04,06,08,10,12]b"));
}

#[test]
fn format_chars() {
    // This is still problematic, as c#index points to the next character instead of the current.
    expr!("txt=\"12😊🙃45\"; \"a{for c in txt[2..-1] {\"{c#index}:{c}\"}}b\"")
        .result(Value::str("a[4:😊,8:🙃,9:4]b"));
}

#[test]
fn format_sub() {
    expr!("txt=\"12😊🙃45\"; \"a{txt[2..-1]}b\"").result(Value::str("a😊🙃4b"));
}
