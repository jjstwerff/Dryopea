// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn constants() {
    expr!("round(PI * 1000.0)").result(Value::Float(3142.0));
}

#[test]
fn exponents() {
    expr!("log(4.0^5, 2)").result(Value::Float(10.0));
}

#[test]
fn trigonometric() {
    expr!("ceil(sin(PI) + cos(PI) * 1000)").result(Value::Float(-1000.0));
}
