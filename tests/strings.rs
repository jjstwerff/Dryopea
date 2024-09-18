// Copyright (c) 2021-2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn append() {
    expr!("a=\"♥😃\" + \"1\" + \"2\"; a").result(Value::str("♥😃12"));
}

#[test]
fn str_index() {
    expr!("a=\"12345\"; a[2]").result(Value::str("3"));
}
#[test]
fn utf8_index() {
    expr!("a=\"♥😃\"; a[0] + a[1] + a[2] + a[3] + a[4] + a[5] + a[6] + \".\" + a[7]")
        .result(Value::str("♥♥♥😃😃😃😃."));
}

#[test]
fn str_len() {
    expr!("a=\"12345\";a.len() * 100 + len(\"😃\") * 10 + len(\"♥\")").result(Value::Int(543));
}

#[test]
fn sub_str() {
    expr!("a=\"12345\";a[1..len(a)-1]").result(Value::str("234"));
}

#[test]
fn sub_open() {
    expr!("a=\"12345\";a[2..]").result(Value::str("345"));
}

#[test]
fn sub_utf8() {
    expr!("a=\"12😊🙃45\";a[1..7]").result(Value::str("2😊🙃"));
}

#[test]
fn iter() {
    expr!(
        "a=[];
b=[];
for c in \"123😊🙃😋8\" {
    a += [c];
    b += [c#index]
};
\"{a} indexes:{b}\""
    )
    .result(Value::str(
        "[\"1\",\"2\",\"3\",\"😊\",\"🙃\",\"😋\",\"8\"] indexes:[1,2,3,7,11,15,16]",
    ));
}
