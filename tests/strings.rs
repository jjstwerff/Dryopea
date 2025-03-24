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

#[test]
fn string_fn() {
    code!(
        "
fn to_text() -> text {
    res = \"aa \";
    for _i in 0..2 {
        res += \"b\";
    }
    res + \" cc\"
}
    "
    )
    .expr("\"1{to_text()}2\"")
    .result(Value::str("1aa bb cc2"));
}

#[test]
fn string_scope() {
    expr!(
        "
  a=1l;
  b=\"\";
  for n in 1..4 {
    t=\"1\";
    b+=\"n\" + \":{n}\" + \"=\";
    for _m in 1..n {
      t+=\"2\";
    };
    b += t+\" \";
    a += t as long
  };
  \"{a} via {b}\"
"
    )
    .result(Value::str("136 via n:1=1 n:2=12 n:3=122 "));
}

#[test]
fn starts() {
    expr!("\"something\".starts_with(\"someone\")").result(Value::Boolean(false));
}

#[test]
fn ends() {
    expr!("v = \"someth\" + \"ing\"; v.ends_with(\"thing\")").result(Value::Boolean(true));
}

#[test]
fn find() {
    expr!("v = \"something\"; v.find(\"t\" + \"h\")").result(Value::Int(4));
}

#[test]
fn contains() {
    expr!("t = \"longer\"; v = \"a longer text\"; v.contains(\"a {t}\")")
        .result(Value::Boolean(true));
}

#[test]
fn reference() {
    code!(
        "fn add(a: &text, b: text=\" world!\") {
    a += b;
}"
    )
    .expr("v = \"Hello\"; add(v); v")
    .result(Value::str("Hello world!"));
}

#[test]
fn default_ref() {
    code!(
        "fn add(a: text, b: &text=\"var\") -> text {
    b += \"_\" + a;
    b
}"
    )
    .expr("add(\"1234\")")
    .result(Value::str("var_1234"));
}

#[test]
fn block() {
    expr!("s = \"1\"; s += \"2\"; s").result(Value::str("12"));
}

#[test]
fn index_block() {
    expr!("s = \"1😊2\"; s[1]").result(Value::str("😊"));
}

#[test]
fn trim_block() {
    expr!("s = \" 12   \"; trim(s)").result(Value::str("12"));
}

#[test]
fn pass_block() {
    expr!("s = \"12 \"; d = s; trim(d)").result(Value::str("12"));
}

#[test]
fn call() {
    code!("fn choice(a: text, b: text) -> text { if len(a) > len(b) { a } else { b } }")
        .expr("choice(\"{1:03}\", \"{2}1\") + choice(\"2\", \"\")")
        .result(Value::str("0012"));
}

// TODO command line arguments  env::args_os() -> Args iterator     (for now Args vector)
// TODO environment variables  evn::var_os(name)  set_var  vars_os() -> iterator (Vars vector)
// TODO current dir / current exe / home_dir
