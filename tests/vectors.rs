// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn access() {
    expr!("v=[1, 10, 100]; v[1]").result(Value::Int(10));
}

#[test]
fn vectors() {
    expr!(
        "v=[1, 2, 1+2];
v += [4];
t = 0;
for e in v { t += e };
v[1 + 2] = 5;
t + v[0] + v[-1] + v.len()"
    )
    .result(Value::Int(20));
}

#[test]
fn enum_vector() {
    code!("enum Val {A, B, C}")
        .expr("v=[A,A,B,B]; v[2] as integer")
        .result(Value::Int(2));
}

#[test]
fn append_vector() {
    code!("enum Val {A, B, C}")
        .expr("v=[A,A]; v += [B] + [C] + [A]; \"{v}\"")
        .result(Value::str("[A,A,B,C,A]"));
}

#[test]
fn iter_vector() {
    expr!(
        "v=[1, 2, 4, 8];
c = 0;
for e in v[1..3] {
  c = c * 10 + e;
}
for e in v[2..] {
  c = c * 10 + e;
}
assert(!v[4], \"Incorrect reading outside vector bounds\");
c"
    )
    .result(Value::Int(2448));
}

#[test]
fn iter_rev_vector() {
    expr!(
        "v=[1, 2, 4, 8];
c = 0;
for e in v[rev(0..=3)] {
  c = c * 10 + e;
}
c"
    )
    .result(Value::Int(8421));
}

#[test]
fn format_vector() {
    expr!(
        // filtered iterators:  {v[$ < 3]}
        "v=[1, 2, 4, 8];
v += [16];
\"{v} {v.len()} {v[2]} {v[1..3]} {v[rev(1..=3)]} {for x in v if x >= 4 {x/2}}\""
    )
    .result(Value::str("[1,2,4,8,16] 5 4 [2,4] [8,4,2] [2,4,8]"));
}

#[test]
fn parse_vector() {
    expr!("a = \"[ 1.2, -10.3, 1.812e4, 1.001e-8 ]\" as vector<float>; a[2] + a[3]")
        .result(Value::Float(1.812e4 + 1.001e-8));
}

/*
#[test]
fn map_vector() {
    expr!("v=[0,1,2,3,4,5,6,7,8,9];
w=[for x in v if x < 4 {x * 3}];
\"{w}\"").result(Value::str("[0, 3, 6, 9]"));
}
*/
#[test]
fn loop_variables() {
    expr!("\"{for x in 0..10 if x != 0 && x % 3 == 0 {if x#first { x } else {x * 2}}}\"")
        .result(Value::str("[3,12,18]"));
}
/*
#[test]
fn store_iterator() {
    expr!("v=[1, 2, 3]; v += v[1..2]; v += [for x in v {x*2}]; \"{v}\"")
        .result(Value::Text("[1, 2, 3, 2, 3, 2, 4, 6, 4, 6]".to_string()));
}
*/
#[test]
fn format_object() {
    code!("struct Elm {a:integer, b:integer}")
        .expr(
            "v=[
    Elm{a:1, b:2},
    Elm{a:12, b:13},
    Elm{a:4, b:5}
];
v[2].b=6;
\"{v} sizeof {sizeof(Elm)}\"",
        )
        .result(Value::str("[{a:1,b:2},{a:12,b:13},{a:4,b:6}] sizeof 8"));
}

#[test]
fn object_vectors() {
    code!("struct Elm {a:integer, b:integer}")
        .expr(
            "v=[Elm{a:1, b:2}, Elm{a:12, b:13}, Elm{a:4, b:5}]; v[2].b=6; e=v[0]; e.b + v[1].a + v[2].b",
        )
        .result(Value::Int(20));
}

#[test]
fn parse_objects() {
    code!("struct Elm {n:text, c:integer}")
        .expr("v = \"[ {{n:'hi', c:10 }}, {{n:'world', c:2 }} ]\" as vector<Elm>; \"{v}\"")
        .result(Value::str("[{n:\"hi\",c:10},{n:\"world\",c:2}]"));
}

#[test]
fn sum_vector() {
    code!("fn sum(v: vector<integer>) -> integer { t = 0; for i in v { t += i }; t}")
        .expr("sum([1, 2, 3, 4, 5])")
        .result(Value::Int(15));
}

#[test]
fn empty_vector() {
    expr!(
        "a = [];
for v in 1..4 { a += [ v * 10 ] };
\"{a}\""
    )
    .result(Value::str("[10,20,30]"));
}

#[test]
fn growing_vector() {
    expr!(
        "a = [];
for v in 1..400 { a += [ v * 10 ] };
sum = 0;
for elm in a { sum += elm }
\"{sum}\""
    )
    .result(Value::str("798000"));
}

#[test]
fn sorted_vector() {
    code!(
        "struct Elm {key: text, value: integer}
struct Db {map: sorted<Elm[-key]>}"
    )
    .expr(
        "db=Db {map: [Elm {key: \"One\", value: 1}, Elm {key: \"Two\", value: 2}]};
db.map += [Elm {key: \"Three\", value: 3}, Elm {key: \"Four\", value: 4}];
assert(db.map[\"Two\"].value == 2, \"Two element\");
assert(db.map[\"Four\"], \"Four element\");
assert(!db.map[\"Five\"], \"No element\");
sum = 0;
for v in db.map {
  sum = sum * 10 + v.value;
};
sum = sum * 10 + db.map[\"Three\"].value;
sum",
    )
    .result(Value::Int(23143));
}

#[test]
fn sorted_iterator() {
    code!(
        "struct Elm {nr: integer, key: text, value: integer}
struct Db {map: sorted<Elm[-nr,key]>}"
    )
    .expr(
        "db=Db {map: [
  Elm {nr: 101, key: \"One\", value: 1},
  Elm {nr: 92, key: \"Two\", value: 2},
  Elm {nr: 83, key: \"Three\", value: 3},
  Elm {nr: 83, key: \"Four\", value: 4},
  Elm {nr: 83, key: \"Five\", value: 5},
  Elm {nr: 63, key: \"Six\", value: 6},
  Elm {nr: 61, key: \"Seven\", value: 7},
]};
sum = 0;
assert(db.map[83,\"Five\"].value == 5, \"Incorrect element {db.map[83,\"Five\"].value}\");
for v in db.map[84..=63,\"Six\"] {
  sum = sum * 10 + v.value;
};
sum",
    )
    .result(Value::Int(5436));
}

#[test]
fn fill_result() {
    code!(
        "pub fn fill() -> vector<text> {
    result = [];
    result += [\"aa\"];
    result += [\"bb\"];
    result
}"
    )
    .expr("t = fill(); \"{t}\"")
    .result(Value::str("[\"aa\",\"bb\"]"));
}

#[test]
fn combination_hash() {
    code!(
        "struct Count { t: text, v: integer};
struct Counting { v: vector<Count>, h: hash<Count[t]> };
fn fill(c: Counting) {
  c.v = [
    {t:\"One\", v:1},
    {t:\"Two\", v:2},
  ];
  c.v += [
    {t:\"Three\", v:3},
    {t:\"Four\", v:4},
    {t:\"Five\", v:5},
    {t:\"Six\", v:6},
    {t:\"Seven\", v:7},
    {t:\"Eight\", v:8},
    {t:\"Nine\", v:9},
    {t:\"Ten\", v:10},
    {t:\"Eleven\", v:11},
    {t:\"Twelve\", v:12},
    {t:\"Thirteen\", v:13}
  ];
}"
    )
    .expr(
        "c = Counting {};
  fill(c);
  assert(!c.h[\"None\"], \"No element\");
  c.h[\"Five\"].v + c.h[\"Seven\"].v",
    )
    .result(Value::Int(12));
}

#[test]
fn hash() {
    code!(
        "struct Keyword {
    name: text
}
struct Data { h: hash<Keyword[name]> }"
    )
        .expr(
            "c = Data {};
  c.h = [ { name: \"one\" }, { name: \"two\" } ];
  c.h += [ { name: \"three\" }, { name: \"four\" } ];
  assert(!c.h[\"None\"], \"No element\");
  assert(\"{c}\" == \"{{h:[{{name:\\\"four\\\"}},{{name:\\\"one\\\"}},{{name:\\\"three\\\"}},{{name:\\\"two\\\"}}]}}\", \"Output hash was {c}\");
  if c.h[\"three\"] { 12 } else { 0 }",
        )
        .result(Value::Int(12));
}

#[test]
fn multi_hash() {
    code!(
        "enum Cat { A, B, C };
struct Count { c: Cat, t: text, v: integer};
struct Counting { v: sorted<Count[t,v]>, h: hash<Count[c,t]> };
fn fill(c: Counting) {
  c.v = [
    {c:A, t:\"One\", v:1},
    {c:B, t:\"Two\", v:2},
    {c:C, t:\"Two\", v:20},
    {c:A, t:\"Three\", v:3},
    {c:C, t:\"Four\", v:4}
  ]
}"
    )
    .expr("c = Counting {}; fill(c); c.h[A,\"Three\"].v + c.h[C,\"Two\"].v + c.v[\"Four\",4].v")
    .result(Value::Int(27));
}

#[test]
fn index_iterator() {
    code!(
        "struct Elm {nr: integer, key: text, value: integer}
struct Db {map: index<Elm[nr,-key]>}"
    )
    .expr(
        "db=Db {map: [
  Elm {nr: 101, key: \"One\", value: 1},
  Elm {nr: 92, key: \"Two\", value: 2},
  Elm {nr: 83, key: \"Three\", value: 3},
  Elm {nr: 83, key: \"Four\", value: 4},
  Elm {nr: 83, key: \"Five\", value: 5},
  Elm {nr: 63, key: \"Six\", value: 6},
]};
assert(db.map[101,\"One\"].value == 1 , \"Missing element\");
sum = 0;
for v in db.map[83..92,\"Two\"] {
  sum = sum * 10 + v.value;
};
assert(!db.map[12,\"\"], \"No element\");
assert(!db.map[83,\"One\"], \"No element\");
sum",
    )
    .result(Value::Int(345));
}

#[test]
fn fill_fn() {
    code!(
        "pub struct Data {
    name: character,
    number: integer
}

fn data(n: text) -> vector<Data> {
    res = [];
    nr = 0;
    for ch in n {
        res += [Data {name: ch, number: nr}];
        nr += 1;
    }
    res
}"
    )
    .expr("d = data(\"test\"); \"{d}\"")
    .result(Value::str(
        "[{name:'t',number:0},{name:'e',number:1},{name:'s',number:2},{name:'t',number:3}]",
    ));
}

#[test]
fn text_vector() {
    code!("fn test() { op = [\"+=\", \"*=\"]; for o in op { print(o); }; }");
}
