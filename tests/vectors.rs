// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

use dryopea::data::Value;

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
/*
#[test]
fn format_vector() {
    expr!(
        // indirect iterators:  {v[1..3]} {v[$ < 3]} =>  [2, 3] [1, 2]
        "v=[1, 2, 4, 8];
v += [16];
\"{v} {v.len()} {v[2]} {for x in v if x >= 4 {x/2}}\""
    )
    .result(Value::Text("[1, 2, 4, 8, 16] 5 4 [2, 4, 8]".to_string()));
}

#[test]
fn map_vector() {
    expr!("v=..10; w=[for x in v if x < 4 {x * 3}]; \"{w}\"")
        .result(Value::Text("[0, 3, 6, 9]".to_string()));
}

#[test]
fn loop_variables() {
    expr!(
        "\"{for x in ..10 if !x#first && x#index % 3 == 0 || x#last {if x < 10 { x#continue }; x * 2}}\"").result(
        Value::Text("[6, 12, 18, 20]".to_string())
    );
}

#[test]
fn store_iterator() {
    expr!("v=[1, 2, 3]; v += v[1..2]; v += [for x in v {x*2}]; \"{v}\"")
        .result(Value::Text("[1, 2, 3, 2, 3, 2, 4, 6, 4, 6]".to_string()));
}

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
\" {v} \"",
        )
        .result(Value::Text(
            " [Elm {a:1, b:2}, Elm {a:12, b:13}, Elm {a:4, b:6}] ".to_string(),
        ));
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
fn sum_vector() {
    code!("fn sum(v: vector<integer>) -> integer { t = 0; for i in v { t += i }; t}")
        .expr("Sum([1, 2, 3, 4, 5])")
        .result(Value::Int(15));
}

#[test]
fn ordered_vector() {
    code!("struct Elm {key: text, value: integer}
struct Db {map: vector<Elm[key desc]>}")
        .expr(
            "db=Db {map: [Elm {key: \"One\", value: 1}, Elm {key: \"Two\", value: 2}]};
db.map += [Elm {key: \"Three\", value: 3}, Elm {key: \"Four\", value: 4}];
sum = 0;
for v in db.map {
  sum = sum * 10 + v.value;
};
sum = sum * 10 + db.map[$.key == \"Three\"].value;
sum",
        )
        .result(Value::Int(41323));
}
*/
