// Copyright (c) 2021-2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

use dryopea::database::{Stores, Str};
use dryopea::hash;
use dryopea::keys::{Content, DbRef};

#[test]
pub fn record() {
    let mut stores = Stores::new();
    let e = stores.enumerate("Category");
    stores.value(e, "Daily");
    stores.value(e, "Hourly");
    stores.value(e, "Weekly");
    let s = stores.structure("Data");
    stores.field(s, "name", stores.name("text"));
    let cat = stores.field(s, "category", stores.name("Category"));
    let size = stores.field(s, "size", stores.name("integer"));
    let amount = stores.field(s, "amount", stores.name("float"));
    let percentage = stores.field(s, "percentage", stores.name("single"));
    let calc = stores.field(s, "calc", stores.name("long"));
    stores.finish();
    assert_eq!(stores.size(stores.name("Data")), 33);
    assert_eq!(stores.enum_val(e, 2), "Hourly");
    assert_eq!(stores.position(s, amount), 8);
    assert_eq!(stores.position(s, cat), 32);
    assert_eq!(stores.position(s, size), 24);
    assert_eq!(stores.position(s, percentage), 28);
    assert_eq!(stores.position(s, calc), 16);
    //stores.dump_types();
    let result = stores.database(1234);
    let test_string = "{ name: \"Hello World!\", category: Hourly, size: 12345, percentage: 0.15 }";
    stores.parse(test_string, s, &result);
    let mut check = String::new();
    stores.show(&mut check, &result, s, true);
    assert_eq!(test_string, check);
    let pf = Stores::get_field(&result, stores.position(s, percentage) as u32);
    assert_eq!(stores.store(&pf).get_single(pf.rec, pf.pos), 0.15);
    stores.store_mut(&pf).set_single(pf.rec, pf.pos, 0.125);
    check.clear();
    stores.show(&mut check, &result, s, true);
    assert_ne!(test_string, check);
    assert_eq!(
        stores.parse_message("{blame:\"nothing\"}", s),
        "line 1:7 path:blame"
    );
}

#[test]
pub fn vector() {
    let mut stores = Stores::new();
    let vec = stores.vector(stores.name("integer"));
    let v = stores.structure("Vector");
    stores.field(v, "numbers", vec);
    stores.finish();
    //stores.dump_types();
    let db = stores.database(2);
    let into = DbRef {
        store_nr: db.store_nr,
        rec: db.rec,
        pos: 4,
    };
    stores.set_default_value(vec, &into);
    let test_string = "{ numbers: [ 1, 2, 55, 11, 22 ]\n}";
    stores.parse(test_string, v, &db);
    let mut check = String::new();
    stores.show(&mut check, &db, v, true);
    assert_eq!(test_string, check);
}

#[test]
pub fn vector_record() {
    let mut stores = Stores::new();
    let s = stores.structure("Elm");
    stores.field(s, "n", stores.name("text"));
    stores.field(s, "c", stores.name("integer"));
    let v = stores.vector(s);
    stores.finish();
    // stores.dump_types();
    let db = stores.database(8);
    let into = DbRef {
        store_nr: db.store_nr,
        rec: db.rec,
        pos: 4,
    };
    stores.set_default_value(v, &into);
    let test_string = "[ { n: \"hi\", c: 10 },\n  { n: \"world\", c: 2 } ]";
    stores.parse(test_string, v, &into);
    let mut check = String::new();
    stores.show(&mut check, &into, v, true);
    assert_eq!(test_string, check);
}

#[test]
pub fn sorted_vector() {
    let mut stores = Stores::new();
    let s = stores.structure("Elm");
    let c = stores.field(s, "cat", stores.name("integer"));
    let n = stores.field(s, "name", stores.name("text"));
    stores.field(s, "value", stores.name("float"));
    let v = stores.sorted(s, &[(c, false), (n, true)]);
    stores.finish();
    //stores.dump_types();
    let db = stores.database(8);
    let into = DbRef {
        store_nr: db.store_nr,
        rec: db.rec,
        pos: 4,
    };
    stores.set_default_value(v, &into);
    let data = "[
        {cat:1, name:\"first\",value:1.23},
        {cat:1, name:\"second\",value:1.34},
        {cat:1, name:\"third\",value:1.45},
        {cat:2, name:\"first\",value:1.56},
        {cat:2, name:\"second\",value:1.67},
        {cat:2, name:\"third\",value:1.78},
        {cat:3, name:\"first\",value:1.89}
    ]";
    stores.parse(data, v, &into);
    let mut check = String::new();
    stores.show(&mut check, &into, v, true);
    assert_eq!(
        "[ { cat: 3, name: \"first\", value: 1.89 },
  { cat: 2, name: \"first\", value: 1.56 },
  { cat: 2, name: \"second\", value: 1.67 },
  { cat: 2, name: \"third\", value: 1.78 },
  { cat: 1, name: \"first\", value: 1.23 },
  { cat: 1, name: \"second\", value: 1.34 },
  { cat: 1, name: \"third\", value: 1.45 } ]",
        check
    );
}

#[test]
pub fn hash() {
    let mut stores = Stores::new();
    let s = stores.structure("Elm");
    let n = stores.field(s, "name", stores.name("text"));
    let c = stores.field(s, "cat", stores.name("integer"));
    stores.field(s, "value", stores.name("float"));
    let v = stores.hash(s, &[n, c]);
    stores.finish();
    //stores.dump_types();
    let db = stores.database(8);
    let into = DbRef {
        store_nr: db.store_nr,
        rec: db.rec,
        pos: 4,
    };
    stores.set_default_value(v, &into);
    let data = "[
        {cat:1, name:\"first\",value:1.23},
        {cat:1, name:\"second\",value:1.34},
        {cat:1, name:\"third\",value:1.45},
        {cat:2, name:\"first\",value:1.56},
        {cat:2, name:\"second\",value:1.67},
        {cat:2, name:\"third\",value:1.78},
        {cat:3, name:\"first\",value:1.89}
    ]";
    stores.parse(data, v, &into);
    let key = [Content::Str(Str::new("second")), Content::Long(2)];
    let mut check = String::new();
    stores.show(
        &mut check,
        &hash::find(&into, &stores.allocations, stores.keys(v), &key),
        s,
        false,
    );
    assert_eq!(check, "{name:\"second\",cat:2,value:1.67}");
}

#[test]
pub fn array_record() {
    let mut stores = Stores::new();
    let s = stores.structure("Elm");
    let n = stores.field(s, "n", stores.name("text"));
    stores.field(s, "c", stores.name("integer"));
    let v = stores.vector(s);
    let h = stores.hash(s, &[n]);
    let m = stores.structure("Main");
    stores.field(m, "list", v);
    stores.field(m, "search", h);
    stores.finish();
    assert_eq!(stores.dump_type("Elm"), "Elm[12]:{n:text[4], c:integer[8]}");
    assert_eq!(
        stores.dump_type("Main"),
        "Main[12]:{list:array<Elm>[4] other [1], search:hash<Elm[n]>[8] other [65535]}"
    );
    let mut into = stores.database(2);
    stores.set_default_value(m, &into);
    let test_string = "{list:[{n:\"hello\",c:10},{n:\"world\",c:2}]}";
    stores.parse(test_string, m, &into);
    let mut check = String::new();
    stores.show(&mut check, &into, m, false);
    assert_eq!(test_string, check);
    let mut check = String::new();
    into.pos = 8;
    let keys = stores.keys(h).to_vec();
    hash::validate(&into, &stores.allocations, &keys);
    let key = [Content::Str(Str::new("hello"))];
    let rec = hash::find(&into, &stores.allocations, &keys, &key);
    stores.show(&mut check, &rec, s, false);
    assert_eq!(check, "{n:\"hello\",c:10}");
}

#[test]
pub fn ordered_record() {
    let mut stores = Stores::new();
    let s = stores.structure("Elm");
    let n = stores.field(s, "n", stores.name("text"));
    stores.field(s, "c", stores.name("integer"));
    let v = stores.sorted(s, &[(n, true)]);
    let h = stores.hash(s, &[n]);
    let m = stores.structure("Main");
    stores.field(m, "list", v);
    stores.field(m, "search", h);
    stores.finish();
    assert_eq!(stores.dump_type("Elm"), "Elm[12]:{n:text[4], c:integer[8]}");
    assert_eq!(
        stores.dump_type("Main"),
        "Main[12]:{list:ordered<Elm[n]>[4] other [1], search:hash<Elm[n]>[8] other [65535]}"
    );
    let db = stores.database(2);
    let mut into = DbRef {
        store_nr: db.store_nr,
        rec: db.rec,
        pos: 0,
    };
    stores.set_default_value(m, &into);
    let test_string = "{list:[{n:\"hello\",c:10},{n:\"world\",c:2}]}";
    stores.parse(test_string, m, &into);
    let mut check = String::new();
    stores.show(&mut check, &into, m, false);
    assert_eq!(test_string, check);
    let mut check = String::new();
    let key = [Content::Str(Str::new("world"))];
    into.pos = 8;
    stores.show(
        &mut check,
        &hash::find(&into, &stores.allocations, stores.keys(h), &key),
        s,
        false,
    );
    assert_eq!(check, "{n:\"world\",c:2}");
}
