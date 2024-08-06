#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {
    let e = db.enumerate("T");
    db.value(e, "A".to_string());
    db.value(e, "C".to_string());
    db.value(e, "B".to_string());
}

fn count(stores: &mut Stores, var_0: u8) -> i32 {
    if var_0 >= 2_i32 as u8 {
        2_i32
    } else {
        1_i32
    }
}

fn test(stores: &mut Stores) -> i32 {
    op_add_int(
        (op_add_int((count(stores, 1_i32 as u8)), (count(stores, 3_i32 as u8)))),
        (count(stores, 3_i32 as u8)),
    )
}

#[test]
fn code_compare() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!(5, test(&mut stores));
}
