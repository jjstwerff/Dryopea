#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn routine(stores: &mut Stores, var_0: i32) -> i32 {
    if op_lt_int((4_i32), (var_0)) {
        return op_add_int((var_0), (1_i32));
    } else {
        return 1_i32;
    };
    2_i32
}

fn test(stores: &mut Stores) -> i32 {
    op_add_int((routine(stores, 5_i32)), (routine(stores, 2_i32)))
}

#[test]
fn code_call_routine() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!(7, test(&mut stores));
}
