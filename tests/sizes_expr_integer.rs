#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external;
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> i32 {
    let var_0 = 1_i32;
    op_add_int(
        (op_add_int(
            (op_add_int((4_i32), (4_i32))),
            (op_mul_int((10_i32), (4_i32))),
        )),
        (op_mul_int((100_i32), (4_i32))),
    )
}

#[test]
fn code_expr_integer() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!(448, test(&mut stores));
}
