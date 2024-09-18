#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]

extern crate dryopea;
use dryopea::database::Stores;
use dryopea::external::*;

fn init(db: &mut Stores) {}

fn test(stores: &mut Stores) -> i64 {
    op_mul_long((10_i64), (op_conv_long_from_int(2_i32)))
}

#[test]
fn code_auto_convert() {
    let mut db = Stores::new();
    init(&mut db);
    assert_eq!(20, test(&mut db));
}
