#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::{DbRef, KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> String {
    {
        let mut var_0 = "a".to_string();
        var_0 = ((var_0) + &(format_float((1.2_f64), (4_i32), (2_i32))));
        var_0 = ((var_0) + &("b".to_string()));
        var_0 = ((var_0) + &(format_float((1.34_f64), (0_i32), (0_i32))));
        var_0 = ((var_0) + &("c".to_string()));
        var_0 = ((var_0) + &(format_float((1.4_f64), (5_i32), (0_i32))));
        var_0 = ((var_0) + &("d".to_string()));
        var_0 = ((var_0) + &(format_float((334.1_f64), (0_i32), (2_i32))));
        var_0 = ((var_0) + &("e".to_string()));
        var_0
    }
}

#[test]
fn code_format_float() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("a1.20b1.34c  1.4d334.10e", test(&mut stores));
}
