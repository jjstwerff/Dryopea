#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> String {
    let mut var_0 = "abc".to_string();
    {
        let mut var_1 = "1".to_string();
        var_1 = ((var_1)
            + &(format_text(&(var_0), (op_add_int((2_i32), (3_i32))), (-1_i32), (32_i32))));
        var_1 = ((var_1) + &("2".to_string()));
        var_1 = ((var_1) + &(format_text(&(var_0), (0_i32), (-1_i32), (32_i32))));
        var_1 = ((var_1) + &("3".to_string()));
        var_1 = ((var_1) + &(format_text(&(var_0), (6_i32), (-1_i32), (32_i32))));
        var_1 = ((var_1) + &("4".to_string()));
        var_1 = ((var_1) + &(format_text(&(var_0), (7_i32), (1_i32), (32_i32))));
        var_1
    }
}

#[test]
fn code_format_text() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("1abc  2abc3abc   4    abc".to_string(), test(&mut stores));
}
