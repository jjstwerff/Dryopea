#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> String {
    {
        let mut var_0 = "a".to_string();
        var_0 = ((var_0)
            + &(format_long(
                (op_add_long((1_i64), (op_conv_long_from_int(1_i32)))),
                (10_i32),
                (4_i32),
                (32_i32),
                (true),
                (false),
            )));
        var_0 = ((var_0) + &("b".to_string()));
        var_0 = ((var_0)
            + &(format_int(
                (op_cast_int_from_long((12_i64))),
                (10_i32),
                (0_i32),
                (32_i32),
                (false),
                (false),
            )));
        var_0 = ((var_0) + &("c ".to_string()));
        var_0 = ((var_0)
            + &(format_text(
                if (op_le_long(
                    (op_conv_long_from_int(8_i32)),
                    (op_mul_long((2_i64), (op_rem_long((4_i64), (6_i64))))),
                )) {
                    "true"
                } else {
                    "false"
                },
                (0_i32),
                (-1_i32),
                (32_i32),
            )));
        var_0 = ((var_0) + &(" d".to_string()));
        var_0
    }
}

#[test]
fn code_format_long() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("a  +2b12c true d".to_string(), test(&mut stores));
}
