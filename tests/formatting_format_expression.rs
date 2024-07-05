#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> String {
    {
        let mut var_0 = "ab".to_string();
        var_0 = ((var_0)
            + &(format_int(
                (op_add_int((op_add_int((1_i32), (2_i32))), (32_i32))),
                (16_i32),
                (0_i32),
                (32_i32),
                (false),
                (true),
            )));
        var_0 = ((var_0) + &("c".to_string()));
        var_0 = ((var_0) + &(format_int((12_i32), (8_i32), (0_i32), (32_i32), (false), (false))));
        var_0 = ((var_0) + &("d".to_string()));
        var_0 = ((var_0) + &(format_int((391_i32), (10_i32), (0_i32), (32_i32), (false), (false))));
        var_0 = ((var_0) + &("e".to_string()));
        var_0 = ((var_0) + &(format_int((12_i32), (10_i32), (4_i32), (32_i32), (true), (false))));
        var_0 = ((var_0) + &("f".to_string()));
        var_0 = ((var_0) + &(format_int((1_i32), (10_i32), (3_i32), (48_i32), (false), (false))));
        var_0 = ((var_0) + &("g".to_string()));
        var_0 = ((var_0) + &(format_int((42_i32), (2_i32), (0_i32), (32_i32), (false), (false))));
        var_0 = ((var_0) + &("h".to_string()));
        var_0
    }
}

#[test]
fn code_format_expression() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!(
        "ab0x23c14d391e +12f001g101010h".to_string(),
        test(&mut stores)
    );
}
