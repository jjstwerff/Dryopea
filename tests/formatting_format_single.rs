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
            + &(format_single(
                ((0.1_f32) + ((op_conv_single_from_int((2_i32))) * (1_f32))),
                (0_i32),
                (0_i32),
            )));
        var_0 = ((var_0) + &("b".to_string()));
        var_0
    }
}

#[test]
fn code_format_single() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("a2.1b".to_string(), test(&mut stores));
}
