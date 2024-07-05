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
    {
        let mut var_0 = "1".to_string();
        var_0 = ((var_0)
            + &(format_text(
                if (true) { "true" } else { "false" },
                (7_i32),
                (0_i32),
                (32_i32),
            )));
        var_0 = ((var_0) + &("2".to_string()));
        var_0
    }
}

#[test]
fn code_format_boolean() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("1 true  2".to_string(), test(&mut stores));
}
