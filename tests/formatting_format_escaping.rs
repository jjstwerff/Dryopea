#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {}

fn test(stores: &mut Stores) -> String {
    "ab{cd}e".to_string()
}

#[test]
fn code_format_escaping() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!("ab{cd}e", test(&mut stores));
}
