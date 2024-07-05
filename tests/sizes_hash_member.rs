#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]

extern crate dryopea;
use dryopea::database::{KnownTypes, Stores};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {
    let s = db.structure("S".to_string(), 20, 65535);
    db.field(s, "a".to_string(), 0, 4);
    db.field(s, "b".to_string(), 1, 8);
    db.field(s, "c".to_string(), 0, 16);
    let s = db.structure("Main".to_string(), 8, 65535);
    db.field(s, "s".to_string(), 65535, 4);
}

fn test(stores: &mut Stores) -> i32 {
    let var_0 = {
        let var_1 = stores.database((8_i32) as u32);
        let db = (var_1);
        stores
            .mut_store(&db)
            .set_int(db.rec, db.pos as isize + (4_i32) as isize, (0_i32));
        var_1
    };
    op_add_int(
        (op_add_int((20_i32), (op_mul_int((100_i32), (8_i32))))),
        (op_mul_int((1000_i32), (8_i32))),
    )
}

#[test]
fn code_hash_member() {
    let mut types = KnownTypes::new();
    init(&mut types);
    let mut stores = Stores::new(&types);
    assert_eq!(8820, test(&mut stores));
}
