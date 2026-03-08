#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(redundant_semicolons)]
#![allow(unused_assignments)]
#![allow(clippy::double_parens)]
#![allow(clippy::unused_unit)]

extern crate dryopea;
use dryopea::database::Stores;
use dryopea::external;
use dryopea::keys::{Content, DbRef, Key, Str};
use dryopea::vector;

fn init(db: &mut Stores) {
    db.finish();
}

fn n_test(stores: &mut Stores) {
    //block_1: void
    let mut var___work_1: String = "".to_string();
    let mut var_b: String = "".to_string();
    let mut var_test_value = { //block_2: text["b"]
    var_b = "".to_string();
    { //For block_3: void
      let mut var_a__index: i32 = i32::MIN;
      loop { //For loop_4
        let mut var_a: i32 = { //Iter range_5: integer
          var_a__index = if !(external::op_conv_bool_from_int((var_a__index))) {0_i32} else {external::op_add_int((var_a__index), (1_i32))};
          if (5_i32) <= (var_a__index) {break} else {()};
          var_a__index
          } /*Iter range_5: integer*/;
        { //block_6: void
          {let c = 48_i32; if c != 0 { var_b.push(external::to_char(c)); } };
          } /*block_6: void*/;
        } /*For loop_4*/;
      } /*For block_3: void*/;
    &var_b
    } /*block_2: text["b"]*/.to_string();
    if (&var_test_value) == ("00000") {
        ()
    } else {
        panic!(
            "{}",
            ({
                //Formatted string_7: text["__work_1"]
                var___work_1 = "Test failed ".to_string();
                external::format_text(&mut var___work_1, &var_test_value, 0_i32, -1, 32);
                var___work_1 += " != \"00000\"";
                &var___work_1
            }/*Formatted string_7: text["__work_1"]*/)
        );
    };
} /*block_1: void*/

#[test]
fn code_add_loop() {
    let mut stores = Stores::new();
    init(&mut stores);
    n_test(&mut stores);
}
