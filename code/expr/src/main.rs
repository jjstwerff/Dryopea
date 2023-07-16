#![allow(unused_parens)]
mod external;
mod store;

use external::*;
fn _tp_integer_abs(var_0: i32, ) -> i32 {
  op_abs_int(var_0)
}

fn _tp_long_abs(var_0: i64, ) -> i64 {
  op_abs_long(var_0)
}

fn _tp_single_abs(var_0: f32, ) -> f32 {
  var_0.abs()
}

fn _tp_float_abs(var_0: f64, ) -> f64 {
  var_0.abs()
}

fn _tp_text_len(var_0: String, ) -> i32 {
  (var_0.len() as i32)
}

fn _tp_text_clear(var_0: String, ) {
  var_0.clear()
}

fn _tp_mutable_remove(var_0: ??, )  {}

fn assert(var_0: bool, var_1: String, ) {
  if !var_0 { panic!("{}", var_1); };
}

fn print(var_0: String, ) {
  print!("{}", var_0)
}

fn main() {
  assert(({
    let var_0 = "a".to_string();
    var_0 = (var_0 + &format_int(12_i32, 0_i32, 32_i32, 0_i32, 0_i32));
    var_0 = (var_0 + &"b".to_string());
    var_0
  } == "a12b".to_string()), "Formatting problem".to_string());
  assert(({
    let var_1 = "a".to_string();
    var_1 = (var_1 + &format_int(op_add_int(1_i32, op_mul_int(2_i32, 3_i32)), 0_i32, 32_i32, 0_i32, 1_i32));
    var_1 = (var_1 + &"b".to_string());
    var_1
  } == "a0x7b".to_string()), "Hex formatting".to_string())
}

