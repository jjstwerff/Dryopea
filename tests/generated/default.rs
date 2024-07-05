#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::{Stores, KnownTypes, DbRef};
use dryopea::external::*;

fn init(db: &mut KnownTypes) {
}

fn _tp_integer_abs(stores: &mut Stores, var_0: i32) -> i32 {
  op_abs_int((var_0))
}

fn _tp_long_abs(stores: &mut Stores, var_0: i64) -> i64 {
  op_abs_long((var_0))
}

fn _tp_single_abs(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).abs()
}

fn _tp_float_abs(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).abs()
}

fn _tp_text_len(stores: &mut Stores, var_0: String) -> i32 {
  (var_0).len() as i32
}

fn _tp_vector_len(stores: &mut Stores, var_0: DbRef) -> i32 {
  stores.length_vector(&(var_0)) as i32
}

fn _tp_vector_clear(stores: &mut Stores, var_0: DbRef) {
  stores.clear_vector(&(var_0))
}

fn assert(stores: &mut Stores, var_0: bool, var_1: String) {
  if !(var_0) { panic!("{}", (var_1)); };
}

fn print(stores: &mut Stores, var_0: String) {
  print!("{}", (var_0))
}

