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
    let s = db.structure("Pixel".to_string(), 3, 65535); // 6
    db.field(s, "r".to_string(), db.byte(0, false), 0);
    db.field(s, "g".to_string(), db.byte(0, false), 1);
    db.field(s, "b".to_string(), db.byte(0, false), 2);
    let s = db.structure("Image".to_string(), 20, 65535); // 10
    db.field(s, "name".to_string(), 5, 4);
    db.field(s, "width".to_string(), 0, 8);
    db.field(s, "height".to_string(), 0, 12);
    db.field(s, "data".to_string(), db.vector(6), 16);
    let s = db.structure("File".to_string(), 17, 65535); // 12
    db.field(s, "path".to_string(), 5, 4);
    db.field(s, "size".to_string(), 1, 8);
    db.field(s, "dir".to_string(), 4, 16);
    let s = db.structure("Unknown(0)".to_string(), 8, 65535); // 65535
    db.field(s, "vector".to_string(), db.vector(4), 4);
    let s = db.structure("Vector(Reference(270))".to_string(), 8, 65535); // 13
    db.field(s, "vector".to_string(), db.vector(6), 4);
    db.vector(6);
    db.vector(12);
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

fn _tp_single_cos(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).cos()
}

fn _tp_single_sin(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).sin()
}

fn _tp_single_tan(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).tan()
}

fn _tp_single_acos(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).acos()
}

fn _tp_single_asin(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).asin()
}

fn _tp_single_atan(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).atan()
}

fn _tp_single_atan2(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_0).atan2((var_1))
}

fn _tp_single_ceil(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).ceil(@v2)
}

fn _tp_single_floor(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).floor(@v2)
}

fn _tp_single_round(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).round(@v2)
}

fn _tp_single_sqrt(stores: &mut Stores, var_0: f32) -> f32 {
  (var_0).sqrt(@v2)
}

fn _tp_single_log(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_0).log((var_1))
}

fn _tp_single_pow(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_0).pow((var_1))
}

fn _tp_float_abs(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).abs()
}

fn _tp_float_cos(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).cos()
}

fn _tp_float_sin(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).sin()
}

fn _tp_float_tan(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).tan()
}

fn _tp_float_acos(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).acos()
}

fn _tp_float_asin(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).asin()
}

fn _tp_float_atan(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).atan()
}

fn _tp_float_atan2(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_0).atan2((var_1))
}

fn _tp_float_ceil(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).ceil(@v2)
}

fn _tp_float_floor(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).floor(@v2)
}

fn _tp_float_round(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).round(@v2)
}

fn _tp_float_sqrt(stores: &mut Stores, var_0: f64) -> f64 {
  (var_0).sqrt(@v2)
}

fn _tp_float_log(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_0).log((var_1))
}

fn _tp_float_pow(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_0).pow((var_1))
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

fn _virtual_attr_pixel_value(stores: &mut Stores, var_0: DbRef) -> i32 {
op_add_int((op_add_int((op_mul_int(({let db=(var_0); stores.store(&db).get_byte(db.rec, db.pos as isize + (0_i32) as isize, (0_i32))}), (65536_i32))), (op_mul_int(({let db=(var_0); stores.store(&db).get_byte(db.rec, db.pos as isize + (1_i32) as isize, (0_i32))}), (256_i32))))), ({let db=(var_0); stores.store(&db).get_byte(db.rec, db.pos as isize + (2_i32) as isize, (0_i32))}))
}


fn OpGetFile(stores: &mut Stores, var_0: DbRef) -> bool {

}


fn OpGetDir(stores: &mut Stores, var_0: String, var_1: DbRef) -> bool {

}


fn OpGetPngImage(stores: &mut Stores, var_0: String, var_1: DbRef) -> bool {

}


fn OpGetFileText(stores: &mut Stores, var_0: DbRef) -> String {

}


fn _tp_File_text(stores: &mut Stores, var_0: DbRef) -> String {
  stores.get_file_text((var_0))
}

fn file(stores: &mut Stores, var_0: String) -> DbRef {
  let mut var_1 = {
    let mut var_2 = stores.database((17_i32) as u32);
    {let db=(var_2); let store=stores.mut_store(&db); store.set_int(db.rec, db.pos as isize + (4_i32) as isize, store.set_str(&(var_0)) as i32)};
    {let db=(var_2); stores.mut_store(&db).set_long(db.rec, db.pos as isize + (8_i32) as isize, (0_i64))};
    {let db=(var_2); stores.mut_store(&db).set_byte(db.rec, db.pos as isize + (16_i32) as isize, (0_i32), (if 0_i32 {1_i32} else {0_i32}))};
    var_2
  };
  if stores.read_file((var_1)) {
    var_1
  } else {stores.null()}
}

fn _tp_File_files(stores: &mut Stores, var_0: DbRef) -> DbRef {
  let mut var_1 = {
    var_4 = stores.database((1_i32) as u32);
    var_3 = OpGetField(stores, var_4, 4_i32);
    {let db=(var_4); stores.mut_store(&db).set_int(db.rec, db.pos as isize + (4_i32) as isize, (0_i32))};
    var_3
  };
  if op_eq_int(({let db=(var_0); stores.store(&db).get_byte(db.rec, db.pos as isize + (16_i32) as isize, (0_i32))}), (1_i32)) {
    stores.get_dir(({let db=(var_0); let store=stores.store(&db); String::from(store.get_str(store.get_int(db.rec, db.pos as isize + (4_i32) as isize) as u32))}), (var_1))
  } else {Null};
  var_1
}

fn _tp_File_png(stores: &mut Stores, var_0: DbRef) -> DbRef {
  if !(op_eq_int(({let db=(var_0); stores.store(&db).get_byte(db.rec, db.pos as isize + (16_i32) as isize, (0_i32))}), (1_i32))) {
    let mut var_1 = {
      let mut var_2 = stores.database((20_i32) as u32);
      {let db=(var_2); let store=stores.mut_store(&db); store.set_int(db.rec, db.pos as isize + (4_i32) as isize, store.set_str(&(Null)) as i32)};
      {let db=(var_2); stores.mut_store(&db).set_int(db.rec, db.pos as isize + (8_i32) as isize, (0_i32))};
      {let db=(var_2); stores.mut_store(&db).set_int(db.rec, db.pos as isize + (12_i32) as isize, (0_i32))};
      {let db=(var_2); stores.mut_store(&db).set_int(db.rec, db.pos as isize + (16_i32) as isize, (0_i32))};
      var_2
    };
    stores.get_png((var_1));
    var_1
  } else {stores.null()}
}

