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
    let s = db.structure("Variable".to_string(), 12, 4294967295); // 6
    db.field(s, "name".to_string(), 5, 4);
    db.field(s, "value".to_string(), 5, 8);
    let s = db.structure("Pixel".to_string(), 3, 4294967295); // 7
    db.field(s, "r".to_string(), db.byte(0, false), 0);
    db.field(s, "g".to_string(), db.byte(0, false), 1);
    db.field(s, "b".to_string(), db.byte(0, false), 2);
    let s = db.structure("Image".to_string(), 20, 4294967295); // 9
    db.field(s, "name".to_string(), 5, 4);
    db.field(s, "width".to_string(), db.int(), 8);
    db.field(s, "height".to_string(), db.int(), 12);
    db.field(s, "data".to_string(), db.vector(7), 16);
    let s = db.structure("File".to_string(), 17, 4294967295); // 11
    db.field(s, "path".to_string(), 5, 4);
    db.field(s, "size".to_string(), 1, 8);
    db.field(s, "dir".to_string(), 4, 16);
    db.vector(7);
    db.vector(11);
    let s = db.structure("main_vector<File>".to_string(), 0, 4294967295); // 65535
    db.field(s, "vector".to_string(), db.vector(11), 65535);
}

fn _tp_integer_abs(stores: &mut Stores, var_0: i32) -> i32 {
  external::op_abs_int((var_both))
}

fn _tp_long_abs(stores: &mut Stores, var_0: i64) -> i64 {
  external::op_abs_long((var_both))
}

fn _tp_single_abs(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).abs()
}

fn _tp_single_cos(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).cos()
}

fn _tp_single_sin(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).sin()
}

fn _tp_single_tan(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).tan()
}

fn _tp_single_acos(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).acos()
}

fn _tp_single_asin(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).asin()
}

fn _tp_single_atan(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).atan()
}

fn _tp_single_atan2(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_both).atan2((var_v2))
}

fn _tp_single_ceil(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).ceil()
}

fn _tp_single_floor(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).floor()
}

fn _tp_single_round(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).round()
}

fn _tp_single_sqrt(stores: &mut Stores, var_0: f32) -> f32 {
  (var_both).sqrt()
}

fn _tp_single_log(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_both).log((var_v2))
}

fn _tp_single_pow(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 {
  (var_both).powf((var_v2))
}

fn _tp_float_abs(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).abs()
}

fn _tp_float_cos(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).cos()
}

fn _tp_float_sin(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).sin()
}

fn _tp_float_tan(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).tan()
}

fn _tp_float_acos(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).acos()
}

fn _tp_float_asin(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).asin()
}

fn _tp_float_atan(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).atan()
}

fn _tp_float_atan2(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_both).atan2((var_v2))
}

fn _tp_float_ceil(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).ceil()
}

fn _tp_float_floor(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).floor()
}

fn _tp_float_round(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).round()
}

fn _tp_float_sqrt(stores: &mut Stores, var_0: f64) -> f64 {
  (var_both).sqrt()
}

fn _tp_float_log(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_both).log((var_v2))
}

fn _tp_float_pow(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 {
  (var_both).powf((var_v2))
}

fn _tp_text_len(stores: &mut Stores, var_0: Str) -> i32 {
  (var_both).len() as i32
}

fn character(stores: &mut Stores, var_0: Str) -> i32 {

}


fn _tp_vector_len(stores: &mut Stores, var_0: DbRef) -> i32 {
  vector::length_vector(&(var_both), &s.database.allocations) as i32
}

fn _tp_vector_clear(stores: &mut Stores, var_0: DbRef) {
  vector::clear_vector(&(var_self), &mut s.database.allocations);
}

fn assert(stores: &mut Stores, var_0: bool, var_1: Str) {

}


fn print(stores: &mut Stores, var_0: Str) {
  print!("{}", (var_v1));
}

fn OpGetFile(stores: &mut Stores, var_0: DbRef) -> bool {

}


fn OpGetDir(stores: &mut Stores, var_0: Str, var_1: DbRef) -> bool {

}


fn OpGetPngImage(stores: &mut Stores, var_0: Str, var_1: DbRef) -> bool {

}


fn OpGetFileText(stores: &mut Stores, var_0: DbRef, var_1: DbRef) {

}


fn _tp_Pixel_value(stores: &mut Stores, var_0: DbRef) -> i32 {
  external::op_add_int((external::op_add_int((external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((0_i32)), i32::from((0_i32)))}), (65536_i32))), (external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((1_i32)), i32::from((0_i32)))}), (256_i32))))), ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((2_i32)), i32::from((0_i32)))}))
}

fn _tp_File_content(stores: &mut Stores, var_0: DbRef, var_1: DbRef) -> Str {
  OpClearRefText(stores, *s.get_var::<DbRef>((var_result)););
  OpAppendRefText(stores, *s.get_var::<DbRef>((var_result));, 0_i32, "".to_string());
  OpGetFileText(stores, var_self, var_result);
  var_result
}

fn file(stores: &mut Stores, var_0: Str) -> DbRef {
  var_result = {
    var__val_1 = OpDatabase(stores, 17_i32, 11_i32);
    {let db = (var__val_1); let s_val = (var_path).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((4_i32)), s_pos as i32);};
    {let db = (var__val_1); stores.store_mut(&db).set_long(db.rec, db.pos + u32::from((8_i32)), (0_i64));};
    {let db = (var__val_1); stores.store_mut(&db).set_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)), (if 0_i32 {1_i32} else {0_i32}));};
    var__val_1
  };
  if stores.get_file(&(var_result)) {
    var_result
  } else {
    Stores::null()
  }
}

fn _tp_File_files(stores: &mut Stores, var_0: DbRef) -> DbRef {
  var_result = {
    var__db_3 = OpDatabase(stores, 1_i32, 65535_i32);
    var__vec_2 = DbRef {store_nr: (var__db_3).store_nr, rec: (var__db_3).rec, pos: (var__db_3).pos + u32::from((65535_i32))};
    {let db = (var__db_3); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((65535_i32)), (0_i32));};
    var__vec_2
  };
  if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)))}) == (1_i32) {
    Drop(Call(316, [Call(263, [Var(0), Int(4)]), Var(1)]))
  } else {Null};
  var_result
}

fn _tp_File_png(stores: &mut Stores, var_0: DbRef) -> DbRef {
  if !(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)))}) == (1_i32)) {
    var_result = {
      var__val_1 = OpDatabase(stores, 20_i32, 9_i32);
      {let db = (var__val_1); let s_val = ("".to_string()).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((4_i32)), s_pos as i32);};
      {let db = (var__val_1); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((8_i32)), (0_i32));};
      {let db = (var__val_1); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((12_i32)), (0_i32));};
      {let db = (var__val_1); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((16_i32)), (0_i32));};
      var__val_1
    };
    Drop(Call(317, [Call(263, [Var(0), Int(4)]), Var(1)]));
    var_result
  } else {
    Stores::null()
  }
}

fn env_variables(stores: &mut Stores) -> DbRef {

}


fn env_variable(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_starts_with(stores: &mut Stores, var_0: Str, var_1: Str) -> bool {

}


fn _tp_text_ends_with(stores: &mut Stores, var_0: Str, var_1: Str) -> bool {

}


fn _tp_text_trim(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_trim_start(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_trim_end(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_find(stores: &mut Stores, var_0: Str, var_1: Str) -> i32 {

}


fn _tp_text_contains(stores: &mut Stores, var_0: Str, var_1: Str) -> bool {

}


fn _tp_text_replace(stores: &mut Stores, var_0: Str, var_1: Str, var_2: Str) -> Str {

}


fn _tp_text_to_lowercase(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_to_uppercase(stores: &mut Stores, var_0: Str) -> Str {

}


fn _tp_text_is_lowercase(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_uppercase(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_numeric(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_alphanumeric(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_alphabetic(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_whitespace(stores: &mut Stores, var_0: Str) -> bool {

}


fn _tp_text_is_control(stores: &mut Stores, var_0: Str) -> bool {

}


fn arguments(stores: &mut Stores) -> DbRef {

}


fn directory(stores: &mut Stores, var_0: DbRef) -> Str {

}


fn user_directory(stores: &mut Stores, var_0: DbRef) -> Str {

}


fn program_directory(stores: &mut Stores, var_0: DbRef) -> Str {

}


