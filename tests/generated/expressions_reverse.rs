#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;
use dryopea::database::Stores;
use dryopea::keys::{DbRef, Str, Key, Content};
use dryopea::external;
use dryopea::external::*;
use dryopea::vector;

fn init(db: &mut Stores) {
    db.vector(5);
    let e = db.enumerate("Format");
    db.value(e, "TextFile", u16::MAX);
    db.value(e, "LittleEndian", u16::MAX);
    db.value(e, "BigEndian", u16::MAX);
    db.value(e, "Directory", u16::MAX);
    let s = db.structure("EnvVariable", 0); // 9
    db.field(s, "name", 5);
    db.field(s, "value", 5);
    let s = db.structure("Pixel", 0); // 10
    let byte_r = db.byte(0, false);
    db.field(s, "r", byte_r);
    let byte_g = db.byte(0, false);
    db.field(s, "g", byte_g);
    let byte_b = db.byte(0, false);
    db.field(s, "b", byte_b);
    let s = db.structure("Image", 0); // 12
    db.field(s, "name", 5);
    db.field(s, "width", 0);
    db.field(s, "height", 0);
    let vec_data = db.vector(10);
    db.field(s, "data", vec_data);
    db.vector(10);
    let s = db.structure("File", 0); // 14
    db.field(s, "path", 5);
    db.field(s, "size", 1);
    db.field(s, "format", 8);
    db.field(s, "ref", 0);
    let s = db.structure("main_vector<text>", 0); // 15
    let vec_vector = db.vector(5);
    db.field(s, "vector", vec_vector);
    let s = db.structure("main_vector<File>", 0); // 16
    let vec_vector = db.vector(14);
    db.field(s, "vector", vec_vector);
    db.vector(14);
    db.finish();
}

fn t_7integer_abs(stores: &mut Stores, mut var_both: i32) -> i32 { //block_1: integer
  ;
  external::op_abs_int((var_both))
} /*block_1: integer*/

fn t_4long_abs(stores: &mut Stores, mut var_both: i64) -> i64 { //block_1: long
  ;
  external::op_abs_long((var_both))
} /*block_1: long*/

fn t_6single_abs(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).abs()
} /*block_1: single*/

fn t_6single_cos(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).cos()
} /*block_1: single*/

fn t_6single_sin(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).sin()
} /*block_1: single*/

fn t_6single_tan(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).tan()
} /*block_1: single*/

fn t_6single_acos(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).acos()
} /*block_1: single*/

fn t_6single_asin(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).asin()
} /*block_1: single*/

fn t_6single_atan(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).atan()
} /*block_1: single*/

fn t_6single_atan2(stores: &mut Stores, mut var_both: f32, mut var_v2: f32) -> f32 { //block_1: single
  ;
  (var_both).atan2((var_v2))
} /*block_1: single*/

fn t_6single_ceil(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).ceil()
} /*block_1: single*/

fn t_6single_floor(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).floor()
} /*block_1: single*/

fn t_6single_round(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).round()
} /*block_1: single*/

fn t_6single_sqrt(stores: &mut Stores, mut var_both: f32) -> f32 { //block_1: single
  ;
  (var_both).sqrt()
} /*block_1: single*/

fn t_6single_log(stores: &mut Stores, mut var_both: f32, mut var_v2: f32) -> f32 { //block_1: single
  ;
  (var_both).log((var_v2))
} /*block_1: single*/

fn t_6single_pow(stores: &mut Stores, mut var_both: f32, mut var_v2: f32) -> f32 { //block_1: single
  ;
  (var_both).powf((var_v2))
} /*block_1: single*/

fn OpFormatStackSingle(stores: &mut Stores, mut var_pos: u16, mut var_val: f32, mut var_width: i32, mut var_precision: i32) {

}


fn t_5float_abs(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).abs()
} /*block_1: float*/

fn t_5float_cos(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).cos()
} /*block_1: float*/

fn t_5float_sin(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).sin()
} /*block_1: float*/

fn t_5float_tan(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).tan()
} /*block_1: float*/

fn t_5float_acos(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).acos()
} /*block_1: float*/

fn t_5float_asin(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).asin()
} /*block_1: float*/

fn t_5float_atan(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).atan()
} /*block_1: float*/

fn t_5float_atan2(stores: &mut Stores, mut var_both: f64, mut var_v2: f64) -> f64 { //block_1: float
  ;
  (var_both).atan2((var_v2))
} /*block_1: float*/

fn t_5float_ceil(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).ceil()
} /*block_1: float*/

fn t_5float_floor(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).floor()
} /*block_1: float*/

fn t_5float_round(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).round()
} /*block_1: float*/

fn t_5float_sqrt(stores: &mut Stores, mut var_both: f64) -> f64 { //block_1: float
  ;
  (var_both).sqrt()
} /*block_1: float*/

fn t_5float_log(stores: &mut Stores, mut var_both: f64, mut var_v2: f64) -> f64 { //block_1: float
  ;
  (var_both).log((var_v2))
} /*block_1: float*/

fn t_5float_pow(stores: &mut Stores, mut var_both: f64, mut var_v2: f64) -> f64 { //block_1: float
  ;
  (var_both).powf((var_v2))
} /*block_1: float*/

fn OpVarText(stores: &mut Stores, mut var_pos: u16) -> String {
  todo!()
}


fn OpArgText(stores: &mut Stores, mut var_pos: u16) -> String {
  todo!()
}


fn t_4text_len(stores: &mut Stores, mut var_both: String) -> i32 { //block_1: integer
  ;
  (var_both).len() as i32
} /*block_1: integer*/

fn t_9character_len(stores: &mut Stores, mut var_both: char) -> i32 { //block_1: integer
  ;
  ((var_both).len_utf8() as i32)
} /*block_1: integer*/

fn OpText(stores: &mut Stores) {

}


fn OpTextCompare(stores: &mut Stores, mut var_v1: String, mut var_v2: char) -> i32 {
  todo!()
}


fn OpFormatStackDatabase(stores: &mut Stores, mut var_pos: u16, mut var_val: DbRef, mut var_db_tp: u16, mut var_db_format: u8) {

}


fn t_6vector_len(stores: &mut Stores, mut var_both: DbRef) -> i32 { //block_1: integer
  ;
  vector::length_vector(&(var_both), &stores.allocations) as i32
} /*block_1: integer*/

fn OpInsertVector(stores: &mut Stores, mut var_r: DbRef, mut var_size: u16, mut var_index: i32, mut var_db_tp: u16) -> DbRef {
  todo!()
}


fn OpValidate(stores: &mut Stores, mut var_data: DbRef, mut var_db_tp: u16) {

}


fn OpHashAdd(stores: &mut Stores, mut var_data: DbRef, mut var_rec: DbRef, mut var_tp: u16) {

}


fn OpHashFind(stores: &mut Stores, mut var_data: DbRef, mut var_tp: u16) -> DbRef {
  todo!()
}


fn OpHashRemove(stores: &mut Stores, mut var_data: DbRef, mut var_rec: DbRef, mut var_tp: u16) {

}


fn n_assert(stores: &mut Stores, mut var_test: bool, mut var_message: String) {

}


fn n_panic(stores: &mut Stores, mut var_message: String) {

}


fn n_print(stores: &mut Stores, mut var_v1: String) { //block_1: void
  ;
  print!("{}", (var_v1));;
} /*block_1: void*/

fn n_println(stores: &mut Stores, mut var_v1: String) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  let _pre0 = { //Formatted string_2: text["__work_1"]
    var___work_1 = "".to_string();
    external::format_text(&mut (var___work_1), &(var_v1), (0_i32), (-1_i32) as i8, (32_i32) as u8);
    (var___work_1).push_str(&("\n".to_string()));
    var___work_1
  } /*Formatted string_2: text["__work_1"]*/;
  print!("{}", (_pre0));;
  {};
} /*block_1: void*/

fn OpRemove(stores: &mut Stores, mut var_state_var: u16, mut var_data: DbRef, mut var_on: u8, mut var_tp: u16) {

}


fn OpClear(stores: &mut Stores, mut var_data: DbRef, mut var_tp: u16) {

}


fn OpAppendCopy(stores: &mut Stores, mut var_data: DbRef, mut var_count: i32, mut var_tp: u16) {

}


fn OpStaticCall(stores: &mut Stores, mut var_call: u16) {

}


fn t_5Pixel_value(stores: &mut Stores, mut var_self: DbRef) -> i32 { //block_1: integer
  ;
  external::op_add_int((external::op_add_int((external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (0_i32) as u32, i32::from((0_i32)))}), (65536_i32))), (external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (1_i32) as u32, i32::from((0_i32)))}), (256_i32))))), ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (2_i32) as u32, i32::from((0_i32)))}))
} /*block_1: integer*/

fn t_4File_little_endian(stores: &mut Stores, mut var_self: DbRef) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) { //block_2: void
    ;
    {let db = (var_self); let _val = i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})); stores.store_mut(&db).set_byte(db.rec, db.pos + (16_i32) as u32, 0, _val);};
  } /*block_2: void*/ else { //block_3: void
    ;
    let _pre0 = { //Formatted string_4: text["__work_1"]
      var___work_1 = "Cannot set '".to_string();
      external::format_text(&mut (var___work_1), &({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), (0_i32), (-1_i32) as i8, (32_i32) as u8);
      (var___work_1).push_str(&("' to little endian binary.".to_string()));
      var___work_1
    } /*Formatted string_4: text["__work_1"]*/;
    panic!("{}", (_pre0));;
  } /*block_3: void*/;
  {};
} /*block_1: void*/

fn t_4File_big_endian(stores: &mut Stores, mut var_self: DbRef) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) { //block_2: void
    ;
    {let db = (var_self); let _val = i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})); stores.store_mut(&db).set_byte(db.rec, db.pos + (16_i32) as u32, 0, _val);};
  } /*block_2: void*/ else { //block_3: void
    ;
    let _pre0 = { //Formatted string_4: text["__work_1"]
      var___work_1 = "Cannot set '".to_string();
      external::format_text(&mut (var___work_1), &({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), (0_i32), (-1_i32) as i8, (32_i32) as u8);
      (var___work_1).push_str(&("' to big endian binary.".to_string()));
      var___work_1
    } /*Formatted string_4: text["__work_1"]*/;
    panic!("{}", (_pre0));;
  } /*block_3: void*/;
  {};
} /*block_1: void*/

fn t_4File_write_bin(stores: &mut Stores, mut var_self: DbRef, mut var_v: DbRef) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  if if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) {true} else {(if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) })} { //block_2: void
    ;
    todo!();
  } /*block_2: void*/ else { //block_3: void
    ;
    let _pre0 = { //Formatted string_4: text["__work_1"]
      var___work_1 = "Set file '".to_string();
      external::format_text(&mut (var___work_1), &({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), (0_i32), (-1_i32) as i8, (32_i32) as u8);
      (var___work_1).push_str(&("' to binary format first.".to_string()));
      var___work_1
    } /*Formatted string_4: text["__work_1"]*/;
    panic!("{}", (_pre0));;
  } /*block_3: void*/;
  {};
} /*block_1: void*/

fn t_4File_read(stores: &mut Stores, mut var_self: DbRef, mut var_v: DbRef) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  if if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) {true} else {(if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) })} { //block_2: void
    ;
    todo!();
  } /*block_2: void*/ else { //block_3: void
    ;
    let _pre0 = { //Formatted string_4: text["__work_1"]
      var___work_1 = "Set file '".to_string();
      external::format_text(&mut (var___work_1), &({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), (0_i32), (-1_i32) as i8, (32_i32) as u8);
      (var___work_1).push_str(&("' to binary format first.".to_string()));
      var___work_1
    } /*Formatted string_4: text["__work_1"]*/;
    panic!("{}", (_pre0));;
  } /*block_3: void*/;
  {};
} /*block_1: void*/

fn t_4File_seek(stores: &mut Stores, mut var_self: DbRef, mut var_pos: i64) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  if if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) {true} else {(if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) })} { //block_2: void
    ;
    todo!();
  } /*block_2: void*/ else { //block_3: void
    ;
    let _pre0 = { //Formatted string_4: text["__work_1"]
      var___work_1 = "Set file '".to_string();
      external::format_text(&mut (var___work_1), &({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), (0_i32), (-1_i32) as i8, (32_i32) as u8);
      (var___work_1).push_str(&("' to binary format first.".to_string()));
      var___work_1
    } /*Formatted string_4: text["__work_1"]*/;
    panic!("{}", (_pre0));;
  } /*block_3: void*/;
  {};
} /*block_1: void*/

fn t_4File_content(stores: &mut Stores, mut var_self: DbRef, mut var_result: DbRef) -> String {
  todo!()

}


fn t_4File_lines(stores: &mut Stores, mut var_self: DbRef, mut var_result: DbRef) -> DbRef { //block_1: vector<text>["result"]
  let mut var___work_1: String = "".to_string();
  ;
  vector::clear_vector(&(var_result), &mut stores.allocations);;
  ;
  let mut var_c: String = t_4File_content(stores, var_self, { //default ref_2: ref(reference)["__work_1"]
    todo!()
  } /*default ref_2: ref(reference)["__work_1"]*/).to_string();
  ;
  let mut var_p: i32 = 0_i32;
  ;
  { //For block_3: void
    let mut var_ch__index: i32 = 0_i32;
    loop { //For loop_4
      let mut var_ch: char = { //for text next_5: character
        let mut var__for_result_1: char = external::text_character(&(var_c), (var_ch__index));
        var_ch__index = external::op_add_int((var_ch__index), (((var__for_result_1).len_utf8() as i32)));
        var__for_result_1
      } /*for text next_5: character*/;
      if !(external::op_conv_bool_from_character((var_ch))) { //break_6: void
        break;
      } /*break_6: void*/ else {()};
      { //block_7: void
        ;
        if (if (var_ch) == char::from(0) { i32::MIN } else { (var_ch) as i32 }) == (if (char::from_u32(10_u32).unwrap_or('\0')) == char::from(0) { i32::MIN } else { (char::from_u32(10_u32).unwrap_or('\0')) as i32 }) { //block_8: void
          ;
          let mut var__elm_2: DbRef = stores.record_new(&(var_result), (7_i32) as u16, (65535_i32) as u16);
          {let db = (var__elm_2); let s_val = (external::sub_text(&(var_c), (var_p), (external::op_min_int((var_ch__index), (1_i32)))).to_string()).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (0_i32) as u32, s_pos as i32);};
          stores.record_finish(&(var_result), &(var__elm_2), (7_i32) as u16, (65535_i32) as u16);
          ;
          var_p = var_ch__index;
        } /*block_8: void*/ else {()};
      } /*block_7: void*/;
    } /*For loop_4*/;
  } /*For block_3: void*/;
  ;
  if (0_i32) < (var_p) { //block_9: void
    ;
    let mut var__elm_3: DbRef = stores.record_new(&(var_result), (7_i32) as u16, (65535_i32) as u16);
    let _pre0 = external::sub_text(&(var_c), (var_p), (t_4text_len(stores, var_c.clone()))).to_string();
    {let db = (var__elm_3); let s_val = (_pre0).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (0_i32) as u32, s_pos as i32);};
    stores.record_finish(&(var_result), &(var__elm_3), (7_i32) as u16, (65535_i32) as u16);
  } /*block_9: void*/ else {()};
  ;
  {};
  {};
  var_result
} /*block_1: vector<text>["result"]*/

fn t_4text_split(stores: &mut Stores, mut var_self: String, mut var_separator: char, mut var_result: DbRef) -> DbRef { //block_1: vector<text>["result"]
  ;
  vector::clear_vector(&(var_result), &mut stores.allocations);;
  ;
  let mut var_p: i32 = 0_i32;
  ;
  { //For block_2: void
    let mut var_c__index: i32 = 0_i32;
    loop { //For loop_3
      let mut var_c: char = { //for text next_4: character
        let mut var__for_result_1: char = external::text_character(&(var_self), (var_c__index));
        var_c__index = external::op_add_int((var_c__index), (((var__for_result_1).len_utf8() as i32)));
        var__for_result_1
      } /*for text next_4: character*/;
      if !(external::op_conv_bool_from_character((var_c))) { //break_5: void
        break;
      } /*break_5: void*/ else {()};
      { //block_6: void
        ;
        if (if (var_c) == char::from(0) { i32::MIN } else { (var_c) as i32 }) == (if (var_separator) == char::from(0) { i32::MIN } else { (var_separator) as i32 }) { //block_7: void
          ;
          let mut var__elm_2: DbRef = stores.record_new(&(var_result), (7_i32) as u16, (65535_i32) as u16);
          {let db = (var__elm_2); let s_val = (external::sub_text(&(var_self), (var_p), (external::op_min_int((var_c__index), (1_i32)))).to_string()).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (0_i32) as u32, s_pos as i32);};
          stores.record_finish(&(var_result), &(var__elm_2), (7_i32) as u16, (65535_i32) as u16);
          ;
          var_p = var_c__index;
        } /*block_7: void*/ else {()};
      } /*block_6: void*/;
    } /*For loop_3*/;
  } /*For block_2: void*/;
  ;
  if (0_i32) < (var_p) { //block_8: void
    ;
    let mut var__elm_3: DbRef = stores.record_new(&(var_result), (7_i32) as u16, (65535_i32) as u16);
    let _pre0 = external::sub_text(&(var_self), (var_p), (t_4text_len(stores, var_self.clone()))).to_string();
    {let db = (var__elm_3); let s_val = (_pre0).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (0_i32) as u32, s_pos as i32);};
    stores.record_finish(&(var_result), &(var__elm_3), (7_i32) as u16, (65535_i32) as u16);
  } /*block_8: void*/ else {()};
  ;
  var_result
} /*block_1: vector<text>["result"]*/

fn n_file(stores: &mut Stores, mut var_path: String, mut var_result: DbRef) -> DbRef { //block_1: ref(File)["result"]
  ;
  (var_result) = stores.alloc_record((14_i32) as u16);
  {let db = (var_result); let s_val = (var_path).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (8_i32) as u32, s_pos as i32);};
  {let db = (var_result); let _val = (i32::MIN); stores.store_mut(&db).set_int(db.rec, db.pos + (12_i32) as u32, _val);};
  {let db = (var_result); let _val = (0_i64); stores.store_mut(&db).set_long(db.rec, db.pos + (0_i32) as u32, _val);};
  {let db = (var_result); let _val = i32::from((0_u8)); stores.store_mut(&db).set_byte(db.rec, db.pos + (16_i32) as u32, 0, _val);};
  ;
  stores.get_file(&(var_result));
  ;
  var_result
} /*block_1: ref(File)["result"]*/

fn t_4File_files(stores: &mut Stores, mut var_self: DbRef, mut var_result: DbRef) -> DbRef { //block_1: vector<ref(File)>["result"]
  ;
  vector::clear_vector(&(var_result), &mut stores.allocations);;
  ;
  if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) { //block_2: void
    ;
    stores.get_dir(&({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), &(var_result));
  } /*block_2: void*/ else {()};
  ;
  var_result
} /*block_1: vector<ref(File)>["result"]*/

fn t_4File_write(stores: &mut Stores, mut var_self: DbRef, mut var_v: String) {

}


fn t_4File_png(stores: &mut Stores, mut var_self: DbRef, mut var_result: DbRef) -> DbRef { //block_1: ref(Image)["result"]
  ;
  if (if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8}) == 255 { i32::MIN } else { i32::from(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + (16_i32) as u32, 0) as u8})) }) == (if (255u8) == 255 { i32::MIN } else { i32::from((255u8)) }) { //block_2: ref(Image)["result"]
    ;
    (var_result) = stores.alloc_record((12_i32) as u16);
    {let db = (var_result); let s_val = ("".to_string()).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + (0_i32) as u32, s_pos as i32);};
    {let db = (var_result); let _val = (0_i32); stores.store_mut(&db).set_int(db.rec, db.pos + (4_i32) as u32, _val);};
    {let db = (var_result); let _val = (0_i32); stores.store_mut(&db).set_int(db.rec, db.pos + (8_i32) as u32, _val);};
    {let db = (var_result); let _val = (0_i32); stores.store_mut(&db).set_int(db.rec, db.pos + (12_i32) as u32, _val);};
    ;
    stores.get_png(&({let db = (var_self); let store = stores.store(&db); Str::new(store.get_str(store.get_int(db.rec, db.pos + (8_i32) as u32) as u32))}), &(var_result));
    ;
    var_result
  } /*block_2: ref(Image)["result"]*/ else { //block_3: ref(Image)["result"]
    ;
    stores.null()
  } /*block_3: ref(Image)["result"]*/
} /*block_1: ref(Image)["result"]*/

fn n_env_variables(stores: &mut Stores) -> DbRef {
  todo!()
}


fn n_env_variable(stores: &mut Stores, mut var_name: String) -> String {
  todo!()
}


fn t_4text_starts_with(stores: &mut Stores, mut var_self: String, mut var_value: String) -> bool {
  todo!()
}


fn t_4text_ends_with(stores: &mut Stores, mut var_self: String, mut var_value: String) -> bool {
  todo!()
}


fn t_4text_trim(stores: &mut Stores, mut var_both: String) -> String {
  todo!()
}


fn t_4text_trim_start(stores: &mut Stores, mut var_self: String) -> String {
  todo!()
}


fn t_4text_trim_end(stores: &mut Stores, mut var_self: String) -> String {
  todo!()
}


fn t_4text_find(stores: &mut Stores, mut var_self: String, mut var_value: String) -> i32 {
  todo!()
}


fn t_4text_rfind(stores: &mut Stores, mut var_self: String, mut var_value: String) -> i32 {
  todo!()
}


fn t_4text_contains(stores: &mut Stores, mut var_self: String, mut var_value: String) -> bool {
  todo!()
}


fn t_4text_replace(stores: &mut Stores, mut var_self: String, mut var_value: String, mut var_with: String) -> String {
  todo!()
}


fn t_4text_to_lowercase(stores: &mut Stores, mut var_self: String) -> String {
  todo!()
}


fn t_4text_to_uppercase(stores: &mut Stores, mut var_self: String) -> String {
  todo!()
}


fn t_4text_is_lowercase(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_9character_is_lowercase(stores: &mut Stores, mut var_self: char) -> bool {
  todo!()
}


fn t_4text_is_uppercase(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_9character_is_uppercase(stores: &mut Stores, mut var_self: char) -> bool {
  todo!()
}


fn t_4text_is_numeric(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_9character_is_numeric(stores: &mut Stores, mut var_self: char) -> bool {
  todo!()
}


fn t_4text_is_alphanumeric(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_9character_is_alphanumeric(stores: &mut Stores, mut var_self: char) -> bool {
  todo!()
}


fn t_4text_is_alphabetic(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_9character_is_alphabetic(stores: &mut Stores, mut var_self: char) -> bool {
  todo!()
}


fn t_4text_is_whitespace(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn t_4text_is_control(stores: &mut Stores, mut var_self: String) -> bool {
  todo!()
}


fn n_arguments(stores: &mut Stores) -> DbRef {
  todo!()
}


fn n_directory(stores: &mut Stores, mut var_v: DbRef) -> String {
  todo!()
}


fn n_user_directory(stores: &mut Stores, mut var_v: DbRef) -> String {
  todo!()
}


fn n_program_directory(stores: &mut Stores, mut var_v: DbRef) -> String {
  todo!()
}


fn n_test(stores: &mut Stores) { //block_1: void
  let mut var___work_1: String = "".to_string();
  ;
  let mut var_test_value: i32 = { //block_2: integer
    let mut var_b: i32 = 0_i32;
    { //For block_3: void
      let mut var_a__index: i32 = i32::MIN;
      loop { //For loop_4
        let mut var_a: i32 = { //Iter range_5: integer
          if !(external::op_conv_bool_from_int((var_a__index))) {var_a__index = 6_i32} else {()};
          var_a__index = external::op_min_int((var_a__index), (1_i32));
          if (var_a__index) < (0_i32) {break} else {()};
          var_a__index
        } /*Iter range_5: integer*/;
        { //block_6: void
          var_b = external::op_add_int((external::op_mul_int((var_b), (10_i32))), (var_a));
        } /*block_6: void*/;
      } /*For loop_4*/;
    } /*For block_3: void*/;
    var_b
  } /*block_2: integer*/;
  ;
  if (var_test_value) == (543210_i32) {()} else {panic!("{}", ({ //Formatted string_7: text["__work_1"]
      var___work_1 = "Test failed ".to_string();
      external::format_long(&mut (var___work_1), (external::op_conv_long_from_int((var_test_value))), (10_i32) as u8, (0_i32), (32_i32) as u8, (false), (false));
      (var___work_1).push_str(&(" != 543210".to_string()));
      var___work_1
    } /*Formatted string_7: text["__work_1"]*/));};
  {};
} /*block_1: void*/

#[test]
fn code_reverse() {
    let mut stores = Stores::new();
    init(&mut stores);
    n_test(&mut stores);
}
