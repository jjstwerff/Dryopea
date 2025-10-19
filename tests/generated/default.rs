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
    let s = db.structure("Variable".to_string(), 12, 4294967295); // 8
    db.field(s, "name".to_string(), 5, 4);
    db.field(s, "value".to_string(), 5, 8);
    let s = db.structure("Pixel".to_string(), 3, 4294967295); // 9
    db.field(s, "r".to_string(), db.byte(0, false), 0);
    db.field(s, "g".to_string(), db.byte(0, false), 1);
    db.field(s, "b".to_string(), db.byte(0, false), 2);
    let s = db.structure("Image".to_string(), 20, 4294967295); // 11
    db.field(s, "name".to_string(), 5, 4);
    db.field(s, "width".to_string(), db.int(), 8);
    db.field(s, "height".to_string(), db.int(), 12);
    db.field(s, "data".to_string(), db.vector(9), 16);
    let s = db.structure("File".to_string(), 17, 4294967295); // 13
    db.field(s, "path".to_string(), 5, 4);
    db.field(s, "size".to_string(), 1, 8);
    db.field(s, "dir".to_string(), 4, 16);
    db.vector(5);
    let s = db.structure("main_vector<text>".to_string(), 8, 4294967295); // 14
    db.field(s, "vector".to_string(), db.vector(5), 4);
    db.vector(13);
    let s = db.structure("main_vector<File>".to_string(), 8, 4294967295); // 15
    db.field(s, "vector".to_string(), db.vector(13), 4);
    db.vector(9);
}

fn _tp_integer_abs(stores: &mut Stores, var_0: i32) -> i32 { //block_1: integer
  external::op_abs_int((var_both))
} //block_1: integer

fn _tp_long_abs(stores: &mut Stores, var_0: i64) -> i64 { //block_1: long
  external::op_abs_long((var_both))
} //block_1: long

fn _tp_single_abs(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).abs()
} //block_1: single

fn _tp_single_cos(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).cos()
} //block_1: single

fn _tp_single_sin(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).sin()
} //block_1: single

fn _tp_single_tan(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).tan()
} //block_1: single

fn _tp_single_acos(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).acos()
} //block_1: single

fn _tp_single_asin(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).asin()
} //block_1: single

fn _tp_single_atan(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).atan()
} //block_1: single

fn _tp_single_atan2(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 { //block_1: single
  (var_both).atan2((var_v2))
} //block_1: single

fn _tp_single_ceil(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).ceil()
} //block_1: single

fn _tp_single_floor(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).floor()
} //block_1: single

fn _tp_single_round(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).round()
} //block_1: single

fn _tp_single_sqrt(stores: &mut Stores, var_0: f32) -> f32 { //block_1: single
  (var_both).sqrt()
} //block_1: single

fn _tp_single_log(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 { //block_1: single
  (var_both).log((var_v2))
} //block_1: single

fn _tp_single_pow(stores: &mut Stores, var_0: f32, var_1: f32) -> f32 { //block_1: single
  (var_both).powf((var_v2))
} //block_1: single

fn _tp_float_abs(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).abs()
} //block_1: float

fn _tp_float_cos(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).cos()
} //block_1: float

fn _tp_float_sin(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).sin()
} //block_1: float

fn _tp_float_tan(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).tan()
} //block_1: float

fn _tp_float_acos(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).acos()
} //block_1: float

fn _tp_float_asin(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).asin()
} //block_1: float

fn _tp_float_atan(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).atan()
} //block_1: float

fn _tp_float_atan2(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 { //block_1: float
  (var_both).atan2((var_v2))
} //block_1: float

fn _tp_float_ceil(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).ceil()
} //block_1: float

fn _tp_float_floor(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).floor()
} //block_1: float

fn _tp_float_round(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).round()
} //block_1: float

fn _tp_float_sqrt(stores: &mut Stores, var_0: f64) -> f64 { //block_1: float
  (var_both).sqrt()
} //block_1: float

fn _tp_float_log(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 { //block_1: float
  (var_both).log((var_v2))
} //block_1: float

fn _tp_float_pow(stores: &mut Stores, var_0: f64, var_1: f64) -> f64 { //block_1: float
  (var_both).powf((var_v2))
} //block_1: float

fn _tp_text_len(stores: &mut Stores, var_0: Str) -> i32 { //block_1: integer
  (var_both).len() as i32
} //block_1: integer

fn _tp_character_len(stores: &mut Stores, var_0: i32) -> i32 { //block_1: integer
  OpLengthCharacter(stores, var_both)
} //block_1: integer

fn _tp_vector_len(stores: &mut Stores, var_0: DbRef) -> i32 { //block_1: integer
  vector::length_vector(&(var_both), &s.database.allocations) as i32
} //block_1: integer

fn assert(stores: &mut Stores, var_0: bool, var_1: Str) {

}


fn panic(stores: &mut Stores, var_0: Str) {

}


fn print(stores: &mut Stores, var_0: Str) { //block_1: void
  print!("{}", (var_v1));
} //block_1: void

fn OpGetFile(stores: &mut Stores, var_0: DbRef) -> bool {

}


fn OpGetDir(stores: &mut Stores, var_0: Str, var_1: DbRef) -> bool {

}


fn OpGetPngImage(stores: &mut Stores, var_0: Str, var_1: DbRef) -> bool {

}


fn OpGetFileText(stores: &mut Stores, var_0: DbRef, var_1: DbRef) {

}


fn _tp_Pixel_value(stores: &mut Stores, var_0: DbRef) -> i32 { //block_1: integer
  external::op_add_int((external::op_add_int((external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((0_i32)), i32::from((0_i32)))}), (65536_i32))), (external::op_mul_int(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((1_i32)), i32::from((0_i32)))}), (256_i32))))), ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((2_i32)), i32::from((0_i32)))}))
} //block_1: integer

fn _tp_File_content(stores: &mut Stores, var_0: DbRef, var_1: DbRef) -> Str { //block_1: &text["result"]
  var_result = "".to_string();
  var_txt = "".to_string();
  OpGetFileText(stores, var_self, OpCreateRef(stores, var_txt));
  OpAppendRefText(stores, {let r = *s.get_var::<DbRef>((var_result)); stores.valid(&r); r}, 0_i32, var_txt);
  OpFreeText(stores, var_txt);
  var_result
} //block_1: &text["result"]

fn _tp_File_lines(stores: &mut Stores, var_0: DbRef, var_1: DbRef) -> DbRef { //block_1: vector<text>["l"]
  var___ref_1 = Null;
  var___work_1 = "".to_string();
  OpDatabase(stores, var___ref_1, 14_i32);
  var_l = DbRef {store_nr: (var___ref_1).store_nr, rec: (var___ref_1).rec, pos: (var___ref_1).pos + u32::from((4_i32))};
  {let db = (var___ref_1); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((4_i32)), (0_i32));};
  var_c = _tp_File_content(stores, var_self, { //default ref_2: ref(reference)["__work_1"]
    OpCreateRef(stores, var___work_1)
  } //default ref_2: ref(reference)["__work_1"]);
  var_p = 0_i32;
  { //For block_3: void
    var_ch#index = 0_i32;
    loop { //For loop_4
      var_ch = { //for text next_5: character
        var__for_result_1 = OpGetCharacter(stores, var_c, var_ch#index);
        var_ch#index = external::op_add_int((var_ch#index), (OpLengthCharacter(stores, var__for_result_1)));
        var__for_result_1
      } //for text next_5: character;
      if !(external::op_conv_bool_from_int((var_ch))) { //break_6: void
        Break(0)
      } //break_6: void else {Null};
      { //block_7: void
        if (external::op_conv_bool_from_int((var_ch))) == (!("
".to_string()).is_empty()) { //block_8: void
          var__elm_2 = OpNewRecord(stores, var_l, 7_i32, 65535_i32);
          {let db = (var__elm_2); let s_val = (OpGetTextSub(stores, var_c, var_p, external::op_min_int((var_ch#index), (1_i32)))).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((0_i32)), s_pos as i32);};
          OpFinishRecord(stores, var_l, var__elm_2, 7_i32, 65535_i32);
          var_p = external::op_min_single_int((1_i32))
        } //block_8: void else {if (var_p) < (0_i32) { //block_9: void
            var_p = var_ch#index
          } //block_9: void else {Null}}
      } //block_7: void;
    } //For loop_4
  } //For block_3: void;
  if (var_p) > (0_i32) { //block_10: void
    var__elm_3 = OpNewRecord(stores, var_l, 7_i32, 65535_i32);
    {let db = (var__elm_3); let s_val = (OpGetTextSub(stores, var_c, var_p, _tp_text_len(stores, var_c))).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((0_i32)), s_pos as i32);};
    OpFinishRecord(stores, var_l, var__elm_3, 7_i32, 65535_i32)
  } //block_10: void else {Null};
  OpFreeText(stores, var_c);
  OpFreeText(stores, var___work_1);
  OpFreeRef(stores, var___ref_1);
  var_l
} //block_1: vector<text>["l"]

fn file(stores: &mut Stores, var_0: Str, var_1: DbRef) -> DbRef { //block_1: ref(File)["result"]
  OpDatabase(stores, var_result, 13_i32);
  {let db = (var_result); let s_val = (var_path).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((4_i32)), s_pos as i32);};
  {let db = (var_result); stores.store_mut(&db).set_long(db.rec, db.pos + u32::from((8_i32)), (0_i64));};
  {let db = (var_result); stores.store_mut(&db).set_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)), (if false {1_i32} else {0_i32}));};
  if stores.get_file(&(var_result)) { //block_2: ref(File)["result"]
    var_result
  } //block_2: ref(File)["result"] else { //block_3: null
    stores.null()
  } //block_3: null
} //block_1: ref(File)["result"]

fn _tp_File_files(stores: &mut Stores, var_0: DbRef, var_1: DbRef) -> DbRef { //block_1: vector<ref(File)>["result"]
  var___ref_1 = Null;
  OpDatabase(stores, var___ref_1, 15_i32);
  var_result = DbRef {store_nr: (var___ref_1).store_nr, rec: (var___ref_1).rec, pos: (var___ref_1).pos + u32::from((4_i32))};
  {let db = (var___ref_1); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((4_i32)), (0_i32));};
  if ({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)))}) == (1_i32) { //block_2: void
    Drop(Call(325, [Call(270, [Var(0), Int(4)]), Var(1)]))
  } //block_2: void else {Null};
  OpFreeRef(stores, var___ref_1);
  var_result
} //block_1: vector<ref(File)>["result"]

fn _tp_File_png(stores: &mut Stores, var_0: DbRef, var_1: DbRef) -> DbRef { //block_1: ref(Image)["result"]
  if !(({let db = (var_self); stores.store(&db).get_byte(db.rec, db.pos + u32::from((16_i32)), i32::from((0_i32)))}) == (1_i32)) { //block_2: ref(Image)["result"]
    OpDatabase(stores, var_result, 11_i32);
    {let db = (var_result); let s_val = ("".to_string()).to_string(); let store = stores.store_mut(&db); let s_pos = store.set_str(&s_val); store.set_int(db.rec, db.pos + u32::from((4_i32)), s_pos as i32);};
    {let db = (var_result); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((8_i32)), (0_i32));};
    {let db = (var_result); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((12_i32)), (0_i32));};
    {let db = (var_result); stores.store_mut(&db).set_int(db.rec, db.pos + u32::from((16_i32)), (0_i32));};
    Drop(Call(326, [Call(270, [Var(0), Int(4)]), Var(1)]));
    var_result
  } //block_2: ref(Image)["result"] else { //block_3: null
    stores.null()
  } //block_3: null
} //block_1: ref(Image)["result"]

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


fn _tp_text_rfind(stores: &mut Stores, var_0: Str, var_1: Str) -> i32 {

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


