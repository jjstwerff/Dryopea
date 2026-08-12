#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(redundant_semicolons)]
#![allow(unused_assignments)]
#![allow(unused_labels)]
#![allow(unused_braces)]
#![allow(clippy::double_parens)]
#![allow(clippy::unused_unit)]
#![allow(unused_unsafe)]

extern crate loft;
unsafe extern "C" {
}
use loft::database::Stores;
use loft::keys::{DbRef, Str, Key, Content};
use loft::ops;
use loft::vector;
use loft::hash;
use loft::tree;
use loft::codegen_runtime;
use loft::codegen_runtime::*;
static __C_LIBS: &[(&str, &str)] = &[
];
static __C_LIB_SYMS: &[(&str, &[&str])] = &[
];
fn init(cell: &std::cell::UnsafeCell<Stores>) {
    let db: &mut Stores = unsafe { &mut *cell.get() };
    let t0: u16 = 0;
    let t1: u16 = 1;
    let t2: u16 = 2;
    let t3: u16 = 3;
    let t4: u16 = 4;
    let t5: u16 = 5;
    let t6: u16 = 6;
    let _ = (t0, t1, t2, t3, t4, t5, t6); // suppress unused-let warnings for unreferenced base types
    let t7 = db.enumerate("FieldValue");
    let t8 = db.structure("__typevar_Self", 0);
    let t9 = db.structure("__typevar_T", 0);
    let t10 = db.structure("main_vector<T>", 0);
    let vec_vector = db.vector(t9);
    db.field(t10, "vector", vec_vector);
    db.set_field_nullable(t10, "vector", true);
    let t11 = db.vector(t9);
    let _ = t11; // may be unused
    let t12 = db.structure("FvBool", 1);
    let byte_enum = db.byte(0, false);
    db.field(t12, "enum", byte_enum);
    db.set_field_nullable(t12, "enum", true);
    db.field(t12, "v", t4);
    let t13 = db.byte(0, false);
    let _ = t13; // may be unused
    let t14 = db.structure("FvInt", 2);
    let byte_enum = db.byte(0, false);
    db.field(t14, "enum", byte_enum);
    db.set_field_nullable(t14, "enum", true);
    db.field(t14, "v", 0);
    let t15 = db.structure("FvLong", 3);
    let byte_enum = db.byte(0, false);
    db.field(t15, "enum", byte_enum);
    db.set_field_nullable(t15, "enum", true);
    db.field(t15, "v", 0);
    let t16 = db.structure("FvFloat", 4);
    let byte_enum = db.byte(0, false);
    db.field(t16, "enum", byte_enum);
    db.set_field_nullable(t16, "enum", true);
    db.field(t16, "v", t3);
    let t17 = db.structure("FvSingle", 5);
    let byte_enum = db.byte(0, false);
    db.field(t17, "enum", byte_enum);
    db.set_field_nullable(t17, "enum", true);
    db.field(t17, "v", t2);
    let t18 = db.structure("FvChar", 6);
    let byte_enum = db.byte(0, false);
    db.field(t18, "enum", byte_enum);
    db.set_field_nullable(t18, "enum", true);
    db.field(t18, "v", t6);
    let t19 = db.structure("FvText", 7);
    let byte_enum = db.byte(0, false);
    db.field(t19, "enum", byte_enum);
    db.set_field_nullable(t19, "enum", true);
    db.field(t19, "v", t5);
    let t20 = db.structure("StructField", 0);
    db.field(t20, "name", t5);
    db.field(t20, "value", t7);
    db.set_field_nullable(t20, "value", true);
    db.field(t20, "nullable", t4);
    let t21 = db.vector(t0);
    let _ = t21; // may be unused
    let t22 = db.structure("main_vector<integer>", 0);
    let vec_vector = db.vector(t0);
    db.field(t22, "vector", vec_vector);
    db.set_field_nullable(t22, "vector", true);
    let t23 = db.vector(t5);
    let _ = t23; // may be unused
    let t24 = db.enumerate("Format");
    let t25 = db.enumerate("FileResult");
    let t26 = db.structure("EnvVariable", 0);
    db.field(t26, "name", t5);
    db.field(t26, "value", t5);
    let t27 = db.structure("File", 0);
    db.field(t27, "path", t5);
    db.field(t27, "size", 0);
    db.field(t27, "format", t24);
    db.set_field_nullable(t27, "format", true);
    let int_ref = db.int(-2147483647, true);
    db.field(t27, "ref", int_ref);
    db.set_field_nullable(t27, "ref", true);
    db.field(t27, "current", 0);
    db.field(t27, "next", 0);
    db.set_field_nullable(t27, "content", true);
    db.set_field_nullable(t27, "lines", true);
    db.set_field_nullable(t27, "exists", true);
    db.set_field_nullable(t27, "set_file_size", true);
    db.set_field_nullable(t27, "seek", true);
    db.set_field_nullable(t27, "position", true);
    db.set_field_nullable(t27, "sync", true);
    db.set_field_nullable(t27, "files", true);
    db.set_field_nullable(t27, "write", true);
    let t28 = db.int(-2147483647, true);
    let _ = t28; // may be unused
    let t29 = db.structure("main_vector<text>", 0);
    let vec_vector = db.vector(t5);
    db.field(t29, "vector", vec_vector);
    db.set_field_nullable(t29, "vector", true);
    let t30 = db.structure("main_vector<File>", 0);
    let vec_vector = db.vector(t27);
    db.field(t30, "vector", vec_vector);
    db.set_field_nullable(t30, "vector", true);
    let t31 = db.vector(t27);
    let _ = t31; // may be unused
    let t32 = db.enumerate("ArgValue");
    let t33 = db.structure("NullVal", 1);
    let byte_enum = db.byte(0, false);
    db.field(t33, "enum", byte_enum);
    db.set_field_nullable(t33, "enum", true);
    let t34 = db.structure("BoolVal", 2);
    let byte_enum = db.byte(0, false);
    db.field(t34, "enum", byte_enum);
    db.set_field_nullable(t34, "enum", true);
    db.field(t34, "b", t4);
    let t35 = db.structure("IntVal", 3);
    let byte_enum = db.byte(0, false);
    db.field(t35, "enum", byte_enum);
    db.set_field_nullable(t35, "enum", true);
    db.field(t35, "n", 0);
    let t36 = db.structure("LongVal", 4);
    let byte_enum = db.byte(0, false);
    db.field(t36, "enum", byte_enum);
    db.set_field_nullable(t36, "enum", true);
    db.field(t36, "n", 0);
    let t37 = db.structure("FloatVal", 5);
    let byte_enum = db.byte(0, false);
    db.field(t37, "enum", byte_enum);
    db.set_field_nullable(t37, "enum", true);
    db.field(t37, "f", t3);
    let t38 = db.structure("SingleVal", 6);
    let byte_enum = db.byte(0, false);
    db.field(t38, "enum", byte_enum);
    db.set_field_nullable(t38, "enum", true);
    db.field(t38, "f", t2);
    let t39 = db.structure("CharVal", 7);
    let byte_enum = db.byte(0, false);
    db.field(t39, "enum", byte_enum);
    db.set_field_nullable(t39, "enum", true);
    db.field(t39, "c", t6);
    let t40 = db.structure("TextVal", 8);
    let byte_enum = db.byte(0, false);
    db.field(t40, "enum", byte_enum);
    db.set_field_nullable(t40, "enum", true);
    db.field(t40, "t", t5);
    let t41 = db.structure("RefVal", 9);
    let byte_enum = db.byte(0, false);
    db.field(t41, "enum", byte_enum);
    db.set_field_nullable(t41, "enum", true);
    db.field(t41, "store", 0);
    db.field(t41, "rec", 0);
    db.field(t41, "pos", 0);
    let t42 = db.structure("FnVal", 10);
    let byte_enum = db.byte(0, false);
    db.field(t42, "enum", byte_enum);
    db.set_field_nullable(t42, "enum", true);
    db.field(t42, "d_nr", 0);
    let t43 = db.structure("OtherVal", 11);
    let byte_enum = db.byte(0, false);
    db.field(t43, "enum", byte_enum);
    db.set_field_nullable(t43, "enum", true);
    db.field(t43, "description", t5);
    let t44 = db.structure("ArgInfo", 0);
    db.field(t44, "name", t5);
    db.field(t44, "type_name", t5);
    db.field(t44, "value", t32);
    db.set_field_nullable(t44, "value", true);
    let t45 = db.structure("VarInfo", 0);
    db.field(t45, "name", t5);
    db.field(t45, "type_name", t5);
    db.field(t45, "value", t32);
    db.set_field_nullable(t45, "value", true);
    let t46 = db.structure("StackFrame", 0);
    db.field(t46, "function", t5);
    db.field(t46, "file", t5);
    db.field(t46, "line", 0);
    let vec_arguments = db.vector(t44);
    db.field(t46, "arguments", vec_arguments);
    db.set_field_nullable(t46, "arguments", true);
    let vec_variables = db.vector(t45);
    db.field(t46, "variables", vec_variables);
    db.set_field_nullable(t46, "variables", true);
    let t47 = db.vector(t44);
    let _ = t47; // may be unused
    let t48 = db.vector(t45);
    let _ = t48; // may be unused
    let t49 = db.structure("main_vector<ArgInfo>", 0);
    let vec_vector = db.vector(t44);
    db.field(t49, "vector", vec_vector);
    db.set_field_nullable(t49, "vector", true);
    let t50 = db.structure("main_vector<VarInfo>", 0);
    let vec_vector = db.vector(t45);
    db.field(t50, "vector", vec_vector);
    db.set_field_nullable(t50, "vector", true);
    let t51 = db.enumerate("CoroutineStatus");
    let t52 = db.enumerate("JsonValue");
    let t53 = db.structure("JNull", 1);
    let byte_enum = db.byte(0, false);
    db.field(t53, "enum", byte_enum);
    db.set_field_nullable(t53, "enum", true);
    let t54 = db.structure("JBool", 2);
    let byte_enum = db.byte(0, false);
    db.field(t54, "enum", byte_enum);
    db.set_field_nullable(t54, "enum", true);
    db.field(t54, "value", t4);
    let t55 = db.structure("JNumber", 3);
    let byte_enum = db.byte(0, false);
    db.field(t55, "enum", byte_enum);
    db.set_field_nullable(t55, "enum", true);
    db.field(t55, "value", t3);
    let t56 = db.structure("JString", 4);
    let byte_enum = db.byte(0, false);
    db.field(t56, "enum", byte_enum);
    db.set_field_nullable(t56, "enum", true);
    db.field(t56, "value", t5);
    let t57 = db.structure("JArray", 5);
    let byte_enum = db.byte(0, false);
    db.field(t57, "enum", byte_enum);
    db.set_field_nullable(t57, "enum", true);
    let vec_items = db.vector(t52);
    db.field(t57, "items", vec_items);
    db.set_field_nullable(t57, "items", true);
    let t58 = db.vector(t52);
    let _ = t58; // may be unused
    let t59 = db.structure("JObject", 6);
    let byte_enum = db.byte(0, false);
    db.field(t59, "enum", byte_enum);
    db.set_field_nullable(t59, "enum", true);
    let t60 = db.structure("JsonField", 0);
    db.field(t60, "name", t5);
    db.field(t60, "value", t52);
    db.set_field_nullable(t60, "value", true);
    let vec_fields = db.vector(t60);
    db.field(t59, "fields", vec_fields);
    db.set_field_nullable(t59, "fields", true);
    let t61 = db.vector(t60);
    let _ = t61; // may be unused
    let t62 = db.structure("JInteger", 7);
    let byte_enum = db.byte(0, false);
    db.field(t62, "enum", byte_enum);
    db.set_field_nullable(t62, "enum", true);
    db.field(t62, "value", 0);
    let t63 = db.structure("main_vector<JsonValue>", 0);
    let vec_vector = db.vector(t52);
    db.field(t63, "vector", vec_vector);
    db.set_field_nullable(t63, "vector", true);
    let t64 = db.structure("main_vector<JsonField>", 0);
    let vec_vector = db.vector(t60);
    db.field(t64, "vector", vec_vector);
    db.set_field_nullable(t64, "vector", true);
    let t65 = db.enumerate("TypeKind");
    let t66 = db.enumerate("CollectionKind");
    let t67 = db.structure("FieldInfo", 0);
    db.field(t67, "name", t5);
    db.field(t67, "type_name", t5);
    db.field(t67, "position", 0);
    db.field(t67, "kind", t65);
    db.set_field_nullable(t67, "kind", true);
    db.field(t67, "nullable", t4);
    let t68 = db.structure("KeyInfo", 0);
    db.field(t68, "name", t5);
    db.field(t68, "position", 0);
    db.field(t68, "ascending", t4);
    let t69 = db.structure("VariantInfo", 0);
    db.field(t69, "name", t5);
    db.field(t69, "tag", 0);
    let t70 = db.structure("TypeInfo", 0);
    db.field(t70, "name", t5);
    db.field(t70, "kind", t65);
    db.set_field_nullable(t70, "kind", true);
    db.field(t70, "size", 0);
    let vec_fields = db.vector(t67);
    db.field(t70, "fields", vec_fields);
    db.set_field_nullable(t70, "fields", true);
    let vec_variants = db.vector(t69);
    db.field(t70, "variants", vec_variants);
    db.set_field_nullable(t70, "variants", true);
    db.field(t70, "element", t5);
    db.field(t70, "collection", t66);
    db.set_field_nullable(t70, "collection", true);
    let vec_keys = db.vector(t68);
    db.field(t70, "keys", vec_keys);
    db.set_field_nullable(t70, "keys", true);
    let t71 = db.vector(t67);
    let _ = t71; // may be unused
    let t72 = db.vector(t69);
    let _ = t72; // may be unused
    let t73 = db.vector(t68);
    let _ = t73; // may be unused
    let t74 = db.structure("ValueInfo", 0);
    db.field(t74, "kind", t65);
    db.set_field_nullable(t74, "kind", true);
    db.field(t74, "is_null", t4);
    db.field(t74, "i", 0);
    db.field(t74, "f", t3);
    db.field(t74, "t", t5);
    let t75 = db.structure("main_vector<FieldInfo>", 0);
    let vec_vector = db.vector(t67);
    db.field(t75, "vector", vec_vector);
    db.set_field_nullable(t75, "vector", true);
    let t76 = db.structure("main_vector<VariantInfo>", 0);
    let vec_vector = db.vector(t69);
    db.field(t76, "vector", vec_vector);
    db.set_field_nullable(t76, "vector", true);
    let t77 = db.structure("main_vector<KeyInfo>", 0);
    let vec_vector = db.vector(t68);
    db.field(t77, "vector", vec_vector);
    db.set_field_nullable(t77, "vector", true);
    let t78 = db.structure("Slot", 0);
    db.field(t78, "idx", 0);
    db.field(t78, "taken", t4);
    let t79 = db.structure("main_vector<Slot>", 0);
    let vec_vector = db.vector(t78);
    db.field(t79, "vector", vec_vector);
    db.set_field_nullable(t79, "vector", true);
    let t80 = db.vector(t78);
    let _ = t80; // may be unused
    db.value(t7, "FvBool", t12);
    db.value(t7, "FvInt", t14);
    db.value(t7, "FvLong", t15);
    db.value(t7, "FvFloat", t16);
    db.value(t7, "FvSingle", t17);
    db.value(t7, "FvChar", t18);
    db.value(t7, "FvText", t19);
    db.value(t24, "TextFile", u16::MAX);
    db.value(t24, "LittleEndian", u16::MAX);
    db.value(t24, "BigEndian", u16::MAX);
    db.value(t24, "Directory", u16::MAX);
    db.value(t24, "NotExists", u16::MAX);
    db.value(t25, "Ok", u16::MAX);
    db.value(t25, "NotFound", u16::MAX);
    db.value(t25, "PermissionDenied", u16::MAX);
    db.value(t25, "IsDirectory", u16::MAX);
    db.value(t25, "Other", u16::MAX);
    db.value(t25, "ok", u16::MAX);
    db.value(t32, "NullVal", t33);
    db.value(t32, "BoolVal", t34);
    db.value(t32, "IntVal", t35);
    db.value(t32, "LongVal", t36);
    db.value(t32, "FloatVal", t37);
    db.value(t32, "SingleVal", t38);
    db.value(t32, "CharVal", t39);
    db.value(t32, "TextVal", t40);
    db.value(t32, "RefVal", t41);
    db.value(t32, "FnVal", t42);
    db.value(t32, "OtherVal", t43);
    db.value(t51, "Created", u16::MAX);
    db.value(t51, "Suspended", u16::MAX);
    db.value(t51, "Running", u16::MAX);
    db.value(t51, "Exhausted", u16::MAX);
    db.value(t52, "JNull", t53);
    db.value(t52, "JBool", t54);
    db.value(t52, "JNumber", t55);
    db.value(t52, "JString", t56);
    db.value(t52, "JArray", t57);
    db.value(t52, "JObject", t59);
    db.value(t52, "JInteger", t62);
    db.value(t52, "field", u16::MAX);
    db.value(t52, "item", u16::MAX);
    db.value(t52, "len", u16::MAX);
    db.value(t52, "as_text", u16::MAX);
    db.value(t52, "as_number", u16::MAX);
    db.value(t52, "as_long", u16::MAX);
    db.value(t52, "as_bool", u16::MAX);
    db.value(t52, "kind", u16::MAX);
    db.value(t52, "keys", u16::MAX);
    db.value(t52, "fields", u16::MAX);
    db.value(t52, "has_field", u16::MAX);
    db.value(t52, "to_json", u16::MAX);
    db.value(t52, "to_json_pretty", u16::MAX);
    db.value(t65, "IntegerKind", u16::MAX);
    db.value(t65, "LongKind", u16::MAX);
    db.value(t65, "SingleKind", u16::MAX);
    db.value(t65, "FloatKind", u16::MAX);
    db.value(t65, "BooleanKind", u16::MAX);
    db.value(t65, "TextKind", u16::MAX);
    db.value(t65, "CharacterKind", u16::MAX);
    db.value(t65, "RecordKind", u16::MAX);
    db.value(t65, "EnumKind", u16::MAX);
    db.value(t65, "VariantKind", u16::MAX);
    db.value(t65, "VectorKind", u16::MAX);
    db.value(t65, "KeyedKind", u16::MAX);
    db.value(t65, "RefKind", u16::MAX);
    db.value(t65, "OtherKind", u16::MAX);
    db.value(t66, "NotKeyed", u16::MAX);
    db.value(t66, "KeyedHash", u16::MAX);
    db.value(t66, "KeyedIndex", u16::MAX);
    db.value(t66, "KeyedSorted", u16::MAX);
    db.value(t66, "KeyedOrdered", u16::MAX);
    db.value(t66, "KeyedRadix", u16::MAX);
    db.value(t66, "KeyedTrie", u16::MAX);
    db.verify_schema_ids(&[
        "integer",
        "long",
        "single",
        "float",
        "boolean",
        "text",
        "character",
        "FieldValue",
        "__typevar_Self",
        "__typevar_T",
        "main_vector<T>",
        "vector<__typevar_T>",
        "FvBool",
        "byte",
        "FvInt",
        "FvLong",
        "FvFloat",
        "FvSingle",
        "FvChar",
        "FvText",
        "StructField",
        "vector<integer>",
        "main_vector<integer>",
        "vector<text>",
        "Format",
        "FileResult",
        "EnvVariable",
        "File",
        "int<-2147483647,true>",
        "main_vector<text>",
        "main_vector<File>",
        "vector<File>",
        "ArgValue",
        "NullVal",
        "BoolVal",
        "IntVal",
        "LongVal",
        "FloatVal",
        "SingleVal",
        "CharVal",
        "TextVal",
        "RefVal",
        "FnVal",
        "OtherVal",
        "ArgInfo",
        "VarInfo",
        "StackFrame",
        "vector<ArgInfo>",
        "vector<VarInfo>",
        "main_vector<ArgInfo>",
        "main_vector<VarInfo>",
        "CoroutineStatus",
        "JsonValue",
        "JNull",
        "JBool",
        "JNumber",
        "JString",
        "JArray",
        "vector<JsonValue>",
        "JObject",
        "JsonField",
        "vector<JsonField>",
        "JInteger",
        "main_vector<JsonValue>",
        "main_vector<JsonField>",
        "TypeKind",
        "CollectionKind",
        "FieldInfo",
        "KeyInfo",
        "VariantInfo",
        "TypeInfo",
        "vector<FieldInfo>",
        "vector<VariantInfo>",
        "vector<KeyInfo>",
        "ValueInfo",
        "main_vector<FieldInfo>",
        "main_vector<VariantInfo>",
        "main_vector<KeyInfo>",
        "Slot",
        "main_vector<Slot>",
        "vector<Slot>",
    ]);
    db.finish();
}

fn i_parse_errors(cell: &std::cell::UnsafeCell<Stores>) -> String {
  let stores: &mut Stores = unsafe { &mut *cell.get() };
  loft::codegen_runtime::i_parse_errors(stores)
}


// loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:8
fn n_mutate_through_a_loop_variable(cell: &std::cell::UnsafeCell<Stores>) -> u8 { //block_1: boolean
  if loft::live_dispatch::live_flipped(0) { return loft::live_dispatch::live_call_u8(cell, 0, |st| { }); }
  let stores: &mut Stores = unsafe { &mut *cell.get() };
  cr_call_push("mutate_through_a_loop_variable", "/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft", 8);
  let _call_guard = codegen_runtime::CallGuard;
  let mut var___vdb_1: DbRef = DbRef::NULL;
  let mut var___ref_1: DbRef = DbRef::NULL;
  var___vdb_1 = DbRef::NULL;
  let mut var_lw_v: DbRef = DbRef::NULL;
  let mut var___ncc_1: DbRef = DbRef::NULL;
  // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:9
  var___vdb_1 = OpDatabase(cell,var___vdb_1, 79_i32);
  var_lw_v = {{ let _v_v1 = (var___vdb_1); DbRef {store_nr: _v_v1.store_nr, rec: _v_v1.rec, pos: _v_v1.pos + (0_i64) as u32} }};
  {{ let _v_val = (0_i64); {let db = (var___vdb_1); let v = if _v_val == i64::MIN { i32::MIN } else { _v_val as i32 }; if db.rec != 0 { stores.store_mut(&db).set_i32_raw(db.rec, db.pos + (0_i64) as u32, v); }} }};
  // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:10
  { //For block_2: void
    let mut var_lw_i__index: i64 = i64::MIN;
    'l3: loop { //For loop_3
      let mut var_lw_i: i64 = { //Iter range_4: integer
        var_lw_i__index = if ((((ops::op_conv_bool_from_int((var_lw_i__index))) as u8) != 1) as u8) == 1 {0_i64} else {ops::op_add_int((var_lw_i__index), (1_i64))};
        if ((((3_i64) as i64) <= ((var_lw_i__index) as i64)) as u8) == 1 {break} else {()};
        var_lw_i__index
        } /*Iter range_4: integer*/;
      { //block_5: void
        {vector::pre_alloc_vector(&(var_lw_v), (1_i64) as u32, (9_i64) as u32, &mut stores.allocations);};
        let mut var__elm_1: DbRef = OpNewRecord(cell, var_lw_v, 80_i32, 65535_i32);
        {{let db = (var__elm_1); let v = (var_lw_i); if db.rec != 0 { stores.store_mut(&db).set_int(db.rec, db.pos + (0_i64) as u32, v); }}};
        {{let db = (var__elm_1); let v = ((false) as u8); if db.rec != 0 { stores.store_mut(&db).set_byte(db.rec, db.pos + (8_i64) as u32, 0, i32::from(v)); }}};
        OpFinishRecord(cell, var_lw_v, var__elm_1, 80_i32, 65535_i32);
        } /*block_5: void*/;
      } /*For loop_3*/;
    } /*For block_2: void*/;
  // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:14
  let mut var_lw_at: i64 = 0_i64;
  // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:15
  { //For block_6: void
    let mut var__vector_1: DbRef = var_lw_v;
    let mut var_lw_t__index: i64 = -1_i64;
    'l7: loop { //For loop_7
      let mut var_lw_t: DbRef = { //iter next_8: ref(Slot)
        var_lw_t__index = ops::op_add_int((var_lw_t__index), (1_i64));
        vector::get_vector(&(var__vector_1), (9_i64) as u32, (var_lw_t__index), &stores.allocations)
        } /*iter next_8: ref(Slot)*/;
      if ((((i64::from(vector::length_vector(&(var__vector_1), &stores.allocations))) as i64) <= ((var_lw_t__index) as i64)) as u8) == 1 { //break_9: void
        break;
        } /*break_9: void*/ else {()};
      if ((((var_lw_t__index) as i64) < ((0_i64) as i64)) as u8) == 1 { //break_10: void
        break;
        } /*break_10: void*/ else {()};
      { //block_11: void
        // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:16
        if ((((var_lw_at) as i64) == ((1_i64) as i64)) as u8) == 1 { //block_12: void
          {{let db = (var_lw_t); let v = ((true) as u8); if db.rec != 0 { stores.store_mut(&db).set_byte(db.rec, db.pos + (8_i64) as u32, 0, i32::from(v)); }}};
          } /*block_12: void*/ else {()};
        // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:17
        var_lw_at = ops::op_add_int((var_lw_at), (1_i64));
        } /*block_11: void*/;
      } /*For loop_7*/;
    } /*For block_6: void*/;
  // loft:/home/jurjens/workspace/dryopea/loft_repros/lost_write_false_positive/prog/../slotlib/src/slotlib.loft:19
  let _pre_0 = { //ncc_13: ref(Slot)["lw_v"]
    ();
    var___ncc_1 = vector::get_vector(&(var_lw_v), (9_i64) as u32, (1_i64), &stores.allocations);
    if (((var___ncc_1).rec != 0) as u8) == 1 {var___ncc_1} else { //Object_14: ref(Slot)["__ref_1"]
      var___ref_1 = OpDatabase(cell,var___ref_1, 78_i32);
      {{let db = (var___ref_1); let v = (0_i64); if db.rec != 0 { stores.store_mut(&db).set_int(db.rec, db.pos + (0_i64) as u32, v); }}};
      {{let db = (var___ref_1); let v = ((false) as u8); if db.rec != 0 { stores.store_mut(&db).set_byte(db.rec, db.pos + (8_i64) as u32, 0, i32::from(v)); }}};
      var___ref_1
      } /*Object_14: ref(Slot)["__ref_1"]*/
    } /*ncc_13: ref(Slot)["lw_v"]*/;
  let mut var___ret_1: u8 = ({{ let _ha0 = _pre_0; {let db = (_ha0); if db.rec == 0 { 255u8 } else { let r = stores.store(&db).get_byte(db.rec, db.pos + (8_i64) as u32, 0); if r < 0 { 255u8 } else { r as u8 } }} }}) as u8;
  OpFreeRef(cell,var___vdb_1, "var___vdb_1"); var___vdb_1.store_nr = u16::MAX;
  OpFreeRef(cell,var___ref_1, "var___ref_1"); var___ref_1.store_nr = u16::MAX;
  return (var___ret_1) as u8
  } /*block_1: boolean*/


#[unsafe(no_mangle)]
pub extern "C" fn loft_shared_n_mutate_through_a_loop_variable(
    stores: *mut Stores,
    args: *const loft::native_lib::LibArg,
    n: usize,
    ret: *mut loft::native_lib::LibArg,
) {
    let cell = unsafe { &*(stores.cast::<std::cell::UnsafeCell<Stores>>()) };
    let a = unsafe { std::slice::from_raw_parts(args, n) };
    let _ = (0, a);
    unsafe { (*ret).scalar = (n_mutate_through_a_loop_variable(cell)) as i64; }
}

#[unsafe(no_mangle)]
pub extern "C" fn loft_type_layout_fp_v1() -> u64 { 3769074584463202348u64 }
