fn OpNot(v1: bool, ) -> bool  {}

fn OpAnd(v1: bool, v2: bool, ) -> bool  {}

fn OpOr(v1: bool, v2: bool, ) -> bool  {}

fn OpFormatBool(db: i32, text: i32, val: bool, radix: i32, width: i32, dir: i32, token: i32, )  {}

fn OpConvIntFromNull() -> i32  {}

fn OpAbsInteger(v1: i32, ) -> i32  {}

fn integer::abs(both: i32, ) -> i32 {
  OpAbsInteger(var_0);
}

fn OpMinSingleInt(v1: i32, ) -> i32  {}

fn OpConvLongFromInt(v1: i32, ) -> i64  {}

fn OpConvFloatFromInt(v1: i32, ) -> f64  {}

fn OpConvSingleFromInt(v1: i32, ) -> f32  {}

fn OpConvBoolFromInt(v1: i32, ) -> bool  {}

fn OpAddInt(v1: i32, v2: i32, ) -> i32  {}

fn OpMinInt(v1: i32, v2: i32, ) -> i32  {}

fn OpMulInt(v1: i32, v2: i32, ) -> i32  {}

fn OpDivInt(v1: i32, v2: i32, ) -> i32  {}

fn OpRemInt(v1: i32, v2: i32, ) -> i32  {}

fn OpEqInt(v1: i32, v2: i32, ) -> bool  {}

fn OpNeInt(v1: i32, v2: i32, ) -> bool  {}

fn OpLtInt(v1: i32, v2: i32, ) -> bool  {}

fn OpLeInt(v1: i32, v2: i32, ) -> bool  {}

fn OpGtInt(v1: i32, v2: i32, ) -> bool  {}

fn OpGeInt(v1: i32, v2: i32, ) -> bool  {}

fn OpFormatInt(db: i32, text: i32, val: i32, radix: i32, width: i32, token: i32, plus: bool, note: bool, )  {}

fn OpConvLongFromNull() -> i64  {}

fn OpAbsLong(v1: i64, ) -> i64  {}

fn long::abs(both: i64, ) -> i64 {
  OpAbsLong(var_0);
}

fn OpMinSingleLong(v1: i64, ) -> i64  {}

fn OpCastIntFromLong(v1: i64, ) -> i32  {}

fn OpConvFloatFromLong(v1: i64, ) -> f64  {}

fn OpConvBoolFromLong(v1: i64, ) -> bool  {}

fn OpAddLong(v1: i64, v2: i64, ) -> i64  {}

fn OpMinLong(v1: i64, v2: i64, ) -> i64  {}

fn OpMulLong(v1: i64, v2: i64, ) -> i64  {}

fn OpDivLong(v1: i64, v2: i64, ) -> i64  {}

fn OpRemLong(v1: i64, v2: i64, ) -> i64  {}

fn OpEqLong(v1: i64, v2: i64, ) -> bool  {}

fn OpNeLong(v1: i64, v2: i64, ) -> bool  {}

fn OpLtLong(v1: i64, v2: i64, ) -> bool  {}

fn OpLeLong(v1: i64, v2: i64, ) -> bool  {}

fn OpGtLong(v1: i64, v2: i64, ) -> bool  {}

fn OpGeLong(v1: i64, v2: i64, ) -> bool  {}

fn OpFormatLong(v1: String, val: i64, radix: i32, width: i32, token: i32, plus: bool, note: bool, )  {}

fn OpConvSingleFromNull() -> f32  {}

fn OpAbsSingle(v1: f32, ) -> f32  {}

fn single::abs(both: f32, ) -> f32 {
  OpAbsSingle(var_0);
}

fn OpMinSingleSingle(v1: f32, ) -> f32  {}

fn OpCastIntFromSingle(v1: f32, ) -> i32  {}

fn OpCastLongFromSingle(v1: f32, ) -> i64  {}

fn OpConvFloatFromSingle(v1: f32, ) -> f64  {}

fn OpConvBoolFromSingle(v1: f32, ) -> bool  {}

fn OpAddSingle(v1: f32, v2: f32, ) -> f32  {}

fn OpMinSingle(v1: f32, v2: f32, ) -> f32  {}

fn OpMulSingle(v1: f32, v2: f32, ) -> f32  {}

fn OpDivSingle(v1: f32, v2: f32, ) -> f32  {}

fn OpRemSingle(v1: f32, v2: f32, ) -> f32  {}

fn OpEqSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpNeSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpLtSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpLeSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpGtSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpGeSingle(v1: f32, v2: f32, ) -> bool  {}

fn OpFormatSingle(v1: String, val: f32, width: i32, precision: i32, )  {}

fn OpConvFloatFromNull() -> f64  {}

fn OpAbsFloat(v1: f64, ) -> f64  {}

fn float::abs(both: f64, ) -> f64 {
  OpAbsFloat(var_0);
}

fn OpMinSingleFloat(v1: f64, ) -> f64  {}

fn OpCastSingleFromFloat(v1: f64, ) -> f32  {}

fn OpCastIntFromFloat(v1: f64, ) -> i32  {}

fn OpCastLongFromFloat(v1: f64, ) -> i64  {}

fn OpConvBoolFromFloat(v1: f64, ) -> bool  {}

fn OpAddFloat(v1: f64, v2: f64, ) -> f64  {}

fn OpMinFloat(v1: f64, v2: f64, ) -> f64  {}

fn OpMulFloat(v1: f64, v2: f64, ) -> f64  {}

fn OpDivFloat(v1: f64, v2: f64, ) -> f64  {}

fn OpRemFloat(v1: f64, v2: f64, ) -> f64  {}

fn OpEqFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpNeFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpLtFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpLeFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpGtFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpGeFloat(v1: f64, v2: f64, ) -> bool  {}

fn OpFormatFloat(v1: String, val: f64, width: i32, precision: i32, )  {}

fn OpConvTextFromNull() -> String  {}

fn OpLengthText(v1: String, ) -> i32  {}

fn text::len(both: String, ) -> i32 {
  OpLengthText(var_0);
}

fn OpConvBoolFromText(v1: String, ) -> bool  {}

fn OpAddText(v1: String, v2: String, )  {}

fn OpAddData(v1: String, pos: i32, length: i32, )  {}

fn OpClearText(v1: String, )  {}

fn text::clear(self: String, ) {
  OpClearText(var_0);
}

fn OpEqText(v1: String, v2: String, ) -> bool  {}

fn OpNeText(v1: String, v2: String, ) -> bool  {}

fn OpLtText(v1: String, v2: String, ) -> bool  {}

fn OpLeText(v1: String, v2: String, ) -> bool  {}

fn OpGtText(v1: String, v2: String, ) -> bool  {}

fn OpGeText(v1: String, v2: String, ) -> bool  {}

fn OpEqData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpNeData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpLtData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpLeData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpGtData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpGeData(v1: String, v2: i32, l2: i32, ) -> bool  {}

fn OpFormatText(v1: String, val: String, width: i32, dir: i32, token: i32, )  {}

fn OpConvBoolFromEnum(v1: u8, ) -> bool  {}

fn OpConvEnumFromNull() -> u8  {}

fn OpEqEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpNeEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpLtEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpLeEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpGtEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpGeEnum(v1: u8, v2: u8, ) -> bool  {}

fn OpFormatEnum(v1: String, val: u8, width: i32, )  {}

fn OpDatabase(size: i32, ) -> (u32, u32)  {}

fn OpAppend(db: (u32, u32), size: i32, ) -> (u32, u32)  {}

fn OpGet(v1: (u32, u32), fld: i32, ) -> (u32, u32)  {}

fn OpConvRefFromNull() -> (u32, u32)  {}

fn OpConvBoolFromRef(v1: (u32, u32), ) -> bool  {}

fn OpEqRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn OpNeRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn OpLtRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn OpLeRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn OpGtRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn OpGeRef(v1: (u32, u32), v2: (u32, u32), ) -> bool  {}

fn reference::remove(self: (u32, u32), )  {}

fn OpFormatRef(v1: String, val: (u32, u32), width: i32, )  {}

fn OpGetInt(v1: (u32, u32), fld: i32, ) -> i32  {}

fn OpGetLong(v1: (u32, u32), fld: i32, ) -> i64  {}

fn OpGetSingle(v1: (u32, u32), fld: i32, ) -> f32  {}

fn OpGetFloat(v1: (u32, u32), fld: i32, ) -> f64  {}

fn OpGetByte(v1: (u32, u32), fld: i32, min: i32, ) -> i32  {}

fn OpGetShort(v1: (u32, u32), fld: i32, min: i32, ) -> i32  {}

fn OpGetText(v1: (u32, u32), fld: i32, ) -> String  {}

fn OpSetInt(v1: (u32, u32), fld: i32, val: i32, )  {}

fn OpSetLong(v1: (u32, u32), fld: i32, val: i64, )  {}

fn OpSetSingle(v1: (u32, u32), fld: i32, val: f32, )  {}

fn OpSetFloat(v1: (u32, u32), fld: i32, val: f64, )  {}

fn OpSetByte(v1: (u32, u32), fld: i32, min: i32, val: i32, )  {}

fn OpSetShort(v1: (u32, u32), fld: i32, min: i32, val: i32, )  {}

fn OpSetText(v1: (u32, u32), fld: i32, val: String, )  {}

fn OpGetReference(v1: (u32, u32), fld: i32, ) -> (u32, u32)  {}

fn OpSetReference(v1: (u32, u32), fld: i32, val: (u32, u32), )  {}

fn OpLengthVector(r: (u32, u32), ) -> i32  {}

fn vector::len(both: (u32, u32), ) -> i32 {
  OpLengthVector(var_0);
}

fn OpClearVector(r: (u32, u32), )  {}

fn vector::clear(self: (u32, u32), ) {
  OpClearVector(var_0);
}

fn OpFinishSorted(r: (u32, u32), )  {}

fn OpGetVector(r: (u32, u32), size: i32, index: i32, ) -> T  {}

fn OpRemoveVector(r: (u32, u32), size: i32, index: i32, ) -> bool  {}

fn OpInsertVector(r: (u32, u32), size: i32, index: i32, ) -> T  {}

fn OpAppendVector(r: (u32, u32), size: i32, ) -> T  {}

fn OpLengthHash(r: (u32, u32), ) -> i32  {}

fn hash::len(both: (u32, u32), ) -> i32 {
  OpLengthHash(var_0);
}

fn OpClearHash(r: (u32, u32), )  {}

fn hash::clear(self: (u32, u32), ) {
  OpClearHash(var_0);
}

fn OpFormatHash(v1: String, val: (u32, u32), width: i32, )  {}

fn OpLengthIndex(r: (u32, u32), ) -> i32  {}

fn index::len(both: (u32, u32), ) -> i32 {
  OpLengthIndex(var_0);
}

fn OpClearIndex(r: (u32, u32), )  {}

fn index::clear(self: (u32, u32), ) {
  OpClearIndex(var_0);
}

fn OpFormatIndex(v1: String, val: (u32, u32), width: i32, )  {}

fn OpLengthRadix(r: (u32, u32), ) -> i32  {}

fn radix::len(both: (u32, u32), ) -> i32 {
  OpLengthRadix(var_0);
}

fn OpClearRadix(r: (u32, u32), )  {}

fn radix::clear(self: (u32, u32), ) {
  OpClearRadix(var_0);
}

fn OpFormatRadix(v1: String, val: (u32, u32), width: i32, )  {}

fn OpEqBool(v1: bool, v2: bool, ) -> bool  {}

fn OpNeBool(v1: bool, v2: bool, ) -> bool  {}

