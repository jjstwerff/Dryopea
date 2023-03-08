// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

pub type boolean;
pub type integer;
pub type long;
pub type single;
pub type float;
pub type text;

fn OpNot(v1: boolean) -> boolean;
fn OpAnd(v1: boolean, v2: boolean) -> boolean;
fn OpOr(v1: boolean, v2: boolean) -> boolean;
fn OpFormatBool(val: boolean, radix: integer, width: integer, token: integer, plus: boolean, note: boolean) -> text;

fn OpConvIntFromNull() -> integer;
fn OpAbsInteger(v1: integer) -> integer;
pub fn abs(both: integer) -> integer { OpAbsInteger(both) }
fn OpMinSingleInt(v1: integer) -> integer;
fn OpConvLongFromInt(v1: integer) -> long;
fn OpConvFloatFromInt(v1: integer) -> float;
fn OpConvSingleFromInt(v1: integer) -> single;
fn OpConvBoolFromInt(v1: integer) -> boolean;
fn OpAddInt(v1: integer, v2: integer) -> integer;
fn OpMinInt(v1: integer, v2: integer) -> integer;
fn OpMulInt(v1: integer, v2: integer) -> integer;
fn OpDivInt(v1: integer, v2: integer) -> integer;
fn OpRemInt(v1: integer, v2: integer) -> integer;
fn OpEqInt(v1: integer, v2: integer) -> boolean;
fn OpNeInt(v1: integer, v2: integer) -> boolean;
fn OpLtInt(v1: integer, v2: integer) -> boolean;
fn OpLeInt(v1: integer, v2: integer) -> boolean;
fn OpGtInt(v1: integer, v2: integer) -> boolean;
fn OpGeInt(v1: integer, v2: integer) -> boolean;
fn OpFormatInt(val: integer, radix: integer, width: integer, token: integer, plus: boolean, note: boolean) -> text;

fn OpConvLongFromNull() -> long;
fn OpAbsLong(v1: long) -> long;
pub fn abs(both: long) -> long { OpAbsLong(both) }
fn OpMinSingleLong(v1: long) -> long;
fn OpCastIntFromLong(v1: long) -> integer;
fn OpConvFloatFromLong(v1: long) -> float;
fn OpConvBoolFromLong(v1: long) -> boolean;
fn OpAddLong(v1: long, v2: long) -> long;
fn OpMinLong(v1: long, v2: long) -> long;
fn OpMulLong(v1: long, v2: long) -> long;
fn OpDivLong(v1: long, v2: long) -> long;
fn OpRemLong(v1: long, v2: long) -> long;
fn OpEqLong(v1: long, v2: long) -> boolean;
fn OpNeLong(v1: long, v2: long) -> boolean;
fn OpLtLong(v1: long, v2: long) -> boolean;
fn OpLeLong(v1: long, v2: long) -> boolean;
fn OpGtLong(v1: long, v2: long) -> boolean;
fn OpGeLong(v1: long, v2: long) -> boolean;
fn OpFormatLong(val: long, radix: integer, width: integer, token: integer, plus: boolean, note: boolean) -> text;

fn OpConvSingleFromNull() -> single;
fn OpAbsSingle(v1: single) -> single;
pub fn abs(both: single) -> single { OpAbsSingle(both) }
fn OpMinSingleSingle(v1: single) -> single;
fn OpCastIntFromSingle(v1: single) -> integer;
fn OpCastLongFromSingle(v1: single) -> long;
fn OpConvFloatFromSingle(v1: single) -> float;
fn OpConvBoolFromSingle(v1: single) -> boolean;
fn OpAddSingle(v1: single, v2: single) -> single;
fn OpMinSingle(v1: single, v2: single) -> single;
fn OpMulSingle(v1: single, v2: single) -> single;
fn OpDivSingle(v1: single, v2: single) -> single;
fn OpRemSingle(v1: single, v2: single) -> single;
fn OpEqSingle(v1: single, v2: single) -> boolean;
fn OpNeSingle(v1: single, v2: single) -> boolean;
fn OpLtSingle(v1: single, v2: single) -> boolean;
fn OpLeSingle(v1: single, v2: single) -> boolean;
fn OpGtSingle(v1: single, v2: single) -> boolean;
fn OpGeSingle(v1: single, v2: single) -> boolean;
fn OpFormatSingle(val: single, width: integer, precision: integer) -> text;

fn OpConvFloatFromNull() -> float;
fn OpAbsFloat(v1: float) -> float;
pub fn abs(both: float) -> float { OpAbsFloat(both) }
fn OpMinSingleFloat(v1: float) -> float;
fn OpCastSingleFromFloat(v1: float) -> single;
fn OpCastIntFromFloat(v1: float) -> integer;
fn OpCastLongFromFloat(v1: float) -> long;
fn OpConvBoolFromFloat(v1: float) -> boolean;
fn OpAddFloat(v1: float, v2: float) -> float;
fn OpMinFloat(v1: float, v2: float) -> float;
fn OpMulFloat(v1: float, v2: float) -> float;
fn OpDivFloat(v1: float, v2: float) -> float;
fn OpRemFloat(v1: float, v2: float) -> float;
fn OpEqFloat(v1: float, v2: float) -> boolean;
fn OpNeFloat(v1: float, v2: float) -> boolean;
fn OpLtFloat(v1: float, v2: float) -> boolean;
fn OpLeFloat(v1: float, v2: float) -> boolean;
fn OpGtFloat(v1: float, v2: float) -> boolean;
fn OpGeFloat(v1: float, v2: float) -> boolean;
fn OpFormatFloat(val: float, width: integer, precision: integer) -> text;

fn OpConvTextFromNull() -> text;
fn OpLengthText(v1: text) -> integer;
pub fn len(both: text) -> integer { OpLengthText(both) }
fn OpConvBoolFromText(v1: text) -> boolean;
fn OpAddText(v1: text, v2: text) -> text;
fn OpClearText(v1: text) -> integer;
pub fn clear(self: text) { OpClearText(self) }
fn OpEqText(v1: text, v2: text) -> boolean;
fn OpNeText(v1: text, v2: text) -> boolean;
fn OpLtText(v1: text, v2: text) -> boolean;
fn OpLeText(v1: text, v2: text) -> boolean;
fn OpGtText(v1: text, v2: text) -> boolean;
fn OpGeText(v1: text, v2: text) -> boolean;
fn OpFormatText(val: text, width: integer, dir: integer, token: integer) -> text;

type enumerate;
fn OpConvBoolFromEnum(v1: enumerate) -> boolean;
fn OpConvEnumFromNull() -> enumerate;
fn OpEqEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpNeEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpLtEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpLeEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpGtEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpGeEnum(v1: enumerate, v2: enumerate) -> boolean;
fn OpFormatEnum(val: enumerate, width: integer) -> text;

type reference;
fn OpDatabase(size: integer) -> reference;
fn OpAppend(db: reference, size: integer) -> reference;
fn OpGet(v1: reference, fld: integer) -> reference;
fn OpConvRefFromNull() -> reference;
fn OpConvBoolFromRef(v1: reference) -> boolean;
fn OpLengthRef(v: reference) -> integer;
pub fn size(both: reference) -> integer { OpLengthRef(both) };
fn OpAlignRef(v: reference) -> integer;
pub fn align(both: reference) -> integer { OpAlignRef(both) };
fn OpEqRef(v1: reference, v2: reference) -> boolean;
fn OpNeRef(v1: reference, v2: reference) -> boolean;
fn OpLtRef(v1: reference, v2: reference) -> boolean;
fn OpLeRef(v1: reference, v2: reference) -> boolean;
fn OpGtRef(v1: reference, v2: reference) -> boolean;
fn OpGeRef(v1: reference, v2: reference) -> boolean;
pub fn remove(self: reference);
fn OpFormatRef(val: reference, width: integer) -> text;
fn OpGetInt(v1: reference, fld: integer) -> integer;
fn OpGetLong(v1: reference, fld: integer) -> long;
fn OpGetSingle(v1: reference, fld: integer) -> single;
fn OpGetFloat(v1: reference, fld: integer) -> float;
fn OpGetByte(v1: reference, fld: integer, min: integer) -> integer;
fn OpGetShort(v1: reference, fld: integer, min: integer) -> integer;
fn OpGetText(v1: reference, fld: integer) -> text;
fn OpSetInt(v1: reference, fld: integer, val: integer);
fn OpSetLong(v1: reference, fld: integer, val: long);
fn OpSetSingle(v1: reference, fld: integer, val: single);
fn OpSetFloat(v1: reference, fld: integer, val: float);
fn OpSetByte(v1: reference, fld: integer, min: integer, val: integer);
fn OpSetShort(v1: reference, fld: integer, min: integer, val: integer);
fn OpSetText(v1: reference, fld: integer, val: text);

pub type vector;
fn OpLengthVector(r: vector) -> integer;
pub fn len(both: vector) -> integer { OpLengthVector(both) }
fn OpClearVector(r: vector);
pub fn clear(self: vector) { OpClearVector(self) }
fn OpFinishSorted(r: vector);
fn OpGetVector(r: vector, size: integer, index: integer) -> reference;
fn OpRemoveVector(r: vector, size: integer, index: integer) -> boolean;
fn OpInsertVector(r: vector, size: integer, index: integer) -> reference;
fn OpAppendVector(r: vector, size: integer) -> reference;

pub type hash;
fn OpLengthHash(r: hash) -> integer;
pub fn len(both: hash) -> integer { OpLengthHash(both) }
fn OpClearHash(r: hash);
pub fn clear(self: hash) { OpClearHash(self); }
fn OpFormatHash(val: hash, width: integer) -> text;

pub type index;
fn OpLengthIndex(r: index) -> integer;
pub fn len(both: index) -> integer { OpLengthIndex(both) }
fn OpClearIndex(r: index);
pub fn clear(self: index) { OpClearIndex(self) }
fn OpFormatIndex(val: index, width: integer) -> text;

pub type radix;
fn OpLengthRadix(r: radix) -> integer;
pub fn len(both: radix) -> integer { OpLengthRadix(both) }
fn OpClearRadix(r: radix);
pub fn clear(self: radix) { OpClearRadix(self) }
fn OpFormatRadix(val: radix, width: integer) -> text;

fn OpEqBool(v1: boolean, v2: boolean) -> boolean;
fn OpNeBool(v1: boolean, v2: boolean) -> boolean;
