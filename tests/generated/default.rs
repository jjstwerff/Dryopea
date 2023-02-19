{
  let d = Definition::new("boolean");
  d.add_attribute("abs", Type::fn boolean::abs mutable);
  d.returned = Type::boolean;
  d.size = 1;
}
{
  let d = Definition::new("integer");
  d.add_attribute("abs", Type::fn integer::abs mutable);
  d.returned = Type::integer;
  d.size = 4;
}
{
  let d = Definition::new("long");
  d.add_attribute("abs", Type::fn long::abs mutable);
  d.returned = Type::long;
  d.size = 8;
}
{
  let d = Definition::new("single");
  d.returned = Type::single;
  d.size = 4;
}
{
  let d = Definition::new("float");
  d.add_attribute("abs", Type::fn float::abs mutable);
  d.returned = Type::float;
  d.size = 8;
}
{
  let d = Definition::new("text");
  d.add_attribute("len", Type::fn text::len mutable);
  d.add_attribute("clear", Type::fn text::clear mutable);
  d.returned = Type::text;
  d.size = 4;
}
fn OpNot(v1: bool, ) Null

fn OpAnd(v1: bool, v2: bool, ) Null

fn OpOr(v1: bool, v2: bool, ) Null

fn OpFormatBool(val: bool, radix: i32, width: i32, token: i32, plus: bool, note: bool, ) Null

fn OpConvIntFromNull() Null

fn OpAbsInteger(v1: i32, ) Null

fn integer::abs(both: i32, ) {
  OpAbsInteger(var_0);
}

{
  let d = Definition::new("abs");
  d.add_attribute("integer", Type::fn integer::abs mutable);
  d.add_attribute("long", Type::fn long::abs mutable);
  d.add_attribute("boolean", Type::fn boolean::abs mutable);
  d.add_attribute("float", Type::fn float::abs mutable);
  d.size = 0;
}
fn OpMinSingleInt(v1: i32, ) Null

fn OpConvLongFromInt(v1: i32, ) Null

fn OpConvFloatFromInt(v1: i32, ) Null

fn OpConvSingleFromInt(v1: i32, ) Null

fn OpConvBoolFromInt(v1: i32, ) Null

fn OpAddInt(v1: i32, v2: i32, ) Null

fn OpMinInt(v1: i32, v2: i32, ) Null

fn OpMulInt(v1: i32, v2: i32, ) Null

fn OpDivInt(v1: i32, v2: i32, ) Null

fn OpRemInt(v1: i32, v2: i32, ) Null

fn OpEqInt(v1: i32, v2: i32, ) Null

fn OpNeInt(v1: i32, v2: i32, ) Null

fn OpLtInt(v1: i32, v2: i32, ) Null

fn OpLeInt(v1: i32, v2: i32, ) Null

fn OpGtInt(v1: i32, v2: i32, ) Null

fn OpGeInt(v1: i32, v2: i32, ) Null

fn OpFormatInt(val: i32, radix: i32, width: i32, token: i32, plus: bool, note: bool, ) Null

fn OpConvLongFromNull() Null

fn OpAbsLong(v1: i64, ) Null

fn long::abs(both: i64, ) {
  OpAbsLong(var_0);
}

fn OpMinSingleLong(v1: i64, ) Null

fn OpCastIntFromLong(v1: i64, ) Null

fn OpConvFloatFromLong(v1: i64, ) Null

fn OpConvBoolFromLong(v1: i64, ) Null

fn OpAddLong(v1: i64, v2: i64, ) Null

fn OpMinLong(v1: i64, v2: i64, ) Null

fn OpMulLong(v1: i64, v2: i64, ) Null

fn OpDivLong(v1: i64, v2: i64, ) Null

fn OpRemLong(v1: i64, v2: i64, ) Null

fn OpEqLong(v1: i64, v2: i64, ) Null

fn OpNeLong(v1: i64, v2: i64, ) Null

fn OpLtLong(v1: i64, v2: i64, ) Null

fn OpLeLong(v1: i64, v2: i64, ) Null

fn OpGtLong(v1: i64, v2: i64, ) Null

fn OpGeLong(v1: i64, v2: i64, ) Null

fn OpFormatLong(val: i64, radix: i32, width: i32, token: i32, plus: bool, note: bool, ) Null

fn OpConvSingleFromNull() Null

fn OpAbsSingle(v1: f32, ) Null

fn boolean::abs(both: f32, ) {
  OpAbsSingle(var_0);
}

fn OpMinSingleSingle(v1: f32, ) Null

fn OpCastIntFromSingle(v1: f32, ) Null

fn OpCastLongFromSingle(v1: f32, ) Null

fn OpConvFloatFromSingle(v1: f32, ) Null

fn OpConvBoolFromSingle(v1: f32, ) Null

fn OpAddSingle(v1: f32, v2: f32, ) Null

fn OpMinSingle(v1: f32, v2: f32, ) Null

fn OpMulSingle(v1: f32, v2: f32, ) Null

fn OpDivSingle(v1: f32, v2: f32, ) Null

fn OpRemSingle(v1: f32, v2: f32, ) Null

fn OpEqSingle(v1: f32, v2: f32, ) Null

fn OpNeSingle(v1: f32, v2: f32, ) Null

fn OpLtSingle(v1: f32, v2: f32, ) Null

fn OpLeSingle(v1: f32, v2: f32, ) Null

fn OpGtSingle(v1: f32, v2: f32, ) Null

fn OpGeSingle(v1: f32, v2: f32, ) Null

fn OpFormatSingle(val: f32, width: i32, precision: i32, ) Null

fn OpConvFloatFromNull() Null

fn OpAbsFloat(v1: f64, ) Null

fn float::abs(both: f64, ) {
  OpAbsFloat(var_0);
}

fn OpMinSingleFloat(v1: f64, ) Null

fn OpCastSingleFromFloat(v1: f64, ) Null

fn OpCastIntFromFloat(v1: f64, ) Null

fn OpCastLongFromFloat(v1: f64, ) Null

fn OpConvBoolFromFloat(v1: f64, ) Null

fn OpAddFloat(v1: f64, v2: f64, ) Null

fn OpMinFloat(v1: f64, v2: f64, ) Null

fn OpMulFloat(v1: f64, v2: f64, ) Null

fn OpDivFloat(v1: f64, v2: f64, ) Null

fn OpRemFloat(v1: f64, v2: f64, ) Null

fn OpEqFloat(v1: f64, v2: f64, ) Null

fn OpNeFloat(v1: f64, v2: f64, ) Null

fn OpLtFloat(v1: f64, v2: f64, ) Null

fn OpLeFloat(v1: f64, v2: f64, ) Null

fn OpGtFloat(v1: f64, v2: f64, ) Null

fn OpGeFloat(v1: f64, v2: f64, ) Null

fn OpFormatFloat(val: f64, width: i32, precision: i32, ) Null

fn OpConvTextFromNull() Null

fn OpLengthText(v1: String, ) Null

fn text::len(both: String, ) {
  OpLengthText(var_0);
}

{
  let d = Definition::new("len");
  d.add_attribute("text", Type::fn text::len mutable);
  d.add_attribute("vector", Type::fn vector::len mutable);
  d.add_attribute("hash", Type::fn hash::len mutable);
  d.add_attribute("index", Type::fn index::len mutable);
  d.add_attribute("radix", Type::fn radix::len mutable);
  d.size = 0;
}
fn OpConvBoolFromText(v1: String, ) Null

fn OpAddText(v1: String, v2: String, ) Null

fn OpClearText(v1: String, ) Null

fn text::clear(self: String, ) {
  OpClearText(var_0);
}

fn OpEqText(v1: String, v2: String, ) Null

fn OpNeText(v1: String, v2: String, ) Null

fn OpLtText(v1: String, v2: String, ) Null

fn OpLeText(v1: String, v2: String, ) Null

fn OpGtText(v1: String, v2: String, ) Null

fn OpGeText(v1: String, v2: String, ) Null

fn OpFormatText(val: String, width: i32, dir: i32, token: i32, ) Null

{
  let d = Definition::new("enumerate");
  d.returned = Type::enumerate;
  d.size = 1;
}
fn OpConvBoolFromEnum(v1: u8, ) Null

fn OpConvEnumFromNull() Null

fn OpEqEnum(v1: u8, v2: u8, ) Null

fn OpNeEnum(v1: u8, v2: u8, ) Null

fn OpLtEnum(v1: u8, v2: u8, ) Null

fn OpLeEnum(v1: u8, v2: u8, ) Null

fn OpGtEnum(v1: u8, v2: u8, ) Null

fn OpGeEnum(v1: u8, v2: u8, ) Null

fn OpFormatEnum(val: u8, width: i32, ) Null

{
  let d = Definition::new("reference");
  d.add_attribute("size", Type::fn reference::size mutable);
  d.add_attribute("align", Type::fn reference::align mutable);
  d.add_attribute("remove", Type::fn reference::remove mutable);
  d.returned = Type::reference;
  d.size = 4;
}
fn OpDatabase(size: i32, ) Null

fn OpConvRefFromNull() Null

fn OpConvBoolFromRef(v1: (u32, u32), ) Null

fn OpLengthRef(v: (u32, u32), ) Null

fn reference::size(both: (u32, u32), ) {
  OpLengthRef(var_0);
}

{
  let d = Definition::new("size");
  d.add_attribute("reference", Type::fn reference::size mutable);
  d.size = 0;
}
fn OpAlignRef(v: (u32, u32), ) Null

fn reference::align(both: (u32, u32), ) {
  OpAlignRef(var_0);
}

{
  let d = Definition::new("align");
  d.add_attribute("reference", Type::fn reference::align mutable);
  d.size = 0;
}
fn OpEqRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn OpNeRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn OpLtRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn OpLeRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn OpGtRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn OpGeRef(v1: (u32, u32), v2: (u32, u32), ) Null

fn reference::remove(self: (u32, u32), ) Null

fn OpFormatRef(val: (u32, u32), width: i32, ) Null

{
  let d = Definition::new("vector");
  d.add_attribute("len", Type::fn vector::len mutable);
  d.add_attribute("clear", Type::fn vector::clear mutable);
  d.add_attribute("remove", Type::fn vector::remove mutable);
  d.returned = Type::vector<unknown>;
  d.size = 4;
}
fn OpLengthVector(r: (u32, u32), ) Null

fn vector::len(both: (u32, u32), ) {
  OpLengthVector(var_0);
}

fn OpClearVector(r: (u32, u32), ) Null

fn vector::clear(self: (u32, u32), ) {
  OpClearVector(var_0);
}

fn OpFinishSorted(r: (u32, u32), ) Null

fn OpInsertVector(r: (u32, u32), content: T, ) Null

fn OpRemoveVector(r: (u32, u32), index: i32, ) Null

fn vector::remove(self: (u32, u32), index: i32, ) {
  OpRemoveVector(var_0, var_1);
}

fn OpGetVector(r: (u32, u32), index: i32, ) Null

fn OpFormatVector(val: String, width: i32, ) Null

{
  let d = Definition::new("hash");
  d.add_attribute("len", Type::fn hash::len mutable);
  d.add_attribute("clear", Type::fn hash::clear mutable);
  d.returned = Type::hash;
  d.size = 4;
}
fn OpLengthHash(r: (u32, u32), ) Null

fn hash::len(both: (u32, u32), ) {
  OpLengthHash(var_0);
}

fn OpClearHash(r: (u32, u32), ) Null

fn hash::clear(self: (u32, u32), ) {
  OpClearHash(var_0);
}

fn OpInsertHash(r: (u32, u32), content: T, ) Null

fn OpRemoveHash(r: (u32, u32), content: T, ) Null

fn OpFormatHash(val: (u32, u32), width: i32, ) Null

{
  let d = Definition::new("index");
  d.add_attribute("len", Type::fn index::len mutable);
  d.add_attribute("clear", Type::fn index::clear mutable);
  d.returned = Type::index;
  d.size = 4;
}
fn OpLengthIndex(r: (u32, u32), ) Null

fn index::len(both: (u32, u32), ) {
  OpLengthIndex(var_0);
}

fn OpClearIndex(r: (u32, u32), ) Null

fn index::clear(self: (u32, u32), ) {
  OpClearIndex(var_0);
}

fn OpInsertIndex(r: (u32, u32), content: T, ) Null

fn OpRemoveIndex(r: (u32, u32), content: T, ) Null

fn OpFormatIndex(val: (u32, u32), width: i32, ) Null

{
  let d = Definition::new("radix");
  d.add_attribute("len", Type::fn radix::len mutable);
  d.add_attribute("clear", Type::fn radix::clear mutable);
  d.returned = Type::radix;
  d.size = 4;
}
fn OpLengthRadix(r: (u32, u32), ) Null

fn radix::len(both: (u32, u32), ) {
  OpLengthRadix(var_0);
}

fn OpClearRadix(r: (u32, u32), ) Null

fn radix::clear(self: (u32, u32), ) {
  OpClearRadix(var_0);
}

fn OpInsertRadix(r: (u32, u32), content: T, ) Null

fn OpRemoveRadix(r: (u32, u32), content: T, ) Null

fn OpFormatRadix(val: (u32, u32), width: i32, ) Null

fn OpEqBool(v1: bool, v2: bool, ) Null

fn OpNeBool(v1: bool, v2: bool, ) Null

