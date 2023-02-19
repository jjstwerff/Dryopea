// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! The interpreter for in memory code.
//! Eventually most projects will only run compiled code.
//! However, it is still an easy way to validate code without relative slow compiling, optimizing and linking.

extern crate strum;
extern crate strum_macros;
use self::strum::IntoEnumIterator;
use self::strum_macros::EnumIter;
use crate::data::{Data, Type, Value};
use crate::external::*;
use crate::format;
use crate::store::{Store, PRIMARY};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

// was 999_999_999;
const MAX_LOOPING: u32 = 999;

pub struct State {
    c_function: u32,
    stack: Vec<Value>,
    databases: Vec<DataStore>,
    logging: Option<File>,
    indent: u32,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> State {
        State {
            c_function: 0,
            stack: Vec::new(),
            databases: Vec::new(),
            logging: None,
            indent: 0,
        }
    }
}

fn ind(indent: u32) -> String {
    let mut res = "".to_string();
    for _ in 0..indent {
        res += "  "
    }
    res
}

pub struct Inter<'a> {
    pub data: &'a Data,               // immutable definitions
    external: HashMap<u32, External>, // immutable known external routines
}

#[derive(Debug, EnumIter)]
pub enum External {
    Database,
    Not,
    CastSingleFromFloat,
    CastIntFromFloat,
    CastLongFromFloat,
    CastIntFromSingle,
    CastLongFromSingle,
    CastIntFromLong,
    ConvLongFromInt,
    ConvFloatFromInt,
    ConvFloatFromLong,
    ConvFloatFromSingle,
    ConvSingleFromInt,
    ConvBoolFromInt,
    ConvBoolFromLong,
    ConvBoolFromFloat,
    ConvBoolFromSingle,
    ConvBoolFromRef,
    ConvBoolFromEnum,
    ConvBoolFromText,
    ConvIntFromNull,
    ConvLongFromNull,
    ConvFloatFromNull,
    ConvSingleFromNull,
    ConvRefFromNull,
    ConvEnumFromNull,
    ConvTextFromNull,
    AddInt,
    MinSingleInt,
    MinInt,
    MulInt,
    DivInt,
    RemInt,
    AddLong,
    MinSingleLong,
    MinLong,
    MulLong,
    DivLong,
    RemLong,
    AddFloat,
    MinSingleFloat,
    MinFloat,
    MulFloat,
    DivFloat,
    RemFloat,
    AddSingle,
    MinSingleSingle,
    MinSingle,
    MulSingle,
    DivSingle,
    RemSingle,
    And,
    Or,
    EqLong,
    NeLong,
    LtLong,
    LeLong,
    GtLong,
    GeLong,
    EqInt,
    NeInt,
    LtInt,
    LeInt,
    GtInt,
    GeInt,
    EqFloat,
    NeFloat,
    LtFloat,
    LeFloat,
    GtFloat,
    GeFloat,
    EqSingle,
    NeSingle,
    LtSingle,
    LeSingle,
    GtSingle,
    GeSingle,
    EqBool,
    NeBool,
    EqRef,
    NeRef,
    EqEnum,
    NeEnum,
    LtEnum,
    GtEnum,
    LeEnum,
    GeEnum,
    EqText,
    NeText,
    LtText,
    LeText,
    GtText,
    GeText,
    ClearText,
    ClearVector,
    LengthVector,
    FinishSorted,
    LengthText,
    AddText,
    AbsInteger,
    AbsLong,
    AbsFloat,
    AbsSingle,
    FormatText,
    FormatInt,
    FormatLong,
    FormatFloat,
    FormatSingle,
}

impl External {
    pub fn name(&self) -> String {
        format!("{:?}", self)
    }
}

impl<'d> Inter<'d> {
    pub fn new(data: &'d Data) -> Inter<'d> {
        let mut external = HashMap::new();
        for ext in External::iter() {
            let nr = data.def_nr(&format!("Op{}", &ext.name()));
            if nr != u32::MAX {
                external.insert(nr, ext);
            } else {
                panic!("Could not find definition @Op{}", ext.name());
            }
        }
        Inter { data, external }
    }

    /// Start the interpreter on a specific piece of code
    pub fn calculate(&self, name: &str) -> Result<Value, String> {
        let dnr = self.data.def_nr(name);
        if dnr != u32::MAX {
            let state = &mut State::new();
            let res = self.calc(self.data.code(dnr), state);
            Ok(res)
        } else {
            Ok(Value::Null)
        }
    }

    fn do_loop(&self, code: &[Value], state: &mut State) -> Value {
        let mut r = Value::Null;
        let mut max = MAX_LOOPING;
        loop {
            max -= 1;
            if max == 0 {
                panic!("Too many iterations")
            }
            for v in code {
                r = self.calc(v, state);
                match r {
                    Value::Return(_) => break,
                    Value::Continue(_) => break,
                    Value::Break(_) => break,
                    _ => (),
                }
            }
            match r {
                Value::Return(_) => break,
                Value::Break(_) => break,
                _ => (),
            }
            if let Some(f) = &mut state.logging {
                writeln!(f, "{}iter:{}", ind(state.indent), MAX_LOOPING - max).unwrap();
            }
        }
        Value::Null
    }

    fn field(&self, td: u32, fld: u16, rec: &Value, state: &mut State) -> Value {
        let pos = if fld == u16::MAX {
            0
        } else {
            self.data.attr_pos(td, fld) as isize
        };
        let td = if fld == u16::MAX {
            self.data.returned(td)
        } else {
            self.data.attr_type(td, fld)
        };
        if let Value::Reference(db, rec, add) = self.calc(rec, state) {
            let DataStore { records, text } = &mut state.databases[db as usize];
            match td {
                Type::Integer => {
                    let v = records.get_int(rec, pos + add as isize);
                    if v != i32::MIN {
                        return Value::Int(v);
                    }
                }
                Type::Float => {
                    let v = records.get_float(rec, pos + add as isize);
                    if !v.is_nan() {
                        return Value::Float(v);
                    }
                }
                Type::Single => {
                    let v = records.get_single(rec, pos + add as isize);
                    if !v.is_nan() {
                        return Value::Single(v);
                    }
                }
                Type::Long => {
                    let v = records.get_long(rec, pos + add as isize);
                    if v != i64::MIN {
                        return Value::Long(v);
                    }
                }
                Type::Boolean => {
                    let v = records.get_byte(rec, pos + add as isize, 0);
                    if v != i32::MIN {
                        return Value::Int(v);
                    }
                }
                Type::Enum(_) => {
                    let v = records.get_byte(rec, pos + add as isize, 0);
                    if v != i32::MIN {
                        return Value::Int(v);
                    }
                }
                Type::Text => {
                    let v = records.get_int(rec, pos + add as isize);
                    if v != i32::MIN {
                        return Value::Text(text.get_str(v as u32).to_string());
                    }
                }
                Type::Reference(_) => {
                    let v = records.get_int(rec, pos + add as isize);
                    if v != i32::MIN {
                        return Value::Reference(db, v as u32, 0);
                    }
                }
                Type::Inner(_)
                | Type::Vector(_)
                | Type::Sorted(_, _)
                | Type::Index(_, _)
                | Type::Hash(_, _) => {
                    return Value::Reference(db, rec, pos as u32 + add);
                }
                _ => {
                    panic!("Not implemented");
                }
            }
        }
        Value::Null
    }

    fn write(&self, td: u32, fld: u16, rec: &Value, val: &Value, state: &mut State) -> Value {
        let pos = self.data.attr_pos(td, fld) as isize;
        let td = self.data.attr_type(td, fld);
        if let (Value::Reference(db, rec, add), v) = (self.calc(rec, state), self.calc(val, state))
        {
            let DataStore { records, text } = &mut state.databases[db as usize];
            match td {
                Type::Integer => {
                    let val = if let Value::Int(vv) = v { vv } else { i32::MIN };
                    records.set_int(rec, pos + add as isize, val);
                }
                Type::Float => {
                    let val = if let Value::Float(vv) = v {
                        vv
                    } else {
                        f64::NAN
                    };
                    records.set_float(rec, pos + add as isize, val);
                }
                Type::Single => {
                    let val = if let Value::Single(vv) = v {
                        vv
                    } else {
                        f32::NAN
                    };
                    records.set_single(rec, pos + add as isize, val);
                }
                Type::Long => {
                    let val = if let Value::Long(vv) = v {
                        vv
                    } else {
                        i64::MIN
                    };
                    records.set_long(rec, pos + add as isize, val);
                }
                Type::Boolean => {
                    let val = if let Value::Int(vv) = v { vv } else { i32::MIN };
                    records.set_byte(rec, pos + add as isize, 0, val);
                }
                Type::Enum(_) => {
                    let val = if let Value::Int(vv) = v { vv } else { i32::MIN };
                    records.set_byte(rec, pos + add as isize, 0, val);
                }
                Type::Text => {
                    let val = if let Value::Text(str_val) = v {
                        text.set_str(&str_val) as i32
                    } else {
                        i32::MIN
                    };
                    records.set_int(rec, pos + add as isize, val);
                }
                Type::Reference(_) => {
                    let val = if let Value::Reference(_, rec, _) = v {
                        rec as i32
                    } else {
                        i32::MIN
                    };
                    records.set_int(rec, pos + add as isize, val);
                }
                _ => {
                    panic!("Not implemented");
                }
            }
        }
        Value::Null
    }

    fn calc(&self, code: &Value, state: &mut State) -> Value {
        match code {
            Value::Call(nr, call_code) => {
                if let Some(e) = self.external.get(nr) {
                    let res = self.call_external(e, call_code, state);
                    if let Some(f) = &mut state.logging {
                        write!(f, "{}{:?}(", ind(state.indent), e).unwrap();
                        for (v, c) in call_code.iter().enumerate() {
                            if v > 0 {
                                write!(f, ", ").unwrap();
                            }
                            self.data.output_code(f, c, state.indent + 1);
                        }
                        writeln!(f, ") -> {:?}", res).unwrap();
                    }
                    res
                } else {
                    self.call_routine(*nr, call_code, state)
                }
            }
            Value::If(test, true_v, false_v) => {
                if self.calc(test, state) == Value::Int(1) {
                    self.calc(true_v, state)
                } else {
                    self.calc(false_v, state)
                }
            }
            Value::Block(code) => {
                let mut r = Value::Null;
                let sp = state.stack.len();
                for v in code {
                    r = self.calc(v, state);
                    match r {
                        Value::Return(_) => break,
                        Value::Continue(_) => break,
                        Value::Break(_) => break,
                        _ => (),
                    }
                }
                while state.stack.len() > sp {
                    state.stack.pop();
                }
                r
            }
            Value::Loop(code) => self.do_loop(code, state),
            Value::Set(var_nr, value) => {
                let pos = (state.c_function + var_nr) as usize;
                while state.stack.len() <= pos {
                    state.stack.push(Value::Null);
                }
                let cv = self.calc(value, state);
                if let Some(f) = &mut state.logging {
                    writeln!(f, "{}var_{} = {:?}", ind(state.indent), var_nr, cv).unwrap();
                }
                state.stack[pos] = cv;
                Value::Null
            }
            Value::Var(var_nr) => {
                let pos = (state.c_function + var_nr) as usize;
                if let Some(f) = &mut state.logging {
                    writeln!(
                        f,
                        "{}var_{} -> {:?}",
                        ind(state.indent),
                        var_nr,
                        state.stack[pos]
                    )
                    .unwrap();
                }
                state.stack[pos].clone()
            }
            Value::Return(def) => {
                let v = self.calc(def, state);
                Value::Return(Box::new(v))
            }
            Value::Field(td, fld, rec) => self.field(*td, *fld, rec, state),
            Value::Write(td, fld, rec, val) => self.write(*td, *fld, rec, val, state),
            Value::Claim(td, db) => {
                if let Value::Reference(db, _, _) = self.calc(db, state) {
                    let size = self.data.def_size(*td);
                    let records = &mut state.databases[db as usize].records;
                    Value::Reference(db, records.claim(size), 0)
                } else {
                    Value::Null
                }
            }
            Value::Vector(td, rec, idx) => {
                if let (Value::Reference(db, v_nr, pos), Value::Int(index)) =
                    (self.calc(rec, state), self.calc(idx, state))
                {
                    let size = self.data.def_size(*td);
                    let records = &mut state.databases[db as usize].records;
                    let (r, p) = op_get_vector(records, v_nr, pos as isize, size, index);
                    Value::Reference(db, r, p)
                } else {
                    Value::Null
                }
            }
            Value::Remove(td, rec, idx) => {
                if let (Value::Reference(db, v_nr, pos), Value::Int(index)) =
                    (self.calc(rec, state), self.calc(idx, state))
                {
                    let size = self.data.def_size(*td);
                    let records = &mut state.databases[db as usize].records;
                    op_remove_vector(records, v_nr, pos as isize, size, index);
                }
                Value::Null
            }
            Value::Insert(td, rec, idx) => {
                if let (Value::Reference(db, v_nr, pos), Value::Int(index)) =
                    (self.calc(rec, state), self.calc(idx, state))
                {
                    let size = self.data.def_size(*td);
                    let records = &mut state.databases[db as usize].records;
                    let (r, p) = op_insert_vector(records, v_nr, pos as isize, size, index);
                    Value::Reference(db, r, p)
                } else {
                    Value::Null
                }
            }
            Value::Append(td, rec) => {
                if let Value::Reference(db, v_nr, pos) = self.calc(rec, state) {
                    let size = self.data.def_size(*td);
                    let records = &mut state.databases[db as usize].records;
                    let (r, p) = op_append_vector(records, v_nr, pos as isize, size);
                    Value::Reference(db, r, p)
                } else {
                    Value::Null
                }
            }
            _ => code.clone(),
        }
    }

    fn call_routine(&self, d_nr: u32, code: &[Value], state: &mut State) -> Value {
        let sp = state.stack.len();
        for c in code {
            let v = self.calc(c, state);
            state.stack.push(v);
        }
        if let Some(f) = &mut state.logging {
            write!(f, "{}{}(", ind(state.indent), self.data.def_name(d_nr)).unwrap();
            for p in 0..code.len() {
                if p > 0 {
                    write!(f, ", ").unwrap();
                }
                write!(f, "var_{} = {:?}", p, state.stack[sp + p]).unwrap();
            }
            writeln!(f, ") {{").unwrap();
            state.indent += 1;
        }
        let c_function = state.c_function;
        let n_function = sp;
        state.c_function = n_function as u32;
        let ret = self.calc(self.data.code(d_nr), state);
        for _p in n_function..state.stack.len() {
            state.stack.pop();
        }
        state.c_function = c_function;
        if state.indent > 0 {
            state.indent -= 1;
        }
        if let Some(f) = &mut state.logging {
            writeln!(f, "{}}} -> {:?}", ind(state.indent), ret).unwrap();
        }
        if let Value::Return(r) = ret {
            *r
        } else {
            ret
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn call_external(&self, e: &External, code: &[Value], state: &mut State) -> Value {
        match e {
            External::Database => {
                if let Value::Int(size) = &code[0] {
                    let mut records = Store::new(50);
                    let p = records.claim(*size as u32);
                    assert_eq!(p, PRIMARY);
                    let db = state.databases.len() as u16;
                    state.databases.push(DataStore {
                        records,
                        text: Store::new(50),
                    });
                    return Value::Reference(db, PRIMARY, 0);
                }
            }
            External::AbsInteger => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Int(x.abs());
                }
            }
            External::AbsLong => {
                if let Value::Long(x) = self.calc(&code[0], state) {
                    return Value::Long(x.abs());
                }
            }
            External::AbsFloat => {
                if let Value::Float(x) = self.calc(&code[0], state) {
                    return Value::Float(x.abs());
                }
            }
            External::AbsSingle => {
                if let Value::Single(x) = self.calc(&code[0], state) {
                    return Value::Single(x.abs());
                }
            }
            External::Not => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Int(1 - x);
                }
            }
            External::CastSingleFromFloat => {
                if let Value::Float(x) = self.calc(&code[0], state) {
                    return Value::Single(x as f32);
                }
            }
            External::CastIntFromFloat => {
                if let Value::Float(x) = self.calc(&code[0], state) {
                    return Value::Int(x as i32);
                }
            }
            External::CastLongFromFloat => {
                if let Value::Float(x) = self.calc(&code[0], state) {
                    return Value::Long(x as i64);
                }
            }
            External::CastIntFromSingle => {
                if let Value::Single(x) = self.calc(&code[0], state) {
                    return Value::Int(x as i32);
                }
            }
            External::CastLongFromSingle => {
                if let Value::Single(x) = self.calc(&code[0], state) {
                    return Value::Long(x as i64);
                }
            }
            External::CastIntFromLong => {
                if let Value::Long(x) = self.calc(&code[0], state) {
                    return Value::Int(x as i32);
                }
            }
            External::ConvLongFromInt => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Long(x as i64);
                }
            }
            External::ConvFloatFromInt => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Float(x as f64);
                }
            }
            External::ConvFloatFromLong => {
                if let Value::Long(x) = self.calc(&code[0], state) {
                    return Value::Float(x as f64);
                }
            }
            External::ConvFloatFromSingle => {
                if let Value::Single(x) = self.calc(&code[0], state) {
                    return Value::Float(x as f64);
                }
            }
            External::ConvSingleFromInt => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Single(x as f32);
                }
            }
            External::ConvBoolFromInt => {
                return Value::Int(if let Value::Int(_x) = self.calc(&code[0], state) {
                    1
                } else {
                    0
                })
            }
            External::ConvBoolFromLong => {
                return Value::Int(if let Value::Long(_x) = self.calc(&code[0], state) {
                    1
                } else {
                    0
                })
            }
            External::ConvBoolFromFloat => {
                return Value::Int(if let Value::Float(x) = self.calc(&code[0], state) {
                    if x.is_nan() {
                        0
                    } else {
                        1
                    }
                } else {
                    0
                })
            }
            External::ConvBoolFromSingle => {
                return Value::Int(if let Value::Single(x) = self.calc(&code[0], state) {
                    if x.is_nan() {
                        0
                    } else {
                        1
                    }
                } else {
                    0
                })
            }
            External::ConvBoolFromRef => {
                return Value::Int(
                    if let Value::Reference(_, _, _) = self.calc(&code[0], state) {
                        1
                    } else {
                        0
                    },
                )
            }
            External::ConvBoolFromEnum => {
                return Value::Int(if let Value::Int(_x) = self.calc(&code[0], state) {
                    1
                } else {
                    0
                })
            }
            External::ConvBoolFromText => {
                return Value::Int(if let Value::Text(_x) = self.calc(&code[0], state) {
                    1
                } else {
                    0
                })
            }
            External::ConvIntFromNull => return Value::Null,
            External::ConvLongFromNull => return Value::Null,
            External::ConvFloatFromNull => return Value::Null,
            External::ConvSingleFromNull => return Value::Null,
            External::ConvRefFromNull => return Value::Null,
            External::ConvEnumFromNull => return Value::Null,
            External::ConvTextFromNull => return Value::Null,
            External::AddInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(x + y);
                }
            }
            External::MinSingleInt => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return Value::Int(-x);
                }
            }
            External::MinInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(x - y);
                }
            }
            External::MulInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(x * y);
                }
            }
            External::DivInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0 {
                        return Value::Int(x / y);
                    }
                }
            }
            External::RemInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0 {
                        return Value::Int(x % y);
                    }
                }
            }
            External::AddLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Long(x + y);
                }
            }
            External::MinSingleLong => {
                if let Value::Long(x) = self.calc(&code[0], state) {
                    return Value::Long(-x);
                }
            }
            External::MinLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Long(x - y);
                }
            }
            External::MulLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Long(x * y);
                }
            }
            External::DivLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0 {
                        return Value::Long(x / y);
                    }
                }
            }
            External::RemLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0 {
                        return Value::Long(x % y);
                    }
                }
            }
            External::AddFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Float(x + y);
                }
            }
            External::MinSingleFloat => {
                if let Value::Float(x) = self.calc(&code[0], state) {
                    return Value::Float(-x);
                }
            }
            External::MinFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Float(x - y);
                }
            }
            External::MulFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Float(x * y);
                }
            }
            External::DivFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0.0 {
                        return Value::Float(x / y);
                    }
                }
            }
            External::RemFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0.0 {
                        return Value::Float(x % y);
                    }
                }
            }
            External::AddSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Single(x + y);
                }
            }
            External::AddText => {
                if let (Value::Text(x), Value::Text(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Text(x + &y);
                }
            }
            External::MinSingleSingle => {
                if let Value::Single(x) = self.calc(&code[0], state) {
                    return Value::Single(-x);
                }
            }
            External::MinSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Single(x - y);
                }
            }
            External::MulSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Single(x * y);
                }
            }
            External::DivSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0.0 {
                        return Value::Single(x / y);
                    }
                }
            }
            External::RemSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    if y != 0.0 {
                        return Value::Single(x % y);
                    }
                }
            }
            External::And => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return if x == 1 {
                        self.calc(&code[1], state)
                    } else {
                        Value::Int(0)
                    };
                }
            }
            External::Or => {
                if let Value::Int(x) = self.calc(&code[0], state) {
                    return if x == 0 {
                        self.calc(&code[1], state)
                    } else {
                        Value::Int(1)
                    };
                }
            }
            External::EqInt => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeInt => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::LtInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::GtInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeInt => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::EqLong => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeLong => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::LtLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::GtLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeLong => {
                if let (Value::Long(x), Value::Long(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::EqFloat => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeFloat => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::LtFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::GtFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeFloat => {
                if let (Value::Float(x), Value::Float(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::EqSingle => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeSingle => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::LtSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::GtSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeSingle => {
                if let (Value::Single(x), Value::Single(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::EqBool => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeBool => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::EqRef => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeRef => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::EqEnum => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeEnum => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::GtEnum => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeEnum => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::LtEnum => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeEnum => {
                if let (Value::Int(x), Value::Int(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::EqText => {
                return Value::Int(
                    if self.calc(&code[0], state) == self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::NeText => {
                return Value::Int(
                    if self.calc(&code[0], state) != self.calc(&code[1], state) {
                        1
                    } else {
                        0
                    },
                );
            }
            External::LtText => {
                if let (Value::Text(x), Value::Text(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x < y { 1 } else { 0 });
                }
            }
            External::LeText => {
                if let (Value::Text(x), Value::Text(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x <= y { 1 } else { 0 });
                }
            }
            External::GtText => {
                if let (Value::Text(x), Value::Text(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x > y { 1 } else { 0 });
                }
            }
            External::GeText => {
                if let (Value::Text(x), Value::Text(y)) =
                    (self.calc(&code[0], state), self.calc(&code[1], state))
                {
                    return Value::Int(if x >= y { 1 } else { 0 });
                }
            }
            External::ClearText => {}
            External::ClearVector => {
                if let Value::Reference(db, v_nr, pos) = self.calc(&code[0], state) {
                    let records = &mut state.databases[db as usize].records;
                    op_clear_vector(records, v_nr, pos as isize);
                }
            }
            External::LengthVector => {
                if let Value::Reference(db, v_nr, pos) = self.calc(&code[0], state) {
                    let records = &mut state.databases[db as usize].records;
                    return Value::Int(op_length_vector(records, v_nr, pos as isize));
                }
            }
            External::FinishSorted => {}
            External::LengthText => {
                if let Value::Text(x) = self.calc(&code[0], state) {
                    return Value::Int(x.len() as i32);
                }
            }
            External::FormatText => {
                if let (Value::Text(val), Value::Int(width), Value::Int(dir), Value::Int(token)) = (
                    self.calc(&code[0], state),
                    self.calc(&code[1], state),
                    self.calc(&code[2], state),
                    self.calc(&code[3], state),
                ) {
                    return Value::Text(format::format_text(
                        &val,
                        width as u8,
                        dir as i8,
                        token as u8 as char,
                    ));
                }
            }
            External::FormatInt => {
                if let (
                    Value::Int(val),
                    Value::Int(radix),
                    Value::Int(width),
                    Value::Int(token),
                    Value::Int(plus),
                    Value::Int(note),
                ) = (
                    self.calc(&code[0], state),
                    self.calc(&code[1], state),
                    self.calc(&code[2], state),
                    self.calc(&code[3], state),
                    self.calc(&code[4], state),
                    self.calc(&code[5], state),
                ) {
                    return Value::Text(format::format_int(
                        val,
                        radix as u8,
                        width as u8,
                        token as u8 as char,
                        plus == 1,
                        note == 1,
                    ));
                }
            }
            External::FormatLong => {
                if let (
                    Value::Long(val),
                    Value::Int(radix),
                    Value::Int(width),
                    Value::Int(token),
                    Value::Int(plus),
                    Value::Int(note),
                ) = (
                    self.calc(&code[0], state),
                    self.calc(&code[1], state),
                    self.calc(&code[2], state),
                    self.calc(&code[3], state),
                    self.calc(&code[4], state),
                    self.calc(&code[5], state),
                ) {
                    return Value::Text(format::format_long(
                        val,
                        radix as u8,
                        width as u8,
                        token as u8 as char,
                        plus == 1,
                        note == 1,
                    ));
                }
            }
            External::FormatFloat => {
                if let (Value::Float(val), Value::Int(width), Value::Int(precision)) = (
                    self.calc(&code[0], state),
                    self.calc(&code[1], state),
                    self.calc(&code[2], state),
                ) {
                    return Value::Text(format::format_float(val, width as u8, precision as u8));
                }
            }
            External::FormatSingle => {
                if let (Value::Single(val), Value::Int(width), Value::Int(precision)) = (
                    self.calc(&code[0], state),
                    self.calc(&code[1], state),
                    self.calc(&code[2], state),
                ) {
                    return Value::Text(format::format_single(val, width as u8, precision as u8));
                }
            }
        };
        Value::Null
    }
}
