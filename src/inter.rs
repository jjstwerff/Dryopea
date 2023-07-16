// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! The interpreter for in memory code.
//! Eventually most projects will only run compiled code.
//! However, it is still an easy way to validate code without relative slow compiling, optimizing and linking.

extern crate strum;
extern crate strum_macros;

use crate::data::{Data, Value};
use crate::external;
use crate::store::Store;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

// was 999_999_999;
const MAX_LOOPING: u32 = 999;

pub struct State {
    c_function: u32,
    stack: Vec<Value>,
    database: Vec<Store>,
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
            database: Vec::new(),
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

type Extern = fn(&Inter, &[Value], &mut State) -> Value;

pub struct Inter<'a> {
    pub data: &'a Data,
    external: HashMap<u32, Extern>,
}

const EXTERN: &[(&str, Extern)] = &[
    ("OpDatabase", int_db_new),
    ("OpConvRefFromMut", int_db_ro),
    ("OpNot", int_not),
    ("OpAppend", int_append),
    ("OpCastSingleFromFloat", int_cast_single_from_float),
    ("OpCastIntFromFloat", int_cast_int_from_float),
    ("OpCastLongFromFloat", int_cast_long_from_float),
    ("OpCastIntFromSingle", int_cast_int_from_single),
    ("OpCastLongFromSingle", int_cast_long_from_single),
    ("OpCastIntFromLong", int_cast_int_from_long),
    ("OpConvLongFromInt", int_conv_long_from_int),
    ("OpConvFloatFromInt", int_conv_float_from_int),
    ("OpConvFloatFromLong", int_conv_float_from_long),
    ("OpConvFloatFromSingle", int_conv_float_from_single),
    ("OpConvSingleFromInt", int_conv_single_from_int),
    ("OpConvBoolFromInt", int_conv_bool_from_int),
    ("OpConvBoolFromLong", int_conv_bool_from_long),
    ("OpConvBoolFromFloat", int_conv_bool_from_float),
    ("OpConvBoolFromSingle", int_conv_bool_from_single),
    ("OpConvBoolFromRef", int_conv_bool_from_ref),
    ("OpConvBoolFromEnum", int_conv_bool_from_enum),
    ("OpConvBoolFromText", int_conv_bool_from_text),
    ("OpConvIntFromNull", int_conv_int_from_null),
    ("OpConvLongFromNull", int_conv_long_from_null),
    ("OpConvFloatFromNull", int_conv_float_from_null),
    ("OpConvSingleFromNull", int_conv_single_from_null),
    ("OpConvRefFromNull", int_conv_ref_from_null),
    ("OpConvEnumFromNull", int_conv_enum_from_null),
    ("OpConvTextFromNull", int_conv_text_from_null),
    ("OpAddInt", int_add_int),
    ("OpMinSingleInt", int_min_single_int),
    ("OpMinInt", int_min_int),
    ("OpMulInt", int_mul_int),
    ("OpDivInt", int_div_int),
    ("OpRemInt", int_rem_int),
    ("OpAddLong", int_add_long),
    ("OpMinSingleLong", int_min_single_long),
    ("OpMinLong", int_min_long),
    ("OpMulLong", int_mul_long),
    ("OpDivLong", int_div_long),
    ("OpRemLong", int_rem_long),
    ("OpAddFloat", int_add_float),
    ("OpMinSingleFloat", int_min_single_float),
    ("OpMinFloat", int_min_float),
    ("OpMulFloat", int_mul_float),
    ("OpDivFloat", int_div_float),
    ("OpRemFloat", int_rem_float),
    ("OpAddSingle", int_add_single),
    ("OpMinSingleSingle", int_min_single_single),
    ("OpMinSingle", int_min_single),
    ("OpMulSingle", int_mul_single),
    ("OpDivSingle", int_div_single),
    ("OpRemSingle", int_rem_single),
    ("OpAnd", int_and),
    ("OpOr", int_or),
    ("OpEqLong", int_eq_long),
    ("OpNeLong", int_ne_long),
    ("OpLtLong", int_lt_long),
    ("OpLeLong", int_le_long),
    ("OpGtLong", int_gt_long),
    ("OpGeLong", int_ge_long),
    ("OpEqInt", int_eq_int),
    ("OpNeInt", int_ne_int),
    ("OpLtInt", int_lt_int),
    ("OpLeInt", int_le_int),
    ("OpGtInt", int_gt_int),
    ("OpGeInt", int_ge_int),
    ("OpEqFloat", int_eq_float),
    ("OpNeFloat", int_ne_float),
    ("OpLtFloat", int_lt_float),
    ("OpLeFloat", int_le_float),
    ("OpGtFloat", int_gt_float),
    ("OpGeFloat", int_ge_float),
    ("OpEqSingle", int_eq_single),
    ("OpNeSingle", int_ne_single),
    ("OpLtSingle", int_lt_single),
    ("OpLeSingle", int_le_single),
    ("OpGtSingle", int_gt_single),
    ("OpGeSingle", int_ge_single),
    ("OpEqBool", int_eq_bool),
    ("OpNeBool", int_ne_bool),
    ("OpEqRef", int_eq_ref),
    ("OpNeRef", int_ne_ref),
    ("OpEqEnum", int_eq_enum),
    ("OpNeEnum", int_ne_enum),
    ("OpLtEnum", int_lt_enum),
    ("OpGtEnum", int_gt_enum),
    ("OpLeEnum", int_le_enum),
    ("OpGeEnum", int_ge_enum),
    ("OpEqText", int_eq_text),
    ("OpNeText", int_ne_text),
    ("OpLtText", int_lt_text),
    ("OpLeText", int_le_text),
    ("OpGtText", int_gt_text),
    ("OpGeText", int_ge_text),
    ("OpClearText", int_clear_text),
    ("OpClearVector", int_clear_vector),
    ("OpLengthVector", int_length_vector),
    ("OpFinishSorted", int_finish_sorted),
    ("OpLengthText", int_length_text),
    ("OpAddText", int_add_text),
    ("OpAbsInt", int_abs_integer),
    ("OpAbsLong", int_abs_long),
    ("OpAbsFloat", int_abs_float),
    ("OpAbsSingle", int_abs_single),
    ("OpFormatText", int_format_text),
    ("OpFormatBool", int_format_bool),
    ("OpFormatInt", int_format_int),
    ("OpFormatLong", int_format_long),
    ("OpFormatFloat", int_format_float),
    ("OpFormatSingle", int_format_single),
    ("OpGetByte", int_get_byte),
    ("OpGetShort", int_get_short),
    ("OpGetInt", int_get_int),
    ("OpGetText", int_get_text),
    ("OpGetSingle", int_get_single),
    ("OpGetFloat", int_get_float),
    ("OpGetLong", int_get_long),
    ("OpSetByte", int_set_byte),
    ("OpSetShort", int_set_short),
    ("OpSetInt", int_set_int),
    ("OpGetRef", int_get_ref),
    ("OpGetMut", int_mut_ref),
    ("OpSetRef", int_set_ref),
    ("OpSetText", int_set_text),
    ("OpSetSingle", int_set_single),
    ("OpSetFloat", int_set_float),
    ("OpSetLong", int_set_long),
    ("OpAppendVector", int_append_vector),
    ("OpRemoveVector", int_remove_vector),
    ("OpInsertVector", int_insert_vector),
    ("OpGetVector", int_get_vector),
    ("OpGetMutVector", int_mut_vector),
    ("OpGetSorted", int_get_sorted),
    ("OpGetMutSorted", int_mut_sorted),
    ("OpGetHash", int_get_hash),
    ("OpGetMutHash", int_mut_hash),
    ("OpGetIndex", int_get_index),
    ("OpGetMutIndex", int_mut_index),
    ("OpGetRadix", int_get_radix),
    ("OpGetMutRadix", int_mut_radix),
    ("OpAssert", int_assert),
    ("OpPrint", int_print),
];

impl<'d> Inter<'d> {
    pub fn new(data: &'d Data) -> Inter<'d> {
        let mut external = HashMap::new();
        for (name, ext) in EXTERN {
            let nr = data.def_nr(name);
            if nr != u32::MAX {
                external.insert(nr, *ext);
            } else {
                panic!("Could not find definition @{name}");
            }
        }
        Inter { data, external }
    }

    /// Start the interpreter on a specific piece of code
    pub fn calculate(&self, name: &str, log: Option<File>) -> Result<Value, String> {
        let dnr = self.data.def_nr(name);
        if dnr != u32::MAX {
            let state = &mut State::new();
            state.logging = log;
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

    fn calc(&self, code: &Value, state: &mut State) -> Value {
        match code {
            Value::Call(nr, call_code) => {
                if let Some(ext) = self.external.get(nr) {
                    let res = ext(self, call_code, state);
                    if let Some(f) = &mut state.logging {
                        write!(f, "{}{:?}(", ind(state.indent), nr).unwrap();
                        for (v, c) in call_code.iter().enumerate() {
                            if v > 0 {
                                write!(f, ", ").unwrap();
                            }
                            self.data.output_code(f, c, state.indent + 1).unwrap();
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
            Value::Set(var_nr, value) | Value::Let(var_nr, value) => {
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
}

fn int_db_new(_i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(size) = &code[0] {
        let mut store = Store::new(50);
        let rec = store.claim(*size as u32);
        store.references.push((rec, 0));
        let db = state.database.len() as u16;
        return Value::Mutable(db, 0);
    }
    Value::Null
}

fn int_db_ro(_i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Mutable(db, r) = &code[0] {
        let (rec, fld) = state.database[*db as usize].references[*r as usize];
        return Value::Reference(*db, rec, fld);
    }
    Value::Null
}

fn int_append(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Mutable(db, _), Value::Int(size)) = (i.calc(&code[0], state), &code[1]) {
        let store = &mut state.database[db as usize];
        let r = store.references.len();
        let rec = store.claim(*size as u32);
        store.references.push((rec, 0));
        return Value::Mutable(db, r as u32);
    }
    Value::Null
}

fn int_abs_integer(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Int(x.abs());
    }
    Value::Null
}

fn int_abs_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Long(x) = i.calc(&code[0], state) {
        return Value::Long(x.abs());
    }
    Value::Null
}

fn int_abs_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Float(x) = i.calc(&code[0], state) {
        return Value::Float(x.abs());
    }
    Value::Null
}

fn int_abs_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Single(x) = i.calc(&code[0], state) {
        return Value::Single(x.abs());
    }
    Value::Null
}

fn int_not(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Int(1 - x);
    }
    Value::Null
}

fn int_cast_single_from_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Float(x) = i.calc(&code[0], state) {
        return Value::Single(x as f32);
    }
    Value::Null
}

fn int_cast_int_from_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Float(x) = i.calc(&code[0], state) {
        return Value::Int(x as i32);
    }
    Value::Null
}

fn int_cast_long_from_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Float(x) = i.calc(&code[0], state) {
        return Value::Long(x as i64);
    }
    Value::Null
}

fn int_cast_int_from_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Single(x) = i.calc(&code[0], state) {
        return Value::Int(x as i32);
    }
    Value::Null
}

fn int_cast_long_from_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Single(x) = i.calc(&code[0], state) {
        return Value::Long(x as i64);
    }
    Value::Null
}

fn int_cast_int_from_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Long(x) = i.calc(&code[0], state) {
        return Value::Int(x as i32);
    }
    Value::Null
}

fn int_conv_long_from_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Long(x as i64);
    }
    Value::Null
}

fn int_conv_float_from_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Float(x as f64);
    }
    Value::Null
}

fn int_conv_float_from_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Long(x) = i.calc(&code[0], state) {
        return Value::Float(x as f64);
    }
    Value::Null
}

fn int_conv_float_from_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Single(x) = i.calc(&code[0], state) {
        return Value::Float(x as f64);
    }
    Value::Null
}

fn int_conv_single_from_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Single(x as f32);
    }
    Value::Null
}

fn int_conv_bool_from_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Int(_x) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_bool_from_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Long(_x) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_bool_from_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Long(_x) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_bool_from_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Single(x) = i.calc(&code[0], state) {
        if x.is_nan() {
            0
        } else {
            1
        }
    } else {
        0
    })
}

fn int_conv_bool_from_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Reference(_, _, _) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_bool_from_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Int(_x) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_bool_from_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if let Value::Text(_x) = i.calc(&code[0], state) {
        1
    } else {
        0
    })
}

fn int_conv_int_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_long_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_float_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_single_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_text_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_ref_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_conv_enum_from_null(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    Value::Null
}

fn int_add_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(x + y);
    }
    Value::Null
}

fn int_min_single_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return Value::Int(-x);
    }
    Value::Null
}

fn int_min_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(x - y);
    }
    Value::Null
}

fn int_mul_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(x * y);
    }
    Value::Null
}

fn int_div_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0 {
            return Value::Int(x / y);
        }
    }
    Value::Null
}

fn int_rem_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0 {
            return Value::Int(x % y);
        }
    }
    Value::Null
}

fn int_add_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Long(x + y);
    }
    Value::Null
}

fn int_min_single_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Long(x) = i.calc(&code[0], state) {
        return Value::Long(-x);
    }
    Value::Null
}

fn int_min_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Long(x - y);
    }
    Value::Null
}

fn int_mul_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Long(x * y);
    }
    Value::Null
}

fn int_div_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0 {
            return Value::Long(x / y);
        }
    }
    Value::Null
}

fn int_rem_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0 {
            return Value::Long(x % y);
        }
    }
    Value::Null
}

fn int_add_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Float(x + y);
    }
    Value::Null
}

fn int_min_single_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Float(x) = i.calc(&code[0], state) {
        return Value::Float(-x);
    }
    Value::Null
}

fn int_min_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Float(x - y);
    }
    Value::Null
}

fn int_mul_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Float(x * y);
    }
    Value::Null
}

fn int_div_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0.0 {
            return Value::Float(x / y);
        }
    }
    Value::Null
}

fn int_rem_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        if y != 0.0 {
            return Value::Float(x % y);
        }
    }
    Value::Null
}

fn int_add_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Single(x + y);
    }
    Value::Null
}

fn int_add_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(x), Value::Text(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Text(x + &y);
    }
    Value::Null
}

fn int_min_single_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Single(x) = i.calc(&code[0], state) {
        return Value::Single(-x);
    }
    Value::Null
}

fn int_min_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Single(x - y);
    }
    Value::Null
}

fn int_mul_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Single(x * y);
    }
    Value::Null
}

fn int_div_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        if y != 0.0 {
            return Value::Single(x / y);
        }
    }
    Value::Null
}

fn int_rem_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        if y != 0.0 {
            return Value::Single(x % y);
        }
    }
    Value::Null
}

fn int_and(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return if x == 1 {
            i.calc(&code[1], state)
        } else {
            Value::Int(0)
        };
    }
    Value::Null
}

fn int_or(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(x) = i.calc(&code[0], state) {
        return if x == 0 {
            i.calc(&code[1], state)
        } else {
            Value::Int(1)
        };
    }
    Value::Null
}

fn int_eq_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_lt_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_gt_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_eq_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_lt_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_gt_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Long(x), Value::Long(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_eq_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_lt_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_gt_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(x), Value::Float(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_eq_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_lt_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_gt_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(x), Value::Single(y)) = (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_eq_bool(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_bool(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_eq_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_eq_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_gt_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_lt_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_enum(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(x), Value::Int(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_eq_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) == i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_ne_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    Value::Int(if i.calc(&code[0], state) != i.calc(&code[1], state) {
        1
    } else {
        0
    })
}

fn int_lt_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(x), Value::Text(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x < y { 1 } else { 0 });
    }
    Value::Null
}

fn int_le_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(x), Value::Text(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x <= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_gt_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(x), Value::Text(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x > y { 1 } else { 0 });
    }
    Value::Null
}

fn int_ge_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(x), Value::Text(y)) = (i.calc(&code[0], state), i.calc(&code[1], state)) {
        return Value::Int(if x >= y { 1 } else { 0 });
    }
    Value::Null
}

fn int_clear_text(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    // TODO clear the specific variable
    Value::Null
}

fn int_clear_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Mutable(db, ref_nr) = i.calc(&code[0], state) {
        let store = &mut state.database[db as usize];
        let (v_nr, _) = store.references[ref_nr as usize];
        external::op_clear_vector(store, v_nr);
    }
    Value::Null
}

fn int_length_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    match i.calc(&code[0], state) {
        Value::Reference(db, rec, _) => {
            let store = &state.database[db as usize];
            Value::Int(external::op_length_vector(store, rec))
        }
        Value::Mutable(db, ref_nr) => {
            let store = &state.database[db as usize];
            let (v_nr, _) = store.references[ref_nr as usize];
            Value::Int(external::op_length_vector(store, v_nr))
        }
        _ => Value::Null,
    }
}

fn int_finish_sorted(_i: &Inter, _code: &[Value], _state: &mut State) -> Value {
    // TODO implement me, move last element to the correct spot
    Value::Null
}

fn int_length_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Text(x) = i.calc(&code[0], state) {
        return Value::Int(x.len() as i32);
    }
    Value::Null
}

fn int_format_bool(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Int(val), Value::Int(width), Value::Int(dir), Value::Int(token)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
    ) {
        return Value::Text(external::format_text(
            if val == 0 { "true" } else { "false" },
            width,
            dir,
            token,
        ));
    }
    Value::Null
}

fn int_format_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Text(val), Value::Int(width), Value::Int(dir), Value::Int(token)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
    ) {
        return Value::Text(external::format_text(&val, width, dir, token));
    }
    Value::Null
}

fn int_format_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (
        Value::Int(val),
        Value::Int(radix),
        Value::Int(width),
        Value::Int(token),
        Value::Int(plus),
        Value::Int(note),
    ) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
        i.calc(&code[4], state),
        i.calc(&code[5], state),
    ) {
        return Value::Text(external::format_int(
            val,
            radix,
            width,
            token,
            plus == 1,
            note == 1,
        ));
    }
    Value::Null
}

fn int_format_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (
        Value::Long(val),
        Value::Int(radix),
        Value::Int(width),
        Value::Int(token),
        Value::Int(plus),
        Value::Int(note),
    ) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
        i.calc(&code[4], state),
        i.calc(&code[5], state),
    ) {
        return Value::Text(external::format_long(
            val,
            radix,
            width,
            token,
            plus == 1,
            note == 1,
        ));
    }
    Value::Null
}

fn int_format_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Float(val), Value::Int(width), Value::Int(precision)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        return Value::Text(external::format_float(val, width, precision));
    }
    Value::Null
}

fn int_format_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Single(val), Value::Int(width), Value::Int(precision)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        return Value::Text(external::format_single(val, width, precision));
    }
    Value::Null
}

fn int_get_byte(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Int(min)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        return Value::Int(state.database[db as usize].get_byte(
            rec,
            pos as isize + fld as isize,
            min,
        ));
    }
    Value::Null
}

fn int_get_short(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Int(min)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        return Value::Int(state.database[db as usize].get_short(
            rec,
            pos as isize + fld as isize,
            min,
        ));
    }
    Value::Null
}

fn int_get_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Int(state.database[db as usize].get_int(rec, pos as isize + fld as isize));
    }
    Value::Null
}

fn int_get_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        let store = &state.database[db as usize];
        return Value::Text(String::from(
            store.get_str(store.get_int(rec, pos as isize + fld as isize) as u32),
        ));
    }
    Value::Null
}

fn int_get_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Long(state.database[db as usize].get_long(rec, pos as isize + fld as isize));
    }
    Value::Null
}

fn int_get_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Single(
            state.database[db as usize].get_single(rec, pos as isize + fld as isize),
        );
    }
    Value::Null
}

fn int_get_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Float(
            state.database[db as usize].get_float(rec, pos as isize + fld as isize),
        );
    }
    Value::Null
}

fn int_set_byte(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Int(min), Value::Int(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
    ) {
        state.database[db as usize].set_byte(rec, pos as isize + fld as isize, min, val);
    }
    Value::Null
}

fn int_set_short(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Int(min), Value::Int(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
        i.calc(&code[3], state),
    ) {
        state.database[db as usize].set_short(rec, pos as isize + fld as isize, min, val);
    }
    Value::Null
}

fn int_set_int(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Int(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        state.database[db as usize].set_int(rec, pos as isize + fld as isize, val);
    }
    Value::Null
}

fn int_get_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        return Value::Reference(
            db,
            state.database[db as usize].get_int(rec, pos as isize + fld as isize) as u32,
            0,
        );
    }
    Value::Null
}

fn int_mut_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld)) =
        (i.calc(&code[0], state), i.calc(&code[1], state))
    {
        let store = &mut state.database[db as usize];
        let r = store.references.len() as u32;
        let p = store.get_int(rec, pos as isize + fld as isize) as u32;
        store.references.push((p, 0));
        return Value::Mutable(db, r);
    }
    Value::Null
}

fn int_set_ref(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Reference(_, val, _)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        state.database[db as usize].set_int(rec, pos as isize + fld as isize, val as i32);
    }
    Value::Null
}

fn int_set_text(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Text(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let txt = store.set_str(&val);
        store.set_int(rec, pos as isize + fld as isize, txt as i32);
    }
    Value::Null
}

fn int_set_long(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Long(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        state.database[db as usize].set_long(rec, pos as isize + fld as isize, val);
    }
    Value::Null
}

fn int_set_single(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Single(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        state.database[db as usize].set_single(rec, pos as isize + fld as isize, val);
    }
    Value::Null
}

fn int_set_float(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, rec, pos), Value::Int(fld), Value::Float(val)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        state.database[db as usize].set_float(rec, pos as isize + fld as isize, val);
    }
    Value::Null
}

fn int_append_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size)) = (i.calc(&code[0], state), &code[1])
    {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_append_vector(store, v_nr, pos as isize, *size as u32);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_remove_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) =
        (i.calc(&code[0], state), &code[1], i.calc(&code[2], state))
    {
        let store = &mut state.database[db as usize];
        external::op_remove_vector(store, v_nr, pos as isize, *size as u32, index);
    }
    Value::Null
}

fn int_insert_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) =
        (i.calc(&code[0], state), &code[1], i.calc(&code[2], state))
    {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_insert_vector(store, v_nr, pos as isize, *size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_get_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_mut_vector(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let rnr = store.references.len() as u32;
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        store.references.push((r, p));
        Value::Mutable(db, rnr)
    } else {
        Value::Null
    }
}

fn int_get_sorted(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_mut_sorted(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let rnr = store.references.len() as u32;
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        store.references.push((r, p));
        Value::Mutable(db, rnr)
    } else {
        Value::Null
    }
}
fn int_get_hash(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_mut_hash(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let rnr = store.references.len() as u32;
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        store.references.push((r, p));
        Value::Mutable(db, rnr)
    } else {
        Value::Null
    }
}
fn int_get_index(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_mut_index(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let rnr = store.references.len() as u32;
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        store.references.push((r, p));
        Value::Mutable(db, rnr)
    } else {
        Value::Null
    }
}
fn int_get_radix(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        Value::Reference(db, r, p)
    } else {
        Value::Null
    }
}

fn int_mut_radix(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let (Value::Reference(db, v_nr, pos), Value::Int(size), Value::Int(index)) = (
        i.calc(&code[0], state),
        i.calc(&code[1], state),
        i.calc(&code[2], state),
    ) {
        let store = &mut state.database[db as usize];
        let rnr = store.references.len() as u32;
        let (r, p) = external::op_get_vector(store, v_nr, pos as isize, size as u32, index);
        store.references.push((r, p));
        Value::Mutable(db, rnr)
    } else {
        Value::Null
    }
}

fn int_assert(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Int(0) = i.calc(&code[0], state) {
        if let Value::Text(t) = i.calc(&code[1], state) {
            panic!("{}", t)
        }
    }
    Value::Null
}

fn int_print(i: &Inter, code: &[Value], state: &mut State) -> Value {
    if let Value::Text(v) = i.calc(&code[0], state) {
        print!("{}", v);
    }
    Value::Null
}
