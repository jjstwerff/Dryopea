// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(dead_code)]

//! Fast interpreter for binary code, including library and coroutines support.
use crate::data::{Data, DefType, Value};
use crate::database::{KnownTypes, Stores};
use crate::external;
use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::HashMap;
use std::io::Write;

static STACK_BLOCK: u16 = 4000;

// Bytecode generation

pub struct State<'a> {
    // Set of stack blocks with each almost 4k data
    bytecode: Vec<u8>,
    text_code: Vec<u8>,
    strings: Vec<String>,
    stacks: Vec<*mut u8>,
    stack_cur: u16,
    stack_pos: u16,
    code_pos: u32,
    database: Stores<'a>,
    operations: Vec<fn(&mut State)>,
    op_names: HashMap<String, u8>,
}

static A: System = System;

#[repr(u8)]
// total length 14 (DbSlice is the longest)
pub enum TextType {
    Null,
    /// A static string type:u8, code:u32, len:u8
    Constant,
    /// A dynamic string type:u8, str:u16
    String,
    /// A slice string type:u8, str:u16, pos:u32, len:u32
    Slice,
    /// A mutable database string from a field type:u8, db:u8, rec:u32, pos:u32
    DbRef,
    /// A part of a database string type:u8, store:u8, rec:u32, str_pos:u32, len:u32
    DbSlice,
}

impl TryFrom<u8> for TextType {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == TextType::Null as u8 => Ok(TextType::Null),
            x if x == TextType::Constant as u8 => Ok(TextType::Constant),
            x if x == TextType::String as u8 => Ok(TextType::String),
            x if x == TextType::Slice as u8 => Ok(TextType::Slice),
            x if x == TextType::DbSlice as u8 => Ok(TextType::DbSlice),
            x if x == TextType::DbRef as u8 => Ok(TextType::DbRef),
            _ => Err(()),
        }
    }
}

fn goto(s: &mut State) {
    let step = *(s.code() as &i8);
    s.code_pos = (s.code_pos as i32 + step as i32) as u32;
}

fn goto_word(s: &mut State) {
    let step = *(s.code() as &i16);
    s.code_pos = (s.code_pos as i32 + step as i32) as u32;
}

fn goto_false(s: &mut State) {
    s.stack_pos -= 1;
    if !*(s.stack(0) as &bool) {
        let step = *(s.code() as &i8);
        s.code_pos = (s.code_pos as i32 + step as i32) as u32;
    } else {
        s.code_pos += 1;
    }
}

fn goto_false_word(s: &mut State) {
    s.stack_pos -= 1;
    if !*(s.stack(0) as &bool) {
        let step = *(s.code() as &i16);
        s.code_pos = (s.code_pos as i32 + step as i32) as u32;
    } else {
        s.code_pos += 2;
    }
}

fn call(s: &mut State) {
    let fn_size = *(s.code() as &u8);
    s.code_pos += 1;
    let to_code = *s.code();
    s.code_pos += 4;
    *s.stack_mut(0) = s.code_pos;
    s.stack_pos += 4;
    if s.stack_pos + fn_size as u16 > STACK_BLOCK {
        s.new_stack();
    }
    s.code_pos = to_code;
}

fn return_call(s: &mut State) {
    let stack_size = *(s.code() as &u8);
    s.code_pos += 1;
    let size = *(s.code() as &u8);
    s.code_pos += 1;
    let return_code = *(s.stack(stack_size) as &u32);
    if s.stack_pos - stack_size as u16 == 4 {
        let cur = *(s.stack(stack_size + 4));
        let pos = *(s.stack(stack_size + 2));
        s.stack_copy(s.stack_pos, stack_size, cur, pos);
        s.stack_cur = cur;
        s.stack_pos = pos;
    } else {
        s.stack_copy(
            s.stack_pos,
            size,
            s.stack_cur,
            s.stack_pos - stack_size as u16,
        );
        s.stack_pos -= stack_size as u16;
    }
    s.stack_pos += size as u16;
    s.code_pos = return_code;
}

fn var_bool(s: &mut State) {
    *s.stack_mut(0) = *(s.stack(*(s.code() as &u8)) as &bool);
    s.code_pos += 1;
    s.stack_pos += 1;
}

fn put_bool(s: &mut State) {
    *s.stack_mut(*(s.code() as &u8)) = *(s.stack(1) as &bool);
    s.code_pos += 1;
    s.stack_pos -= 1;
}

fn get_bool(s: &mut State) {
    let ref_var = *(s.code() as &u8);
    s.code_pos += 1;
    let fld = *(s.code() as &u8);
    s.code_pos += 1;
    let store = *(s.stack(ref_var) as &u8);
    let rec = *(s.stack(ref_var - 1) as &u32);
    let pos = *(s.stack(ref_var - 5) as &u32);
    let store = &s.database.stores[store as usize];
    *s.stack_mut(0) = store.get_byte(rec, pos as isize + fld as isize, 0) as u8;
    s.stack_pos += 1;
}

fn set_bool(s: &mut State) {
    let ref_var = *(s.code() as &u8);
    s.code_pos += 1;
    let fld = *(s.code() as &u8);
    s.code_pos += 1;
    let store = *(s.stack(ref_var) as &u8);
    let rec = *(s.stack(ref_var - 1) as &u32);
    let pos = *(s.stack(ref_var - 5) as &u32);
    let val = *(s.stack(0) as &u8);
    let store = &mut s.database.stores[store as usize];
    store.set_byte(rec, pos as isize + fld as isize, 0, val as i32);
    s.stack_pos -= 1;
}

fn format_bool(s: &mut State) {
    let ref_var = *(s.code() as &u8);
    s.code_pos += 1;
    let val = *(s.stack(13) as &bool);
    let width = *(s.stack(12) as &i32);
    let dir = *(s.stack(8) as &i32);
    let token = *(s.stack(4) as &i32);
    let res = external::format_text(if val { "true" } else { "false" }, width, dir, token);
    s.strings.push(res);
    *s.stack_mut(ref_var + 4) = TextType::String as u8;
    *s.stack_mut(ref_var) = s.strings.len();
    s.stack_pos += 13;
}

fn parse_bool(s: &mut State) {
    let ref_var = *(s.code() as &u8);
    s.code_pos += 1;
    *s.stack_mut(0) = if s.get_str(ref_var) == "true" {
        1u8
    } else {
        0u8
    };
    s.stack_pos += 1;
}

fn not(s: &mut State) {
    *s.stack_mut(1) = !*(s.stack(1) as &bool);
}

fn const_byte(s: &mut State) {
    *s.stack_mut(1) = *(s.code() as &i8) as i32;
    s.code_pos += 1;
    s.stack_pos += 4;
}

fn const_int(s: &mut State) {
    *s.stack_mut(1) = *(s.code() as &i32);
    s.code_pos += 4;
    s.stack_pos += 4;
}

fn var_int(s: &mut State) {
    *s.stack_mut(0) = *(s.stack(*(s.code() as &u8)) as &i32);
    s.code_pos += 1;
    s.stack_pos += 4;
}

fn put_int(s: &mut State) {
    *s.stack_mut(*(s.code() as &u8)) = *(s.stack(4) as &i32);
    s.code_pos += 1;
    s.stack_pos -= 4;
}

fn add_int(s: &mut State) {
    let val = external::op_add_int(*(s.stack(4) as &i32), *(s.stack(8) as &i32));
    *s.stack_mut(8) = val;
    s.stack_pos -= 4;
}

fn neg_int(s: &mut State) {
    let val = *(s.stack(4) as &i32);
    *s.stack_mut(8) = if val == i32::MIN { val } else { -val };
}

fn min_int(s: &mut State) {
    let val = external::op_min_int(*(s.stack(4) as &i32), *(s.stack(8) as &i32));
    *s.stack_mut(8) = val;
    s.stack_pos -= 4;
}

fn mul_int(s: &mut State) {
    let val = external::op_mul_int(*(s.stack(4) as &i32), *(s.stack(8) as &i32));
    *s.stack_mut(8) = val;
    s.stack_pos -= 4;
}

fn div_int(s: &mut State) {
    let val = external::op_div_int(*(s.stack(4) as &i32), *(s.stack(8) as &i32));
    *s.stack_mut(8) = val;
    s.stack_pos -= 4;
}

fn eq_int(s: &mut State) {
    let val = *(s.stack(4) as &i32) == *(s.stack(8) as &i32);
    *s.stack_mut(8) = val;
    s.stack_pos -= 7;
}

fn abs_int(s: &mut State) {
    let val = *(s.stack(4) as &i32);
    *s.stack_mut(4) = if val == i32::MIN { val } else { val.abs() };
}

fn format_int(s: &mut State) {
    let val = *(s.stack(18) as &i32);
    let width = *(s.stack(14) as &i32);
    let dir = *(s.stack(10) as &i32);
    let token = *(s.stack(6) as &i32);
    let plus = *(s.stack(2) as &bool);
    let note = *(s.stack(1) as &bool);
    let res = external::format_int(val, width, dir, token, plus, note);
    s.strings.push(res);
    *s.stack_mut(18) = TextType::String as u8;
    *s.stack_mut(17) = s.strings.len();
    s.stack_pos -= 4;
}

fn abs_long(s: &mut State) {
    let val = *(s.stack(4) as &i64);
    *s.stack_mut(4) = val.abs();
}

fn abs_single(s: &mut State) {
    let val = *(s.stack(4) as &f32);
    *s.stack_mut(4) = val.abs();
}

fn abs_float(s: &mut State) {
    let val = *(s.stack(4) as &f64);
    *s.stack_mut(4) = val.abs();
}

type Operations = [(&'static str, fn(&mut State)); 23];

static OPS: Operations = [
    ("call", call),
    ("return", return_call),
    ("var_bool", var_bool),
    ("put_bool", put_bool),
    ("get_bool", get_bool),
    ("set_bool", set_bool),
    ("format_bool", format_bool),
    ("parse_bool", parse_bool),
    ("not", not),
    ("const_byte", const_byte),
    ("const_int", const_int),
    ("var_int", var_int),
    ("put_int", put_int),
    ("add_int", add_int),
    ("neg_int", neg_int),
    ("min_int", min_int),
    ("mul_int", mul_int),
    ("div_int", div_int),
    ("eq_int", eq_int),
    ("abs_int", abs_int),
    ("abs_long", abs_long),
    ("abs_single", abs_single),
    ("abs_float", abs_float),
];

struct GenState {
    stack_pos: u32,
    stack_max: u32,
    d_nr: u32,
}

impl GenState {
    pub fn new(d_nr: u32) -> GenState {
        GenState {
            stack_pos: 0,
            stack_max: 0,
            d_nr,
        }
    }
    pub fn stack(&mut self, add: i32) {
        assert!(add >= 0 || -add < self.stack_pos as i32);
        self.stack_pos = (self.stack_pos as i32 + add) as u32;
        if self.stack_pos > self.stack_max {
            self.stack_max = self.stack_pos;
        }
    }
}

impl<'a> State<'a> {
    pub fn new(data: &mut Data, known_types: &'a KnownTypes) -> State<'a> {
        let mut res = State {
            bytecode: Vec::new(),
            strings: Vec::new(),
            text_code: "truefalsenull".as_bytes().to_vec(),
            stacks: Vec::new(),
            stack_cur: 0,
            stack_pos: 0,
            code_pos: 0,
            database: Stores::new(known_types),
            operations: Vec::new(),
            op_names: HashMap::new(),
        };
        let l = Layout::from_size_align(STACK_BLOCK as usize, 8).expect("Problem");
        let stack = unsafe { A.alloc(l) };
        res.stacks.push(stack);
        for (op_name, op_fn) in OPS {
            res.operations.push(op_fn);
            res.op_names
                .insert(op_name.to_string(), (res.operations.len() - 1) as u8);
        }
        for def_nr in 0..data.definitions() {
            if matches!(data.def_type(def_nr), DefType::Function(_)) {
                data.definitions[def_nr as usize].code_position = res.code_pos;
                res.byte_code(def_nr, data);
                data.definitions[def_nr as usize].code_length =
                    res.code_pos - data.def(def_nr).code_position;
            }
        }
        res
    }

    fn get_str(&self, ref_var: u8) -> &str {
        let text = TextType::try_from(*(self.stack(ref_var + 15) as &u8)).unwrap();
        match text {
            TextType::Null => "null",
            TextType::Constant => {
                let code = *self.stack(ref_var + 13);
                let len = *self.stack(ref_var + 9);
                self.code_str(code, len)
            }
            TextType::String => {
                let str: u16 = *self.stack(ref_var + 13);
                &self.strings[str as usize]
            }
            TextType::Slice => {
                let str: u16 = *self.stack(ref_var + 13);
                let pos = *(self.stack(ref_var + 9) as &u32);
                let len = *(self.stack(ref_var + 5) as &u32);
                &self.strings[str as usize][pos as usize..pos as usize + len as usize]
            }
            TextType::DbRef => {
                // A mutable database string from a field type:u8, db:u8, rec:u32, pos:u32
                panic!("not yet implemented")
            }
            TextType::DbSlice => {
                // A part of a database string type:u8, store:u8, rec:u32, str_pos:u32, len:u32
                panic!("not yet implemented")
            }
        }
    }

    fn new_stack(&mut self) {
        let cur = self.stack_cur;
        let pos = self.stack_pos;
        self.stack_cur += 1;
        if self.stack_cur as usize >= self.stacks.len() {
            let l = Layout::from_size_align(STACK_BLOCK as usize, 8).expect("Problem");
            let stack = unsafe { A.alloc(l) };
            self.stacks.push(stack);
        }
        self.stack_pos = 4;
        *(self.stack_mut(2) as &mut u16) = cur;
        *(self.stack_mut(0) as &mut u16) = pos;
    }

    fn stack<T>(&self, pos: u8) -> &T {
        unsafe {
            let off = self.stacks[self.stack_cur as usize]
                .offset(self.stack_pos as isize - pos as isize) as *const T;
            off.as_ref().expect("Stack")
        }
    }

    fn stack_mut<T>(&mut self, pos: u8) -> &mut T {
        unsafe {
            let off = self.stacks[self.stack_cur as usize]
                .offset(self.stack_pos as isize - pos as isize) as *mut T;
            off.as_mut().expect("Reference")
        }
    }

    fn stack_copy(&mut self, from: u16, size: u8, stack: u16, pos: u16) {
        unsafe {
            let from_ptr = self.stacks[self.stack_cur as usize].offset(from as isize);
            let to_ptr = self.stacks[stack as usize].offset(pos as isize);
            std::ptr::copy_nonoverlapping(from_ptr, to_ptr, size as usize);
        }
    }

    fn code<T>(&self) -> &T {
        unsafe {
            let off = self.bytecode.as_ptr().offset(self.code_pos as isize) as *const T;
            off.as_ref().expect("code")
        }
    }

    fn code_str(&self, code: u32, len: u32) -> &str {
        unsafe {
            let off = self.bytecode.as_ptr().offset(code as isize);
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(off, len as usize))
        }
    }

    pub fn code_mut<T>(&mut self) -> &mut T {
        if self.code_pos as usize + size_of::<T>() > self.bytecode.len() {
            self.bytecode
                .resize(self.code_pos as usize + size_of::<T>(), 0);
        }
        unsafe {
            let off = self.bytecode.as_ptr().offset(self.code_pos as isize) as *mut T;
            self.code_pos += size_of::<T>() as u32;
            off.as_mut().expect("code")
        }
    }

    pub fn byte_code(&mut self, d_nr: u32, data: &Data) {
        let mut s = GenState::new(d_nr);
        self.generate(&data.def(d_nr).code, data, &mut s);
    }

    fn generate(&mut self, val: &Value, data: &Data, state: &mut GenState) {
        match val {
            Value::Int(nr) => {
                if *nr < 128 && *nr >= -128 {
                    *self.code_mut() = *self.op_names.get("const_byte").unwrap();
                    state.stack(4);
                } else {
                    *self.code_mut() = *self.op_names.get("const_int").unwrap();
                    state.stack(4);
                }
            }
            Value::Var(_var) => {}
            Value::Let(_var, _val) => {
                // nothing yet
            }
            Value::Set(_var, _val) => {
                // nothing yet
            }
            Value::If(test, t_val, f_val) => {
                self.generate(test, data, state);
                *self.code_mut() = *self.op_names.get("goto_false").unwrap();
                state.stack(-1);
                let start = self.code_pos;
                *self.code_mut() = 0u8;
                self.generate(t_val, data, state);
                *self.code_mut() = *self.op_names.get("goto").unwrap();
                let t_pos = self.code_pos;
                *self.code_mut() = 0u8;
                self.generate(f_val, data, state);
                let f_pos = self.code_pos;
                self.code_pos = start;
                *self.code_mut() = t_pos - start;
                self.code_pos = t_pos;
                *self.code_mut() = f_pos - t_pos;
                self.code_pos = f_pos;
            }
            Value::Return(val) => {
                if **val != Value::Null {
                    self.generate(val, data, state);
                }
                *self.code_mut() = *self.op_names.get("return").unwrap();
            }
            Value::Null => {
                // nothing here, we might want to convert some values to the correct types
            }
            Value::Block(values) => {
                for v in values {
                    self.generate(v, data, state);
                }
            }
            Value::Call(op, parameters) => {
                let op_name: &str = &data.def(*op).op;
                if !op_name.is_empty() {
                    if let Some(op) = self.op_names.get(op_name) {
                        *self.code_mut() = *op;
                    } else {
                        panic!(
                            "Unknown operation {op_name} on {}",
                            data.def(state.d_nr).name
                        );
                    }
                    match op_name {
                        "format_bool" => state.stack(-13),
                        "parse_bool" => {
                            *self.code_mut() = (state.stack_pos - 5) as u8;
                            state.stack(1)
                        }
                        _ => {}
                    }
                    for p in parameters {
                        self.generate(p, data, state);
                    }
                } else {
                    state.stack(4);
                    for p in parameters {
                        self.generate(p, data, state);
                    }
                    *self.code_mut() = *self.op_names.get("call").unwrap();
                    *self.code_mut() = data.def(*op).size as u8;
                    *self.code_mut() = data.def(*op).code_position;
                }
            }
            _ => panic!("Unknown value {:?}", val),
        }
    }

    pub fn dump_code(&self, f: &mut std::fs::File, d_nr: u32, data: &Data) {
        writeln!(f, "fn {}", data.def(d_nr).name).unwrap();
        let pos = data.def(d_nr).code_position;
        for p in pos..pos + data.def(d_nr).code_length {
            writeln!(f, "{p}:{}", self.bytecode[p as usize]).unwrap();
        }
    }

    pub fn run() {}
}

// ref(9): DbRef(store_nr:u8,rec:u32,pos:u32)
// text(14): type:byte (null, Code(pos:u32,len:u8), String(pos:u16), slice(pos:u16,start:u32,len:u32), DbStr(store_nr:u8,rec:u32), DbSlice(store_nr:u8,rec:u32,pos:u32,len:u32)
// vector(14): type:byte (DbRec(store_nr:u8,rec:u32,fld:u32), DbSlice(store_nr:u8,rec:u32,pos:u32,len:u32), Iter(code:u32))
// fn(5+x): stack:u8,return:u32,parameters:x
// int_iter():
// str_iter():
// vec_iter():

#[test]
fn test_alignment() {
    let types = KnownTypes::new();
    let mut s = State::new(&mut Data::new(), &types);
    *s.code_mut() = 11u8;
    *s.code_mut() = 0x11223344u32;
    *s.code_mut() = 22u8;
    *s.code_mut() = 33u8;
    *s.code_mut() = 44u8;
    s.code_pos = 1;
    assert_eq!(0x11223344u32, *(s.code() as &u32));
    s.code_pos = 2;
    assert_eq!(0x33, *(s.code() as &u8));
}
