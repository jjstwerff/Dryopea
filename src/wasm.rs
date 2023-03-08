// Copyright (c) 2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
extern crate strum;
extern crate strum_macros;

use crate::data::{Data, Type, Value};
use std::collections::HashMap;

#[allow(dead_code)]
enum Operator {
    Unreachable = 0x00,
    Nop = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    BrTable = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    CallIndirect = 0x11,
    Drop = 0x1A,
    Select = 0x1B,
    SelectT = 0x1C,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    TableGet = 0x25,
    TableSet = 0x26,
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2A,
    F64Load = 0x2B,
    I32Load8S = 0x2C,
    I32Load8U = 0x2D,
    I32Load16S = 0x2E,
    I32Load16U = 0x2F,
    I64Load8S = 0x30,
    I64Load8U = 0x31,
    I64Load16S = 0x32,
    I64Load16U = 0x33,
    I64Load32S = 0x34,
    I64Load32U = 0x35,
    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3A,
    I32Store16 = 0x3B,
    I64Store8 = 0x3C,
    I64Store16 = 0x3D,
    I64Store32 = 0x3E,
    MemorySize = 0x3F,
    MemoryGrow = 0x40,
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4A,
    I32GtU = 0x4B,
    I32LeS = 0x4C,
    I32LeU = 0x4D,
    I32GeS = 0x4E,
    I32GeU = 0x4F,
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5A,
    F32Eq = 0x5B,
    F32Ne = 0x5C,
    F32Lt = 0x5D,
    F32Gt = 0x5E,
    F32Le = 0x5F,
    F32Ge = 0x60,
    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32PopCnt = 0x69,
    I32Add = 0x6A,
    I32Sub = 0x6B,
    I32Mul = 0x6C,
    I32DivS = 0x6D,
    I32DivU = 0x6E,
    I32RemS = 0x6F,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I32RotL = 0x77,
    I32RotR = 0x78,
    I64Clz = 0x79,
    I64Ctz = 0x7A,
    I64PopCnt = 0x7B,
    I64Add = 0x7C,
    I64Sub = 0x7D,
    I64Mul = 0x7E,
    I64DivS = 0x7F,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,
    I64Shl = 0x86,
    I64ShrS = 0x87,
    I64ShrU = 0x88,
    I64RotL = 0x89,
    I64RotR = 0x8A,
    F32Abs = 0x8B,
    F32Neg = 0x8C,
    F32Ceil = 0x8D,
    F32Floor = 0x8E,
    F32Trunc = 0x8F,
    F32Nearest = 0x90,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F32Min = 0x96,
    F32Max = 0x97,
    F32Copysign = 0x98,
    F64Abs = 0x99,
    F64Neg = 0x9A,
    F64Ceil = 0x9B,
    F64Floor = 0x9C,
    F64Trunc = 0x9D,
    F64Nearest = 0x9E,
    F64Sqrt = 0x9F,
    F64Add = 0xA0,
    F64Sub = 0xA1,
    F64Mul = 0xA2,
    F64Div = 0xA3,
    F64Min = 0xA4,
    F64Max = 0xA5,
    F64Copysign = 0xA6,
    I32WrapI64 = 0xA7,
    I32TruncF32S = 0xA8,
    I32TruncF32U = 0xA9,
    I32TrunkF64S = 0xAA,
    I32TrunkF64U = 0xAB,
    I64ExtendI32S = 0xAC,
    I64ExtendI32U = 0xAD,
    I64TruncF32S = 0xAE,
    I64TruncF32U = 0xAF,
    I64TrunkF64S = 0xB0,
    I64TrunkF64U = 0xB1,
    F32ConvertI32S = 0xB2,
    F32ConvertI32U = 0xB3,
    F32ConvertI64S = 0xB4,
    F32ConvertI64U = 0xB5,
    F32DemoteF64 = 0xB6,
    F64ConvertI32S = 0xB7,
    F64ConvertI32U = 0xB8,
    F64ConvertI64S = 0xB9,
    F64ConvertI64U = 0xBA,
    F64PromoteF32 = 0xBB,
    I32ReinterpretF32 = 0xBC,
    I64ReinterpretF64 = 0xBD,
    F32ReinterpretI32 = 0xBE,
    F64ReinterpretI64 = 0xBF,
    I32Extend8S = 0xC0,
    I32Extend16S = 0xC1,
    I64Extend8S = 0xC2,
    I64Extend16S = 0xC3,
    I64Extend32S = 0xC4,
    RefNull = 0xD0,
    RefIsNull = 0xD1,
    RefFunc = 0xD2,
    MemoryInit = 0xFC08,
    MemoryCopy = 0xFC0A,
    MemoryFill = 0xFC0B,
}

#[allow(dead_code)]
enum TypeCode {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
    FuncRef = 0x70,
    ExternRef = 0x6F,
    FuncType = 0x60,
    ResultType = 0x40,
}

#[allow(dead_code)]
pub struct Sections {
    // Each current section on id to (position, size).
    current: HashMap<u8, (u32, u32)>,
    old: Vec<u8>,
    // Type definition to number, to deduplicate them.
    type_hash: HashMap<Vec<u8>, u32>,
    types: Vec<Vec<u8>>,
    // Maximal found type
    max_type: u32,
    // Known functions with type and exported name
    functions: Vec<(u32, String)>,
    // Number of parameters (inside variable structure)
    parameters: u32,
    // Function variables, including parameters
    variables: Vec<u8>,
    // Function result
    result: u8,
    // Current function body
    body: Vec<u8>,
    // Total wasm data block
    data: Vec<u8>,
}

impl Sections {
    fn new() -> Sections {
        Sections {
            current: HashMap::new(),
            old: Vec::new(),
            type_hash: HashMap::new(),
            types: Vec::new(),
            max_type: 0,
            functions: Vec::new(),
            parameters: 0,
            variables: Vec::new(),
            result: 0,
            body: Vec::new(),
            data: Vec::new(),
        }
    }

    pub fn write(&mut self, data: &Data) {
        self.data
            .extend([0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]);
        let mut v = Vec::new();
        set_u32(&mut v, self.types.len() as u32);
        for t in &self.types {
            v.push(TypeCode::FuncType as u8);
            set_u32(&mut v, t.len() as u32 - 1);
            for (tnr, tp) in t.iter().enumerate() {
                if tnr == t.len() {
                    break;
                }
                v.push(*tp);
            }
            if t[t.len() - 1] != TypeCode::ResultType as u8 {
                v.push(1);
                v.push(t[t.len() - 1]);
            } else {
                v.push(0);
            }
        }
        self.data.push(1);
        set_u32(&mut self.data, v.len() as u32);
        self.data.append(&mut v);
        // Import section
        self.current(2);
        // Function section, the type per function
        self.data.push(3);
        set_u32(&mut v, self.functions.len() as u32);
        for (type_idx, _) in &self.functions {
            set_u32(&mut v, *type_idx);
        }
        set_u32(&mut self.data, v.len() as u32);
        self.data.append(&mut v);
        // table, lookup table of functions, ignored for now
        self.current(4);
        // memory, limits, ignored for now
        self.current(5);
        // global, ignored for now, maybe the global $0 is needed in the future as temporary memory
        self.current(6);
        // export, pub functions and memory allocation
        /*
            let exports = get_u32(&s.old, &mut sec);
            for _ in 0..exports {
                let name = get_str(&s.old, &mut sec);
                let tp = s.old[sec];
                sec += 1;
                if tp == 0 {
                    let function_idx = get_u32(&s.old, &mut sec) as usize;
                    s.functions[function_idx].1 = name;
                } else if tp == 2 {
                    // ignore memory index
                    get_u32(&s.old, &mut sec);
                }
            }
        */
        // start function, add old data
        self.current(8);
        // elements for the table, add old data
        self.current(9);
        // data count section
        self.current(12);
        // code, ignored for now, all current code & add own
        // current 10
        // data segments: for memory, possibly with static code, ignored for now
        self.current(11);
    }

    fn current(&mut self, section: u8) {
        let Some((pos, size)) = self.current.get(&section) else {
            panic!("Unknown section {section}")
        };
        self.data.push(section);
        set_u32(&mut self.data, *size);
        self.data
            .extend(&self.old[*pos as usize..*pos as usize + *size as usize]);
    }

    fn op(&mut self, op: Operator) {
        let vl = op as u32;
        if vl <= 255 || vl > 0xFF00 {
            self.body.push(vl as u8);
        } else {
            self.body.push((vl >> 8) as u8);
            self.body.push(vl as u8);
        }
    }

    fn tp(&mut self, tp: TypeCode) {
        self.body.push(tp as u8);
    }

    fn convert(&mut self, val: i64) {
        let mut more = true;
        let mut value = val;
        while more {
            let vl = (value & 127) as u8;
            value >>= 7;
            let flag = val & 64 > 0;
            if (flag && value == -1) || (!flag && value == 0) {
                more = false;
            }
            self.body.push(vl + if more { 128 } else { 0 });
        }
    }

    pub fn const_int(&mut self, val: i32) {
        self.op(Operator::I32Const);
        self.convert(val as i64);
    }

    pub fn const_long(&mut self, val: i64) {
        self.op(Operator::I64Const);
        self.convert(val as i64);
    }

    pub fn const_float(&mut self, val: f64) {
        self.op(Operator::F64Const);
        for b in val.to_le_bytes() {
            self.body.push(b);
        }
    }

    pub fn const_single(&mut self, val: f32) {
        self.op(Operator::F32Const);
        for b in val.to_le_bytes() {
            self.body.push(b);
        }
    }

    pub fn create(&mut self, val: &Value, tp: Type) {
        match val {
            Value::Null => match tp {
                Type::Long => self.const_long(i64::MIN),
                Type::Float => self.const_float(f64::NAN),
                Type::Single => self.const_single(f32::NAN),
                Type::Boolean
                | Type::Text
                | Type::Enum(_)
                | Type::Reference(_)
                | Type::Inner(_)
                | Type::Vector(_)
                | Type::Routine(_)
                | Type::Iterator(_)
                | Type::Sorted(_, _)
                | Type::Index(_, _)
                | Type::Link
                | Type::Radix(_, _)
                | Type::Hash(_, _) => self.const_int(0),
                _ => self.const_int(i32::MIN),
            },
            Value::Int(v) => self.const_int(*v),
            Value::Reference(_, _, _) => {}
            Value::Range(_, _) => {}
            Value::Float(v) => self.const_float(*v),
            Value::Long(v) => self.const_long(*v),
            Value::Single(v) => self.const_single(*v),
            Value::Text(_) => panic!("Constant texts should be Data"),
            Value::Data(_, _) => panic!("Data outside append context"),
            Value::Call(_, _) => {
                // Rewrite Value::Data to separate call parameters
            }
            Value::Block(code) => {
                for v in code {
                    self.create(v, Type::Void)
                }
            }
            Value::Var(_) => {}
            Value::Set(_, _) => {}
            Value::Return(v) => {
                if **v == Value::Null {
                    self.create(v, Type::Null);
                }
                self.op(Operator::Return);
            }
            Value::Break(nr) => {
                self.op(Operator::Br);
                self.const_int(*nr as i32 * 2 + 1);
            }
            Value::Continue(nr) => {
                self.op(Operator::Br);
                self.const_int(*nr as i32 * 2);
            }
            Value::If(expr, true_code, false_code) => {
                // calculate expr and leave i32 on stack 0=false
                self.create(expr, Type::Boolean);
                self.op(Operator::If);
                // Void result, change to other when different
                self.tp(TypeCode::ResultType);
                self.create(true_code, Type::Void);
                self.op(Operator::Else);
                self.create(false_code, Type::Void);
                self.op(Operator::End);
            }
            Value::Loop(code) => {
                self.op(Operator::Block);
                self.tp(TypeCode::ResultType);
                self.op(Operator::Loop);
                self.tp(TypeCode::ResultType);
                for v in code {
                    self.create(v, Type::Void)
                }
                self.op(Operator::End);
                self.op(Operator::End);
            }
        }
    }
}

pub fn parse_wasm() -> Sections {
    let filename = "webassembly/pkg/scriptlib_bg.wasm";
    let mut s = Sections::new();
    let Ok(data) = std::fs::read(filename) else {
        panic!("Could not read {filename}")
    };
    let mut pos = 0;
    s.old = data;
    parse_elements(&mut s, &mut pos);
    if pos != s.old.len() {
        panic!("Syntax error on {filename}");
    };
    s
}

fn get_int(data: &[u8], pos: usize) -> u32 {
    data[pos] as u32
        + ((data[pos + 1] as u32) << 8)
        + ((data[pos + 2] as u32) << 16)
        + ((data[pos + 3] as u32) << 24)
}

fn set_u32(data: &mut Vec<u8>, val: u32) {
    let mut value = val;
    loop {
        let byte = value & 127;
        value >>= 7;
        data.push(byte as u8 + if value != 0 { 128 } else { 0 });
        if value == 0 {
            return;
        }
    }
}

fn get_u32(data: &[u8], pos: &mut usize) -> u32 {
    let mut result = 0;
    let mut shift = 0;
    loop {
        let byte = data[*pos] as u32;
        *pos += 1;
        result |= (byte & 127) << shift;
        shift += 7;
        if byte < 128 {
            return result;
        }
    }
}

fn set_str(data: &mut Vec<u8>, val: &str) {
    set_u32(data, val.len() as u32);
    for ch in val.as_bytes() {
        data.push(*ch);
    }
}

fn get_str(data: &[u8], pos: &mut usize) -> String {
    let len = get_u32(data, pos) as usize;
    let mut res = "".to_string();
    unsafe {
        res += std::str::from_utf8_unchecked(&data[*pos..*pos + len]);
    }
    *pos += len;
    res
}

fn get_types(data: &[u8], pos: &mut usize) -> Vec<u8> {
    let len = get_u32(data, pos) as usize;
    let res = &data[*pos..*pos + len];
    *pos += len;
    res.to_vec()
}

fn parse_elements(s: &mut Sections, pos: &mut usize) {
    let magic = get_int(&s.old, *pos);
    if magic != 0x6d736100 {
        println!("found magic {:x}", magic);
        return;
    }
    *pos += 4;
    if get_int(&s.old, *pos) != 1 {
        return;
    }
    *pos += 4;
    loop {
        let section = s.old[*pos];
        *pos += 1;
        let size = get_u32(&s.old, pos);
        s.current.insert(section, (*pos as u32, size));
        let mut sec = *pos;
        match section {
            // skip custom sections
            0 => {
                get_str(&s.old, &mut sec);
                //println!("custom {} size {size}", get_str(data, &mut sec));
            }
            // types
            1 => {
                let types = get_u32(&s.old, &mut sec);
                for tp in 0..types {
                    if s.old[sec] != TypeCode::FuncType as u8 {
                        panic!("Expected a function type");
                    }
                    sec += 1;
                    let mut parameters = get_types(&s.old, &mut sec);
                    let result = get_types(&s.old, &mut sec);
                    if result.is_empty() {
                        parameters.push(TypeCode::ResultType as u8);
                    } else {
                        parameters.push(result[0]);
                    }
                    s.types.push(parameters.clone());
                    s.type_hash.insert(parameters, tp);
                }
            }
            // import
            2 => {
                let imports = get_u32(&s.old, &mut sec);
                for _ in 0..imports {
                    let _module = get_str(&s.old, &mut sec);
                    let _name = get_str(&s.old, &mut sec);
                    let tp = s.old[sec];
                    sec += 1;
                    // function
                    let _type_index = if tp == 0 {
                        get_u32(&s.old, &mut sec)
                    } else {
                        panic!("Unknown import {}", tp)
                    };
                }
            }
            // function, remember the type per function
            3 => {
                let functions = get_u32(&s.old, &mut sec);
                for _ in 0..functions {
                    let type_idx = get_u32(&s.old, &mut sec);
                    s.functions.push((type_idx, "".to_string()));
                }
            }
            // table, lookup table of functions, ignored for now
            4 => {}
            // memory, limits, ignored for now
            5 => {}
            // global, ignored for now, maybe the global $0 is needed in the future as temporary memory
            6 => {}
            // export, we remember the names for debugging purpose
            7 => {
                let exports = get_u32(&s.old, &mut sec);
                for _ in 0..exports {
                    let name = get_str(&s.old, &mut sec);
                    let tp = s.old[sec];
                    sec += 1;
                    if tp == 0 {
                        let function_idx = get_u32(&s.old, &mut sec) as usize;
                        s.functions[function_idx].1 = name;
                    } else if tp == 2 {
                        // ignore memory index
                        get_u32(&s.old, &mut sec);
                    } else {
                        panic!("Unknown export {tp} at {sec}");
                    }
                }
                //println!("functions {:?}", s.functions);
            }
            // start function, ignored for now, possibly interesting for store setup.
            8 => {
                panic!("Start function at {sec}");
            }
            // elements for the table, ignored for now
            9 => {}
            // code, ignored for now, we will add to this section on writing
            10 => {
                // vector of {size:u32 locals:vec[(nr, tp)], expr}
                // validate the use of nr
            }
            // data segments: for memory, possibly with static code, ignored for now
            11 => {
                let segments = get_u32(&s.old, &mut sec);
                for _ in 0..segments {
                    let tp = s.old[sec];
                    sec += 1;
                    let mut expr = Vec::new();
                    let mut block: Vec<u8> = Vec::new();
                    match tp {
                        0 => {
                            loop {
                                expr.push(s.old[sec]);
                                sec += 1;
                                if s.old[sec - 1] == Operator::End as u8 {
                                    break;
                                }
                            }
                            block.extend(&get_types(&s.old, &mut sec));
                        }
                        1 => {
                            let elms = get_u32(&s.old, &mut sec);
                            sec += elms as usize;
                        }
                        2 => {
                            get_u32(&s.old, &mut sec); // ignored pos
                            loop {
                                expr.push(s.old[sec]);
                                sec += 1;
                                if s.old[sec - 1] == Operator::End as u8 {
                                    break;
                                }
                            }
                            block.extend(&get_types(&s.old, &mut sec));
                        }
                        _ => {
                            panic!("Unknown data segment {tp}");
                        }
                    }
                    // The current static code claims a memory size of 1MB and fills the start with static data
                    // println!("Data pos {pos} expr {expr:?} block {block:?}");
                }
            }
            _ => {
                panic!("Unknown section {} at {pos}", section);
            }
        }
        // skip the found section
        *pos += size as usize;
        if *pos == s.old.len() {
            break;
        }
    }
}
