// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Parse scripts and create internal code from it.
//! Including type checking.

use crate::data::*;
use crate::diagnostics::*;
use crate::lexer::{LexItem, LexResult, Lexer, Mode};
use crate::typedef;
use crate::types::Types;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs::{metadata, read_dir, File};
use std::io::prelude::*;
use std::io::BufReader;
use std::string::ToString;
use typedef::complete_definition;

pub struct Parser<'a> {
    /// All definitions
    pub data: Data,
    /// The lexer on the current text file
    pub lexer: Lexer,
    types: Types<'a>,
    /// Current stack position used by variables
    var_nr: u32,
    /// Are we currently allowing break/continue statements
    in_loop: bool,
    /// The current file number that is being parsed
    file: u32,
    /// Subtype names in the current function definition
    sub_names: HashMap<String, u32>,
    pub diagnostics: Diagnostics,
    default: bool,
    /// The definition that is currently parsed (function or struct)
    context: u32,
    /// Is this the first pass on parsing:
    /// - do not assume that all struct / enum types are parsed
    first_pass: bool,
}

// Operators ordered on their precedence
static OPERATORS: &[&[&str]] = &[
    &[".."],
    &["||", "or"],
    &["&&", "and"],
    &["==", "!=", "<", "<=", ">", ">="],
    &["|"],
    &["^"],
    &["&"],
    &["<<", ">>"],
    &["-", "+"],
    &["*", "/", "%"],
    &["as"],
];

static SKIP_TOKEN: [&str; 8] = ["}", ".", "<", ">", "^", "+", "-", "#"];
static SKIP_WIDTH: [&str; 7] = ["}", ".", "x", "X", "o", "b", "e"];

struct OutputState<'a> {
    radix: i32,
    width: Value,
    token: &'a str,
    plus: bool,
    note: bool,
    dir: i32,
    float: bool,
}

const OUTPUT_DEFAULT: OutputState = OutputState {
    radix: 10,
    width: Value::Int(0),
    token: " ",
    plus: false,
    note: false,
    dir: -1,
    float: false,
};

impl<'a> Default for Parser<'a> {
    fn default() -> Self {
        Self::new()
    }
}

fn is_op(name: &str) -> bool {
    name.len() >= 3 && name.starts_with("Op") && name.chars().nth(2).unwrap().is_uppercase()
}

/// Validate function, attribute, value and field names
fn is_lower(name: &str) -> bool {
    for c in name.chars() {
        if c.is_uppercase() {
            return false;
        }
    }
    true
}

#[allow(dead_code)]
/// Used to validate constants names
fn is_upper(name: &str) -> bool {
    for c in name.chars() {
        if c.is_lowercase() {
            return false;
        }
    }
    true
}

/// Validate type, enum, enum values and struct names
fn is_camel(name: &str) -> bool {
    let c = name.chars().next().unwrap();
    if c.is_lowercase() {
        return false;
    }
    for c in name.chars() {
        if c == '_' {
            return false;
        }
    }
    true
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser {
            data: Data::new(),
            lexer: Lexer::default(),
            types: Types::new(),
            var_nr: 1,
            in_loop: false,
            file: 1,
            sub_names: HashMap::new(),
            diagnostics: Diagnostics::new(),
            default: false,
            context: u32::MAX,
            first_pass: true,
        }
    }

    /// Parse the content of a given file.
    /// default: parsing system definitions
    pub fn parse(&mut self, filename: &str, default: bool) -> bool {
        self.default = default;
        let fp = match File::open(filename) {
            Ok(f) => f,
            Err(err) => {
                self.diagnostics.add(
                    Level::Fatal,
                    &format!("Unknown file:{filename} with:{err:?}"),
                );
                return false;
            }
        };
        self.lexer = Lexer::lines(BufReader::new(fp).lines(), filename);
        self.first_pass = true;
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl != Level::Error && lvl != Level::Fatal {
            self.lexer = Lexer::lines(
                BufReader::new(File::open(filename).unwrap()).lines(),
                filename,
            );
            self.parse_file();
        }
        self.diagnostics.fill(self.lexer.diagnostics());
        self.diagnostics.is_empty()
    }

    /// Parse all .gcp files found in a directory tree in alphabetical ordering.
    pub fn parse_dir(&mut self, dir: &str, default: bool) -> std::io::Result<()> {
        let paths = read_dir(dir)?;
        let mut files: BTreeSet<String> = BTreeSet::new();
        for path in paths {
            let file_name = path?.path().to_string_lossy().to_string();
            let data = metadata(&file_name)?;
            if file_name.ends_with(".gcp") || data.is_dir() {
                files.insert(file_name);
            }
        }
        for f in files {
            let data = metadata(&f)?;
            if data.is_dir() {
                self.parse_dir(&f, default)?
            } else if !self.parse(&f, default) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("{}", self.diagnostics),
                ));
            }
        }
        self.types.validate(&mut self.lexer);
        Ok(())
    }

    /// Only parse a specific string, only useful for parser tests.
    #[allow(dead_code)]
    pub fn parse_str(&mut self, text: &str, filename: &str) {
        self.first_pass = true;
        self.default = false;
        self.lexer = Lexer::from_str(text, filename);
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl == Level::Error || lvl == Level::Fatal {
            self.diagnostics.fill(self.lexer.diagnostics());
            return;
        }
        self.lexer = Lexer::from_str(text, filename);
        self.parse_file();
        self.diagnostics.fill(self.lexer.diagnostics());
    }

    // ********************
    // * Helper functions *
    // ********************

    /// Get an iterator.
    /// The iterable expression is in *code.
    /// Creating the iterator will be in *code afterward.
    /// Return the next expression; with Value::None the iterator creations was impossible.
    fn iterator(&mut self, name: &str, code: &mut Value, is_type: &Type, should: &Type) -> Value {
        if *is_type == Type::Text {
            let iter_var =
                self.types
                    .create_var(format!("{name}#index"), Type::Integer, self.lexer.pos());
            self.var_nr += 1;
            let res_var =
                self.types
                    .create_var(format!("res_{}", self.var_nr), Type::Text, self.lexer.pos());
            self.var_nr += 1;
            let i = Value::Var(iter_var);
            let ref_expr = self.cl(
                "OpGetTextSub",
                &[code.clone(), i.clone(), Value::Int(i32::MIN)],
            );
            let res_len = self.cl("OpLengthText", &[Value::Var(res_var)]);
            let next = vec![
                v_set(res_var, ref_expr),
                v_set(iter_var, self.op("+", i.clone(), res_len, Type::Integer)),
                Value::Var(res_var),
            ];
            let len = self.cl("OpLengthText", &[code.clone()]);
            *code = v_set(iter_var, Value::Int(0));
            return v_if(
                self.op(">=", i, len, Type::Integer),
                Value::Null,
                Value::Block(next),
            );
        }
        if is_type == should {
            // there was already an iterator.
            let orig = code.clone();
            *code = Value::Null; // there is no iterator to create, we got it already
            return orig;
        }
        if let Type::Iterator(_) = should {
            if let Type::Vector(tp) = is_type {
                let iter_var =
                    self.types
                        .create_var(format!("{name}#index"), Type::Integer, self.lexer.pos());
                self.var_nr += 1;
                let res_var = self.types.create_var(
                    format!("res_{}", self.var_nr),
                    *tp.clone(),
                    self.lexer.pos(),
                );
                self.var_nr += 1;
                let i = Value::Var(iter_var);
                let vec_tp = self.data.type_def_nr(tp);
                let mut ref_expr = self.cl(
                    "OpGetVector",
                    &[
                        code.clone(),
                        Value::Int(self.data.def_size(vec_tp) as i32),
                        i.clone(),
                    ],
                );
                if let Type::Reference(_) = *tp.clone() {
                } else {
                    ref_expr = self.get_field(vec_tp, u16::MAX, ref_expr);
                }
                let next = vec![
                    v_set(res_var, ref_expr),
                    v_set(
                        iter_var,
                        self.op("+", i.clone(), Value::Int(1), Type::Integer),
                    ),
                    Value::Var(res_var),
                ];
                let len = self.cl("OpLengthVector", &[code.clone()]);
                *code = v_set(iter_var, Value::Int(0));
                return v_if(
                    self.op(">=", i, len, Type::Integer),
                    Value::Null,
                    Value::Block(next),
                );
            } else if let Type::Sorted(d_nr, _) = is_type {
                let tp = Type::Vector(Box::new(self.data.returned(*d_nr)));
                let mut iter_expr = Value::Null;
                let iter_type = self.call_op(&mut iter_expr, "Iter", vec![code.clone()], vec![tp]);
                let iter_var = self.types.create_var(
                    format!("{name}#index"),
                    iter_type.clone(),
                    self.lexer.pos(),
                );
                self.var_nr += 1;
                *code = v_set(iter_var, iter_expr);
                let mut next_expr = Value::Null;
                self.call_op(
                    &mut next_expr,
                    "Next",
                    vec![Value::Var(iter_var)],
                    vec![iter_type],
                );
                return next_expr;
            } else if let Type::Iterator(_) = is_type {
                if let Value::Range(minimum, maximum) = code.clone() {
                    let it = Type::Integer;
                    let iter_var = self.types.create_var(
                        format!("{name}#index"),
                        it.clone(),
                        self.lexer.pos(),
                    );
                    self.var_nr += 1;
                    *code = Value::Let(iter_var, minimum);
                    let res_var = self.types.create_var(
                        format!("res_{}", self.var_nr),
                        it.clone(),
                        self.lexer.pos(),
                    );
                    let i = Value::Var(iter_var);
                    let increment = Value::Block(vec![
                        v_set(res_var, i.clone()),
                        v_set(iter_var, self.op("+", i.clone(), Value::Int(1), it.clone())),
                        Value::Var(res_var),
                    ]);
                    return v_if(self.op(">=", i, *maximum, it), Value::Null, increment);
                }
                return Value::Null;
            }
        }
        Value::Null
    }

    /// Convert a type to another type when possible
    /// Returns false when impossible. However, the other way round might still be possible.
    fn convert(&mut self, code: &mut Value, is_type: &Type, should: &Type) -> bool {
        if is_type == should {
            return true;
        }
        let mut check_type = is_type;
        if let Type::Vector(_nr) = is_type {
            if let Type::Vector(v) = should {
                if v.is_unknown() {
                    return true;
                }
            }
        } else if let Type::Reference(_) = is_type {
            if *should == Type::Reference(0) {
                return true;
            }
            check_type = &Type::Reference(0);
        } else if let Type::Enum(_) = is_type {
            if *should == Type::Enum(0) {
                return true;
            }
            check_type = &Type::Enum(0);
        }
        for &dnr in self.types.get_possible("Conv") {
            if self.data.def_name(dnr).ends_with("FromNull") {
                if *is_type == Type::Null {
                    if self.data.returned(dnr) == *should {
                        *code = Value::Null;
                        return true;
                    } else if matches!(self.data.returned(dnr), Type::Reference(_)) {
                        if let Type::Reference(_) = *should {
                            *code = Value::Call(dnr, vec![]);
                            return true;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                } else {
                    continue;
                }
            } else if self.data.attributes(dnr) > 0
                && self.data.attr_type(dnr, 0) == *check_type
                && self.data.returned(dnr) == *should
            {
                *code = Value::Call(dnr, vec![code.clone()]);
                return true;
            }
        }
        false
    }

    /// Cast a type to another type when possible
    /// Returns false when impossible.
    fn cast(&mut self, code: &mut Value, is_type: &Type, should: &Type) -> bool {
        if self.first_pass {
            return true;
        }
        let mut should_nr = self.data.type_def_nr(should);
        if let Type::Vector(c_tp) = should {
            let c_nr = self.data.type_def_nr(c_tp);
            let tp = self.data.known_types.vector(self.data.def_known_type(c_nr));
            should_nr = self.data.check_vector(c_nr, tp, self.lexer.pos());
        }
        let should_kt = if should_nr != u32::MAX {
            self.data.def_known_type(should_nr)
        } else {
            u16::MAX
        };
        let is_nr = self.data.type_def_nr(is_type);
        let is_kt = if is_nr != u32::MAX {
            self.data.def_known_type(is_nr)
        } else {
            u16::MAX
        };
        for &dnr in self.types.get_possible("Cast") {
            if self.data.attributes(dnr) == 1
                && self.data.attr_type(dnr, 0) == *is_type
                && self.data.returned(dnr) == *should
            {
                *code = Value::Call(dnr, vec![code.clone()]);
                return true;
            } else if self.data.attributes(dnr) == 2
                && self.data.attr_type(dnr, 0) == *is_type
                && self.data.returned(dnr).is_same(should)
                && should_kt != u16::MAX
            {
                *code = Value::Call(dnr, vec![code.clone(), Value::Int(should_kt as i32)]);
                return true;
            } else if self.data.attributes(dnr) == 2
                && self.data.attr_type(dnr, 0).is_same(is_type)
                && self.data.returned(dnr) == *should
                && is_kt != u16::MAX
            {
                *code = Value::Call(dnr, vec![code.clone(), Value::Int(is_kt as i32)]);
                return true;
            }
        }
        false
    }

    /// Validate that two types are equal, show an error when impossible
    fn can_convert(&mut self, test_type: &Type, should: &Type) -> bool {
        if *test_type != *should && !test_type.is_unknown() {
            if let (Type::Enum(_e), Type::Enum(o)) = (test_type, should) {
                if self.data.def_name(*o) == "enumerate" {
                    return true;
                }
            }
            if let Type::Reference(r) = should {
                if *r == self.data.def_nr("reference") {
                    if let Type::Reference(_) = test_type {
                        return true;
                    }
                }
                if matches!(test_type, Type::Inner(i) if i==r) {
                    return true;
                }
            }
            false
        } else {
            true
        }
    }

    fn validate_convert(&mut self, context: &str, test_type: &Type, should: &Type) {
        if !self.first_pass && !self.can_convert(test_type, should) {
            let res = self.lexer.peek();
            specific!(
                &mut self.lexer,
                res,
                Level::Error,
                "{} should be {} on {context}",
                self.data.show_type(test_type),
                self.data.show_type(should)
            )
        }
    }

    /// Search for definitions with the given name and call that with the given parameters.
    fn call(&mut self, code: &mut Value, name: &str, list: &[Value], types: &[Type]) -> Type {
        // Create a new list of parameters based on the current ones
        // We still need to know the types though.
        let d_nr = self.data.def_nr(name);
        if d_nr != u32::MAX {
            self.call_nr(code, d_nr, list, types, true)
        } else if self.first_pass && !self.default {
            return Type::Unknown(0);
        } else {
            diagnostic!(self.lexer, Level::Error, "Unknown function {name}");
            Type::Unknown(0)
        }
    }

    fn op(&mut self, op: &str, f: Value, n: Value, t: Type) -> Value {
        let mut code = Value::Null;
        self.call_op(&mut code, op, vec![f, n], vec![t.clone(), t]);
        code
    }

    fn get_field(&mut self, d_nr: u32, f_nr: u16, code: Value) -> Value {
        let pos = self.data.attr_pos(d_nr, f_nr) as i32;
        let tp = self.data.attr_type(d_nr, f_nr);
        if tp == Type::Integer {
            let min = self.data.attr_min(d_nr, f_nr);
            let max = self.data.attr_max(d_nr, f_nr);
            let nullable = self.data.attr_nullable(d_nr, f_nr);
            if max == i32::MAX || min == i32::MIN {
                self.cl("OpGetInt", &[code, Value::Int(pos)])
            } else if max - min < 256 || (!nullable && max - min == 256) {
                self.cl("OpGetByte", &[code, Value::Int(pos), Value::Int(min)])
            } else if max - min < 56636 || (!nullable && max - min == 65536) {
                self.cl("OpGetShort", &[code, Value::Int(pos), Value::Int(min)])
            } else {
                self.cl("OpGetInt", &[code, Value::Int(pos)])
            }
        } else {
            self.get_val(&tp, pos, code)
        }
    }

    fn get_val(&mut self, tp: &Type, pos: i32, code: Value) -> Value {
        match tp {
            Type::Integer => self.cl("OpGetInt", &[code, Value::Int(pos)]),
            Type::Enum(_) => self.cl("OpGetByte", &[code, Value::Int(pos), Value::Int(0)]),
            Type::Boolean => {
                let val = self.cl("OpGetByte", &[code, Value::Int(pos), Value::Int(0)]);
                self.cl("OpEqInt", &[val, Value::Int(1)])
            }
            Type::Long => self.cl("OpGetLong", &[code, Value::Int(pos)]),
            Type::Float => self.cl("OpGetFloat", &[code, Value::Int(pos)]),
            Type::Single => self.cl("OpGetSingle", &[code, Value::Int(pos)]),
            Type::Text => self.cl("OpGetText", &[code, Value::Int(pos)]),
            Type::Vector(_) => self.cl("OpGetField", &[code, Value::Int(pos)]),
            Type::Reference(_) => self.cl("OpGetRef", &[code, Value::Int(pos)]),
            _ => panic!("Get not implemented on {} at {}", tp, self.lexer.pos()),
        }
    }

    fn set_field(&mut self, d_nr: u32, f_nr: u16, ref_code: Value, val_code: Value) -> Value {
        let tp = self.data.attr_type(d_nr, f_nr);
        let pos = self.data.attr_pos(d_nr, f_nr) as i32;
        let min = self.data.attr_min(d_nr, f_nr);
        let max = self.data.attr_max(d_nr, f_nr);
        let nullable = self.data.attr_nullable(d_nr, f_nr);
        if tp == Type::Integer {
            if max == i32::MAX || min == i32::MIN {
                self.cl("OpSetInt", &[ref_code, Value::Int(pos), val_code])
            } else if max - min < 256 || (!nullable && max - min == 256) {
                self.cl(
                    "OpSetByte",
                    &[ref_code, Value::Int(pos), Value::Int(min), val_code],
                )
            } else if max - min < 56636 || (!nullable && max - min == 65536) {
                self.cl(
                    "OpSetShort",
                    &[ref_code, Value::Int(pos), Value::Int(min), val_code],
                )
            } else {
                self.cl("OpSetInt", &[ref_code, Value::Int(pos), val_code])
            }
        } else {
            match tp {
                Type::Integer
                | Type::Vector(_)
                | Type::Hash(_, _)
                | Type::Index(_, _)
                | Type::Radix(_, _)
                | Type::Inner(_)
                | Type::Reference(_)
                | Type::Sorted(_, _) => self.cl("OpSetInt", &[ref_code, Value::Int(pos), val_code]),
                Type::Enum(_) => self.cl(
                    "OpSetByte",
                    &[ref_code, Value::Int(pos), Value::Int(0), val_code],
                ),
                Type::Boolean => self.cl(
                    "OpSetByte",
                    &[
                        ref_code,
                        Value::Int(pos),
                        Value::Int(0),
                        v_if(val_code, Value::Int(1), Value::Int(0)),
                    ],
                ),
                Type::Long => self.cl("OpSetLong", &[ref_code, Value::Int(pos), val_code]),
                Type::Float => self.cl("OpSetFloat", &[ref_code, Value::Int(pos), val_code]),
                Type::Single => self.cl("OpSetSingle", &[ref_code, Value::Int(pos), val_code]),
                Type::Text => self.cl("OpSetText", &[ref_code, Value::Int(pos), val_code]),
                _ => {
                    if !self.first_pass {
                        panic!(
                            "Set not implemented on {}/{} at {}",
                            self.data.attr_name(d_nr, f_nr),
                            self.data.attr_type(d_nr, f_nr),
                            self.lexer.pos()
                        )
                    } else {
                        Value::Null
                    }
                }
            }
        }
    }

    fn cl(&mut self, op: &str, list: &[Value]) -> Value {
        let d_nr = self.data.def_nr(op);
        if d_nr != u32::MAX {
            Value::Call(d_nr, list.to_vec())
        } else {
            diagnostic!(self.lexer, Level::Error, "Call to unknown {op}");
            Value::Null
        }
    }

    /// Try to find a matching defined operator. There can be multiple possible definitions for each operator.
    fn call_op(&mut self, code: &mut Value, op: &str, list: Vec<Value>, types: Vec<Type>) -> Type {
        let mut possible = Vec::new();
        for pos in self.types.get_possible(op) {
            possible.push(*pos);
        }
        for pos in possible {
            let tp = self.call_nr(code, pos, &list, &types, false);
            if tp != Type::Null {
                // We cannot compare two different types of enums, both will be integers in the same range
                if let (Some(Type::Enum(f)), Some(Type::Enum(s))) = (types.first(), types.get(1)) {
                    if f != s {
                        break;
                    }
                }
                return tp;
            }
        }
        if types.len() > 1 {
            specific!(
                self.lexer,
                self.lexer.peek(),
                Level::Error,
                "No matching operator {op} on {} and {}",
                self.data.show_type(&types[0]),
                self.data.show_type(&types[1])
            );
        } else {
            specific!(
                self.lexer,
                self.lexer.peek(),
                Level::Error,
                "No matching operator {op} on {}",
                self.data.show_type(&types[0])
            );
        }
        Type::Unknown(0)
    }

    /// Call a specific definition
    fn call_nr(
        &mut self,
        code: &mut Value,
        d_nr: u32,
        list: &[Value],
        types: &[Type],
        report: bool,
    ) -> Type {
        let mut sub_types = Vec::new();
        if self.data.def_type(d_nr) == DefType::Dynamic {
            for a_nr in 0..self.data.attributes(d_nr) {
                let Type::Routine(r_nr) = self.data.attr_type(d_nr, a_nr) else {
                    panic!("Incorrect Dynamic function {}", self.data.def_name(d_nr));
                };
                if let (Type::Vector(_), Type::Vector(_)) =
                    (self.data.attr_type(r_nr, 0), &types[0])
                {
                    return self.call_nr(code, r_nr, list, types, report);
                }
                if self.data.attr_type(r_nr, 0) == types[0] {
                    return self.call_nr(code, r_nr, list, types, report);
                }
            }
            diagnostic!(
                self.lexer,
                Level::Error,
                "No matching function {}",
                self.data.def_name(d_nr)
            );
        } else if self.data.def_type(d_nr) != DefType::Function {
            if report {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown definition {}",
                    self.data.def_name(d_nr)
                );
            }
            return Type::Null;
        }
        *code = if types.is_empty() {
            Value::Call(d_nr, list.to_vec())
        } else {
            let mut actual: Vec<Value> = Vec::new();
            if list.len() > self.data.attributes(d_nr) as usize {
                if report {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Too many parameters for {}",
                        self.data.def_name(d_nr)
                    );
                }
                return Type::Null;
            }
            for (nr, a_code) in list.iter().enumerate() {
                let tp = self.data.attr_type(d_nr, nr as u16);
                // Remember the subtype of a vector
                if let Type::Vector(vtp) = tp.clone() {
                    if vtp.is_unknown() {
                        if let Some(Type::Vector(tp)) = types.get(nr) {
                            sub_types.push(*tp.clone());
                        }
                    }
                }
                if let Some(actual_type) = types.get(nr) {
                    let mut actual_code = a_code.clone();
                    // When encountered a subtype reference, find the actual corresponding type
                    let mut test_type = tp.clone();
                    if let Type::Subtype(snr) = test_type {
                        let Some(tp) = sub_types.get(snr as usize) else {
                            diagnostic!(
                                self.lexer,
                                Level::Error,
                                "Unknown subtype {snr} on {}",
                                self.data.def_name(d_nr)
                            );
                            return Type::Null;
                        };
                        test_type = tp.clone();
                    }
                    if let (Type::Vector(to_tp), Type::Vector(a_tp)) = (&test_type, actual_type) {
                        if a_tp.is_unknown() && !to_tp.is_unknown() {
                            self.types.change_var_type(
                                &mut self.lexer,
                                &mut self.data,
                                &actual_code,
                                &test_type,
                            );
                            actual.push(actual_code);
                            continue;
                        }
                    }
                    if !self.convert(&mut actual_code, actual_type, &test_type) {
                        if report {
                            let context = format!("call to {}", self.data.def_name(d_nr));
                            self.validate_convert(&context, actual_type, &tp);
                        } else if !self.can_convert(actual_type, &tp) {
                            return Type::Null;
                        }
                    }
                    actual.push(actual_code);
                }
            }
            Value::Call(d_nr, actual)
        };
        self.data.returned(d_nr)
    }

    // ********************
    // * Parser functions *
    // ********************

    /// Parse data from the current lexer.
    fn parse_file(&mut self) {
        let start_def = self.data.definitions();
        self.file += 1;
        loop {
            self.lexer.has_token("pub");
            if self.lexer.diagnostics().level() == Level::Fatal
                || (!self.parse_enum()
                    && !self.parse_typedef()
                    && !self.parse_function()
                    && !self.parse_struct()
                    && !self.parse_constant())
            {
                break;
            }
        }
        let res = self.lexer.peek();
        if res.has != LexItem::None && self.lexer.diagnostics().level() != Level::Fatal {
            diagnostic!(self.lexer, Level::Fatal, "Syntax error")
        };
        if self.first_pass {
            typedef::actual_types(&mut self.data, &mut self.lexer, start_def);
        }
        self.first_pass = false;
    }

    // <typedef> ::= 'enum' <identifier> '{' <value> {, <value>} '}' [';']
    fn parse_enum(&mut self) -> bool {
        if !self.lexer.has_token("enum") {
            return false;
        }
        let Some(type_name) = self.lexer.has_identifier() else {
            diagnostic!(self.lexer, Level::Error, "Expect name in type definition");
            return false;
        };
        if !is_camel(&type_name) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect enum definitions to be in camel case style"
            );
        }
        let mut d_nr = self.data.def_nr(&type_name);
        if d_nr == u32::MAX {
            let pos = self.lexer.pos();
            d_nr = self.data.add_def(type_name, pos, DefType::Enum);
            self.data.def_set_size(d_nr, 1, 1);
        } else if self.first_pass && self.data.def_type(d_nr) == DefType::Unknown {
            self.data.set_def_type(d_nr, DefType::Enum);
            self.data.set_def_pos(d_nr, self.lexer.pos());
            self.data.def_set_size(d_nr, 1, 1);
        } else if self.first_pass {
            diagnostic!(self.lexer, Level::Error, "Cannot redefine {type_name}");
        }
        if self.first_pass {
            self.data.set_returned(d_nr, Type::Enum(d_nr));
        }
        if !self.lexer.token("{") {
            return false;
        }
        let mut nr = 0;
        loop {
            let Some(value_name) = self.lexer.has_identifier() else {
                diagnostic!(self.lexer, Level::Error, "Expect name in type definition");
                return false;
            };
            if !is_camel(&value_name) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect enum values to be in camel case style"
                );
            }
            let v_nr = if self.first_pass {
                self.data
                    .add_def(value_name.clone(), self.lexer.pos(), DefType::EnumValue)
            } else {
                self.data.def_nr(&value_name)
            };
            if self.first_pass {
                self.data.set_returned(v_nr, Type::Enum(d_nr));
                self.data.def_set_size(v_nr, 1, 1);
                let a_nr =
                    self.data
                        .add_attribute(&mut self.lexer, d_nr, &value_name, Type::Enum(d_nr));
                self.data.set_attr_value(d_nr, a_nr, Value::Int(nr));
            }
            nr += 1;
            if !self.lexer.has_token(",") {
                break;
            }
        }
        if self.first_pass {
            complete_definition(&mut self.lexer, &mut self.data, d_nr);
        }
        self.lexer.token("}");
        self.lexer.has_token(";");
        true
    }

    fn parse_typedef(&mut self) -> bool {
        if !self.lexer.has_token("type") {
            return false;
        }
        let Some(type_name) = self.lexer.has_identifier() else {
            diagnostic!(self.lexer, Level::Error, "Expect name in type definition");
            return false;
        };
        if !self.default && !is_camel(&type_name) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect type definitions to be in camel case style"
            );
        }
        let d_nr = if self.first_pass {
            self.data
                .add_def(type_name, self.lexer.pos(), DefType::Type)
        } else {
            self.data.def_nr(&type_name)
        };
        if self.lexer.has_token("=") {
            if let Some(type_name) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &type_name) {
                    self.data.set_returned(d_nr, tp);
                } else if !self.first_pass {
                    panic!("Incorrect handling of unknown types");
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Expected a type after =");
            }
        }
        if self.first_pass {
            complete_definition(&mut self.lexer, &mut self.data, d_nr);
        }
        self.lexer.token(";");
        true
    }

    // <constant>
    fn parse_constant(&mut self) -> bool {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("=");
            if !is_upper(&id) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect constants to be in upper case style"
                );
            }
            let mut val = Value::Null;
            let tp = self.expression(&mut val);
            if self.first_pass {
                let c_nr = self.data.add_def(id, self.lexer.pos(), DefType::Constant);
                self.data.set_returned(c_nr, tp);
                self.data.set_code(c_nr, val);
            }
            self.lexer.token(";");
            true
        } else {
            false
        }
    }

    // <function> ::= 'fn' <identifier>[ '(' <attributes> ] [ '->' <type> ] <code>
    fn parse_function(&mut self) -> bool {
        // prevent sub_names from other function definitions to bleed into this new one.
        self.sub_names.clear();
        if !self.lexer.has_token("fn") {
            return false;
        }
        let Some(fn_name) = self.lexer.has_identifier() else {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect name in function definition"
            );
            return false;
        };
        if !self.default && !is_lower(&fn_name) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect function names to be in lower case style"
            );
        }
        if self.lexer.has_token("<") {
            let mut nr = 0;
            loop {
                if let Some(id) = self.lexer.has_identifier() {
                    self.sub_names.insert(id, nr);
                    nr += 1;
                }
                if !self.lexer.has_token(",") {
                    break;
                }
            }
            self.lexer.token(">");
        }
        let mut arguments: Arguments = Vec::new();
        if self.lexer.token("(") {
            if !self.parse_fn_arguments(&fn_name, &mut arguments) {
                return true;
            }
            self.lexer.token(")");
        }
        let result = if self.lexer.has_token("->") {
            // Will be the correct def_nr on the second pass
            if let Some(type_name) = self.lexer.has_identifier() {
                let Some(tp) = self.parse_type(self.data.def_nr(&fn_name), &type_name) else {
                    // Message
                    return false;
                };
                tp
            } else {
                // message
                Type::Void
            }
        } else {
            Type::Void
        };
        self.context = if self.default && self.first_pass && is_op(&fn_name) {
            self.data
                .add_op(&mut self.lexer, &mut self.types, fn_name.clone(), arguments)
        } else if self.first_pass {
            self.data
                .add_fn(&mut self.lexer, fn_name.clone(), arguments)
        } else {
            self.data.get_fn(&fn_name, &arguments)
        };
        if self.first_pass && self.context != u32::MAX {
            self.data.set_returned(self.context, result);
        }
        if !self.lexer.has_token(";") {
            self.parse_code();
        }
        if !self.first_pass && self.context != u32::MAX {
            self.types
                .test_used(self.data.attributes(self.context), &mut self.diagnostics);
        }
        self.types.clear();
        self.lexer.has_token(";");
        if self.default && self.lexer.has_token("#") {
            if self.lexer.has_identifier() == Some("rust".to_string()) {
                if let Some(c) = self.lexer.has_cstring() {
                    self.data.set_rust(self.context, c);
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect rust string");
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Expect #rust");
            }
        }
        self.context = u32::MAX;
        true
    }

    fn parse_fn_arguments(&mut self, fn_name: &str, arguments: &mut Arguments) -> bool {
        loop {
            if self.lexer.peek_token(")") {
                break;
            }
            let Some(attr_name) = self.lexer.has_identifier() else {
                diagnostic!(self.lexer, Level::Error, "Expect attribute");
                return false;
            };
            if !is_lower(&attr_name) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect function attributes to be in lower case style"
                );
            }
            for (nm, _, _) in arguments.iter() {
                if attr_name == *nm {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Double attribute '{fn_name}.{attr_name}'"
                    );
                }
            }
            let typedef = if self.lexer.has_token(":") {
                // Will be the correct def_nr on the second pass
                if self.lexer.has_token("fn") {
                    self.parse_fn_type(self.data.def_nr(fn_name))
                } else if let Some(type_name) = self.lexer.has_identifier() {
                    if let Some(tp) = self.parse_type(self.data.def_nr(fn_name), &type_name) {
                        tp
                    } else if !self.first_pass {
                        panic!("Incorrect handling of unknown types");
                    } else {
                        Type::Unknown(0)
                    }
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expecting a type");
                    return true;
                }
            } else {
                Type::Unknown(0)
            };
            let val = if self.lexer.has_token("=") {
                let mut t = Value::Null;
                self.expression(&mut t);
                t
            } else {
                Value::Null
            };
            if !self.first_pass && typedef.is_unknown() && val == Value::Null {
                diagnostic!(self.lexer, Level::Error, "Expecting a clear type");
            }
            (*arguments).push((attr_name, typedef, val));
            if !self.lexer.has_token(",") {
                break;
            }
        }
        true
    }

    fn parse_fn_type(&mut self, d_nr: u32) -> Type {
        let mut r_type = Type::Void;
        let mut args = Vec::new();
        self.lexer.token("(");
        loop {
            if let Some(id) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &id) {
                    args.push(tp);
                }
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        if self.lexer.has_token("->") {
            if let Some(id) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &id) {
                    r_type = tp;
                }
            }
        }
        Type::Function(args, Box::new(r_type))
    }

    // <type> ::= <identifier> [ '<' ( <sub_type> | <type> ) '>' ]
    fn parse_type(&mut self, on_d: u32, type_name: &str) -> Option<Type> {
        if self.sub_names.contains_key(type_name) {
            return Some(Type::Subtype(self.sub_names[type_name]));
        }
        let tp_nr = self.data.def_nr(type_name);
        if self.first_pass && tp_nr == u32::MAX {
            let u_nr = self
                .data
                .add_def(type_name.to_string(), self.lexer.pos(), DefType::Unknown);
            return Some(Type::Unknown(u_nr));
        }
        if tp_nr != u32::MAX && self.data.def_type(tp_nr) == DefType::Unknown {
            return Some(Type::Unknown(tp_nr));
        }
        let link = self.lexer.link();
        if self.lexer.has_token("<") {
            if let Some(sub_name) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(on_d, &sub_name) {
                    self.lexer.token(">");
                    let sub_nr = if let Type::Unknown(d) = tp {
                        d
                    } else {
                        self.data.type_def_nr(&tp)
                    };
                    return Some(match type_name {
                        "index" => {
                            if sub_nr != u32::MAX {
                                let link = self.data.attr(sub_nr, "continue");
                                if link == u16::MAX {
                                    self.data.add_attribute(
                                        &mut self.lexer,
                                        sub_nr,
                                        "continue",
                                        Type::Link,
                                    );
                                }
                                self.data.set_referenced(sub_nr, on_d, Value::Null);
                            }
                            Type::Index(self.data.type_def_nr(&tp), vec![])
                        }
                        "hash" => {
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Hash(sub_nr, vec![])
                        }
                        "vector" => Type::Vector(Box::new(tp)),
                        "sorted" => Type::Sorted(sub_nr, vec![]),
                        "radix" => {
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Radix(sub_nr, vec![])
                        }
                        "reference" => {
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Reference(sub_nr)
                        }
                        _ => {
                            diagnostic!(
                                self.lexer,
                                Level::Error,
                                "Sub-type only allowed on structures"
                            );
                            Type::Unknown(0)
                        }
                    });
                }
                if !self.first_pass {
                    panic!("Incorrect handling of unknown types");
                }
            } else {
                self.lexer.revert(link);
            }
        }
        let dt = self.data.def_type(tp_nr);
        if tp_nr != u32::MAX
            && (dt == DefType::Type || dt == DefType::Enum || dt == DefType::Struct)
        {
            if self.first_pass && dt == DefType::Struct {
                Some(Type::Reference(tp_nr))
            } else {
                Some(self.data.returned(tp_nr))
            }
        } else {
            None
        }
    }

    // <struct> = 'struct' <identifier> [ ':' <type> ] '{' <param-id> ':' <field> { ',' <param-id> ':' <field> } '}'
    fn parse_struct(&mut self) -> bool {
        if !self.lexer.has_token("struct") {
            return false;
        }
        let Some(id) = self.lexer.has_identifier() else {
            diagnostic!(self.lexer, Level::Error, "Expect attribute");
            return true;
        };
        let mut d_nr = self.data.def_nr(&id);
        if d_nr == u32::MAX {
            d_nr = self.data.add_def(id, self.lexer.pos(), DefType::Struct);
        } else if self.first_pass {
            self.data.set_def_pos(d_nr, self.lexer.pos());
            self.data.set_def_type(d_nr, DefType::Struct);
        };
        self.context = d_nr;
        self.lexer.token("{");
        loop {
            let Some(a_name) = self.lexer.has_identifier() else {
                diagnostic!(self.lexer, Level::Error, "Expect attribute");
                return true;
            };
            if self.first_pass && self.data.attr(d_nr, &a_name) != u16::MAX {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "field `{}` is already declared",
                    a_name
                );
            }
            self.lexer.token(":");
            self.parse_field(d_nr, &a_name);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token("}");
        self.lexer.has_token(";");
        true
    }

    // <field> ::= { <field_limit> | 'not' 'null' | <field_default> | 'check' '(' <expr> ')' | <type-id> [ '[' <field> [ 'desc' ] { ',' <field> [ 'desc' ] } ']' ] } }
    fn parse_field(&mut self, d_nr: u32, a_name: &String) {
        let mut a_type: Type = Type::Unknown(0);
        let mut defined = false;
        let mut min = i32::MIN;
        let mut max = i32::MAX;
        let mut value = Value::Null;
        let mut check = Value::Null;
        let mut nullable = true;
        let mut virt = false;
        loop {
            if self.parse_field_limit(&mut min, &mut max, &mut a_type) {
                defined = true;
            }
            if self.lexer.has_keyword("not") {
                // This field cannot be null, this allows for 256 values in a byte
                self.lexer.token("null");
                nullable = false;
            }
            if self.parse_field_default(&mut value, &mut a_type, d_nr, a_name, &mut defined) {
                virt = true;
            }
            if self.lexer.has_keyword("check") {
                self.lexer.token("(");
                let tp = self.expression(&mut check);
                self.convert(&mut check, &tp, &Type::Boolean);
                self.lexer.token(")");
            }
            if let Some(id) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &id) {
                    defined = true;
                    a_type = tp;
                    if self.lexer.has_token("[") {
                        loop {
                            let Some(_name) = self.lexer.has_identifier() else {
                                diagnostic!(self.lexer, Level::Error, "Expect attribute");
                                return;
                            };
                            if self.lexer.has_keyword("desc") {
                                // descending ordered field on index
                            }
                            // validate known parameters
                            if !self.lexer.has_token(",") {
                                break;
                            }
                        }
                        self.lexer.has_token("]");
                    }
                }
            } else {
                break;
            }
        }
        if !defined {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Attribute {a_name} needs type or definition"
            );
        }
        if self.first_pass {
            let a = self
                .data
                .add_attribute(&mut self.lexer, d_nr, a_name, a_type);
            if min != i32::MIN {
                self.data.set_attr_min(d_nr, a, min);
            }
            if max != i32::MAX {
                self.data.set_attr_max(d_nr, a, max);
            }
            if check != Value::Null {
                self.data.set_attr_check(d_nr, a, check);
            }
            if virt {
                self.data.set_attr_mutable(d_nr, a, false);
            }
            self.data.set_attr_nullable(d_nr, a, nullable);
        } else {
            let a = self.data.attr(d_nr, a_name);
            if value != Value::Null {
                self.data.set_attr_value(d_nr, a, value);
            }
        }
    }

    // <field_limit> ::= 'limit' '(' [ '-' ] <min-integer> ',' [ '-' ] <max-integer> ')'
    fn parse_field_limit(&mut self, min: &mut i32, max: &mut i32, a_type: &mut Type) -> bool {
        let mut defined = false;
        if self.lexer.has_keyword("limit") {
            self.lexer.token("(");
            if matches!(a_type, Type::Unknown(_)) {
                *a_type = Type::Integer;
                defined = true;
            } else if *a_type != Type::Integer && *a_type != Type::Text {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Limit only allowed on integer or text fields"
                );
            }
            let min_neg = self.lexer.has_token("-");
            if let Some(nr) = self.lexer.has_integer() {
                *min = if min_neg { -(nr as i32) } else { nr as i32 };
            }
            self.lexer.token(",");
            let max_neg = self.lexer.has_token("-");
            if let Some(nr) = self.lexer.has_integer() {
                *max = if max_neg { -(nr as i32) } else { nr as i32 };
            }
            self.lexer.token(")");
            if *a_type == Type::Unknown(0) {
                *a_type = Type::Integer;
            }
        }
        defined
    }

    // <field_default> ::= 'virtual' <value-expr> | 'default' '(' <value-expr> ')'
    fn parse_field_default(
        &mut self,
        value: &mut Value,
        a_type: &mut Type,
        d_nr: u32,
        a_name: &String,
        defined: &mut bool,
    ) -> bool {
        let mut virt = false;
        if self.lexer.has_keyword("virtual") {
            virt = true;
            // Define the result of a field that cannot be written to
            self.lexer.token("(");
            let args = vec![("rec".to_string(), Type::Reference(d_nr), Value::Null)];
            let c = self.context;
            let mut virt = Value::Null;
            let name = format!(
                "_virtual_attr_{}_{}",
                self.data.def_name(d_nr).to_lowercase(),
                a_name
            );
            let v_nr = if self.first_pass {
                self.data.add_fn(&mut self.lexer, name.clone(), args)
            } else {
                self.data.def_nr(&name.clone())
            };
            self.context = d_nr;
            let tp = self.expression(&mut virt);
            self.context = c;
            if a_type.is_unknown() {
                *a_type = tp;
                *defined = true;
            } else {
                self.convert(&mut virt, &tp, a_type);
            }
            if !self.first_pass {
                self.data.set_code(v_nr, virt);
            } else {
                self.data.set_returned(v_nr, a_type.clone());
            }
            // We still need to replace Var(0) with the actual record on the call
            *value = self.cl(&name, &[Value::Var(0)]);
            self.lexer.token(")");
        }
        if self.lexer.has_keyword("default") {
            // Define a default value on an attribute
            self.lexer.token("(");
            let tp = self.expression(value);
            if a_type.is_unknown() {
                *a_type = tp;
                *defined = true;
            } else {
                self.convert(value, &tp, a_type);
            }
            self.lexer.token(")");
            if virt {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Attribute {a_name} cannot be virtual and have a default"
                );
            }
        }
        virt
    }

    // <code> = '{' <block> '}'
    /// Parse the code on the last inserted definition.
    /// This way we can use recursion with the definition itself.
    fn parse_code(&mut self) -> Type {
        self.lexer.token("{");
        let mut v = Value::Null;
        let result = if self.context != u32::MAX {
            self.types.parameters(&self.data, self.context)
        } else {
            Type::Void
        };
        let t = self.parse_block(&mut v, result);
        if self.context != u32::MAX && !self.first_pass {
            self.data.set_code(self.context, v);
        }
        t
    }

    // <expression> ::= <for> | 'continue' | 'break' | 'return' <return> | '{' <block> | <operators>
    fn expression(&mut self, val: &mut Value) -> Type {
        if self.lexer.has_token("for") {
            self.parse_for(val);
            Type::Void
        } else if self.lexer.has_token("continue") {
            if !self.in_loop {
                diagnostic!(self.lexer, Level::Error, "Cannot continue outside a loop");
            }
            *val = Value::Continue(0);
            Type::Void
        } else if self.lexer.has_token("break") {
            if !self.in_loop {
                diagnostic!(self.lexer, Level::Error, "Cannot break outside a loop");
            }
            *val = Value::Break(0);
            Type::Void
        } else if self.lexer.has_token("return") {
            self.parse_return(val);
            Type::Void
        } else if self.lexer.has_token("{") {
            self.parse_block(val, Type::Void)
        } else {
            let res = self.parse_assign(val);
            self.known_var_or_type(&res, val);
            res
        }
    }

    // <assign> ::= <operators> [ '=' | '+=' | '-=' | '*=' | '%=' | '/=' <operators> ]
    fn parse_assign(&mut self, code: &mut Value) -> Type {
        let f_type = self.parse_operators(code, 0);
        let to = code.clone();
        //let mut found = false;
        for op in ["=", "+=", "-=", "*=", "%=", "/="] {
            if self.lexer.has_token(op) {
                //found = true;
                if op == "=" {
                    *code = Value::Null; // do not bleed the original value
                }
                let s_type = self.parse_operators(code, 0);
                let new = self
                    .types
                    .change_var_type(&mut self.lexer, &mut self.data, &to, &s_type);
                *code = self.towards_set(to, code, &f_type, &op[0..1], new);
                return Type::Void;
            }
        }
        *code = to;
        f_type
    }

    fn towards_set(&mut self, to: Value, val: &Value, f_type: &Type, op: &str, new: bool) -> Value {
        if let Type::Vector(_) = f_type {
            return val.clone();
        }
        let code = if op != "=" {
            self.op(op, to.clone(), val.clone(), f_type.clone())
        } else {
            val.clone()
        };
        if let Value::Call(d_nr, args) = &to {
            let name = self.data.def_name(*d_nr);
            match &name as &str {
                "OpGetInt" => self.cl("OpSetInt", &[args[0].clone(), args[1].clone(), code]),
                "OpGetByte" => self.cl("OpSetByte", &[args[0].clone(), args[1].clone(), code]),
                "OpGetShort" => self.cl("OpSetShort", &[args[0].clone(), args[1].clone(), code]),
                "OpGetLong" => self.cl("OpSetLong", &[args[0].clone(), args[1].clone(), code]),
                "OpGetFloat" => self.cl("OpSetFloat", &[args[0].clone(), args[1].clone(), code]),
                "OpGetSingle" => self.cl("OpSetSingle", &[args[0].clone(), args[1].clone(), code]),
                "OpGetText" => self.cl("OpSetText", &[args[0].clone(), args[1].clone(), code]),
                _ => panic!("Unknown {op}= for {name}"),
            }
        } else if let Value::Var(nr) = to {
            self.types.assign(nr);
            if op == "=" && new {
                v_let(nr, code)
            } else {
                v_set(nr, code)
            }
        } else {
            panic!("Unknown assign");
        }
    }

    // <block> ::= '}' | <expression> {';' <expression} '}'
    fn parse_block(&mut self, val: &mut Value, result: Type) -> Type {
        let mut t = Type::Void;
        if self.lexer.has_token("}") {
            *val = Value::Block(Vec::new());
            return t;
        }
        // Start a new set of variables.
        self.types.push();
        let mut l = Vec::new();
        loop {
            if self.lexer.has_token(";") {
                continue;
            }
            if self.lexer.peek_token("}") {
                break;
            }
            let mut n = Value::Null;
            t = self.expression(&mut n);
            l.push(n);
            if self.lexer.peek_token("}") {
                break;
            }
            t = Type::Void;
            match l[l.len() - 1] {
                Value::If(_, _, _) => (),
                Value::Loop(_) => (),
                Value::Block(_) => (),
                _ => {
                    if !self.lexer.token(";") {
                        break;
                    }
                }
            }
        }
        self.lexer.token("}");
        self.types.pop();
        if result != Type::Void {
            let last = l.len() - 1;
            if !self.convert(&mut l[last], &t, &result) {
                self.validate_convert("return from block", &t, &result);
            }
        }
        *val = Value::Block(l);
        t
    }

    // <operator> ::= '..' ['='] |
    //                '||' | 'or' |
    //                '&&' | 'and' |
    //                '==' | '!=' | '<' | '<=' | '>' | '>=' |
    //                '|' |
    //                '^' |
    //                '&' |
    //                '<<' | '>>' |
    //                '-' | '+' |
    //                '*' | '/' | '%'
    // <operators> ::= <single> | <operators> <operator> <operators>
    fn parse_operators(&mut self, code: &mut Value, precedence: usize) -> Type {
        if precedence >= OPERATORS.len() {
            return self.parse_single(code);
        }
        let mut current_type = self.parse_operators(code, precedence + 1);
        loop {
            let mut operator = "";
            for op in OPERATORS[precedence] {
                if self.lexer.has_token(op) {
                    operator = op;
                    break;
                }
            }
            if operator.is_empty() {
                return current_type;
            }
            self.known_var_or_type(&current_type, code);
            if operator == "and" {
                operator = "&&"
            } else if operator == "or" {
                operator = "||"
            }
            if operator == "as" {
                if let Some(tps) = self.lexer.has_identifier() {
                    let tp = if let Some(found_tp) = self.parse_type(u32::MAX, &tps) {
                        found_tp
                    } else {
                        diagnostic!(self.lexer, Level::Error, "Expect type");
                        return Type::Null;
                    };
                    if !self.convert(code, &current_type, &tp)
                        && !self.cast(code, &current_type, &tp)
                    {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Unknown cast from {} to {tps}",
                            self.data.show_type(&current_type),
                        );
                    }
                    return tp;
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect type after as");
                }
            } else if operator == ".." {
                let including = self.lexer.has_token("=");
                if !including && (self.lexer.peek_token("]") || self.lexer.peek_token(")")) {
                    current_type = Type::Iterator(Box::new(Type::Null));
                    *code = Value::Range(Box::new(code.clone()), Box::new(Value::Int(i32::MAX)));
                } else {
                    let mut second_code = Value::Null;
                    let second_type = self.parse_operators(&mut second_code, precedence + 1);
                    self.known_var_or_type(&second_type, &second_code);
                    if including {
                        second_code = self.cl("OpAddInt", &[second_code, Value::Int(1)]);
                    }
                    current_type = Type::Iterator(Box::new(Type::Null));
                    *code = Value::Range(Box::new(code.clone()), Box::new(second_code));
                }
            } else {
                let mut second_code = Value::Null;
                let second_type = self.parse_operators(&mut second_code, precedence + 1);
                self.known_var_or_type(&second_type, &second_code);
                current_type = self.call_op(
                    code,
                    operator,
                    vec![code.clone(), second_code],
                    vec![current_type, second_type],
                );
            }
        }
    }

    // <single> ::= '!' <expression> |
    //              '(' <expression> ')' |
    //              <vector> |
    //              'if' <if> |
    //              <identifier:var> |
    //              <number> | <float> | <cstring> |
    //              'true' | 'false'
    fn parse_single(&mut self, val: &mut Value) -> Type {
        if self.lexer.has_token("!") {
            let t = self.expression(val);
            self.call_op(val, "!", vec![val.clone()], vec![t])
        } else if self.lexer.has_token("-") {
            let t = self.expression(val);
            self.call_op(val, "-", vec![val.clone()], vec![t])
        } else if self.lexer.has_token("(") {
            let t = self.expression(val);
            self.lexer.token(")");
            t
        } else if self.lexer.has_token("[") {
            self.parse_vector(val)
        } else if self.lexer.has_token("if") {
            self.parse_if(val)
        } else if let Some(name) = self.lexer.has_identifier() {
            self.parse_var(val, name)
        } else if self.lexer.has_token("$") {
            self.parse_var(val, "$".to_string())
        } else if let Some(nr) = self.lexer.has_integer() {
            *val = Value::Int(nr as i32);
            Type::Integer
        } else if let Some(nr) = self.lexer.has_long() {
            *val = Value::Long(nr as i64);
            Type::Long
        } else if let Some(nr) = self.lexer.has_float() {
            *val = Value::Float(nr);
            Type::Float
        } else if let Some(nr) = self.lexer.has_single() {
            *val = Value::Single(nr);
            Type::Single
        } else if let Some(s) = self.lexer.has_cstring() {
            self.parse_string(val, s);
            Type::Text
        } else if self.lexer.has_token("true") {
            *val = Value::Boolean(true);
            Type::Boolean
        } else if self.lexer.has_token("false") {
            *val = Value::Boolean(false);
            Type::Boolean
        } else if self.lexer.has_token("null") {
            *val = Value::Null;
            Type::Null
        } else {
            Type::Unknown(0)
        }
    }

    // <vector> ::= '[' <expr> { ',' <expr> } ']'
    fn parse_vector(&mut self, val: &mut Value) -> Type {
        let mut in_t = Type::Unknown(0);
        let elm = self.types.create_var(
            format!("elm_{}", self.var_nr),
            Type::Reference(0),
            self.lexer.pos(),
        );
        self.var_nr += 1;
        let vec = self.types.create_var(
            format!("vec_{}", self.var_nr),
            Type::Unknown(0),
            self.lexer.pos(),
        );
        self.var_nr += 1;
        if self.lexer.has_token("]") {
            let new_store = *val == Value::Null;
            if new_store {
                let mut ls = Vec::new();
                let vec_def = self.data.vector_def(&mut self.lexer, &in_t);
                let db = self.types.create_var(
                    format!("db_{}", self.var_nr),
                    Type::Reference(vec_def),
                    self.lexer.pos(),
                );
                ls.push(v_set(db, self.cl("OpDatabase", &[Value::Int(1)])));
                // Reference to vector field.
                ls.push(v_set(vec, self.get_field(vec_def, 0, Value::Var(db))));
                // Write 0 into this reference.
                ls.push(self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
                ls.push(Value::Var(vec));
                *val = Value::Block(ls);
            }
            return Type::Vector(Box::new(Type::Unknown(0)));
        }
        let mut size = 0;
        let new_store = *val == Value::Null;
        let mut res = Vec::new();
        loop {
            let mut p = Value::Var(elm);
            let t = self.expression(&mut p);
            if in_t.is_unknown() {
                in_t = t.clone();
            }
            if let Type::Reference(dnr) = t {
                let d_nr = if self.data.def_parent(dnr) != 0 {
                    self.data.def_parent(dnr)
                } else {
                    dnr
                };
                let s = if self.data.def_referenced(d_nr) {
                    self.data.def_size(d_nr)
                } else {
                    4
                };
                if s > size {
                    size = s;
                }
            } else {
                // double conversion check: can t become in_t or vice versa
                if !self.convert(&mut p, &t, &in_t) {
                    if self.convert(&mut p, &in_t, &t) {
                        in_t = t;
                    } else {
                        diagnostic!(self.lexer, Level::Error, "No common type for vector");
                    }
                }
            }
            res.push(p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        // determine the element size by the resulting type
        size = self.data.def_size(self.data.type_def_nr(&in_t));
        // convert parts to common type
        if in_t == Type::Null {
            return in_t;
        }
        self.types.change_var_type(
            &mut self.lexer,
            &mut self.data,
            &Value::Var(vec),
            &Type::Vector(Box::new(in_t.clone())),
        );
        let mut ls = Vec::new();
        let ed_nr = self.data.type_def_nr(&in_t);
        let attr = self.data.def_type(ed_nr) == DefType::Type;
        for p in res {
            let vec_tp = self.data.type_def_nr(&in_t);
            let vec_size = self.data.def_size(vec_tp) as i32;
            let set_value = self.types.create_var(
                format!("set_{}", self.var_nr),
                Type::Reference(0),
                self.lexer.pos(),
            );
            let app_v = self.cl("OpAppendVector", &[Value::Var(vec), Value::Int(vec_size)]);
            if attr {
                ls.push(v_set(set_value, p.clone()));
                ls.push(v_set(elm, app_v));
                ls.push(self.set_field(vec_tp, u16::MAX, Value::Var(elm), Value::Var(set_value)));
            } else if self.data.def_inner(ed_nr) {
                ls.push(v_set(elm, app_v));
                Self::push(&mut ls, &p);
            } else {
                ls.push(v_set(
                    elm,
                    self.cl("OpAppend", &[Value::Var(vec), Value::Int(vec_size)]),
                ));
                Self::push(&mut ls, &p);
                ls.push(self.cl("OpSetRef", &[app_v, Value::Int(0), Value::Var(elm)]));
            }
        }
        if new_store {
            let vec_def = self.data.vector_def(&mut self.lexer, &in_t);
            let db = self.types.create_var(
                format!("db_{}", self.var_nr),
                Type::Reference(vec_def),
                self.lexer.pos(),
            );
            ls.insert(
                0,
                v_set(db, self.cl("OpDatabase", &[Value::Int(size as i32)])),
            );
            // Reference to vector field.
            ls.insert(1, v_set(vec, self.get_field(vec_def, 0, Value::Var(db))));
            // Write 0 into this reference.
            ls.insert(2, self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
        } else {
            ls.insert(0, v_set(vec, val.clone()));
        }
        if new_store {
            ls.push(Value::Var(vec));
        }
        self.lexer.token("]");
        *val = Value::Block(ls);
        Type::Vector(Box::new(in_t))
    }

    fn push(ls: &mut Vec<Value>, p: &Value) {
        if let Value::Block(bl) = p {
            for e in bl {
                ls.push(e.clone());
            }
        } else {
            ls.push(p.clone());
        }
    }

    // <children> ::=
    fn field(&mut self, code: &mut Value, tp: Type) -> Type {
        if let Type::Unknown(_) = tp {
            diagnostic!(self.lexer, Level::Error, "Field of unknown type");
            return tp;
        }
        let mut t = tp;
        if let Some(field) = self.lexer.has_identifier() {
            let enr = self.data.type_elm(&t);
            let e_size = self.data.def_size(enr) as i32;
            let dnr = self.data.type_def_nr(&t);
            if let Type::Vector(et) = t.clone() {
                if field == "remove" {
                    let (tps, ls) = self.parse_parameters();
                    let mut cd = ls[0].clone();
                    // validate types
                    if tps.len() != 1 || self.convert(&mut cd, &tps[0], &Type::Integer) {
                        diagnostic!(self.lexer, Level::Error, "Invalid index in remove");
                    }
                    *code = self.cl("OpRemoveVector", &[code.clone(), Value::Int(e_size), cd]);
                } else if field == "insert" {
                    let (tps, ls) = self.parse_parameters();
                    let mut cd = ls[0].clone();
                    // validate types
                    if tps.len() != 2 || self.convert(&mut cd, &tps[0], &Type::Integer) {
                        diagnostic!(self.lexer, Level::Error, "Invalid index in insert");
                    }
                    let mut vl = ls[1].clone();
                    if !self.convert(&mut vl, &tps[1], &et) {
                        diagnostic!(self.lexer, Level::Error, "Invalid value in insert");
                    }
                    *code = self.cl("OpInsertVector", &[code.clone(), Value::Int(e_size), cd]);
                    // TODO copy vl into newly created vector element
                    // Inner should be different from Reference
                }
            }
            let fnr = self.data.attr(dnr, &field);
            if fnr != u16::MAX {
                if let Type::Routine(r_nr) = self.data.attr_type(dnr, fnr) {
                    if self.lexer.has_token("(") {
                        t = self.parse_method(code, r_nr, t.clone());
                    } else {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Expect call of method {}.{}",
                            self.data.def_name(dnr),
                            self.data.attr_name(dnr, fnr)
                        );
                    }
                } else if !self.data.attr_mutable(dnr, fnr) {
                    let mut new = self.data.attr_value(dnr, fnr);
                    if let Value::Call(_, args) = &mut new {
                        args[0] = code.clone();
                    };
                    *code = new;
                    t = self.data.attr_type(dnr, fnr);
                } else {
                    let last_t = t.clone();
                    t = self.data.attr_type(dnr, fnr);
                    if let Type::Enum(_) = last_t.clone() {
                        // do something with enum fields
                    } else {
                        *code = self.get_field(dnr, fnr, code.clone());
                    }
                }
                fnr
            } else {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown field {}.{field}",
                    self.data.def_name(dnr)
                );
                return t;
            };
            self.data.attr_used(dnr, fnr);
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect a field name")
        }
        t
    }

    fn index(&mut self, code: &mut Value, t: Type) -> Type {
        let mut p = Value::Null;
        let elm_type = if let Type::Vector(v_t) = t.clone() {
            *v_t
        } else if let Type::Sorted(d_nr, _) = t {
            self.data.returned(d_nr)
        } else if t == Type::Text {
            Type::Text
        } else {
            diagnostic!(self.lexer, Level::Error, "Indexing a non vector");
            Type::Unknown(0)
        };
        if let Some((nr, _)) = self.types.var_name("$") {
            self.types.assign(nr);
        } else if !self.types.used("$") {
            let nr = self
                .types
                .create_var("$".to_string(), elm_type.clone(), self.lexer.pos());
            self.types.assign(nr);
        }
        let index_t = self.expression(&mut p);
        if self.types.used("$") {
            self.first_match(code, p, t);
        } else if let Type::Vector(etp) = t {
            let elm_td = self.data.type_elm(&etp);
            let elm_size = self.data.def_size(elm_td) as i32;
            *code = self.cl("OpGetVector", &[code.clone(), Value::Int(elm_size), p]);
            if !self.data.def_inner(elm_td) {
                *code = self.get_val(&etp, 0, code.clone());
            }
        } else if t == Type::Text {
            if let Value::Range(a, b) = &p {
                *code = self.cl(
                    "OpGetTextSub",
                    &[code.clone(), (**a).clone(), (**b).clone()],
                );
            } else {
                *code = self.cl("OpGetTextSub", &[code.clone(), p, Value::Int(i32::MIN)]);
            }
        } else {
            self.call_op(code, "Get", vec![code.clone(), p], vec![t, index_t]);
        }
        elm_type
    }

    // <var> ::= <object> | [ '(' <call> | <var> | <enum> ] <children> { '.' <field> | '[' <index> ']' }
    fn parse_var(&mut self, code: &mut Value, name: String) -> Type {
        let mut t;
        let d_nr = self.data.def_nr(&name);
        if d_nr != u32::MAX {
            self.data.def_used(d_nr);
            t = self.data.returned(d_nr);
            if self.data.def_type(d_nr) == DefType::Function {
                t = Type::Routine(d_nr)
            } else if self.data.def_type(d_nr) == DefType::Struct {
                return self.parse_object(d_nr, code);
            } else if self.data.def_type(d_nr) == DefType::Constant {
                *code = self.data.code(d_nr).clone();
                return self.data.returned(d_nr);
            }
            if let Type::Enum(en) = t {
                for a_nr in 0..self.data.attributes(en) {
                    if self.data.attr_name(en, a_nr) == name {
                        *code = self.data.attr_value(en, a_nr);
                        return self.data.attr_type(en, a_nr);
                    }
                }
            }
        }
        if self.lexer.has_token("(") {
            if name == "sizeof" || name == "alignment" {
                t = self.parse_size(code, &name);
            } else {
                t = self.parse_call(code, &name);
            }
        } else if let Some((var_nr, tp)) = self.types.var_name(&name) {
            if self.lexer.has_token("#") {
                if self.lexer.has_keyword("index") {
                    if let Some((i_nr, i_tp)) = self.types.var_name(&format!("{name}#index")) {
                        t = i_tp;
                        *code = Value::Var(i_nr);
                    } else {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Incorrect #index variable on {}",
                            name
                        );
                        t = Type::Unknown(0)
                    }
                } else {
                    diagnostic!(self.lexer, Level::Error, "Incorrect # variable on {}", name);
                    t = Type::Unknown(0)
                }
            } else {
                t = tp;
                *code = Value::Var(var_nr);
            }
        } else if self.data.def_nr(&name) != u32::MAX {
            let dnr = self.data.def_nr(&name);
            if self.data.def_type(dnr) == DefType::Enum {
                t = Type::Enum(dnr);
            } else if self.data.def_type(dnr) == DefType::EnumValue {
                t = Type::Enum(self.data.def_parent(dnr));
            } else {
                t = Type::Null;
            };
        } else if self.data.def_type(self.context) == DefType::Struct
            && self.data.attr(self.context, &name) != u16::MAX
        {
            let fnr = self.data.attr(self.context, &name);
            *code = self.get_field(self.context, fnr, Value::Var(0));
            t = self.data.attr_type(self.context, fnr);
        } else {
            *code = Value::Var(self.types.create_var(
                name.clone(),
                Type::Unknown(0),
                self.lexer.pos(),
            ));
            t = Type::Unknown(0);
        }
        // TODO move this to general expression code instead of variable specific
        while self.lexer.peek_token(".") || self.lexer.peek_token("[") {
            if !self.first_pass && t.is_unknown() && matches!(code, Value::Var(_)) {
                diagnostic!(self.lexer, Level::Error, "Unknown variable {}", name);
            }
            if self.lexer.has_token(".") {
                t = self.field(code, t)
            } else if self.lexer.has_token("[") {
                t = self.index(code, t);
                self.lexer.token("]");
            }
        }
        t
    }

    fn known_var_or_type(&mut self, tp: &Type, code: &Value) {
        if !self.first_pass && tp.is_unknown() {
            if let Value::Var(nr) = code {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown variable {}",
                    self.types.name(*nr)
                );
            } else {
                diagnostic!(self.lexer, Level::Error, "Undefined type");
            }
        }
    }

    fn first_match(&mut self, val: &mut Value, if_expr: Value, in_type: Type) {
        let var_type;
        if let Type::Vector(t_nr) = &in_type {
            var_type = *t_nr.clone();
        } else if let Type::Sorted(td, _keys) = &in_type {
            var_type = self.data.returned(*td);
        } else {
            panic!("Unknown type {}", self.data.show_type(&in_type))
        }
        let mut create_iter = val.clone();
        let it = Type::Iterator(Box::new(var_type.clone()));
        let iter_next = self.iterator("", &mut create_iter, &in_type, &it);
        if iter_next == Value::Null {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Need an iterable in a match expression"
            );
            return;
        }
        let for_var = if let Some((nr, _)) = self.types.var_name("$") {
            nr
        } else {
            0
        };
        self.var_nr += 1;
        // loop {
        //     for_var = Next(iter_var);
        //     if for_var == null {break}
        //     if if_expr(for_var) {break}
        // }
        // for_var
        *val = Value::Block(vec![
            create_iter,
            Value::Loop(vec![
                v_set(for_var, iter_next),
                v_if(
                    self.op("==", Value::Var(for_var), Value::Null, var_type),
                    Value::Break(0),
                    Value::Null,
                ),
                v_if(if_expr, Value::Break(0), Value::Null),
            ]),
            Value::Var(for_var),
        ]);
    }

    fn parse_string(&mut self, code: &mut Value, string: String) {
        *code = Value::Text(string);
        let mut var = u32::MAX;
        let mut list = vec![];
        if self.lexer.mode() == Mode::Formatting {
            // Define a new variable to append to
            var = self.types.create_var(
                format!("__append_{}_", self.types.next_var()),
                Type::Text,
                self.lexer.pos(),
            );
            list.push(v_let(var, code.clone()));
        }
        while self.lexer.mode() == Mode::Formatting {
            self.lexer.set_mode(Mode::Code);
            let mut format = Value::Null;
            let tp = if self.lexer.has_token("for") {
                let mut for_type = Type::Null;
                let mut init = Value::Null;
                format = self.iter_for(&mut init, &mut for_type);
                list.push(init);
                Type::Iterator(Box::new(for_type))
            } else {
                self.expression(&mut format)
            };
            if !self.first_pass && tp.is_unknown() {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Incorrect expression in string was {tp:?}"
                );
                return;
            }
            self.lexer.set_mode(Mode::Formatting);
            let mut state = OUTPUT_DEFAULT;
            let mut token = "0".to_string();
            if self.lexer.has_token(":") {
                if let LexResult {
                    has: LexItem::Token(t),
                    position: _pos,
                } = self.lexer.peek()
                {
                    let st: &str = &t;
                    if !SKIP_TOKEN.contains(&st) {
                        token.clear();
                        token += &t;
                        state.token = &token;
                        self.lexer.cont();
                    }
                }
                if self.lexer.has_token("<") {
                    state.dir = -1;
                } else if self.lexer.has_token("^") {
                    state.dir = 0;
                } else if self.lexer.has_token(">") {
                    state.dir = 1;
                }
                if self.lexer.has_token("+") {
                    state.plus = true
                }
                if self.lexer.has_token("#") {
                    // show 0x 0b or 0o in front of numbers when applicable
                    state.note = true
                }
                if self.lexer.has_token(".") {
                    state.float = true;
                }
                let LexResult {
                    has: h,
                    position: _pos,
                } = self.lexer.peek();
                if match h {
                    LexItem::Token(st) => {
                        let s: &str = &st;
                        !SKIP_WIDTH.contains(&s)
                    }
                    LexItem::Identifier(st) => {
                        let s: &str = &st;
                        !SKIP_WIDTH.contains(&s)
                    }
                    LexItem::Integer(_, _) => true,
                    LexItem::Float(_) => true,
                    _ => false,
                } {
                    if let LexResult {
                        has: LexItem::Integer(_, true),
                        position: _pos,
                    } = self.lexer.peek()
                    {
                        state.token = "0"
                    }
                    self.lexer.set_mode(Mode::Code);
                    self.expression(&mut state.width);
                    self.lexer.set_mode(Mode::Formatting);
                }
                state.radix = self.get_radix();
            }
            if !self.first_pass {
                self.append_data(tp, &mut list, var, &mut format, state);
            }
            if let Some(text) = self.lexer.has_cstring() {
                if !text.is_empty() {
                    list.push(v_set(
                        var,
                        self.cl("OpAddText", &[Value::Var(var), Value::Text(text)]),
                    ))
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Formatter error");
                return;
            }
        }
        if var < u32::MAX {
            list.push(Value::Var(var));
            *code = Value::Block(list);
        }
    }

    fn get_radix(&mut self) -> i32 {
        if let Some(id) = self.lexer.has_identifier() {
            if id == "x" || id == "X" {
                16
            } else if id == "b" {
                2
            } else if id == "o" {
                8
            } else if id == "e" {
                1
            } else {
                diagnostic!(self.lexer, Level::Error, "Unexpected formatting type: {id}");
                10
            }
        } else {
            10
        }
    }

    // Iterator for: val is the initializing, return Value is the next step
    // <for> ::= <identifier> 'in' <expression> '{' <block>
    fn iter_for(&mut self, val: &mut Value, tp: &mut Type) -> Value {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("in");
            let mut in_expr = Value::Null;
            let in_type = self.expression(&mut in_expr);
            let mut var_type = Type::Unknown(0);
            if let Type::Vector(t_nr) | Type::Iterator(t_nr) = &in_type {
                var_type = *t_nr.clone();
            }
            *tp = var_type.clone();
            let for_var = self
                .types
                .create_var(id.clone(), var_type.clone(), self.lexer.pos());
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let mut create_iter = in_expr;
            let it = Type::Iterator(Box::new(var_type.clone()));
            let iter_next = self.iterator(&id, &mut create_iter, &in_type, &it);
            if iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return Value::Null;
            }
            *val = create_iter;
            let in_loop = self.in_loop;
            self.in_loop = true;
            let block_type = self.parse_block(&mut block, Type::Void);
            self.in_loop = in_loop;
            let res_var =
                self.types
                    .create_var("res".to_string(), block_type.clone(), self.lexer.pos());
            let mut l_steps = vec![
                v_set(for_var, iter_next),
                v_if(
                    self.op("==", Value::Var(for_var), Value::Null, var_type),
                    Value::Break(0),
                    Value::Null,
                ),
            ];
            if if_step != Value::Null {
                l_steps.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            l_steps.push(v_set(res_var, block));
            l_steps.push(v_if(
                self.op("==", Value::Var(res_var), Value::Null, block_type),
                Value::Null,
                Value::Break(0),
            ));
            return Value::Block(vec![
                v_set(res_var, Value::Null),
                Value::Loop(l_steps),
                Value::Var(res_var),
            ]);
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        }
        Value::Null
    }

    fn append_data(
        &mut self,
        tp: Type,
        list: &mut Vec<Value>,
        var: u32,
        format: &mut Value,
        state: OutputState,
    ) {
        let mut append = true;
        match tp {
            Type::Integer => {
                let fmt = format.clone();
                self.call(
                    format,
                    "OpFormatInt",
                    &[
                        fmt,
                        Value::Int(state.radix),
                        state.width,
                        Value::Int(state.token.as_bytes()[0] as i32),
                        Value::Boolean(state.plus),
                        Value::Boolean(state.note),
                    ],
                    &[
                        Type::Integer,
                        Type::Integer,
                        Type::Integer,
                        Type::Integer,
                        Type::Boolean,
                        Type::Boolean,
                    ],
                );
            }
            Type::Long => {
                let fmt = format.clone();
                self.call(
                    format,
                    "OpFormatLong",
                    &[
                        fmt,
                        Value::Int(state.radix),
                        state.width,
                        Value::Int(state.token.as_bytes()[0] as i32),
                        Value::Boolean(state.plus),
                        Value::Boolean(state.note),
                    ],
                    &[
                        Type::Long,
                        Type::Integer,
                        Type::Integer,
                        Type::Integer,
                        Type::Boolean,
                        Type::Boolean,
                    ],
                );
            }
            Type::Boolean => {
                let fmt = format.clone();
                self.call(
                    format,
                    "OpFormatBool",
                    &[
                        fmt,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(state.token.as_bytes()[0] as i32),
                    ],
                    &[Type::Boolean, Type::Integer, Type::Integer, Type::Integer],
                );
            }
            Type::Text => {
                let fmt = format.clone();
                self.call(
                    format,
                    "OpFormatText",
                    &[
                        fmt,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(state.token.as_bytes()[0] as i32),
                    ],
                    &[Type::Text, Type::Integer, Type::Integer, Type::Integer],
                );
            }
            Type::Float => {
                let fmt = format.clone();
                let mut a_width = state.width;
                let mut p_rec = Value::Int(0);
                if let Value::Float(w) = a_width {
                    let s = format!("{w}");
                    let mut split = s.split('.');
                    a_width = Value::Int(split.next().unwrap().parse::<i32>().unwrap());
                    p_rec = Value::Int(split.next().unwrap().parse::<i32>().unwrap());
                }
                if state.float {
                    p_rec = a_width;
                    a_width = Value::Int(0);
                }
                *format = self.cl("OpFormatFloat", &[fmt, a_width, p_rec]);
            }
            Type::Single => {
                let fmt = format.clone();
                let mut p_rec = Value::Int(0);
                let mut a_width = state.width;
                if let Value::Float(w) = a_width {
                    let s = format!("{w}");
                    let mut split = s.split('.');
                    a_width = Value::Int(split.next().unwrap().parse::<i32>().unwrap());
                    p_rec = Value::Int(split.next().unwrap().parse::<i32>().unwrap());
                }
                if state.float {
                    p_rec = a_width;
                    a_width = Value::Int(0);
                }
                self.call(
                    format,
                    "OpFormatSingle",
                    &[fmt, a_width, p_rec],
                    &[Type::Single, Type::Integer, Type::Integer],
                );
            }
            Type::Vector(cont) => {
                let fmt = format.clone();
                let d_nr = self.data.type_def_nr(&cont);
                let db_tp = self.data.def_known_type(d_nr);
                let vec_tp = self.data.known_types.vector(db_tp);
                self.data.check_vector(d_nr, vec_tp, self.lexer.pos());
                self.call(
                    format,
                    "OpFormatDatabase",
                    &[fmt, Value::Int(vec_tp as i32), Value::Boolean(state.note)],
                    &[Type::Reference(d_nr), Type::Integer, Type::Boolean],
                );
            }
            Type::Iterator(vtp) => {
                append = false;
                self.append_iter(list, var, vtp.as_ref(), format.clone());
            }
            Type::Reference(d_nr) => {
                let fmt = format.clone();
                let db_tp = self.data.def_known_type(d_nr);
                self.call(
                    format,
                    "OpFormatDatabase",
                    &[fmt, Value::Int(db_tp as i32), Value::Boolean(state.note)],
                    &[Type::Reference(d_nr), Type::Integer, Type::Boolean],
                );
            }
            Type::Enum(d_nr) => {
                let fmt = format.clone();
                let e_tp = self.data.def_known_type(d_nr);
                let e_val = self.cl("OpCastTextFromEnum", &[fmt, Value::Int(e_tp as i32)]);
                self.call(
                    format,
                    "OpFormatText",
                    &[
                        e_val,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(state.token.as_bytes()[0] as i32),
                    ],
                    &[Type::Text, Type::Integer, Type::Integer, Type::Integer],
                );
            }
            _ => {
                diagnostic!(self.lexer, Level::Error, "Cannot format type {tp}");
                return;
            }
        }
        if append {
            list.push(v_set(
                var,
                self.cl("OpAddText", &[Value::Var(var), format.clone()]),
            ));
        }
    }

    fn append_iter(&mut self, list: &mut Vec<Value>, var: u32, var_type: &Type, next: Value) {
        list.push(v_set(
            var,
            self.cl("OpAddText", &[Value::Var(var), text("[")]),
        ));
        let peek_var = self.types.create_var(
            format!("iter_{}", self.var_nr),
            var_type.clone(),
            self.lexer.pos(),
        );
        self.var_nr += 1;
        let for_var = self.types.create_var(
            format!("iter_{}", self.var_nr),
            var_type.clone(),
            self.lexer.pos(),
        );
        self.var_nr += 1;
        list.push(v_set(peek_var, next.clone()));
        let switch = v_set(for_var, Value::Var(peek_var));
        let next_test = v_if(
            self.op("==", Value::Var(for_var), Value::Null, var_type.clone()),
            Value::Break(0),
            Value::Null,
        );
        let mut steps = vec![switch, v_set(peek_var, next), next_test];
        let mut for_value = Value::Var(for_var);
        self.append_data(
            var_type.clone(),
            &mut steps,
            var,
            &mut for_value,
            OUTPUT_DEFAULT,
        );
        steps.push(v_if(
            self.op("==", Value::Var(peek_var), Value::Null, var_type.clone()),
            Value::Null,
            v_set(var, self.cl("OpAddText", &[Value::Var(var), text(", ")])),
        ));
        list.push(Value::Loop(steps));
        list.push(v_set(
            var,
            self.cl("OpAddText", &[Value::Var(var), text("]")]),
        ));
    }

    // <object> ::= '{' [ <identifier> ':' <expression> { ',' <identifier> ':' <expression> } ] '}'
    fn parse_object(&mut self, td_nr: u32, code: &mut Value) -> Type {
        let link = self.lexer.link();
        if !self.lexer.token("{") {
            return Type::Unknown(0);
        }
        let mut list = vec![];
        let rec_size = Value::Int(self.data.def_size(td_nr) as i32);
        let v = if let Value::Var(v_nr) = code {
            *v_nr
        } else {
            self.types.create_var(
                "val".to_string(),
                self.data.returned(td_nr),
                self.lexer.pos(),
            )
        };
        if !matches!(code, Value::Var(_)) {
            list.push(v_let(
                v,
                if let Value::Reference(_) = code {
                    self.cl(
                        "OpAppend",
                        &[code.clone(), Value::Int(self.data.def_size(td_nr) as i32)],
                    )
                } else {
                    self.data.set_referenced(td_nr, self.context, Value::Null);
                    self.cl("OpDatabase", &[rec_size])
                },
            ));
        }
        let mut found_fields = HashSet::new();
        loop {
            if self.lexer.peek_token("}") {
                break;
            }
            if let Some(field) = self.lexer.has_identifier() {
                if !self.lexer.has_token(":") {
                    self.lexer.revert(link);
                    return Type::Unknown(0);
                }
                let nr = self.data.attr(td_nr, &field);
                if nr != u16::MAX {
                    found_fields.insert(field);
                    let td = self.data.attr_type(td_nr, nr);
                    let pos = self.data.attr_pos(td_nr, nr);
                    let mut value = if let Type::Vector(_) = td {
                        list.push(self.cl(
                            "OpSetInt",
                            &[Value::Var(v), Value::Int(pos as i32), Value::Int(0)],
                        ));
                        self.cl("OpGetField", &[Value::Var(v), Value::Int(pos as i32)])
                    } else {
                        Value::Null
                    };
                    self.expression(&mut value);
                    if let Type::Vector(_) = td {
                        if let Value::Block(ls) = value {
                            for v in ls {
                                list.push(v);
                            }
                        }
                    } else {
                        list.push(self.set_field(td_nr, nr, Value::Var(v), value));
                    }
                } else {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown field {}.{field}",
                        self.data.def_name(td_nr)
                    );
                }
            } else {
                // We have not encountered an identifier
                self.lexer.revert(link);
                return Type::Unknown(0);
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token("}");
        // fill the not mentioned fields with their default value
        for aid in 0..self.data.attributes(td_nr) {
            if found_fields.contains(&self.data.attr_name(td_nr, aid))
                || !self.data.attr_mutable(td_nr, aid)
            {
                continue;
            }
            let mut default = self.data.attr_value(td_nr, aid);
            if default == Value::Null {
                default = to_default(&self.data.attr_type(td_nr, aid))
            }
            list.push(self.set_field(td_nr, aid, Value::Var(v), default));
        }
        if !matches!(code, Value::Var(_)) {
            list.push(Value::Var(v));
        }
        *code = Value::Block(list);
        Type::Reference(td_nr)
    }

    // <if> ::= <expression> '{' <block> [ 'else' ( 'if' <if> | '{' <block> ) ]
    fn parse_if(&mut self, code: &mut Value) -> Type {
        let mut test = Value::Null;
        self.expression(&mut test);
        self.lexer.token("{");
        let mut true_code = Value::Null;
        let true_type = self.parse_block(&mut true_code, Type::Void);
        let mut return_type = true_type.clone();
        let mut false_code = Value::Null;
        if self.lexer.has_token("else") {
            let false_type = if self.lexer.has_token("if") {
                self.parse_if(&mut false_code)
            } else {
                self.lexer.token("{");
                self.parse_block(&mut false_code, Type::Void)
            };
            if !self.convert(&mut false_code, &false_type, &true_type) {
                if !self.convert(&mut true_code, &true_type, &false_type) {
                    self.validate_convert("if", &false_type, &true_type)
                }
                return_type = false_type.clone();
            }
        }
        *code = v_if(test, true_code, false_code);
        return_type
    }

    // <for> ::= <identifier> 'in' <expression> '{' <block>
    fn parse_for(&mut self, val: &mut Value) {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("in");
            let mut in_expr = Value::Null;
            let in_type = self.expression(&mut in_expr);
            let var_tp = if let Type::Vector(t_nr) = &in_type {
                *t_nr.clone()
            } else if let Type::Sorted(dnr, _) = in_type {
                Type::Reference(dnr)
            } else if let Type::Iterator(i_tp) = &in_type {
                if **i_tp == Type::Null {
                    Type::Integer
                } else {
                    *i_tp.clone()
                }
            } else if in_type == Type::Text {
                Type::Text
            } else {
                if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown in expression type {}",
                        self.data.show_type(&in_type)
                    );
                }
                Type::Null
            };
            let for_var = self
                .types
                .create_var(id.clone(), var_tp.clone(), self.lexer.pos());
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let mut create_iter = in_expr;
            let it = Type::Iterator(Box::new(var_tp.clone()));
            let iter_next = self.iterator(&id, &mut create_iter, &in_type, &it);
            if iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return;
            }
            let in_loop = self.in_loop;
            self.in_loop = true;
            self.parse_block(&mut block, Type::Void);
            self.in_loop = in_loop;
            let mut for_steps = vec![create_iter];
            let for_next = v_set(for_var, iter_next);
            let mut lp = vec![for_next];
            let test_for = self.op("==", Value::Var(for_var), Value::Null, var_tp);
            lp.push(v_if(test_for, Value::Break(0), Value::Null));
            if if_step != Value::Null {
                lp.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            lp.push(block);
            for_steps.push(Value::Loop(lp));
            *val = Value::Block(for_steps);
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        }
    }

    // <return> ::= [ <expression> ]
    fn parse_return(&mut self, val: &mut Value) {
        // validate if there is a defined return value
        let mut v = Value::Null;
        let r_type = self.data.returned(self.context);
        if !self.lexer.peek_token(";") && !self.lexer.peek_token("}") {
            let t = self.expression(&mut v);
            if r_type == Type::Void {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect no expression after return"
                );
                *val = Value::Return(Box::new(Value::Null));
                return;
            }
            if !self.convert(val, &t, &r_type) {
                self.validate_convert("return", &t, &r_type);
            }
        } else if !self.first_pass && r_type != Type::Void {
            diagnostic!(self.lexer, Level::Error, "Expect expression after return")
        }
        *val = Value::Return(Box::new(v));
    }

    // <call> ::= [ <expression> { ',' <expression> } ] ')'
    fn parse_call(&mut self, val: &mut Value, name: &str) -> Type {
        let mut list = Vec::new();
        let mut types = Vec::new();
        if self.lexer.has_token(")") {
            return self.call(val, name, &list, &Vec::new());
        }
        loop {
            let mut p = Value::Null;
            types.push(self.expression(&mut p));
            list.push(p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        self.call(val, name, &list, &types)
    }

    // <size> ::= ( <type> | <var> ) ')'
    fn parse_size(&mut self, val: &mut Value, name: &str) -> Type {
        let sizeof = name == "sizeof";
        let mut found = false;
        let lnk = self.lexer.link();
        if let Some(id) = self.lexer.has_identifier() {
            let d_nr = self.data.def_nr(&id);
            if d_nr != u32::MAX && self.data.def_type(d_nr) != DefType::EnumValue {
                if !self.first_pass && self.data.def_type(d_nr) == DefType::Unknown {
                    found = true;
                } else if let Some(tp) = self.parse_type(u32::MAX, &id) {
                    found = true;
                    if !self.first_pass {
                        if sizeof {
                            *val = Value::Int(self.data.def_size(self.data.type_elm(&tp)) as i32);
                        } else {
                            *val = Value::Int(self.data.def_align(self.data.type_elm(&tp)) as i32);
                        }
                    }
                }
            }
        }
        if !found {
            let mut drop = Value::Null;
            self.lexer.revert(lnk);
            let tp = self.expression(&mut drop);
            // TODO FEA0002 call function to get size or alignment of correct child
            let e_tp = self.data.type_elm(&tp);
            if e_tp != u32::MAX {
                found = true;
                if sizeof {
                    *val = Value::Int(self.data.def_size(e_tp) as i32);
                } else {
                    *val = Value::Int(self.data.def_align(e_tp) as i32);
                }
            }
        }
        if !self.first_pass && !found {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect a variable or type after {name}"
            );
        }
        self.lexer.token(")");
        Type::Integer
    }

    // <call> ::= [ <expression> { ',' <expression> } ] ')'
    fn parse_method(&mut self, val: &mut Value, md_nr: u32, on: Type) -> Type {
        let mut list = vec![val.clone()];
        let mut types = vec![on];
        if self.lexer.has_token(")") {
            return self.call_nr(val, md_nr, &list, &types, true);
        }
        loop {
            let mut p = Value::Null;
            types.push(self.expression(&mut p));
            list.push(p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        self.call_nr(val, md_nr, &list, &types, true)
    }

    fn parse_parameters(&mut self) -> (Vec<Type>, Vec<Value>) {
        let mut list = vec![];
        let mut types = vec![];
        if self.lexer.has_token(")") {
            return (types, list);
        }
        loop {
            let mut p = Value::Null;
            types.push(self.expression(&mut p));
            list.push(p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        (types, list)
    }
}
