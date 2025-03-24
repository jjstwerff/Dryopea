// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Parse scripts and create internal code from it.
//! Including type checking.
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

use crate::data::{Argument, Data, DefType, I32, Type, Value, to_default, v_if, v_let, v_set};
use crate::database::{Parts, Stores};
use crate::diagnostics::{Diagnostics, Level, diagnostic_format};
use crate::lexer::{LexItem, LexResult, Lexer, Mode};
use crate::typedef;
use crate::variables::Function;
use std::collections::{BTreeSet, HashSet};
use std::fs::{File, metadata, read_dir};
use std::io::BufReader;
use std::io::prelude::BufRead;
use std::string::ToString;
use typedef::complete_definition;

/**
The amount of defined reserved text worker variables. A worker variable is needed when
two texts are added or a formatting text is used and the result is used as a parameter to a call.
These are reused when possible, however when in calculating a text value a new text expression
is used a next worker variable is needed.
This number indicated the depth of these expressions not the amount of these expressions in a
function.
*/
pub struct Parser {
    /// All definitions
    pub data: Data,
    pub database: Stores,
    /// The lexer on the current text file
    pub lexer: Lexer,
    /// Are we currently allowing break/continue statements
    in_loop: bool,
    /// The current file number that is being parsed
    file: u32,
    pub diagnostics: Diagnostics,
    default: bool,
    /// The definition that is currently parsed (function or struct)
    context: u32,
    /// Is this the first pass on parsing:
    /// - do not assume that all struct / enum types are already parsed
    /// - define variables, try to determine their type (can become clear from later code)
    /// - claim working text variables for expressions that gather text data outside variables
    /// - links between memory allocations (text, stores) their type knows the variable numbers
    /// - move variables to a lower scope if an expression still links to their content
    /// - determine mutations to stores and administer these in arguments
    ///
    /// The second pass:
    /// - creates code, assumes that all types are known
    /// - frees allocations when variables run out of scope
    first_pass: bool,
    vars: Function,
}

// Operators ordered on their precedence
static OPERATORS: &[&[&str]] = &[
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

impl Default for Parser {
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

impl Parser {
    #[must_use]
    pub fn new() -> Self {
        Parser {
            data: Data::new(),
            database: Stores::new(),
            lexer: Lexer::default(),
            in_loop: false,
            file: 1,
            diagnostics: Diagnostics::new(),
            default: false,
            context: u32::MAX,
            first_pass: true,
            vars: Function::new(),
        }
    }

    /// Parse the content of a given file.
    /// default: parsing system definitions
    /// # Panics
    /// With filesystem problems.
    pub fn parse(&mut self, filename: &str, default: bool) -> bool {
        self.default = default;
        self.vars.logging = false;
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
    /// # Errors
    /// With filesystem problems.
    pub fn parse_dir(&mut self, dir: &str, default: bool) -> std::io::Result<()> {
        let paths = read_dir(dir)?;
        let mut files: BTreeSet<String> = BTreeSet::new();
        for path in paths {
            let p = path?;
            let own_file = p
                .path()
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("gcp"));
            let file_name = p.path().to_string_lossy().to_string();
            let data = metadata(&file_name)?;
            if own_file || data.is_dir() {
                files.insert(file_name);
            }
        }
        for f in files {
            let data = metadata(&f)?;
            if data.is_dir() {
                self.parse_dir(&f, default)?;
            } else if !self.parse(&f, default) {
                for l in self.diagnostics.lines() {
                    println!("{l}");
                }
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("{}", self.diagnostics),
                ));
            }
        }
        Ok(())
    }

    /// Only parse a specific string, only useful for parser tests.
    #[allow(dead_code)]
    pub fn parse_str(&mut self, text: &str, filename: &str, logging: bool) {
        self.first_pass = true;
        self.default = false;
        self.vars.logging = logging;
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
    /// Return the next expression; with `Value::None` the iterator creations was impossible.
    fn iterator(
        &mut self,
        code: &mut Value,
        is_type: &Type,
        should: &Type,
        iter_var: u16,
    ) -> Value {
        if let Value::Iter(start, next) = code.clone() {
            let nx = self.vars.start_scope(self.lexer.at(), "for next");
            let result = self.vars.tp(iter_var).clone();
            if let Value::Block(v) = &*next {
                if contains_block(v) {
                    let bl = self.vars.start_scope(self.lexer.at(), "for next block");
                    self.vars.finish_scope(bl, &result, self.lexer.at());
                }
                let mut tp = is_type.clone();
                if matches!(tp, Type::Iterator(_, _)) {
                    tp = self.vars.tp(iter_var).clone();
                }
                self.vars.finish_scope(nx, &tp, self.lexer.at());
                *code = *start;
                return Value::Block(v.clone());
            }
            panic!("Incorrect Iter");
        }
        if matches!(*is_type, Type::Text(_)) {
            let nx = self.vars.start_scope(self.lexer.at(), "for text next");
            let res_var = self.create_unique("tres", is_type);
            let i = Value::Var(iter_var);
            let ref_expr = self.cl(
                "OpGetTextSub",
                &[code.clone(), i.clone(), Value::Int(i32::MIN)],
            );
            let res_len = self.cl("OpLengthText", &[Value::Var(res_var)]);
            let next = vec![
                v_let(res_var, ref_expr),
                v_set(iter_var, self.op("Add", i.clone(), res_len, I32.clone())),
                Value::Var(res_var),
            ];
            *code = v_let(iter_var, Value::Int(0));
            self.vars.finish_scope(nx, is_type, self.lexer.at());
            return Value::Block(next);
        }
        if is_type == should {
            // there was already an iterator.
            let orig = code.clone();
            *code = Value::Null; // there is no iterator to create, we got it already
            return orig;
        }
        if let Type::Iterator(_, _) = should {
            match is_type {
                Type::Vector(tp, _) => {
                    let i = Value::Var(iter_var);
                    let vec_tp = self.data.type_def_nr(tp);
                    let size = self.database.size(self.data.def(vec_tp).known_type);
                    let mut ref_expr = self.cl(
                        "OpGetVector",
                        &[code.clone(), Value::Int(i32::from(size)), i.clone()],
                    );
                    if let Type::Reference(_, _) = *tp.clone() {
                    } else {
                        ref_expr = self.get_field(vec_tp, usize::MAX, ref_expr);
                    }
                    let nx = self.vars.start_scope(self.lexer.at(), "iter next");
                    let res_var = self.create_unique("res", tp);
                    let next = vec![
                        v_let(res_var, ref_expr),
                        v_set(
                            iter_var,
                            self.op("Add", i.clone(), Value::Int(1), I32.clone()),
                        ),
                        Value::Var(res_var),
                    ];
                    self.vars.finish_scope(nx, tp, self.lexer.at());
                    let len = self.cl("OpLengthVector", &[code.clone()]);
                    *code = v_let(iter_var, Value::Int(0));
                    return v_if(
                        self.op("Ge", i, len, I32.clone()),
                        self.null(&I32),
                        Value::Block(next),
                    );
                }
                Type::Sorted(_, _, _)
                | Type::Hash(_, _, _)
                | Type::Index(_, _, _)
                | Type::Spacial(_, _, _) => {
                    let known = if self.first_pass {
                        Value::Null
                    } else {
                        self.type_info(is_type)
                    };
                    let iter_expr =
                        self.cl("OpStart", &[code.clone(), known.clone(), Value::Int(0)]);
                    let next_expr = self.cl(
                        "OpNext",
                        &[code.clone(), Value::Var(iter_var), known, Value::Int(0)],
                    );
                    *code = v_let(iter_var, iter_expr);
                    return next_expr;
                }
                _ => {
                    panic!("Unknown iterator type {}", is_type.name(&self.data));
                }
            }
        }
        Value::Null
    }

    /// Convert a type to another type when possible
    /// Returns false when impossible. However, the other way round might still be possible.
    fn convert(&mut self, code: &mut Value, is_type: &Type, should: &Type) -> bool {
        if is_type.is_equal(should) {
            return true;
        }
        if let Type::RefVar(ref_tp) = is_type {
            if ref_tp.is_equal(should) {
                return true;
            }
        }
        if let Type::RefVar(ref_tp) = should {
            if ref_tp.is_equal(is_type) {
                *code = self.cl("OpCreateRef", &[code.clone()]);
                return true;
            }
        }
        let mut check_type = is_type;
        let r = Type::Reference(self.data.def_nr("reference"), Vec::new());
        if let Type::Vector(_nr, _) = is_type {
            if let Type::Vector(v, _) = should {
                if v.is_unknown() {
                    return true;
                }
            }
        } else if let Type::Reference(_, _) = is_type {
            if matches!(*should, Type::Reference(0, _)) {
                return true;
            }
            check_type = &r;
        } else if let Type::Enum(_) = is_type {
            if *should == Type::Enum(0) {
                return true;
            }
            check_type = &Type::Enum(0);
        }
        for &dnr in self.data.get_possible("OpConv", &self.lexer) {
            if self.data.def(dnr).name.ends_with("FromNull") {
                if *is_type == Type::Null {
                    if self.data.def(dnr).returned == *should {
                        *code = Value::Call(dnr, vec![]);
                        return true;
                    } else if matches!(self.data.def(dnr).returned, Type::Reference(_, _)) {
                        if let Type::Reference(_, _) = *should {
                            *code = Value::Call(dnr, vec![]);
                            return true;
                        }
                    }
                }
                continue;
            } else if self.data.attributes(dnr) > 0
                && self.data.attr_type(dnr, 0).is_equal(check_type)
                && self.data.def(dnr).returned == *should
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
        if let Type::Vector(c_tp, _) = should {
            let c_nr = self.data.type_def_nr(c_tp);
            let tp = self.database.vector(self.data.def(c_nr).known_type);
            should_nr = self.data.check_vector(c_nr, tp, self.lexer.pos());
        }
        let should_kt = if should_nr == u32::MAX {
            u16::MAX
        } else {
            self.data.def(should_nr).known_type
        };
        let is_nr = self.data.type_def_nr(is_type);
        let is_kt = if is_nr == u32::MAX {
            u16::MAX
        } else {
            self.data.def(is_nr).known_type
        };
        for &dnr in self.data.get_possible("OpCast", &self.lexer) {
            if self.data.attributes(dnr) == 1
                && self.data.attr_type(dnr, 0).is_same(is_type)
                && self.data.def(dnr).returned.is_same(should)
            {
                if let Type::Enum(tp) = should {
                    *code = Value::Call(
                        dnr,
                        vec![
                            code.clone(),
                            Value::Int(i32::from(self.data.def(*tp).known_type)),
                        ],
                    );
                } else {
                    *code = Value::Call(dnr, vec![code.clone()]);
                }
                return true;
            } else if self.data.attributes(dnr) == 2
                && self.data.attr_type(dnr, 0).is_same(is_type)
                && self.data.def(dnr).returned.is_same(should)
                && should_kt != u16::MAX
            {
                *code = Value::Call(dnr, vec![code.clone(), Value::Int(i32::from(should_kt))]);
                return true;
            } else if self.data.attributes(dnr) == 2
                && self.data.attr_type(dnr, 0).is_same(is_type)
                && self.data.def(dnr).returned.is_same(should)
                && is_kt != u16::MAX
            {
                *code = Value::Call(dnr, vec![code.clone(), Value::Int(i32::from(is_kt))]);
                return true;
            }
        }
        false
    }

    /// Validate that two types are equal, show an error when impossible
    fn can_convert(&mut self, test_type: &Type, should: &Type) -> bool {
        if *test_type != *should && !test_type.is_unknown() {
            if let Type::RefVar(tp) = should {
                if tp.is_equal(test_type) {
                    return true;
                }
            }
            if let (Type::Enum(_e), Type::Enum(o)) = (test_type, should) {
                if self.data.def(*o).name == "enumerate" {
                    return true;
                }
            }
            if let Type::Reference(r, _) = should {
                if *r == self.data.def_nr("reference") {
                    if let Type::Reference(_, _) = test_type {
                        return true;
                    }
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
                &res,
                Level::Error,
                "{} should be {} on {context}",
                test_type.name(&self.data),
                should.name(&self.data)
            );
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

    fn single_op(&mut self, op: &str, f: Value, t: Type) -> Value {
        let mut code = Value::Null;
        self.call_op(&mut code, op, &[f], &[t]);
        code
    }

    fn conv_op(&mut self, op: &str, f: Value, n: Value, f_tp: Type, n_tp: Type) -> Value {
        let mut code = Value::Null;
        self.call_op(&mut code, op, &[f, n], &[f_tp, n_tp]);
        code
    }

    fn op(&mut self, op: &str, f: Value, n: Value, t: Type) -> Value {
        let mut code = Value::Null;
        self.call_op(&mut code, op, &[f, n], &[t.clone(), t]);
        code
    }

    fn get_field(&mut self, d_nr: u32, f_nr: usize, code: Value) -> Value {
        let tp = self.data.attr_type(d_nr, f_nr);
        let pos = if f_nr == usize::MAX {
            0
        } else {
            self.database
                .position(self.data.def(d_nr).known_type, f_nr as u16)
        };
        self.get_val(
            &tp,
            self.data.attr_nullable(d_nr, f_nr),
            u32::from(pos),
            code,
        )
    }

    fn get_val(&mut self, tp: &Type, nullable: bool, pos: u32, code: Value) -> Value {
        let p = Value::Int(pos as i32);
        match tp {
            Type::Integer(min, _) => {
                let s = tp.size(nullable);
                if s == 1 {
                    self.cl("OpGetByte", &[code, p, Value::Int(*min)])
                } else if s == 2 {
                    self.cl("OpGetShort", &[code, p, Value::Int(*min)])
                } else {
                    self.cl("OpGetInt", &[code, p])
                }
            }
            Type::Enum(_) => self.cl("OpGetEnum", &[code, p]),
            Type::Boolean => {
                let val = self.cl("OpGetByte", &[code, p, Value::Int(0)]);
                self.cl("OpEqInt", &[val, Value::Int(1)])
            }
            Type::Long => self.cl("OpGetLong", &[code, p]),
            Type::Float => self.cl("OpGetFloat", &[code, p]),
            Type::Single => self.cl("OpGetSingle", &[code, p]),
            Type::Text(_) => self.cl("OpGetText", &[code, p]),
            Type::Hash(_, _, _)
            | Type::Sorted(_, _, _)
            | Type::Spacial(_, _, _)
            | Type::Index(_, _, _)
            | Type::Vector(_, _) => self.cl("OpGetField", &[code, p, self.type_info(tp)]),
            _ => panic!(
                "Get not implemented on '{}' at {}",
                tp.name(&self.data),
                self.lexer.pos()
            ),
        }
    }

    fn set_field(&mut self, d_nr: u32, f_nr: usize, ref_code: Value, val_code: Value) -> Value {
        let tp = self.data.attr_type(d_nr, f_nr);
        let pos = self
            .database
            .position(self.data.def(d_nr).known_type, f_nr as u16);
        let pos_val = Value::Int(if f_nr == usize::MAX {
            0
        } else {
            i32::from(pos)
        });
        match tp {
            Type::Integer(min, _) => {
                let m = Value::Int(min);
                let s = tp.size(self.data.attr_nullable(d_nr, f_nr));
                if s == 1 {
                    self.cl("OpSetByte", &[ref_code, pos_val, m, val_code])
                } else if s == 2 {
                    self.cl("OpSetShort", &[ref_code, pos_val, m, val_code])
                } else {
                    self.cl("OpSetInt", &[ref_code, pos_val, val_code])
                }
            }
            Type::Vector(_, _)
            | Type::Hash(_, _, _)
            | Type::Index(_, _, _)
            | Type::Spacial(_, _, _)
            | Type::Reference(_, _)
            | Type::Sorted(_, _, _) => self.cl("OpSetInt", &[ref_code, pos_val, val_code]),
            Type::Enum(_) => self.cl("OpSetEnum", &[ref_code, pos_val, val_code]),
            Type::Boolean => {
                let v = v_if(val_code, Value::Int(1), Value::Int(0));
                self.cl("OpSetByte", &[ref_code, pos_val, Value::Int(0), v])
            }
            Type::Long => self.cl("OpSetLong", &[ref_code, pos_val, val_code]),
            Type::Float => self.cl("OpSetFloat", &[ref_code, pos_val, val_code]),
            Type::Single => self.cl("OpSetSingle", &[ref_code, pos_val, val_code]),
            Type::Text(_) => self.cl("OpSetText", &[ref_code, pos_val, val_code]),
            _ => {
                if self.first_pass {
                    Value::Null
                } else {
                    panic!(
                        "Set not implemented on {}/{} at {}",
                        self.data.attr_name(d_nr, f_nr),
                        self.data.attr_type(d_nr, f_nr).name(&self.data),
                        self.lexer.pos()
                    )
                }
            }
        }
    }

    fn cl(&mut self, op: &str, list: &[Value]) -> Value {
        let d_nr = self.data.def_nr(op);
        if d_nr == u32::MAX {
            diagnostic!(self.lexer, Level::Error, "Call to unknown {op}");
            Value::Null
        } else {
            Value::Call(d_nr, list.to_vec())
        }
    }

    /// Try to find a matching defined operator. There can be multiple possible definitions for each operator.
    fn call_op(&mut self, code: &mut Value, op: &str, list: &[Value], types: &[Type]) -> Type {
        let mut possible = Vec::new();
        for pos in self
            .data
            .get_possible(&format!("Op{}", rename(op)), &self.lexer)
        {
            possible.push(*pos);
        }
        for pos in possible {
            let tp = self.call_nr(code, pos, list, types, false);
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
                &self.lexer.peek(),
                Level::Error,
                "No matching operator '{op}' on '{}' and '{}'",
                types[0].name(&self.data),
                types[1].name(&self.data)
            );
        } else {
            specific!(
                self.lexer,
                &self.lexer.peek(),
                Level::Error,
                "No matching operator {op} on {}",
                types[0].name(&self.data)
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
        if self.data.def_type(d_nr) == DefType::Dynamic {
            for a_nr in 0..self.data.attributes(d_nr) {
                let Type::Routine(r_nr) = self.data.attr_type(d_nr, a_nr) else {
                    panic!("Incorrect Dynamic function {}", self.data.def(d_nr).name);
                };
                if self.data.attr_type(r_nr, 0).is_equal(&types[0]) {
                    return self.call_nr(code, r_nr, list, types, report);
                }
            }
            diagnostic!(
                self.lexer,
                Level::Error,
                "No matching function {}",
                self.data.def(d_nr).name
            );
        } else if !matches!(self.data.def_type(d_nr), DefType::Function) {
            if report {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown definition {}",
                    self.data.def(d_nr).name
                );
            }
            return Type::Null;
        }
        let mut actual: Vec<Value> = Vec::new();
        if !types.is_empty() {
            if list.len() > self.data.attributes(d_nr) {
                if report {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Too many parameters for {}",
                        self.data.def(d_nr).name
                    );
                }
                return Type::Null;
            }
            for (nr, a_code) in list.iter().enumerate() {
                let tp = self.data.attr_type(d_nr, nr);
                if let Some(actual_type) = types.get(nr) {
                    let mut actual_code = a_code.clone();
                    // When encountered a subtype reference, find the actual corresponding type
                    if let (Type::Vector(to_tp, _), Type::Vector(a_tp, _)) = (&tp, actual_type) {
                        if a_tp.is_unknown() && !to_tp.is_unknown() {
                            self.change_var_type(&actual_code, &tp);
                            actual.push(actual_code);
                            continue;
                        }
                    }
                    if actual_type.is_unknown() {
                        if let Type::Vector(_, _) = &tp {
                            self.change_var_type(&actual_code, &tp);
                            actual.push(actual_code);
                            continue;
                        }
                    }
                    if !self.convert(&mut actual_code, actual_type, &tp) {
                        if report {
                            let context = format!("call to {}", self.data.def(d_nr).name);
                            self.validate_convert(&context, actual_type, &tp);
                        } else if !self.can_convert(actual_type, &tp) {
                            return Type::Null;
                        }
                    }
                    actual.push(actual_code);
                }
            }
        }
        self.add_defaults(d_nr, &mut actual);
        let tp = self.call_dependencies(d_nr, types);
        *code = Value::Call(d_nr, actual);
        tp
    }

    // Gather depended on variables from arguments of the given called routine.
    fn call_dependencies(&mut self, d_nr: u32, types: &[Type]) -> Type {
        let mut tp = self.data.def(d_nr).returned.clone();
        if let Type::Text(d) = &tp {
            let mut dp = HashSet::new();
            for ar in d {
                if *ar as usize >= types.len() {
                    continue;
                }
                if let Type::Text(ad) = &types[*ar as usize] {
                    for a in ad {
                        dp.insert(*a);
                    }
                }
            }
            if !dp.is_empty() {
                tp = Type::Text(Vec::from_iter(dp));
            }
        }
        tp
    }

    fn add_defaults(&mut self, d_nr: u32, actual: &mut Vec<Value>) {
        if actual.len() < self.data.attributes(d_nr) {
            // Insert the default values for not given attributes
            for a_nr in actual.len()..self.data.attributes(d_nr) {
                let default = self.data.def(d_nr).attributes[a_nr].value.clone();
                if let Type::RefVar(tp) = self.data.attr_type(d_nr, a_nr) {
                    if matches!(*tp, Type::Text(_)) {
                        let ref_scope = self.vars.start_scope(self.lexer.at(), "default ref");
                        let mut ls = Vec::new();
                        let vr = self.vars.work(&mut self.lexer);
                        ls.push(self.cl("OpAppendText", &[Value::Var(vr), default]));
                        ls.push(self.cl("OpCreateRef", &[Value::Var(vr)]));
                        actual.push(Value::Block(ls));
                        self.vars.finish_scope(
                            ref_scope,
                            &Type::Reference(self.data.def_nr("reference"), vec![vr]),
                            self.lexer.at(),
                        );
                    } else {
                        panic!("No defaults for references allowed");
                    }
                } else {
                    actual.push(default);
                }
            }
        }
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
            diagnostic!(self.lexer, Level::Fatal, "Syntax error");
        };
        if self.first_pass {
            typedef::actual_types(
                &mut self.data,
                &mut self.database,
                &mut self.lexer,
                start_def,
            );
            typedef::fill_all(&mut self.data, &mut self.database, start_def);
            self.database.finish();
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
            d_nr = self.data.add_def(&type_name, pos, DefType::Enum);
        } else if self.first_pass && self.data.def_type(d_nr) == DefType::Unknown {
            self.data.definitions[d_nr as usize].def_type = DefType::Enum;
            self.data.definitions[d_nr as usize].position = self.lexer.pos().clone();
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
                    .add_def(&value_name, self.lexer.pos(), DefType::EnumValue)
            } else {
                self.data.def_nr(&value_name)
            };
            if self.first_pass {
                self.data.set_returned(v_nr, Type::Enum(d_nr));
                self.data
                    .add_attribute(&mut self.lexer, d_nr, &value_name, Type::Enum(d_nr));
                // Enum values start with 1 as 0 is de null/undefined value.
                self.data
                    .set_attr_value(d_nr, nr as usize, Value::Enum(nr + 1, u16::MAX));
            }
            if !self.lexer.has_token(",") {
                break;
            }
            if nr == 255 {
                self.lexer
                    .diagnostic(Level::Error, "Too many enumerate values");
                break;
            }
            nr += 1;
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
                .add_def(&type_name, self.lexer.pos(), DefType::Type)
        } else {
            self.data.def_nr(&type_name)
        };
        if self.lexer.has_token("=") {
            if let Some(type_name) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &type_name, false) {
                    if self.first_pass {
                        self.data.set_returned(d_nr, tp);
                    }
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
                let c_nr = self.data.add_def(&id, self.lexer.pos(), DefType::Constant);
                self.data.set_returned(c_nr, tp);
                self.data.definitions[c_nr as usize].code = val;
            }
            self.lexer.token(";");
            true
        } else {
            false
        }
    }

    pub fn create_var(&mut self, name: &str, var_type: &Type) -> u16 {
        if self.context == u32::MAX {
            return u16::MAX;
        }
        self.vars.add_variable(name, var_type, &mut self.lexer)
    }

    fn create_unique(&mut self, name: &str, var_type: &Type) -> u16 {
        self.vars.unique(name, var_type, &mut self.lexer)
    }

    fn var_usages(&mut self, vnr: u16, plus: bool) {
        if vnr == u16::MAX {
            return;
        }
        if plus {
            self.vars.in_use(vnr, true);
        } else if self.vars.uses(vnr) > 0 {
            self.vars.in_use(vnr, false);
        }
    }

    // <function> ::= 'fn' <identifier>[ '(' <attributes> ] [ '->' <type> ] (';' <rust> | <code>)
    fn parse_function(&mut self) -> bool {
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
        if self.data.def_names.contains_key(&fn_name)
            && self.first_pass
            && self.data.def_name(&fn_name).def_type != DefType::Dynamic
        {
            diagnostic!(
                self.lexer,
                Level::Fatal,
                "Cannot redefine {} {fn_name}",
                self.data.def_name(&fn_name).def_type
            );
            return false;
        }
        if !self.default && !is_lower(&fn_name) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect function names to be in lower case style"
            );
        }
        let mut arguments = Vec::new();
        if self.lexer.token("(") {
            if !self.parse_arguments(&fn_name, &mut arguments) {
                return true;
            }
            self.lexer.token(")");
        }
        self.context = if self.default && self.first_pass && is_op(&fn_name) {
            self.data.add_op(&mut self.lexer, &fn_name, &arguments)
        } else if self.first_pass {
            self.data.add_fn(&mut self.lexer, &fn_name, &arguments)
        } else {
            self.data.get_fn(&fn_name, &arguments)
        };
        let result = if self.lexer.has_token("->") {
            // Will be the correct def_nr on the second pass
            if let Some(type_name) = self.lexer.has_identifier() {
                let Some(tp) = self.parse_type(self.data.def_nr(&fn_name), &type_name, true) else {
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
        self.vars
            .append(&mut self.data.definitions[self.context as usize].variables);
        /*
        if !self.default {
            self.vars.logging = true;
        }*/
        if self.first_pass && self.context != u32::MAX {
            self.data.set_returned(self.context, result);
        }
        if !self.lexer.has_token(";") {
            let hd = self
                .vars
                .start_scope(self.lexer.at(), &format!("function {fn_name} header"));
            for (v_nr, a) in arguments.iter().enumerate() {
                if self.first_pass {
                    let v_nr = self.create_var(&a.name, &a.typedef);
                    self.var_usages(v_nr, false);
                } else {
                    self.vars
                        .change_var_type(v_nr as u16, &a.typedef, &self.data, &mut self.lexer);
                }
            }
            let t = self.parse_code();
            self.vars.finish_scope(hd, &t, self.lexer.at());
        }
        if !self.first_pass && self.context != u32::MAX {
            self.vars.test_used(&mut self.lexer, &self.data);
        }
        self.lexer.has_token(";");
        self.parse_rust();
        self.data.definitions[self.context as usize]
            .variables
            .append(&mut self.vars);
        self.context = u32::MAX;
        true
    }

    // <rust> ::= { '#rust' <string> | '#iterator' <string> <string> }
    fn parse_rust(&mut self) {
        while self.default && self.lexer.has_token("#") {
            let id = self.lexer.has_identifier();
            if id == Some("rust".to_string()) {
                if let Some(c) = self.lexer.has_cstring() {
                    self.data.definitions[self.context as usize].rust = c;
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect rust string");
                }
            }
            if id == Some("iterator".to_string()) {
                if let Some(init) = self.lexer.has_cstring() {
                    self.data.definitions[self.context as usize].rust = init;
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect rust init string");
                }
                if let Some(next) = self.lexer.has_cstring() {
                    self.data.definitions[self.context as usize].rust += "#";
                    self.data.definitions[self.context as usize].rust += &next;
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect rust next string");
                }
            }
        }
    }

    fn parse_arguments(&mut self, fn_name: &str, arguments: &mut Vec<Argument>) -> bool {
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
            for a in arguments.iter() {
                if attr_name == a.name {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Double attribute '{fn_name}.{attr_name}'"
                    );
                }
            }
            let mut constant = false;
            let mut reference = false;
            let typedef = if self.lexer.has_token(":") {
                if self.lexer.has_token("&") {
                    reference = true;
                }
                // Will be the correct def_nr on the second pass
                if self.lexer.has_token("fn") {
                    self.parse_fn_type(self.data.def_nr(fn_name))
                } else {
                    if self.lexer.has_keyword("const") {
                        constant = true;
                    }
                    if let Some(type_name) = self.lexer.has_identifier() {
                        if let Some(tp) =
                            self.parse_type(self.data.def_nr(fn_name), &type_name, false)
                        {
                            if reference {
                                Type::RefVar(Box::new(tp))
                            } else {
                                tp
                            }
                        } else if !self.first_pass {
                            panic!("Incorrect handling of unknown types");
                        } else {
                            Type::Unknown(0)
                        }
                    } else {
                        diagnostic!(self.lexer, Level::Error, "Expecting a type");
                        return true;
                    }
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
            if !self.first_pass
                && typedef.is_unknown()
                && val == Value::Null
                && (!self.default || !matches!(typedef, Type::Vector(_, _)))
            {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expecting a clear type, found {}",
                    typedef.name(&self.data)
                );
            }
            (*arguments).push(Argument {
                name: attr_name,
                typedef,
                default: val,
                constant,
            });
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
                if let Some(tp) = self.parse_type(d_nr, &id, false) {
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
                if let Some(tp) = self.parse_type(d_nr, &id, false) {
                    r_type = tp;
                }
            }
        }
        Type::Function(args, Box::new(r_type))
    }

    // <type> ::= <identifier> [ '<' ( <sub_type> | <type> ) '>' ] [ '[' ( <nr> { ',' <nr> } ']' ]
    fn parse_type(&mut self, on_d: u32, type_name: &str, returned: bool) -> Option<Type> {
        let tp_nr = self.data.def_nr(type_name);
        if self.first_pass && tp_nr == u32::MAX {
            let u_nr = self
                .data
                .add_def(type_name, self.lexer.pos(), DefType::Unknown);
            return Some(Type::Unknown(u_nr));
        }
        if tp_nr != u32::MAX && self.data.def_type(tp_nr) == DefType::Unknown {
            return Some(Type::Unknown(tp_nr));
        }
        let link = self.lexer.link();
        if self.lexer.has_token("<") {
            if let Some(sub_name) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(on_d, &sub_name, false) {
                    let sub_nr = if let Type::Unknown(d) = tp {
                        d
                    } else {
                        self.data.type_def_nr(&tp)
                    };
                    let mut fields = Vec::new();
                    return Some(match type_name {
                        "index" => {
                            self.parse_fields(true, sub_nr, &mut fields);
                            let mut f = Vec::new();
                            for fld in fields {
                                if fld < 0 {
                                    f.push(((-1 - fld) as u16, false));
                                } else {
                                    f.push((fld as u16, true));
                                }
                            }
                            Type::Index(self.data.type_def_nr(&tp), f, Vec::new())
                        }
                        "hash" => {
                            self.parse_fields(false, sub_nr, &mut fields);
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            let mut f = Vec::new();
                            for fld in fields {
                                f.push(fld as u16);
                            }
                            Type::Hash(sub_nr, f, Vec::new())
                        }
                        "vector" => {
                            self.lexer.token(">");
                            Type::Vector(Box::new(tp), Vec::new())
                        }
                        "sorted" => {
                            self.parse_fields(true, sub_nr, &mut fields);
                            let mut f = Vec::new();
                            for fld in fields {
                                if fld < 0 {
                                    f.push(((-1 - fld) as u16, false));
                                } else {
                                    f.push((fld as u16, true));
                                }
                            }
                            Type::Sorted(sub_nr, f, Vec::new())
                        }
                        /*
                        "spacial" => {
                            self.parse_fields(false, sub_nr, &mut fields);
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            let mut f = Vec::new();
                            for fld in fields {
                                f.push(fld as u16);
                            }
                            Type::Spacial(sub_nr, f)
                        }*/
                        "reference" => {
                            self.lexer.token(">");
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Reference(sub_nr, Vec::new())
                        }
                        "iterator" => {
                            self.lexer.token(",");
                            let mut it_tp = Type::Null;
                            if let Some(iter) = self.lexer.has_identifier() {
                                if let Some(it) = self.parse_type(on_d, &iter, false) {
                                    self.data.set_referenced(sub_nr, on_d, Value::Null);
                                    it_tp = it;
                                } else {
                                    diagnostic!(
                                        self.lexer,
                                        Level::Error,
                                        "Expect an iterator type"
                                    );
                                }
                            } else {
                                diagnostic!(self.lexer, Level::Error, "Expect an iterator type");
                            }
                            self.lexer.token(">");
                            Type::Iterator(Box::new(tp), Box::new(it_tp))
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
                assert!(self.first_pass, "Incorrect handling of unknown types");
            } else {
                self.lexer.revert(link);
            }
        }
        let mut dep = Vec::new();
        self.parse_depended(returned, &mut dep);
        let mut min = 0;
        let mut max = 0;
        if type_name == "integer" && self.parse_type_limit(&mut min, &mut max) {
            return Some(Type::Integer(min, max));
        }
        let dt = self.data.def_type(tp_nr);
        if tp_nr != u32::MAX
            && (dt == DefType::Type || dt == DefType::Enum || dt == DefType::Struct)
        {
            if self.first_pass && dt == DefType::Struct {
                Some(Type::Reference(tp_nr, dep))
            } else if matches!(self.data.def(tp_nr).returned, Type::Text(_)) {
                Some(Type::Text(dep))
            } else {
                Some(self.data.def(tp_nr).returned.clone())
            }
        } else {
            None
        }
    }

    fn parse_depended(&mut self, returned: bool, dep: &mut Vec<u16>) {
        if self.default && returned && self.lexer.has_token("[") && self.context != u32::MAX {
            loop {
                if let Some(id) = self.lexer.has_identifier() {
                    if let Some(nr) = self.data.def(self.context).attr_names.get(&id) {
                        dep.push(*nr as u16);
                    } else {
                        diagnostic!(self.lexer, Level::Error, "Unknown field name '{id}'");
                    }
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expected a field name");
                }
                if !self.lexer.has_token(",") {
                    break;
                }
            }
            self.lexer.token("]");
        }
    }

    fn parse_fields(&mut self, directions: bool, d_nr: u32, result: &mut Vec<i32>) {
        self.lexer.token("[");
        loop {
            let desc = self.lexer.has_token("-");
            if !directions && desc {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Structure doesn't support descending fields"
                );
            }
            if let Some(field) = self.lexer.has_identifier() {
                if self.data.def(d_nr).attr_names.contains_key(&field) {
                    let f_nr = self.data.def(d_nr).attr_names[&field] as i32;
                    result.push(if desc { -1 - f_nr } else { f_nr });
                } else {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown field {field} in {}",
                        self.data.def(d_nr).name
                    );
                }
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token("]");
        self.lexer.token(">");
    }

    // <field_limit> ::= 'limit' '(' [ '-' ] <min-integer> ',' [ '-' ] <max-integer> ')'
    fn parse_type_limit(&mut self, min: &mut i32, max: &mut u32) -> bool {
        if self.lexer.has_keyword("limit") {
            self.lexer.token("(");
            let min_neg = self.lexer.has_token("-");
            if let Some(nr) = self.lexer.has_integer() {
                *min = if min_neg { -(nr as i32) } else { nr as i32 };
            }
            self.lexer.token(",");
            if let Some(nr) = self.lexer.has_integer() {
                *max = nr;
            }
            self.lexer.token(")");
            true
        } else {
            false
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
            d_nr = self.data.add_def(&id, self.lexer.pos(), DefType::Struct);
            self.data.definitions[d_nr as usize].returned = Type::Reference(d_nr, Vec::new());
        } else if self.first_pass {
            self.data.definitions[d_nr as usize].position = self.lexer.pos().clone();
            self.data.definitions[d_nr as usize].def_type = DefType::Struct;
        };
        let context = self.context;
        self.context = d_nr;
        self.lexer.token("{");
        loop {
            let Some(a_name) = self.lexer.has_identifier() else {
                diagnostic!(self.lexer, Level::Error, "Expect attribute");
                self.context = context;
                return true;
            };
            if self.first_pass && self.data.attr(d_nr, &a_name) != usize::MAX {
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
        self.context = context;
        true
    }

    // <field> ::= { <field_limit> | 'not' 'null' | <field_default> | 'check' '(' <expr> ')' | <type-id> [ '[' ['-'] <field> { ',' ['-'] <field> } ']' ] } }
    fn parse_field(&mut self, d_nr: u32, a_name: &String) {
        let mut a_type: Type = Type::Unknown(0);
        let mut defined = false;
        let mut value = Value::Null;
        //let mut check = Value::Null;
        let mut nullable = true;
        let mut is_virtual = false;
        loop {
            if self.lexer.has_keyword("not") {
                // This field cannot be null, this allows for 256 values in a byte
                self.lexer.token("null");
                nullable = false;
            }
            if self.parse_field_default(&mut value, &mut a_type, d_nr, a_name, &mut defined) {
                is_virtual = true;
            }
            /* TODO for now ignore this, we have to properly implement this in the future
            if self.lexer.has_keyword("check") {
                self.lexer.token("(");
                let tp = self.expression(&mut check);
                self.convert(&mut check, &tp, &Type::Boolean);
                self.lexer.token(")");
            }
            */
            if let Some(id) = self.lexer.has_identifier() {
                if let Some(tp) = self.parse_type(d_nr, &id, false) {
                    defined = true;
                    a_type = tp;
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
            /* TODO implement me!
            if check != Value::Null {
                self.data.set_attr_check(d_nr, a, check);
            }
            */
            if is_virtual {
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

    // <field_default> ::= 'virtual' <value-expr> | 'default' '(' <value-expr> ')'
    fn parse_field_default(
        &mut self,
        value: &mut Value,
        a_type: &mut Type,
        d_nr: u32,
        a_name: &String,
        defined: &mut bool,
    ) -> bool {
        let mut is_virtual = false;
        if self.lexer.has_keyword("virtual") {
            is_virtual = true;
            // Define the result of a field that cannot be written to
            self.lexer.token("(");
            let args = vec![Argument {
                name: "rec".to_string(),
                typedef: Type::Reference(d_nr, Vec::new()),
                default: Value::Null,
                constant: false,
            }];
            let mut is_virtual = Value::Null;
            let name = format!(
                "_virtual_attr_{}_{a_name}",
                self.data.def(d_nr).name.to_lowercase()
            );
            let v_nr = if self.first_pass {
                self.data.add_fn(&mut self.lexer, &name, &args)
            } else {
                self.data.def_nr(&name)
            };
            let tp = self.expression(&mut is_virtual);
            if a_type.is_unknown() {
                *a_type = tp;
                *defined = true;
            } else {
                self.convert(&mut is_virtual, &tp, a_type);
            }
            if self.first_pass {
                self.data.set_returned(v_nr, a_type.clone());
            } else {
                self.data.definitions[v_nr as usize].code = is_virtual;
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
            if is_virtual {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Attribute {a_name} cannot be virtual and have a default"
                );
            }
        }
        is_virtual
    }

    // <code> = '{' <block> '}'
    /// Parse the code on the last inserted definition.
    /// This way we can use recursion with the definition itself.
    fn parse_code(&mut self) -> Type {
        self.lexer.token("{");
        let mut v = Value::Null;
        let result = if self.context == u32::MAX {
            Type::Void
        } else {
            self.data.def(self.context).returned.clone()
        };
        self.parse_block("return from block", &mut v, &result);
        if let Value::Block(ls) = &mut v {
            for v in self.vars.work_vars() {
                ls.insert(0, v_let(*v, Value::Text(String::new())));
            }
        }
        if self.context != u32::MAX && !self.first_pass {
            self.data.definitions[self.context as usize].code = v;
        }
        result
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
            self.parse_block("block", val, &Type::Void)
        } else {
            let res = self.parse_assign(val);
            self.known_var_or_type(val);
            res
        }
    }

    fn change_var_type(&mut self, val: &Value, tp: &Type) -> bool {
        if let Value::Var(vnr) = val {
            self.vars
                .change_var_type(*vnr, tp, &self.data, &mut self.lexer)
        } else {
            false
        }
    }

    // <assign> ::= <operators> [ '=' | '+=' | '-=' | '*=' | '%=' | '/=' <operators> ]
    fn parse_assign(&mut self, code: &mut Value) -> Type {
        let mut parent_tp = Type::Null;
        let f_type = self.parse_operators(&Type::Unknown(0), code, &mut parent_tp, 0);
        if let (Type::RefVar(_), Value::Var(v_nr)) = (&f_type, &code) {
            self.vars.in_use(*v_nr, true);
        }
        let to = code.clone();
        for op in ["=", "+=", "-=", "*=", "%=", "/="] {
            if self.lexer.has_token(op) {
                let mut assign_tp = Type::Unknown(0);
                let vr = if let Value::Var(v_nr) = *code {
                    if op == "=" {
                        self.vars.in_use(v_nr, false);
                    }
                    if let Type::Vector(inner, _) = &self.vars.tp(v_nr) {
                        assign_tp = (**inner).clone();
                    }
                    v_nr
                } else {
                    0
                };
                if op == "=" && matches!(code, Value::Var(_)) {
                    *code = Value::Null; // do not bleed the original value
                }
                match &f_type {
                    Type::Vector(inner, _) => assign_tp = (**inner).clone(),
                    Type::Hash(in_def, _, _)
                    | Type::Sorted(in_def, _, _)
                    | Type::Index(in_def, _, _)
                    | Type::Spacial(in_def, _, _) => {
                        assign_tp = self.data.def(*in_def).returned.clone();
                    }
                    _ => {}
                }
                let s_type = self.parse_operators(&assign_tp, code, &mut parent_tp, 0);
                let new = self.change_var_type(&to, &s_type);
                let add = self.data.def_nr("OpAddText");
                if let Type::RefVar(tp) = &f_type {
                    if (op == "=" || op == "+=") && matches!(**tp, Type::Text(_)) {
                        let mut ls = Vec::new();
                        let rf = self.cl("OpVarRef", &[Value::Var(vr)]);
                        if op == "=" {
                            ls.push(self.cl("OpClearRefText", &[rf.clone()]));
                        }
                        self.replace_ref_text(&mut ls, add, code, &rf);
                        *code = Value::Block(ls);
                        return Type::Unknown(u32::MAX);
                    }
                }
                if (op == "=" || op == "+=") && matches!(s_type, Type::Text(_)) && code.is_op(add) {
                    let mut ls = Vec::new();
                    if new && op == "=" {
                        ls.push(v_let(vr, Value::Text(String::new())));
                    }
                    self.replace_add_text(&mut ls, add, code, vr);
                    *code = Value::Block(ls);
                    return Type::Unknown(u32::MAX);
                }
                *code = self.towards_set(&to, code, &f_type, &op[0..1], new);
                return Type::Void;
            }
        }
        *code = to;
        f_type
    }

    fn replace_ref_text(&mut self, ls: &mut Vec<Value>, add: u32, code: &Value, vr: &Value) {
        if let Value::Call(_, ps) = code {
            if ps[0].is_op(add) {
                self.replace_ref_text(ls, add, &ps[0], vr);
            } else {
                ls.push(self.cl(
                    "OpAppendRefText",
                    &[vr.clone(), Value::Int(0), ps[0].clone()],
                ));
            }
            ls.push(self.cl(
                "OpAppendRefText",
                &[vr.clone(), Value::Int(0), ps[1].clone()],
            ));
        } else {
            ls.push(self.cl(
                "OpAppendRefText",
                &[vr.clone(), Value::Int(0), code.clone()],
            ));
        }
    }

    fn replace_add_text(&mut self, ls: &mut Vec<Value>, add: u32, code: &Value, vr: u16) {
        if let Value::Call(_, ps) = code {
            if ps[0].is_op(add) {
                self.replace_add_text(ls, add, &ps[0], vr);
            } else {
                ls.push(self.cl("OpAppendText", &[Value::Var(vr), ps[0].clone()]));
            }
            ls.push(self.cl("OpAppendText", &[Value::Var(vr), ps[1].clone()]));
        }
    }

    /** Mutate current code when it reads a value into writing it. This is needed for assignments.
     */
    fn towards_set(
        &mut self,
        to: &Value,
        val: &Value,
        f_type: &Type,
        op: &str,
        new: bool,
    ) -> Value {
        if matches!(*f_type, Type::Text(_)) {
            return self.text_change(op, to, val.clone());
        }
        if matches!(*f_type, Type::Vector(_, _) | Type::Sorted(_, _, _)) {
            if let Value::Var(nr) = to {
                if self.vars.uses(*nr) > 0 {
                    return val.clone();
                }
            } else {
                return val.clone();
            }
        }
        let code = if op == "=" {
            val.clone()
        } else {
            self.op(rename(op), to.clone(), val.clone(), f_type.clone())
        };
        if let Value::Call(d_nr, args) = &to {
            let name = &self.data.def(*d_nr).name as &str;
            match name {
                "OpGetInt" => self.cl("OpSetInt", &[args[0].clone(), args[1].clone(), code]),
                "OpGetByte" => self.cl(
                    "OpSetByte",
                    &[args[0].clone(), args[1].clone(), args[2].clone(), code],
                ),
                "OpGetEnum" => self.cl("OpSetEnum", &[args[0].clone(), args[1].clone(), code]),
                "OpGetShort" => self.cl(
                    "OpSetShort",
                    &[args[0].clone(), args[1].clone(), args[2].clone(), code],
                ),
                "OpGetLong" => self.cl("OpSetLong", &[args[0].clone(), args[1].clone(), code]),
                "OpGetFloat" => self.cl("OpSetFloat", &[args[0].clone(), args[1].clone(), code]),
                "OpGetSingle" => self.cl("OpSetSingle", &[args[0].clone(), args[1].clone(), code]),
                "OpGetField" => code.clone(),
                _ => panic!("Unknown {op}= for {name}"),
            }
        } else if let Value::Var(nr) = to {
            // This variable was created here and thus not yet used.
            self.var_usages(*nr, false);
            if op == "=" && new {
                v_let(*nr, code)
            } else {
                v_set(*nr, code)
            }
        } else {
            Value::Null
        }
    }

    fn text_change(&mut self, op: &str, to: &Value, val: Value) -> Value {
        if let Value::Call(_, a) = &to {
            if op == "=" {
                self.cl("OpSetText", &[a[0].clone(), a[1].clone(), val])
            } else {
                let code = self.op(rename(op), to.clone(), val.clone(), Type::Text(Vec::new()));
                self.cl("OpSetText", &[a[0].clone(), a[1].clone(), code])
            }
        } else if let Value::Var(nr) = to {
            if matches!(self.vars.tp(*nr), Type::Text(_)) && op == "+" {
                self.cl("OpAppendText", &[to.clone(), val])
            } else if self.vars.uses(*nr) == 0 {
                v_let(*nr, val)
            } else {
                v_set(*nr, val)
            }
        } else {
            panic!("Incorrect {op}= for text types found {to:?} and val {val:?}");
        }
    }

    // <block> ::= '}' | <expression> {';' <expression} '}'
    fn parse_block(&mut self, context: &str, val: &mut Value, result: &Type) -> Type {
        if let Value::Var(v) = val {
            if let Type::Reference(r, _) = self.vars.tp(*v).clone() {
                if context == "block" {
                    // We actually scan a record here instead of a block of statement
                    self.parse_object(r, val);
                    return Type::Reference(r, Vec::new());
                }
            }
        }
        let mut t = Type::Void;
        if self.lexer.has_token("}") {
            *val = Value::Block(Vec::new());
            return t;
        }
        let bl = self.vars.start_scope(self.lexer.at(), "block");
        let mut l = Vec::new();
        let add = self.data.def_nr("OpAddText");
        loop {
            if self.lexer.has_token(";") {
                continue;
            }
            if self.lexer.peek_token("}") {
                break;
            }
            let mut n = Value::Null;
            t = self.expression(&mut n);
            if self.first_pass {
                l.push(n);
            } else if t == Type::Unknown(u32::MAX) {
                if let Value::Block(ls) = &mut n {
                    for v in ls {
                        l.push(v.clone());
                    }
                }
                t = Type::Void;
            } else if n.is_op(add) {
                let mut dep = Vec::new();
                self.return_text(&mut l, add, &mut n, &mut dep);
                t = Type::Text(dep);
            } else if t != Type::Void && (self.lexer.peek_token(";") || *result == Type::Void) {
                l.push(Value::Drop(Box::new(n)));
            } else {
                l.push(n);
            }
            if self.lexer.peek_token("}") {
                break;
            }
            t = Type::Void;
            match l.last() {
                Some(Value::If(_, _, _) | Value::Loop(_) | Value::Block(_)) => (),
                _ => {
                    if !self.lexer.token(";") {
                        break;
                    }
                }
            }
        }
        self.lexer.token("}");
        if *result != Type::Void && !matches!(*result, Type::Unknown(_)) {
            let last = l.len() - 1;
            if !self.convert(&mut l[last], &t, result) {
                self.validate_convert(context, &t, result);
            }
        }
        if matches!(t, Type::Text(_)) && self.vars.scope() == 1 {
            // validate that we do not depend on internal variables (or work variables)
            // update the returned types with the known dependencies
            self.data.definitions[self.context as usize].returned = t.clone();
            //self.text_return(&l[last]);
        }
        self.vars.finish_scope(bl, &t, self.lexer.at());
        *val = Value::Block(l);
        t
    }

    fn return_text(&mut self, l: &mut Vec<Value>, add: u32, n: &mut Value, dep: &mut Vec<u16>) {
        let work = self.work_var(l);
        dep.push(work);
        self.replace_add_text(l, add, n, work);
        if !self.lexer.peek_token(";") {
            l.push(Value::Var(work));
        }
    }

    fn work_var(&mut self, l: &mut Vec<Value>) -> u16 {
        if self.first_pass {
            return 0;
        }
        let work = self.vars.work(&mut self.lexer);
        l.push(self.cl("OpClearText", &[Value::Var(work)]));
        work
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
    // <operators> ::= <single>  { '.' <field> | '[' <index> ']' } | <operators> <operator> <operators>
    fn parse_operators(
        &mut self,
        assign_tp: &Type,
        code: &mut Value,
        parent_tp: &mut Type,
        precedence: usize,
    ) -> Type {
        if precedence >= OPERATORS.len() {
            let mut t = self.parse_single(assign_tp, code, parent_tp);
            while self.lexer.peek_token(".") || self.lexer.peek_token("[") {
                if !self.first_pass && t.is_unknown() && matches!(code, Value::Var(_)) {
                    diagnostic!(self.lexer, Level::Error, "Unknown variable");
                }
                if self.lexer.has_token(".") {
                    *parent_tp = t.clone();
                    t = self.field(code, t);
                } else if self.lexer.has_token("[") {
                    t = self.parse_index(code, &t);
                    self.lexer.token("]");
                }
            }
            return t;
        }
        let mut current_type = self.parse_operators(assign_tp, code, parent_tp, precedence + 1);
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
            self.known_var_or_type(code);
            if operator == "as" {
                if let Some(tps) = self.lexer.has_identifier() {
                    let Some(tp) = self.parse_type(u32::MAX, &tps, false) else {
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
                            &current_type.name(&self.data),
                        );
                    }
                    return tp;
                }
                diagnostic!(self.lexer, Level::Error, "Expect type after as");
            } else if operator == "or" || operator == "||" {
                self.boolean_operator(code, &current_type, precedence, true);
                current_type = Type::Boolean;
            } else if operator == "and" || operator == "&&" {
                self.boolean_operator(code, &current_type, precedence, false);
                current_type = Type::Boolean;
            } else {
                let mut second_code = Value::Null;
                let second_type =
                    self.parse_operators(assign_tp, &mut second_code, parent_tp, precedence + 1);
                self.known_var_or_type(&second_code);
                current_type = self.call_op(
                    code,
                    operator,
                    &[code.clone(), second_code],
                    &[current_type, second_type],
                );
            }
        }
    }

    /// Rewrite boolean operators into an `IF` statements to prevent the calculation of the second
    /// expression when it is not needed.
    fn boolean_operator(&mut self, code: &mut Value, tp: &Type, precedence: usize, is_or: bool) {
        if !self.convert(code, tp, &Type::Boolean) && !self.first_pass {
            self.can_convert(tp, &Type::Boolean);
        }
        let mut second_code = Value::Null;
        let mut parent_tp = Type::Unknown(0);
        let second_type = self.parse_operators(
            &Type::Unknown(0),
            &mut second_code,
            &mut parent_tp,
            precedence + 1,
        );
        self.known_var_or_type(&second_code);
        if !self.convert(&mut second_code, &second_type, &Type::Boolean) && !self.first_pass {
            self.can_convert(&second_type, &Type::Boolean);
        }
        *code = v_if(
            code.clone(),
            if is_or {
                Value::Boolean(true)
            } else {
                second_code.clone()
            },
            if is_or {
                second_code
            } else {
                Value::Boolean(false)
            },
        );
    }

    // <single> ::= '!' <expression> |
    //              '(' <expression> ')' |
    //              <vector> |
    //              'if' <if> |
    //              <identifier:var> |
    //              <number> | <float> | <cstring> |
    //              'true' | 'false'
    fn parse_single(&mut self, assign_tp: &Type, val: &mut Value, parent_tp: &mut Type) -> Type {
        if self.lexer.has_token("!") {
            let t = self.expression(val);
            self.call_op(val, "Not", &[val.clone()], &[t])
        } else if self.lexer.has_token("-") {
            let t = self.expression(val);
            self.call_op(val, "Min", &[val.clone()], &[t])
        } else if self.lexer.has_token("(") {
            let t = self.expression(val);
            self.lexer.token(")");
            t
        } else if self.lexer.has_token("{") {
            self.parse_block("block", val, &Type::Unknown(0))
        } else if self.lexer.has_token("[") {
            self.parse_vector(assign_tp, val, parent_tp)
        } else if self.lexer.has_token("if") {
            self.parse_if(val)
        } else if let Some(name) = self.lexer.has_identifier() {
            self.parse_var(val, &name)
        } else if self.lexer.has_token("$") {
            self.parse_var(val, "$")
        } else if let Some(nr) = self.lexer.has_integer() {
            *val = Value::Int(nr as i32);
            I32.clone()
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
            self.parse_string(val, &s);
            Type::Text(Vec::new())
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
    fn parse_vector(&mut self, assign_tp: &Type, val: &mut Value, parent_tp: &Type) -> Type {
        let new_store = if let Value::Var(nr) = *val {
            self.vars.uses(nr) == 0 && !self.vars.is_argument(nr)
        } else {
            *val == Value::Null
        };
        let is_field = self.is_field(val);
        let is_var = matches!(val, Value::Var(_));
        let tp = self.content(parent_tp);
        let was = Type::Reference(
            if tp == Type::Null {
                0
            } else {
                self.data.type_def_nr(&tp)
            },
            Vec::new(),
        );
        let vc = self.vars.start_scope(self.lexer.at(), "vector");
        let elm = self.create_unique(
            "elm",
            if let Type::Reference(_, _) = assign_tp {
                assign_tp
            } else {
                &was
            },
        );
        let vec = if is_field {
            u16::MAX
        } else if let Value::Var(nr) = val {
            *nr
        } else {
            self.create_unique(
                "vec",
                &Type::Vector(Box::new(assign_tp.clone()), Vec::new()),
            )
        };
        if self.lexer.has_token("]") {
            if new_store {
                self.vector_db(assign_tp, val, vec);
            }
            let tp = Type::Vector(Box::new(assign_tp.clone()), Vec::new());
            self.vars.finish_scope(vc, &tp, self.lexer.at());
            return tp;
        }
        let mut in_t = assign_tp.clone();
        let mut res = Vec::new();
        loop {
            if let Some(value) = self.parse_item(elm, &mut in_t, &mut res) {
                return value;
            }
            if !self.lexer.has_token(",") {
                break;
            }
            if self.lexer.peek_token("]") {
                break;
            }
        }
        // convert parts to common type
        if in_t == Type::Null {
            return in_t;
        }
        let struct_tp = Type::Vector(Box::new(in_t.clone()), Vec::new());
        if !is_field {
            self.vars
                .change_var_type(vec, &struct_tp, &self.data, &mut self.lexer);
            self.data.vector_def(&mut self.lexer, &in_t);
        }
        let mut ls = self.new_record(val, parent_tp, elm, vec, &res, &in_t);
        if new_store && !is_var && in_t != Type::Void {
            self.insert_new(vec, &in_t, &mut ls);
        } else if !is_field && !is_var {
            ls.insert(0, v_let(vec, val.clone()));
        }
        if new_store {
            ls.push(Value::Var(vec));
        }
        self.lexer.token("]");
        let tp = Type::Vector(Box::new(in_t), Vec::new());
        self.vars.finish_scope(
            vc,
            if new_store { &tp } else { &Type::Void },
            self.lexer.at(),
        );
        *val = Value::Block(ls);
        tp
    }

    fn parse_item(&mut self, elm: u16, in_t: &mut Type, res: &mut Vec<Value>) -> Option<Type> {
        let mut p = Value::Var(elm);
        let mut t = if self.lexer.has_token("for") {
            //self.iter_for(&mut p)
            diagnostic!(
                self.lexer,
                Level::Error,
                "For inside a vector is not yet implemented"
            );
            return Some(Type::Unknown(0));
        } else {
            let mut parent_tp = Type::Null;
            self.parse_operators(&Type::Unknown(0), &mut p, &mut parent_tp, 0)
        };
        if in_t.is_unknown() {
            *in_t = t.clone();
        }
        if t.is_unknown() {
            t = in_t.clone();
        }
        // double conversion check: can t become in_t or vice versa
        if !self.convert(&mut p, &t, in_t) {
            if self.convert(&mut p, in_t, &t) {
                *in_t = t;
            } else {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "No common type {} for vector {}",
                    t.name(&self.data),
                    in_t.name(&self.data)
                );
            }
        }
        res.push(p);
        None
    }

    fn is_field(&self, val: &Value) -> bool {
        if let Value::Call(o, _) = *val {
            o == self.data.def_nr("OpGetField")
        } else {
            false
        }
    }

    fn new_record(
        &mut self,
        val: &mut Value,
        parent_tp: &Type,
        elm: u16,
        vec: u16,
        res: &[Value],
        in_t: &Type,
    ) -> Vec<Value> {
        let mut ls = Vec::new();
        let is_field = self.is_field(val);
        let ed_nr = self.data.type_def_nr(in_t);
        assert_ne!(
            ed_nr,
            u32::MAX,
            "Unknown type {} at {}",
            in_t.name(&self.data),
            self.lexer.pos()
        );
        for (idx, p) in res.iter().enumerate() {
            let known = Value::Int(i32::from(
                if ed_nr == u32::from(u16::MAX) || self.data.def(ed_nr).known_type == u16::MAX {
                    0
                } else {
                    self.database.vector(self.data.def(ed_nr).known_type)
                },
            ));
            let fld = Value::Int(i32::from(u16::MAX));
            let app_v = if is_field {
                if let Value::Call(_, ps) = val {
                    let parent = self.data.def(self.data.type_def_nr(parent_tp)).known_type;
                    let field_nr = if let Value::Int(pos) = ps[1] {
                        self.database.field_nr(parent, pos)
                    } else {
                        0
                    };
                    self.cl(
                        "OpNewRecord",
                        &[
                            ps[0].clone(),
                            Value::Int(i32::from(parent)),
                            Value::Int(i32::from(field_nr)),
                        ],
                    )
                } else {
                    Value::Null
                }
            } else {
                self.cl(
                    "OpNewRecord",
                    &[Value::Var(vec), known.clone(), fld.clone()],
                )
            };
            ls.push(if idx == 0 {
                v_let(elm, app_v)
            } else {
                v_set(elm, app_v)
            });
            if let Type::Reference(_, _) = in_t {
                ls.push(p.clone());
            } else {
                ls.push(self.set_field(ed_nr, usize::MAX, Value::Var(elm), p.clone()));
            }
            ls.push(if is_field {
                if let Value::Call(_, ps) = val {
                    let parent = self.data.def(self.data.type_def_nr(parent_tp)).known_type;
                    let field_nr = if let Value::Int(pos) = ps[1] {
                        self.database.field_nr(parent, pos)
                    } else {
                        0
                    };
                    self.cl(
                        "OpFinishRecord",
                        &[
                            ps[0].clone(),
                            Value::Var(elm),
                            Value::Int(i32::from(parent)),
                            Value::Int(i32::from(field_nr)),
                        ],
                    )
                } else {
                    Value::Null
                }
            } else {
                self.cl(
                    "OpFinishRecord",
                    &[Value::Var(vec), Value::Var(elm), known, fld],
                )
            });
        }
        ls
    }

    fn vector_db(&mut self, assign_tp: &Type, val: &mut Value, vec: u16) {
        if !self.first_pass {
            let mut ls = Vec::new();
            let vec_def = self.data.vector_def(&mut self.lexer, assign_tp);
            let db = self.create_unique("db", &Type::Reference(vec_def, Vec::new()));
            ls.push(v_let(
                db,
                self.cl(
                    "OpDatabase",
                    &[
                        Value::Int(1),
                        Value::Int(i32::from(self.data.def(vec_def).known_type)),
                    ],
                ),
            ));
            // Reference to vector field.
            ls.push(v_let(vec, self.get_field(vec_def, 0, Value::Var(db))));
            // Write 0 into this reference.
            ls.push(self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
            ls.push(Value::Var(vec));
            *val = Value::Block(ls);
        }
    }

    fn insert_new(&mut self, vec: u16, in_t: &Type, ls: &mut Vec<Value>) {
        // determine the element size by the resulting type
        let rec_size = self
            .database
            .size(self.data.def(self.data.type_def_nr(in_t)).known_type);
        let vec_def = self.data.vector_def(&mut self.lexer, in_t);
        let db = self.create_unique("db", &Type::Reference(vec_def, Vec::new()));
        let known = Value::Int(i32::from(self.data.def(vec_def).known_type));
        let op_db = self.cl("OpDatabase", &[Value::Int(i32::from(rec_size)), known]);
        ls.insert(0, v_let(db, op_db));
        // Reference to vector field.
        ls.insert(1, v_let(vec, self.get_field(vec_def, 0, Value::Var(db))));
        // Write 0 into this reference.
        ls.insert(2, self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
    }

    fn type_info(&self, in_t: &Type) -> Value {
        Value::Int(i32::from(self.get_type(in_t)))
    }

    fn content(&self, in_t: &Type) -> Type {
        if self.first_pass {
            return Type::Null;
        }
        match in_t {
            Type::Index(r, _, _) | Type::Sorted(r, _, _) | Type::Hash(r, _, _) | Type::Enum(r) => {
                self.data.def(*r).returned.clone()
            }
            Type::Vector(tp, _) => *tp.clone(),
            _ => Type::Null,
        }
    }

    fn get_type(&self, in_t: &Type) -> u16 {
        if self.first_pass {
            return u16::MAX;
        }
        match in_t {
            Type::Reference(r, _) | Type::Enum(r) => self.data.def(*r).known_type,
            Type::Hash(tp, key, _) => {
                let mut name = "hash<".to_string() + &self.data.def(*tp).name + "[";
                self.database
                    .field_name(self.data.def(*tp).known_type, key, &mut name);
                self.database.name(&name)
            }
            Type::Sorted(tp, key, _) => {
                let mut name = "sorted<".to_string() + &self.data.def(*tp).name + "[";
                self.database
                    .field_id(self.data.def(*tp).known_type, key, &mut name);
                let r = self.database.name(&name);
                if r == u16::MAX {
                    name = "ordered<".to_string() + &self.data.def(*tp).name + "[";
                    self.database
                        .field_id(self.data.def(*tp).known_type, key, &mut name);
                }
                self.database.name(&name)
            }
            Type::Index(tp, key, _) => {
                let mut name = "index<".to_string() + &self.data.def(*tp).name + "[";
                self.database
                    .field_id(self.data.def(*tp).known_type, key, &mut name);
                let r = self.database.name(&name);
                if r == u16::MAX {
                    name = "index<".to_string() + &self.data.def(*tp).name + "[";
                    self.database
                        .field_id(self.data.def(*tp).known_type, key, &mut name);
                }
                self.database.name(&name)
            }
            Type::Vector(tp, _) => {
                let vec_name = format!("vector<{}>", tp.name(&self.data));
                let typedef: &Type = if self.data.def_names.contains_key(&vec_name) {
                    tp
                } else {
                    &Type::Unknown(0)
                };
                self.data
                    .def_name(if matches!(typedef, Type::Unknown(_)) {
                        "vector"
                    } else {
                        &vec_name
                    })
                    .known_type
            }
            _ => u16::MAX,
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
            let e_size = i32::from(self.database.size(self.data.def(enr).known_type));
            let dnr = self.data.type_def_nr(&t);
            if let Type::Vector(et, _) = t.clone() {
                if field == "remove" {
                    let (tps, ls) = self.parse_parameters();
                    let mut cd = ls[0].clone();
                    // validate types
                    if tps.len() != 1 || self.convert(&mut cd, &tps[0], &I32) {
                        diagnostic!(self.lexer, Level::Error, "Invalid index in remove");
                    }
                    *code = self.cl("OpRemoveVector", &[code.clone(), Value::Int(e_size), cd]);
                } else if field == "insert" {
                    let (tps, ls) = self.parse_parameters();
                    let mut cd = ls[0].clone();
                    // validate types
                    if tps.len() != 2 || self.convert(&mut cd, &tps[0], &I32) {
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
            if fnr == usize::MAX {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown field {}.{field}",
                    self.data.def(dnr).name
                );
                return t;
            }
            if let Type::Routine(r_nr) = self.data.attr_type(dnr, fnr) {
                if self.lexer.has_token("(") {
                    t = self.parse_method(code, r_nr, t.clone());
                } else {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Expect call of method {}.{}",
                        self.data.def(dnr).name,
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
            self.data.attr_used(dnr, fnr);
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect a field name");
        }
        t
    }

    fn parse_index(&mut self, code: &mut Value, t: &Type) -> Type {
        let elm_type = if let Type::Vector(v_t, _) = t {
            *v_t.clone()
        } else if let Type::Sorted(d_nr, _, _)
        | Type::Hash(d_nr, _, _)
        | Type::Index(d_nr, _, _)
        | Type::Spacial(d_nr, _, _) = t
        {
            self.data.def(*d_nr).returned.clone()
        } else if matches!(t, Type::Text(_)) {
            t.clone()
        } else {
            diagnostic!(self.lexer, Level::Error, "Indexing a non vector");
            Type::Unknown(0)
        };
        /*let nr = if self.types.exists("$") {
            self.types.var_nr("$")
        } else {
            self.create_var("$".to_string(), elm_type.clone())
        };
        self.data.definitions[self.context as usize].variables[nr as usize].uses = 0;
         */
        let mut p = Value::Null;
        if let Type::Vector(etp, _) = &t {
            let index_t = self.parse_in_range(&mut p, code, "$");
            /*
            if self.var("$").used() {
                self.first_match(code, p, t);
            } else {
                self.data.definitions[self.context as usize].variables[nr as usize].uses = 1;
             */
            let elm_td = self.data.type_elm(etp);
            let known = self.data.def(elm_td).known_type;
            let elm_size = i32::from(self.database.size(known));
            if let Value::Iter(init, next) = p {
                if let Value::Block(ls) = *next {
                    let mut op = self.cl(
                        "OpGetVector",
                        &[code.clone(), Value::Int(elm_size), Value::Block(ls)],
                    );
                    if self.database.is_base(known) || self.database.is_linked(known) {
                        op = self.get_val(etp, true, 0, op);
                    }
                    *code = Value::Iter(init, Box::new(Value::Block(vec![op])));
                    return Type::Iterator(Box::new(elm_type), Box::new(Type::Null));
                }
                panic!("Incorrect iter");
            }
            if !self.convert(&mut p, &index_t, &I32) {
                diagnostic!(self.lexer, Level::Error, "Invalid index on vector");
            }
            *code = self.cl("OpGetVector", &[code.clone(), Value::Int(elm_size), p]);
            if self.database.is_base(known) || self.database.is_linked(known) {
                *code = self.get_val(etp, true, 0, code.clone());
            }
        } else if matches!(*t, Type::Text(_)) {
            let index_t = self.expression(&mut p);
            self.parse_text_index(code, &mut p, &index_t);
        } else if let Type::Hash(el, keys, _) | Type::Spacial(el, keys, _) = &t {
            let mut key_types = Vec::new();
            for k in keys {
                key_types.push(self.data.def(*el).attributes[*k as usize].typedef.clone());
            }
            self.parse_key(code, t, &key_types);
        } else if let Type::Sorted(el, keys, _) | Type::Index(el, keys, _) = &t {
            let mut key_types = Vec::new();
            for (k, _) in keys {
                key_types.push(self.data.def(*el).attributes[*k as usize].typedef.clone());
            }
            self.parse_key(code, t, &key_types);
        } else {
            panic!("Unknown type to index");
        }
        //self.data.definitions[self.context as usize].variables[nr as usize].uses = 1;
        elm_type
    }

    fn parse_text_index(&mut self, code: &mut Value, p: &mut Value, index_t: &Type) {
        if !self.convert(p, index_t, &I32) {
            diagnostic!(self.lexer, Level::Error, "Invalid index on string");
        }
        let mut other = Value::Null;
        if self.lexer.has_token("..") {
            let incl = self.lexer.has_token("=");
            if self.lexer.peek_token("]") {
                *code = self.cl(
                    "OpGetTextSub",
                    &[code.clone(), p.clone(), Value::Int(i32::MAX)],
                );
            } else {
                let ot_type = self.expression(&mut other);
                if !self.convert(&mut other, &ot_type, &I32) {
                    diagnostic!(self.lexer, Level::Error, "Invalid index on string",);
                }
                if incl {
                    other = self.cl("OpAddInt", &[other.clone(), Value::Int(1)]);
                }
                *code = self.cl("OpGetTextSub", &[code.clone(), p.clone(), other]);
            }
        } else {
            *code = self.cl(
                "OpGetTextSub",
                &[code.clone(), p.clone(), Value::Int(i32::MIN)],
            );
        }
    }

    fn parse_key(&mut self, code: &mut Value, typedef: &Type, key_types: &[Type]) {
        let mut p = Value::Null;
        let index_t = self.expression(&mut p);
        if !self.convert(&mut p, &index_t, &key_types[0]) {
            diagnostic!(self.lexer, Level::Error, "Invalid index key");
        }
        let known = if self.first_pass {
            Value::Null
        } else {
            self.type_info(typedef)
        };
        let mut nr = 1;
        let mut key = Vec::new();
        key.push(p);
        if key_types.len() > 1 {
            while self.lexer.has_token(",") {
                if nr >= key_types.len() {
                    diagnostic!(self.lexer, Level::Error, "Too many key values on index");
                    break;
                }
                let mut ex = Value::Null;
                let ex_t = self.expression(&mut ex);
                if !self.convert(&mut ex, &ex_t, &key_types[nr]) {
                    diagnostic!(self.lexer, Level::Error, "Invalid index key");
                }
                key.push(ex);
                nr += 1;
            }
        }
        if self.lexer.has_token("..") {
            let mut on = 0;
            let mut arg = 0;
            let _inclusive = self.lexer.has_token("=");
            let iter = self.create_unique("iter", &Type::Long);
            let mut ls = Vec::new();
            if !self.first_pass {
                let known = self.get_type(typedef);
                match self.database.types[known as usize].parts {
                    Parts::Index(_, _, _) => {
                        on = 1;
                        arg = self.database.fields(known);
                    }
                    Parts::Sorted(tp, _) => {
                        on = 2;
                        arg = self.database.size(tp);
                    }
                    Parts::Ordered(_, _) => {
                        on = 3;
                        arg = 4;
                    }
                    _ => {
                        diagnostic!(self.lexer, Level::Error, "Cannot iterate");
                        return;
                    }
                }
                ls.push(code.clone());
                ls.push(Value::Int(on));
                ls.push(Value::Int(i32::from(arg)));
                ls.push(Value::Keys(
                    self.database.types[known as usize].keys.clone(),
                ));
                ls.push(Value::Int(nr as i32));
                ls.append(&mut key);
            }
            let mut n = Value::Null;
            self.expression(&mut n);
            if !self.convert(&mut n, &index_t, &key_types[0]) {
                diagnostic!(self.lexer, Level::Error, "Invalid index key");
            }
            key.push(n);
            let mut nr = 1;
            if key_types.len() > 1 {
                while self.lexer.has_token(",") {
                    if nr >= key_types.len() {
                        diagnostic!(self.lexer, Level::Error, "Too many key values on index");
                        break;
                    }
                    let mut ex = Value::Null;
                    let ex_t = self.expression(&mut ex);
                    if !self.convert(&mut ex, &ex_t, &key_types[nr]) {
                        diagnostic!(self.lexer, Level::Error, "Invalid index key");
                    }
                    key.push(ex);
                    nr += 1;
                }
            }
            ls.push(Value::Int(nr as i32));
            ls.append(&mut key);
            let start = v_let(iter, self.cl("OpIterate", &ls));
            *code = Value::Iter(
                Box::new(start),
                Box::new(Value::Block(vec![self.cl(
                    "OpStep",
                    &[
                        Value::Var(iter),
                        code.clone(),
                        Value::Int(on),
                        Value::Int(i32::from(arg)),
                    ],
                )])),
            );
        } else {
            let mut ls = vec![code.clone(), known.clone(), Value::Int(nr as i32)];
            ls.append(&mut key);
            *code = self.cl("OpGetRecord", &ls);
            if matches!(typedef, Type::Hash(_, _, _)) && nr < key_types.len() {
                diagnostic!(self.lexer, Level::Error, "Too few key fields");
            }
        }
    }

    // <var> ::= <object> | [ <call> | <var> | <enum> ] <children> }
    fn parse_var(&mut self, code: &mut Value, name: &str) -> Type {
        let mut t = self.parse_constant_value(code, name);
        if t != Type::Null {
            return t;
        }
        if self.lexer.has_token("(") {
            if name == "sizeof" {
                t = self.parse_size(code);
            } else {
                t = self.parse_call(code, name);
            }
        } else if self.vars.name_exists(name) {
            if self.lexer.has_token("#") {
                if self.lexer.has_keyword("index") {
                    let i_name = &format!("{name}#index");
                    if self.vars.name_exists(i_name) {
                        let v = self.vars.var(i_name);
                        t = self.vars.tp(v).clone();
                        *code = Value::Var(v);
                    } else {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Incorrect #index variable on {}",
                            name
                        );
                        t = Type::Unknown(0);
                    }
                } else {
                    diagnostic!(self.lexer, Level::Error, "Incorrect # variable on {}", name);
                    t = Type::Unknown(0);
                }
            } else {
                let v_nr = self.vars.var(name);
                t = self.vars.tp(v_nr).clone();
                self.var_usages(v_nr, true);
                if matches!(t, Type::Text(_)) {
                    t = Type::Text(vec![v_nr]);
                }
                *code = Value::Var(v_nr);
            }
        } else if self.data.def_nr(name) != u32::MAX {
            let dnr = self.data.def_nr(name);
            if self.data.def_type(dnr) == DefType::Enum {
                t = Type::Enum(dnr);
            } else if self.data.def_type(dnr) == DefType::EnumValue {
                t = Type::Enum(self.data.def(dnr).parent);
            } else {
                t = Type::Null;
            };
        } else if self.data.def_type(self.context) == DefType::Struct
            && self.data.attr(self.context, name) != usize::MAX
        {
            let fnr = self.data.attr(self.context, name);
            *code = self.get_field(self.context, fnr, Value::Var(0));
            t = self.data.attr_type(self.context, fnr);
        } else {
            *code = Value::Var(self.create_var(name, &Type::Unknown(0)));
            t = Type::Unknown(0);
        }
        t
    }

    fn parse_constant_value(&mut self, code: &mut Value, name: &str) -> Type {
        let mut t;
        let d_nr = self.data.def_nr(name);
        if d_nr != u32::MAX {
            self.data.def_used(d_nr);
            t = self.data.def(d_nr).returned.clone();
            if self.data.def_type(d_nr) == DefType::Function {
                t = Type::Routine(d_nr);
            } else if self.data.def_type(d_nr) == DefType::Struct {
                if !self.lexer.token("{") {
                    return Type::Unknown(0);
                }
                return self.parse_object(d_nr, code);
            } else if self.data.def_type(d_nr) == DefType::Constant {
                *code = self.data.def(d_nr).code.clone();
                return self.data.def(d_nr).returned.clone();
            }
            if let Type::Enum(en) = t {
                for a_nr in 0..self.data.attributes(en) {
                    if self.data.attr_name(en, a_nr) == name {
                        *code = self.data.attr_value(en, a_nr);
                        return t;
                    }
                }
            }
        }
        Type::Null
    }

    fn known_var_or_type(&mut self, code: &Value) {
        if let Value::Var(nr) = code {
            if self.default && matches!(self.vars.tp(*nr), Type::Vector(_, _)) {
                return;
            }
            if !self.first_pass && self.vars.tp(*nr).is_unknown() {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown variable '{}'",
                    self.vars.name(*nr)
                );
            }
        }
    }

    /*
    fn first_match(&mut self, val: &mut Value, if_expr: Value, in_type: &Type) {
        let var_type;
        if let Type::Vector(t_nr) = &in_type {
            var_type = *t_nr.clone();
        } else if let Type::Sorted(td, _keys) = &in_type {
            var_type = self.data.def(*td).returned.clone();
        } else {
            panic!("Unknown type {}", self.data.show_type(in_type))
        }
        let mut create_iter = val.clone();
        let it = Type::Iterator(Box::new(var_type.clone()), Box::new(Type::Null));
        let iter_next = self.iterator("", &mut create_iter, in_type, &it);
        if iter_next == Value::Null {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Need an iterable in a match expression"
            );
            return;
        }
        let for_var = self.types.var_nr("$");
        // loop {
        //     for_var = Next(iter_var);
        //     if !for_var {break}
        //     if if_expr(for_var) {break}
        // }
        // for_var
        *val = Value::Block(vec![
            create_iter,
            Value::Loop(vec![
                v_set(for_var, iter_next),
                v_if(
                    self.single_op("!", Value::Var(for_var), var_type),
                    Value::Break(0),
                    Value::Null,
                ),
                v_if(if_expr, Value::Break(0), Value::Null),
            ]),
            Value::Var(for_var),
        ]);
    }*/

    fn parse_string(&mut self, code: &mut Value, string: &str) {
        let mut append_value = u16::MAX;
        *code = Value::str(string);
        let mut var = u16::MAX;
        let mut list = vec![];
        let scope = if self.lexer.mode() == Mode::Formatting {
            // Define a new variable to append to
            var = self.work_var(&mut list);
            list.push(self.cl("OpAppendText", &[Value::Var(var), code.clone()]));
            self.vars.start_scope(self.lexer.at(), "formatting string")
        } else {
            u16::MAX
        };
        while self.lexer.mode() == Mode::Formatting {
            self.lexer.set_mode(Mode::Code);
            let mut format = Value::Null;
            let tp = if self.lexer.has_token("for") {
                self.iter_for(&mut format, &mut append_value)
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
                    state.plus = true;
                }
                if self.lexer.has_token("#") {
                    // show 0x 0b or 0o in front of numbers when applicable
                    state.note = true;
                }
                if self.lexer.has_token(".") {
                    state.float = true;
                }
                let LexResult {
                    has: h,
                    position: _pos,
                } = self.lexer.peek();
                if match h {
                    LexItem::Token(st) | LexItem::Identifier(st) => {
                        let s: &str = &st;
                        !SKIP_WIDTH.contains(&s)
                    }
                    LexItem::Integer(_, _) | LexItem::Float(_) => true,
                    _ => false,
                } {
                    if let LexResult {
                        has: LexItem::Integer(_, true),
                        position: _pos,
                    } = self.lexer.peek()
                    {
                        state.token = "0";
                    }
                    self.lexer.set_mode(Mode::Code);
                    self.expression(&mut state.width);
                    self.lexer.set_mode(Mode::Formatting);
                }
                state.radix = self.get_radix();
            }
            self.append_data(tp, &mut list, var, append_value, &format, state);
            if let Some(text) = self.lexer.has_cstring() {
                if !text.is_empty() {
                    list.push(self.cl("OpAppendText", &[Value::Var(var), Value::str(&text)]));
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Formatter error");
                return;
            }
        }
        if var < u16::MAX {
            list.push(Value::Var(var));
            *code = Value::Block(list);
        }
        if scope != u16::MAX {
            self.vars
                .finish_scope(scope, &Type::Text(Vec::new()), self.lexer.at());
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

    // Iterator for
    // <for> ::= <identifier> 'in' <range> '{' <block>
    fn iter_for(&mut self, val: &mut Value, append_value: &mut u16) -> Type {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("in");
            let mut expr = Value::Null;
            let iter_var = self.create_var(&format!("{id}#index"), &I32);
            let lp_scope = self.vars.start_scope(self.lexer.at(), "iter for loop");
            *append_value = self.create_unique("val", &Type::Unknown(0));
            let it_scope = self.vars.start_scope(self.lexer.at(), "iter for fill");
            let in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            let mut var_type = in_type.clone();
            if let Type::Vector(t_nr, _) | Type::Iterator(t_nr, _) = &in_type {
                var_type = *t_nr.clone();
            }
            let for_var = self.vars.add_variable(&id, &var_type, &mut self.lexer);
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let mut create_iter = expr;
            let it = Type::Iterator(Box::new(var_type.clone()), Box::new(Type::Null));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var);
            if iter_next == Value::Null && !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return Type::Null;
            }
            let in_loop = self.in_loop;
            self.in_loop = true;
            let format_type = self.parse_block("for", &mut block, &Type::Unknown(0));
            self.vars
                .change_var_type(*append_value, &format_type, &self.data, &mut self.lexer);
            self.in_loop = in_loop;
            let for_next = v_let(for_var, iter_next);
            let mut lp = vec![for_next];
            if !matches!(in_type, Type::Iterator(_, _)) {
                lp.push(v_if(
                    self.single_op("!", Value::Var(for_var), var_type),
                    Value::Break(0),
                    Value::Null,
                ));
            }
            if if_step != Value::Null {
                lp.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            lp.push(block);
            self.vars
                .finish_scope(it_scope, &format_type, self.lexer.at());
            let tp = Type::Iterator(Box::new(format_type), Box::new(Type::Null));
            *val = Value::Iter(Box::new(create_iter), Box::new(Value::Block(lp)));
            self.vars
                .finish_scope(lp_scope, &Type::Void, self.lexer.at());
            return tp;
        }
        diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        Type::Null
    }

    // range ::= rev(<expr> '..' ['='] <expr>) | <expr> [ '..' ['='] <expr> ]
    fn parse_in_range(&mut self, expr: &mut Value, data: &Value, name: &str) -> Type {
        let mut reverse = false;
        if let LexItem::Identifier(rev) = self.lexer.peek().has {
            if &rev == "rev" {
                self.lexer.has_identifier();
                self.lexer.token("(");
                reverse = true;
            }
        }
        let in_type = self.expression(expr);
        if !self.lexer.has_token("..") {
            return in_type;
        }
        let incl = self.lexer.has_token("=");
        let mut till = Value::Null;
        let till_tp = if self.lexer.peek_token("]") {
            till = if *data == Value::Null {
                Value::Int(i32::MAX)
            } else {
                self.cl("OpLengthVector", &[data.clone()])
            };
            in_type.clone()
        } else {
            self.expression(&mut till)
        };
        let ivar = if name == "$" {
            self.create_unique("index", &in_type.clone())
        } else {
            self.create_var(&format!("{name}#index"), &in_type)
        };
        let mut ls = Vec::new();
        let test = if reverse {
            if incl {
                ls.push(v_set(
                    ivar,
                    v_if(
                        self.single_op("!", Value::Var(ivar), in_type.clone()),
                        till,
                        self.conv_op(
                            "-",
                            Value::Var(ivar),
                            Value::Int(1),
                            in_type.clone(),
                            I32.clone(),
                        ),
                    ),
                ));
            } else {
                ls.push(v_if(
                    self.single_op("!", Value::Var(ivar), in_type.clone()),
                    v_set(ivar, till),
                    Value::Null,
                ));
                ls.push(v_set(
                    ivar,
                    self.conv_op(
                        "-",
                        Value::Var(ivar),
                        Value::Int(1),
                        in_type.clone(),
                        I32.clone(),
                    ),
                ));
            }
            self.conv_op(
                "<",
                Value::Var(ivar),
                expr.clone(),
                in_type.clone(),
                till_tp,
            )
        } else {
            ls.push(v_set(
                ivar,
                v_if(
                    self.single_op("!", Value::Var(ivar), in_type.clone()),
                    expr.clone(),
                    self.conv_op(
                        "+",
                        Value::Var(ivar),
                        Value::Int(1),
                        in_type.clone(),
                        I32.clone(),
                    ),
                ),
            ));
            self.conv_op(
                if incl { ">" } else { ">=" },
                Value::Var(ivar),
                till,
                in_type.clone(),
                till_tp,
            )
        };
        ls.push(v_if(test, Value::Break(0), Value::Null));
        ls.push(Value::Var(ivar));
        *expr = Value::Iter(
            Box::new(v_let(ivar, self.null(&in_type))),
            Box::new(Value::Block(ls)),
        );
        if reverse {
            self.lexer.token(")");
        }
        Type::Iterator(Box::new(in_type), Box::new(Type::Null))
    }

    fn append_data(
        &mut self,
        tp: Type,
        list: &mut Vec<Value>,
        append: u16,
        append_value: u16,
        format: &Value,
        state: OutputState,
    ) {
        let var = Value::Var(append);
        match tp {
            Type::Integer(_, _) => {
                let fmt = format.clone();
                list.push(self.cl(
                    "OpFormatInt",
                    &[
                        var,
                        fmt,
                        Value::Int(state.radix),
                        state.width,
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                        Value::Boolean(state.plus),
                        Value::Boolean(state.note),
                    ],
                ));
            }
            Type::Long => {
                let fmt = format.clone();
                list.push(self.cl(
                    "OpFormatLong",
                    &[
                        var,
                        fmt,
                        Value::Int(state.radix),
                        state.width,
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                        Value::Boolean(state.plus),
                        Value::Boolean(state.note),
                    ],
                ));
            }
            Type::Boolean => {
                let fmt = format.clone();
                list.push(self.cl(
                    "OpFormatBool",
                    &[
                        var,
                        fmt,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                    ],
                ));
            }
            Type::Text(_) => {
                let fmt = format.clone();
                list.push(self.cl(
                    "OpFormatText",
                    &[
                        var,
                        fmt,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                    ],
                ));
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
                list.push(self.cl("OpFormatFloat", &[var, fmt, a_width, p_rec]));
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
                list.push(self.cl("OpFormatSingle", &[var, fmt, a_width, p_rec]));
            }
            Type::Vector(cont, _) => {
                let fmt = format.clone();
                let d_nr = self.data.type_def_nr(&cont);
                let db_tp = self.data.def(d_nr).known_type;
                let vec_tp = if db_tp == u16::MAX {
                    0
                } else {
                    let v = self.database.vector(db_tp);
                    self.data.check_vector(d_nr, v, self.lexer.pos());
                    v
                };
                list.push(self.cl(
                    "OpFormatDatabase",
                    &[
                        var,
                        fmt,
                        Value::Int(i32::from(vec_tp)),
                        Value::Boolean(state.note),
                    ],
                ));
            }
            Type::Iterator(vtp, _) => {
                self.append_iter(list, append, append_value, vtp.as_ref(), format, state);
            }
            Type::Reference(d_nr, _) => {
                let fmt = format.clone();
                let db_tp = self.data.def(d_nr).known_type;
                list.push(self.cl(
                    "OpFormatDatabase",
                    &[
                        var,
                        fmt,
                        Value::Int(i32::from(db_tp)),
                        Value::Boolean(state.note),
                    ],
                ));
            }
            Type::Enum(d_nr) => {
                let fmt = format.clone();
                let e_tp = self.data.def(d_nr).known_type;
                let e_val = self.cl("OpCastTextFromEnum", &[fmt, Value::Int(i32::from(e_tp))]);
                list.push(self.cl(
                    "OpFormatText",
                    &[
                        var,
                        e_val,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                    ],
                ));
            }
            _ => {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Cannot format type {}",
                    tp.name(&self.data)
                );
            }
        }
    }

    fn append_iter(
        &mut self,
        list: &mut Vec<Value>,
        append: u16,
        append_value: u16,
        var_type: &Type,
        value: &Value,
        state: OutputState,
    ) {
        if let Value::Iter(init, next) = value {
            if let Value::Block(next_ls) = &**next {
                let first = self.create_unique("first", &Type::Boolean);
                list.push(self.cl("OpAppendText", &[Value::Var(append), Value::str("[")]));
                list.push(*init.clone());
                list.push(v_let(first, Value::Boolean(true)));
                let lp = if append_value == u16::MAX {
                    self.vars.start_scope(self.lexer.at(), "append iter loop")
                } else {
                    u16::MAX
                };
                let mut append_var = append_value;
                if append_value == u16::MAX {
                    append_var = self.create_unique("val", var_type);
                }
                let mut steps = Vec::new();
                let nx = if append_value == u16::MAX {
                    self.vars.start_scope(self.lexer.at(), "append iter next")
                } else {
                    u16::MAX
                };
                let bl = if nx != u16::MAX && contains_block(next_ls) {
                    self.vars.start_scope(self.lexer.at(), "next block")
                } else {
                    u16::MAX
                };
                steps.push(v_let(append_var, Value::Block(next_ls.clone())));
                if bl != u16::MAX {
                    self.vars.finish_scope(bl, var_type, self.lexer.at());
                }
                if nx != u16::MAX {
                    self.vars.finish_scope(nx, var_type, self.lexer.at());
                }
                steps.push(v_if(
                    Value::Var(first),
                    v_set(first, Value::Boolean(false)),
                    self.cl("OpAppendText", &[Value::Var(append), Value::str(",")]),
                ));
                self.append_data(
                    var_type.clone(),
                    &mut steps,
                    append,
                    append_var,
                    &Value::Var(append_var),
                    state,
                );
                list.push(Value::Loop(steps));
                if lp != u16::MAX {
                    self.vars.finish_scope(lp, &Type::Void, self.lexer.at());
                }
                list.push(self.cl("OpAppendText", &[Value::Var(append), Value::str("]")]));
            }
        }
    }

    // <object> ::= [ <identifier> ':' <expression> { ',' <identifier> ':' <expression> } ] '}'
    fn parse_object(&mut self, td_nr: u32, code: &mut Value) -> Type {
        let link = self.lexer.link();
        let mut list = vec![];
        let rec_size = Value::Int(i32::from(
            self.database.size(self.data.def(td_nr).known_type),
        ));
        let ob = self.vars.start_scope(self.lexer.at(), "object");
        let v = if let Value::Var(v_nr) = code {
            *v_nr
        } else {
            let val_type = self.data.def(td_nr).returned.clone();
            self.create_unique("val", &val_type)
        };
        if !matches!(code, Value::Var(_)) {
            self.data.set_referenced(td_nr, self.context, Value::Null);
            list.push(v_let(
                v,
                self.cl(
                    "OpDatabase",
                    &[
                        rec_size,
                        Value::Int(i32::from(self.data.def(td_nr).known_type)),
                    ],
                ),
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
                if nr == usize::MAX {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown field {}.{field}",
                        self.data.def(td_nr).name
                    );
                } else {
                    found_fields.insert(field);
                    let td = self.data.attr_type(td_nr, nr);
                    let pos = self
                        .database
                        .position(self.data.def(td_nr).known_type, nr as u16);
                    let mut value = if let Type::Vector(_, _)
                    | Type::Sorted(_, _, _)
                    | Type::Hash(_, _, _)
                    | Type::Spacial(_, _, _)
                    | Type::Index(_, _, _) = td
                    {
                        list.push(self.cl(
                            "OpSetInt",
                            &[Value::Var(v), Value::Int(i32::from(pos)), Value::Int(0)],
                        ));
                        self.cl(
                            "OpGetField",
                            &[
                                Value::Var(v),
                                Value::Int(i32::from(pos)),
                                self.type_info(&td),
                            ],
                        )
                    } else {
                        Value::Null
                    };
                    let mut parent_tp = Type::Reference(td_nr, Vec::new());
                    self.parse_operators(&self.content(&td), &mut value, &mut parent_tp, 0);
                    if let Type::Vector(_, _)
                    | Type::Sorted(_, _, _)
                    | Type::Hash(_, _, _)
                    | Type::Spacial(_, _, _)
                    | Type::Index(_, _, _) = td
                    {
                        list.push(value);
                    } else {
                        list.push(self.set_field(td_nr, nr, Value::Var(v), value));
                    }
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
                default = to_default(&self.data.attr_type(td_nr, aid));
            }
            list.push(self.set_field(td_nr, aid, Value::Var(v), default));
        }
        let new_object = !matches!(code, Value::Var(_));
        if new_object {
            list.push(Value::Var(v));
        }
        *code = Value::Block(list);
        let tp = Type::Reference(td_nr, Vec::new());
        self.vars.finish_scope(
            ob,
            if new_object { &tp } else { &Type::Void },
            self.lexer.at(),
        );
        tp
    }

    // <if> ::= <expression> '{' <block> [ 'else' ( 'if' <if> | '{' <block> ) ]
    fn parse_if(&mut self, code: &mut Value) -> Type {
        let mut test = Value::Null;
        self.expression(&mut test);
        self.lexer.token("{");
        let mut true_code = Value::Null;
        let true_type = self.parse_block("if", &mut true_code, &Type::Unknown(0));
        let mut false_type = Type::Void;
        let mut false_code = Value::Null;
        if self.lexer.has_token("else") {
            if self.lexer.has_token("if") {
                self.parse_if(&mut false_code);
            } else {
                self.lexer.token("{");
                false_type = self.parse_block("if", &mut false_code, &true_type);
            }
        }
        *code = v_if(test, true_code, false_code);
        merge_dependencies(&true_type, &false_type)
    }

    // <for> ::= <identifier> 'in' <expression> '{' <block>
    fn parse_for(&mut self, val: &mut Value) {
        if let Some(id) = self.lexer.has_identifier() {
            let for_scope = self.vars.start_scope(self.lexer.at(), "for");
            self.lexer.token("in");
            let mut expr = Value::Null;
            let in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            let var_tp = if let Type::Vector(t_nr, _) = &in_type {
                *t_nr.clone()
            } else if let Type::Sorted(dnr, _, _) = in_type {
                Type::Reference(dnr, Vec::new())
            } else if let Type::Iterator(i_tp, _) = &in_type {
                if **i_tp == Type::Null {
                    I32.clone()
                } else {
                    *i_tp.clone()
                }
            } else if let Type::Reference(_, _) | Type::Text(_) | Type::Integer(_, _) | Type::Long =
                in_type
            {
                in_type.clone()
            } else {
                if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown in expression type {}",
                        in_type.name(&self.data)
                    );
                }
                Type::Null
            };
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
            let iter_var = self.create_var(&format!("{id}#index"), &I32);
            let loop_scope = self.vars.start_scope(self.lexer.at(), "for loop");
            let in_loop = self.in_loop;
            let for_var = self.create_var(&id, &var_tp);
            let mut create_iter = expr;
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var);
            if iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return;
            }
            let for_next = v_let(for_var, iter_next);
            self.in_loop = true;
            self.parse_block("for", &mut block, &Type::Void);
            self.in_loop = in_loop;
            let mut for_steps = vec![create_iter];
            let mut lp = vec![for_next];
            if !matches!(in_type, Type::Iterator(_, _)) {
                let mut test_for = Value::Var(for_var);
                self.convert(&mut test_for, &var_tp, &Type::Boolean);
                test_for = self.cl("OpNot", &[test_for]);
                lp.push(v_if(test_for, Value::Break(0), Value::Null));
            }
            if if_step != Value::Null {
                lp.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            lp.push(block);
            for_steps.push(Value::Loop(lp));
            *val = Value::Block(for_steps);
            self.vars
                .finish_scope(loop_scope, &Type::Void, self.lexer.at());
            self.vars
                .finish_scope(for_scope, &Type::Void, self.lexer.at());
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        }
    }

    pub fn null(&mut self, tp: &Type) -> Value {
        match tp {
            Type::Integer(_, _) => self.cl("OpConvIntFromNull", &[]),
            Type::Boolean => self.cl("OpConvBoolFromNull", &[]),
            Type::Enum(tp) => self.cl(
                "OpConvEnumFromNull",
                &[Value::Int(i32::from(self.data.def(*tp).known_type))],
            ),
            Type::Long => self.cl("OpConvLongFromNull", &[]),
            Type::Float => self.cl("OpConvFloatFromNull", &[]),
            Type::Single => self.cl("OpConvSingleFromNull", &[]),
            Type::Text(_) => self.cl("OpConvTextFromNull", &[]),
            Type::Reference(_, _) => self.cl("OpConvRefFromNull", &[]),
            _ => Value::Null,
        }
    }

    // For now, assume that returned texts are always related to internal variables
    #[allow(clippy::unused_self)]
    fn text_return(&mut self, _code: &Value) {
        //if !self.first_pass {
        // Assume that the first pass already added the needed attributes
        //}
        /*
        let mut ls = HashSet::new();
        self.gather_text_vars(&mut ls, code);
        println!("vars {ls:?}");
        for v in ls {
            let var_name = self.vars.name(v);
            println!("var {var_name}");
            self.data.add_attribute(
                &mut self.lexer,
                self.context,
                var_name,
                Type::RefVar(Box::new(Type::Text(false, Vec::new()))),
            );
            // move variable to that type
            // - position of variable will move towards the attributes position
            // - positions of earlier variables will need to move to make room
            // - treat this as an argument versus a variable for byte code types?
            //    *variable < stack.def().attributes.len() as u32;
        }*/
    }

    // <return> ::= [ <expression> ]
    fn parse_return(&mut self, val: &mut Value) {
        // validate if there is a defined return value
        let mut v = Value::Null;
        let r_type = self.data.def(self.context).returned.clone();
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
            if !self.convert(&mut v, &t, &r_type) {
                self.validate_convert("return", &t, &r_type);
            }
            if matches!(r_type, Type::Text(_)) {
                self.text_return(&v);
            }
        } else if !self.first_pass && r_type != Type::Void {
            diagnostic!(self.lexer, Level::Error, "Expect expression after return");
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
            let mut t = self.expression(&mut p);
            let add = self.data.def_nr("OpAddText");
            if let Value::Call(c, _) = p {
                if matches!(t, Type::Text(_)) && c == add {
                    let mut l = Vec::new();
                    let work = self.work_var(&mut l);
                    self.replace_add_text(&mut l, add, &p, work);
                    l.push(Value::Var(work));
                    p = Value::Block(l);
                    t = Type::Text(vec![work]);
                }
            }
            types.push(t);
            list.push(p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        if name == "assert" {
            let mut test = list[0].clone();
            self.convert(&mut test, &types[0], &Type::Boolean);
            let mut message = list[1].clone();
            self.convert(&mut message, &types[1], &Type::Text(Vec::new()));
            let show = self.cl("OpPanic", &[message]);
            *val = v_if(test, Value::Null, show);
            Type::Void
        } else {
            self.call(val, name, &list, &types)
        }
    }

    // <size> ::= ( <type> | <var> ) ')'
    fn parse_size(&mut self, val: &mut Value) -> Type {
        let mut found = false;
        let lnk = self.lexer.link();
        if let Some(id) = self.lexer.has_identifier() {
            let d_nr = self.data.def_nr(&id);
            if d_nr != u32::MAX && self.data.def_type(d_nr) != DefType::EnumValue {
                if !self.first_pass && self.data.def_type(d_nr) == DefType::Unknown {
                    found = true;
                } else if let Some(tp) = self.parse_type(u32::MAX, &id, false) {
                    found = true;
                    if !self.first_pass {
                        *val = Value::Int(i32::from(
                            self.database
                                .size(self.data.def(self.data.type_elm(&tp)).known_type),
                        ));
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
                *val = Value::Int(i32::from(
                    self.database.size(self.data.def(e_tp).known_type),
                ));
            }
        }
        if !self.first_pass && !found {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect a variable or type after sizeof"
            );
        }
        self.lexer.token(")");
        I32.clone()
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
            let t = self.expression(&mut p);
            let add = self.data.def_nr("OpAddText");
            if let Value::Call(c, _) = p {
                if matches!(t, Type::Text(_)) && c == add {
                    let arg_scope = self.vars.start_scope(self.lexer.at(), "text argument");
                    let mut l = Vec::new();
                    let work = self.work_var(&mut l);
                    self.replace_add_text(&mut l, add, &p, work);
                    l.push(Value::Var(work));
                    self.vars.finish_scope(arg_scope, &t, self.lexer.at());
                    p = Value::Block(l);
                }
            }

            types.push(t);
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

fn merge_dependencies(a: &Type, b: &Type) -> Type {
    if let (Type::Text(da), Type::Text(db)) = (a, b) {
        let mut d = HashSet::new();
        for v in da {
            d.insert(*v);
        }
        for v in db {
            d.insert(*v);
        }
        Type::Text(d.into_iter().collect())
    } else {
        a.clone()
    }
}

fn rename(op: &str) -> &str {
    match op {
        "*" => "Mul",
        "+" => "Add",
        "-" => "Min",
        "/" => "Div",
        "&" => "Land",
        "|" => "Lor",
        "^" => "Pow",
        "<<" => "SLeft",
        ">>" => "SRight",
        "==" => "Eq",
        "!=" => "Ne",
        "<" => "Lt",
        "<=" => "Le",
        ">" => "Gt",
        ">=" => "Ge",
        "%" => "Rem",
        "!" => "Not",
        "+=" => "Append",
        _ => op,
    }
}

fn contains_block(code: &[Value]) -> bool {
    for v in code {
        if block(v) {
            return true;
        }
    }
    false
}

fn block(code: &Value) -> bool {
    match code {
        Value::Call(_, ps) => {
            for p in ps {
                if block(p) {
                    return true;
                }
            }
        }
        Value::Block(_) | Value::Loop(_) => return true,
        _ => {}
    }
    false
}
