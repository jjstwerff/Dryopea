// Copyright (c) 2022-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Parse scripts and create internal code from it.
//! Including type checking.
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

use crate::data::{
    Argument, Data, DefType, I32, Type, Value, to_default, v_block, v_if, v_loop, v_set,
};
use crate::database::{Parts, Stores};
use crate::diagnostics::{Diagnostics, Level, diagnostic_format};
use crate::lexer::{LexItem, LexResult, Lexer, Link, Mode};
use crate::variables::Function;
use crate::{scopes, typedef};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::env;
use std::fs::{File, metadata, read_dir};
use std::io::BufReader;
use std::io::Write;
use std::io::prelude::BufRead;
use std::string::ToString;
use typedef::complete_definition;

/**
The number of defined reserved text worker variables. A worker variable is needed when
two texts are added or a formatting text is used, and the result is used as a parameter to a call.
These are reused when possible. However, when calculating a text, a new text expression
is used a next worker variable is needed.
This number indicated the depth of these expressions, not the number of these expressions in a
function.
*/
pub struct Parser {
    pub todo_files: HashMap<String, u16>,
    /// All definitions
    pub data: Data,
    pub database: Stores,
    /// The lexer on the current text file
    pub lexer: Lexer,
    /// Are we currently allowing break/continue statements?
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
    first_pass: bool,
    vars: Function,
    /// Last seen line inside the source code, an increase inserts it in the internal code.
    line: u32,
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
/// Used to validate constant names
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
            todo_files: HashMap::new(),
            data: Data::new(),
            database: Stores::new(),
            lexer: Lexer::default(),
            in_loop: false,
            file: 1,
            diagnostics: Diagnostics::new(),
            default: false,
            context: u32::MAX,
            first_pass: true,
            vars: Function::new("", "none"),
            line: 0,
        }
    }

    /// Parse the content of a given file.
    /// - filename: the file to parse
    /// - default: parsing system definitions
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
        self.data.reset();
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl != Level::Error && lvl != Level::Fatal {
            self.first_pass = false;
            self.data.reset();
            self.lexer = Lexer::lines(
                BufReader::new(File::open(filename).unwrap()).lines(),
                filename,
            );
            self.parse_file();
        }
        self.diagnostics.fill(self.lexer.diagnostics());
        self.diagnostics.is_empty()
    }

    /// Parse all .lav files found in a directory tree in alphabetical ordering.
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
                .is_some_and(|e| e.eq_ignore_ascii_case("lav"));
            let file_name = p.path().to_string_lossy().to_string();
            let data = metadata(&file_name)?;
            if own_file || data.is_dir() {
                files.insert(file_name);
            }
        }
        for f in files {
            let types = self.database.types.len();
            let from = self.data.definitions();
            let data = metadata(&f)?;
            if data.is_dir() {
                self.parse_dir(&f, default)?;
            } else if !self.parse(&f, default) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("{}", self.diagnostics),
                ));
            }
            scopes::check(&mut self.data);
            self.output(&f, types, from)?;
        }
        Ok(())
    }

    fn output(&mut self, f: &str, types: usize, from: u32) -> std::io::Result<()> {
        let file = if let Some(p) = f.rfind('/') {
            &f[p + 1..]
        } else {
            f
        };
        let mut w = File::create(format!("tests/code/{file}.txt"))?;
        let to = self.database.types.len();
        for tp in types..to {
            writeln!(w, "Type {tp}:{}", self.database.show_type(tp as u16, true))?;
        }
        for d_nr in from..self.data.definitions() {
            if self.data.def(d_nr).code == Value::Null {
                continue;
            }
            write!(w, "{} ", self.data.def(d_nr).header(&self.data, d_nr))?;
            let mut vars = Function::copy(&self.data.def(d_nr).variables);
            self.data
                .show_code(&mut w, &mut vars, &self.data.def(d_nr).code, 0, false)?;
            writeln!(w, "\n")?;
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
        self.data.reset();
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl == Level::Error || lvl == Level::Fatal {
            self.diagnostics.fill(self.lexer.diagnostics());
            return;
        }
        self.data.reset();
        self.lexer = Lexer::from_str(text, filename);
        self.first_pass = false;
        self.parse_file();
        self.diagnostics.fill(self.lexer.diagnostics());
    }

    // ********************
    // * Helper functions *
    // ********************

    /// Get an iterator.
    /// The iterable expression is in *code.
    /// Creating the iterator will be in *code afterward.
    /// Return the next expression; with `Value::None` the iterator creation was impossible.
    fn iterator(
        &mut self,
        code: &mut Value,
        is_type: &Type,
        should: &Type,
        iter_var: u16,
    ) -> Value {
        if let Value::Iter(_, start, next) = code.clone() {
            if matches!(*next, Value::Block(_)) {
                *code = *start;
                return *next.clone();
            }
            panic!("Incorrect Iter");
        }
        if matches!(*is_type, Type::Text(_)) {
            let res_var = self
                .vars
                .unique("for_result", &Type::Character, &mut self.lexer);
            let l = self.cl("OpLengthCharacter", &[Value::Var(res_var)]);
            let next = vec![
                v_set(
                    res_var,
                    self.cl("OpTextCharacter", &[code.clone(), Value::Var(iter_var)]),
                ),
                v_set(iter_var, self.cl("OpAddInt", &[Value::Var(iter_var), l])),
                Value::Var(res_var),
            ];
            *code = v_set(iter_var, Value::Int(0));
            return v_block(next, Type::Character, "for text next");
        }
        if is_type == should {
            // there was already an iterator.
            let orig = code.clone();
            *code = Value::Null; // there is no iterator to create, we got it already
            return orig;
        }
        if let Type::Iterator(_, _) = should {
            match is_type {
                Type::Vector(vtp, dep) => {
                    let i = Value::Var(iter_var);
                    let vec_tp = self.data.type_def_nr(vtp);
                    let size = self.database.size(self.data.def(vec_tp).known_type);
                    let mut ref_expr = self.cl(
                        "OpGetVector",
                        &[code.clone(), Value::Int(i32::from(size)), i.clone()],
                    );
                    if let Type::Reference(_, _) = *vtp.clone() {
                    } else {
                        ref_expr = self.get_field(vec_tp, usize::MAX, ref_expr);
                    }
                    let mut tp = *vtp.clone();
                    for d in dep {
                        tp = tp.depending(*d);
                    }
                    let next = v_block(
                        vec![
                            v_set(
                                iter_var,
                                self.op("Add", i.clone(), Value::Int(1), I32.clone()),
                            ),
                            ref_expr,
                        ],
                        *vtp.clone(),
                        "iter next",
                    );
                    self.vars
                        .set_loop(0, self.data.def(vec_tp).known_type, code);
                    *code = v_set(iter_var, Value::Int(-1));
                    return next;
                }
                Type::Sorted(_, _, _)
                | Type::Hash(_, _, _)
                | Type::Index(_, _, _)
                | Type::Spacial(_, _, _) => {
                    let mut ls = Vec::new();
                    self.fill_iter(&mut ls, code, is_type, true, true);
                    ls.push(Value::Int(0));
                    ls.push(Value::Int(0));
                    let iter_expr = self.cl("OpIterate", &ls);
                    let mut ls = vec![Value::Var(iter_var)];
                    self.fill_iter(&mut ls, code, is_type, false, true);
                    let next_expr = self.cl("OpStep", &ls);
                    *code = v_set(iter_var, iter_expr);
                    return next_expr;
                }
                _ => {
                    if self.first_pass {
                        return Value::Null;
                    }
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown iterator type {}",
                        is_type.name(&self.data)
                    );
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
        if let (Type::Reference(ref_tp, _), Type::Enum(enum_tp, true, _)) = (is_type, should) {
            for a in &self.data.def(*enum_tp).attributes {
                if a.name == self.data.def(*ref_tp).name {
                    return true;
                }
            }
        }
        if let Type::RefVar(ref_tp) = is_type
            && self.convert(code, ref_tp, should)
        {
            return true;
        }
        if let Type::RefVar(ref_tp) = should
            && ref_tp.is_equal(is_type)
        {
            *code = self.cl("OpCreateRef", std::slice::from_ref(code));
            return true;
        }
        let mut check_type = is_type;
        let r = Type::Reference(self.data.def_nr("reference"), Vec::new());
        let e = Type::Enum(0, false, Vec::new());
        if let Type::Vector(_nr, _) = is_type {
            if let Type::Vector(v, _) = should
                && v.is_unknown()
            {
                return true;
            }
        } else if let Type::Reference(_, _) = is_type {
            if matches!(*should, Type::Reference(0, _)) {
                return true;
            }
            check_type = &r;
        } else if let Type::Enum(_, false, _) = is_type {
            if *should == e {
                return true;
            }
            check_type = &e;
        }
        for &dnr in self.data.get_possible("OpConv", &self.lexer) {
            if self.data.def(dnr).name.ends_with("FromNull") {
                if *is_type == Type::Null {
                    if self.data.def(dnr).returned == *should {
                        *code = Value::Call(dnr, vec![]);
                        return true;
                    } else if matches!(self.data.def(dnr).returned, Type::Reference(_, _))
                        && let Type::Reference(_, _) = *should
                    {
                        *code = Value::Call(dnr, vec![]);
                        return true;
                    }
                }
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
        if matches!(is_type, Type::Text(_))
            && matches!(should, Type::Enum(_, true, _) | Type::Reference(_, _))
        {
            *code = self.cl(
                "OpCastVectorFromText",
                &[code.clone(), Value::Int(i32::from(should_kt))],
            );
            return true;
        }
        for &dnr in self.data.get_possible("OpCast", &self.lexer) {
            if self.data.attributes(dnr) == 1
                && self.data.attr_type(dnr, 0).is_same(is_type)
                && self.data.def(dnr).returned.is_same(should)
            {
                if let Type::Enum(tp, false, _) = should {
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
            if let Type::RefVar(tp) = should
                && tp.is_equal(test_type)
            {
                return true;
            }
            if let (Type::Enum(_e, _, _), Type::Enum(o, _, _)) = (test_type, should)
                && self.data.def(*o).name == "enumerate"
            {
                return true;
            }
            if let (Type::Enum(t, false, _), Type::Enum(s, false, _)) = (test_type, should)
                && *t == *s
            {
                return true;
            }
            if let (Type::Enum(_, false, _), Type::Integer(_, _)) = (test_type, should) {
                return true;
            }
            if let Type::Reference(r, _) = should
                && *r == self.data.def_nr("reference")
                && let Type::Reference(_, _) = test_type
            {
                return true;
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
        // We still need to know the types.
        let d_nr = self.data.def_nr(name);
        if d_nr != u32::MAX {
            self.call_nr(code, d_nr, list, types, true)
        } else if self.first_pass && !self.default {
            Type::Unknown(0)
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
            let nm = self.data.attr_name(d_nr, f_nr);
            self.database.position(self.data.def(d_nr).known_type, &nm)
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
            Type::Enum(_, false, _) => self.cl("OpGetEnum", &[code, p]),
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
            | Type::Enum(_, true, _)
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
        let nm = self.data.attr_name(d_nr, f_nr);
        let pos = self.database.position(self.data.def(d_nr).known_type, &nm);
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
            | Type::Sorted(_, _, _)
            | Type::Character => self.cl("OpSetInt", &[ref_code, pos_val, val_code]),
            Type::Enum(_, false, _) => self.cl("OpSetEnum", &[ref_code, pos_val, val_code]),
            Type::Enum(_, true, _) => {
                panic!("Still implement copy enum content")
            }
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
                if let (Some(Type::Enum(f, _, _)), Some(Type::Enum(s, _, _))) =
                    (types.first(), types.get(1))
                    && f != s
                {
                    break;
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
        let mut all_types = Vec::from(types);
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
                    if let (Type::Vector(to_tp, _), Type::Vector(a_tp, _)) = (&tp, actual_type)
                        && a_tp.is_unknown()
                        && !to_tp.is_unknown()
                    {
                        self.change_var(&actual_code, &tp);
                        actual.push(actual_code);
                        continue;
                    }
                    if actual_type.is_unknown()
                        && let Type::Vector(_, _) = &tp
                    {
                        self.change_var(&actual_code, &tp);
                        actual.push(actual_code);
                        continue;
                    }
                    if let (Type::Integer(_, _), Type::Enum(_, true, _)) = (&tp, actual_type) {
                        // An enum with a structure is normally a reference to the data.
                        // But for compares we can expect be a constant Enum value.
                        let cd = if matches!(actual_code, Value::Enum(_, _)) {
                            actual_code
                        } else {
                            self.cl("OpGetEnum", &[actual_code, Value::Int(0)])
                        };
                        actual.push(self.cl("OpConvIntFromEnum", &[cd]));
                        continue;
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
        self.add_defaults(d_nr, &mut actual, &mut all_types);
        let tp = self.call_dependencies(d_nr, &all_types);
        *code = Value::Call(d_nr, actual);
        tp
    }

    // Gather depended on variables from arguments of the given called routine.
    fn call_dependencies(&mut self, d_nr: u32, types: &[Type]) -> Type {
        let tp = self.data.def(d_nr).returned.clone();
        if let Type::Text(d) = tp {
            Type::Text(Self::resolve_deps(types, &d))
        } else if let Type::Vector(to, d) = tp {
            Type::Vector(to, Self::resolve_deps(types, &d))
        } else if let Type::Sorted(to, key, d) = tp {
            Type::Sorted(to, key, Self::resolve_deps(types, &d))
        } else if let Type::Hash(to, key, d) = tp {
            Type::Hash(to, key, Self::resolve_deps(types, &d))
        } else if let Type::Index(to, key, d) = tp {
            Type::Index(to, key, Self::resolve_deps(types, &d))
        } else if let Type::Spacial(to, key, d) = tp {
            Type::Spacial(to, key, Self::resolve_deps(types, &d))
        } else if let Type::Reference(to, d) = tp {
            Type::Reference(to, Self::resolve_deps(types, &d))
        } else {
            tp
        }
    }

    fn resolve_deps(types: &[Type], d: &[u16]) -> Vec<u16> {
        let mut dp = HashSet::new();
        for ar in d {
            if *ar as usize >= types.len() {
                continue;
            }
            if let Type::Text(ad)
            | Type::Vector(_, ad)
            | Type::Sorted(_, _, ad)
            | Type::Hash(_, _, ad)
            | Type::Index(_, _, ad)
            | Type::Spacial(_, _, ad)
            | Type::Reference(_, ad) = &types[*ar as usize]
            {
                for a in ad {
                    dp.insert(*a);
                }
            }
        }
        Vec::from_iter(dp)
    }

    fn add_defaults(&mut self, d_nr: u32, actual: &mut Vec<Value>, all_types: &mut Vec<Type>) {
        if actual.len() < self.data.attributes(d_nr) {
            // Insert the default values for not given attributes
            for a_nr in actual.len()..self.data.attributes(d_nr) {
                let default = self.data.def(d_nr).attributes[a_nr].value.clone();
                let tp = self.data.attr_type(d_nr, a_nr);
                if let Type::Vector(content, _) = &tp {
                    assert_eq!(
                        default,
                        Value::Null,
                        "Expect a null default on database references"
                    );
                    let vr = self.vars.work_refs(&tp, &mut self.lexer);
                    self.data.vector_def(&mut self.lexer, content);
                    all_types.push(Type::Vector(content.clone(), vec![vr]));
                    actual.push(Value::Var(vr));
                } else if let Type::Reference(content, _) = tp {
                    assert_eq!(
                        default,
                        Value::Null,
                        "Expect a null default on database references"
                    );
                    let vr = self.vars.work_refs(&tp, &mut self.lexer);
                    all_types.push(Type::Reference(content, vec![vr]));
                    actual.push(Value::Var(vr));
                } else if let Type::RefVar(vtp) = &tp {
                    let mut ls = Vec::new();
                    let vr = if matches!(**vtp, Type::Text(_)) {
                        let wv = self.vars.work_text(&mut self.lexer);
                        if default != Value::Null
                            && if let Value::Text(t) = &default {
                                !t.is_empty()
                            } else {
                                true
                            }
                        {
                            ls.push(self.cl("OpAppendText", &[Value::Var(wv), default]));
                        }
                        wv
                    } else {
                        panic!("Unexpected reference type {}", vtp.name(&self.data));
                    };
                    ls.push(self.cl("OpCreateRef", &[Value::Var(vr)]));
                    actual.push(v_block(
                        ls,
                        Type::Reference(self.data.def_nr("reference"), vec![vr]),
                        "default ref",
                    ));
                    all_types.push(tp.clone());
                } else {
                    actual.push(default);
                    all_types.push(tp.clone());
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
        while self.lexer.has_token("use") {
            if let Some(id) = self.lexer.has_identifier() {
                if self.data.use_exists(&id) {
                    self.lexer.token(";");
                    continue;
                }
                let f = self.lib_path(&id);
                if std::path::Path::new(&f).exists() {
                    self.todo_files
                        .insert(self.lexer.pos().file.clone(), self.data.source);
                    self.data.use_add(&id);
                    self.lexer = Lexer::lines(BufReader::new(File::open(&f).unwrap()).lines(), &f);
                } else {
                    diagnostic!(self.lexer, Level::Error, "Included file {id} not found");
                }
            }
        }
        self.file += 1;
        self.line = 0;
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
        }
        typedef::actual_types(
            &mut self.data,
            &mut self.database,
            &mut self.lexer,
            start_def,
        );
        typedef::fill_all(&mut self.data, &mut self.database, start_def);
        self.database.finish();
        let lvl = self.lexer.diagnostics().level();
        if lvl == Level::Error || lvl == Level::Fatal {
            return;
        }
        // Parse all files left in the todo_files list, as they are halted to parse a use file.
        for (t, s) in self.todo_files.clone() {
            self.todo_files.remove(&t);
            self.lexer = Lexer::lines(BufReader::new(File::open(&t).unwrap()).lines(), &t);
            self.data.source = s;
            self.parse_file();
        }
    }

    fn lib_path(&mut self, id: &String) -> String {
        // - a source file the lib directory in the project (project-supplied)
        let mut f = format!("lib/{id}.lav");
        if !std::path::Path::new(&f).exists() {
            f = format!("{id}.lav");
        }
        let cur_script = &self.lexer.pos().file;
        let cur_dir = if let Some(p) = cur_script.rfind('/') {
            &cur_script[0..p]
        } else {
            ""
        };
        let base_dir = if cur_dir.contains("/tests/") {
            &cur_dir[..cur_dir.find("/tests/").unwrap()]
        } else {
            ""
        };
        // - a lib directory relative to the current directory
        if !cur_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{cur_dir}/lib/{id}.lav");
        }
        // - a lib directory relative to the base directory when inside /tests/
        if !base_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{base_dir}/lib/{id}.lav");
        }
        // - a directory with the same name of the current script
        if !std::path::Path::new(&f).exists() {
            f = format!("{}/{id}.lav", &cur_script[0..cur_script.len() - 4]);
        }
        // - a user defined lib directory (externally downloaded)
        if !std::path::Path::new(&f).exists()
            && let Some(v) = env::var_os("LAVITION_LIB")
        {
            let libs = v.to_str().unwrap();
            for l in libs.split(':') {
                f = format!("{l}/{id}.lav");
                if std::path::Path::new(&f).exists() {
                    break;
                }
            }
        }
        // - the current directory (beside the parsed file)
        if !cur_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{cur_dir}/{id}.lav");
        }
        // - the base directory when inside /tests/
        if !base_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{base_dir}/{id}.lav");
        }
        f
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
            self.data
                .set_returned(d_nr, Type::Enum(d_nr, false, Vec::new()));
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
                let v = self
                    .data
                    .add_def(&value_name, self.lexer.pos(), DefType::EnumValue);
                self.data.definitions[v as usize].parent = d_nr;
                v
            } else {
                self.data.def_nr(&value_name)
            };
            if self.lexer.has_token("{") {
                if self.first_pass {
                    if matches!(self.data.def(d_nr).returned, Type::Enum(_, false, _)) {
                        self.data.definitions[d_nr as usize].returned =
                            Type::Enum(d_nr, true, Vec::new());
                    }
                    self.data
                        .set_returned(v_nr, Type::Enum(d_nr, true, Vec::new()));
                    self.data.add_attribute(
                        &mut self.lexer,
                        d_nr,
                        &value_name,
                        Type::Enum(d_nr, true, Vec::new()),
                    );
                    // Enum values start with 1 as 0 is de null/undefined value.
                    self.data
                        .set_attr_value(d_nr, nr as usize, Value::Enum(nr + 1, u16::MAX));
                }
                loop {
                    let Some(a_name) = self.lexer.has_identifier() else {
                        diagnostic!(self.lexer, Level::Error, "Expect attribute");
                        return true;
                    };
                    if self.first_pass && self.data.attr(v_nr, &a_name) != usize::MAX {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "field `{}` is already declared",
                            a_name
                        );
                    }
                    self.lexer.token(":");
                    self.parse_field(v_nr, &a_name);
                    if !self.lexer.has_token(",") {
                        break;
                    }
                }
                self.lexer.token("}");
            } else if self.first_pass {
                self.data
                    .set_returned(v_nr, Type::Enum(d_nr, false, Vec::new()));
                self.data.add_attribute(
                    &mut self.lexer,
                    d_nr,
                    &value_name,
                    Type::Enum(d_nr, false, Vec::new()),
                );
                // Enum values start with 1 as 0 is de null/undefined value.
                self.data
                    .set_attr_value(d_nr, nr as usize, Value::Enum(nr + 1, u16::MAX));
            } else if self.data.def(d_nr).returned != self.data.def(v_nr).returned {
                self.data.definitions[v_nr as usize].returned =
                    self.data.def(d_nr).returned.clone();
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
        self.vars = Function::new(&fn_name, &self.lexer.pos().file);
        if self.data.def_nr(&fn_name) != u32::MAX
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
        if self.context != u32::MAX {
            self.vars
                .append(&mut self.data.definitions[self.context as usize].variables);
        }
        /*
        if !self.default {
            self.vars.logging = true;
        }*/
        if self.first_pass && self.context != u32::MAX {
            self.data.set_returned(self.context, result);
        }
        if !self.lexer.has_token(";") {
            for (a_nr, a) in arguments.iter().enumerate() {
                if self.first_pass {
                    let v_nr = self.create_var(&a.name, &a.typedef);
                    self.vars.become_argument(v_nr);
                    self.var_usages(v_nr, false);
                } else {
                    self.change_var_type(a_nr as u16, &a.typedef);
                }
            }
            self.parse_code();
        }
        if !self.first_pass && self.context != u32::MAX {
            self.vars.test_used(&mut self.lexer, &self.data);
        }
        self.lexer.has_token(";");
        self.parse_rust();
        self.data.op_code(self.context);
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
                let mut t = Value::Var(arguments.len() as u16);
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
            if let Some(id) = self.lexer.has_identifier()
                && let Some(tp) = self.parse_type(d_nr, &id, false)
            {
                args.push(tp);
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
        if self.lexer.has_token("->")
            && let Some(id) = self.lexer.has_identifier()
            && let Some(tp2) = self.parse_type(d_nr, &id, false)
        {
            r_type = tp2;
        }
        Type::Function(args, Box::new(r_type))
    }

    // <type> ::= <identifier> [::<identifier>] [ '<' ( <sub_type> | <type> ) '>' ] [ '[' ( <nr> { ',' <nr> } ']' ]
    fn parse_type(&mut self, on_d: u32, type_name: &str, returned: bool) -> Option<Type> {
        let tp_nr = if self.lexer.has_token("::") {
            if let Some(name) = self.lexer.has_identifier() {
                let source = self.data.get_source(type_name);
                self.data.source_nr(source, &name)
            } else {
                diagnostic!(self.lexer, Level::Error, "Expect type from {type_name}");
                return None;
            }
        } else {
            self.data.def_nr(type_name)
        };
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
        if self.lexer.has_token("<")
            && let Some(value) = self.sub_type(on_d, type_name, link)
        {
            return Some(value);
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
            && matches!(
                dt,
                DefType::Type
                    | DefType::Enum
                    | DefType::EnumValue
                    | DefType::Struct
                    | DefType::Main
            )
        {
            if matches!(dt, DefType::EnumValue)
                || (self.first_pass && matches!(dt, DefType::Struct | DefType::Main))
            {
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

    fn sub_type(&mut self, on_d: u32, type_name: &str, link: Link) -> Option<Type> {
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
                                diagnostic!(self.lexer, Level::Error, "Expect an iterator type");
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
                            "Subtype only allowed on structures"
                        );
                        Type::Unknown(0)
                    }
                });
            }
            assert!(self.first_pass, "Incorrect handling of unknown types");
        } else {
            self.lexer.revert(link);
        }
        None
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
            if let Type::Unknown(_) = self.data.definitions[d_nr as usize].returned {
                self.data.definitions[d_nr as usize].position = self.lexer.pos().clone();
                self.data.definitions[d_nr as usize].def_type = DefType::Struct;
                self.data.definitions[d_nr as usize].returned = Type::Reference(d_nr, Vec::new());
            } else {
                diagnostic!(self.lexer, Level::Error, "Redefined struct {}", id);
            }
        }
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
        if let Value::Block(bl) = &mut v {
            let ls = &mut bl.operators;
            for wt in self.vars.work_texts() {
                ls.insert(0, v_set(wt, Value::Text(String::new())));
            }
            for r in self.vars.work_references() {
                if !self.vars.is_argument(r) && self.vars.tp(r).depend().is_empty() {
                    ls.insert(0, v_set(r, Value::Null));
                }
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

    fn change_var(&mut self, code: &Value, tp: &Type) -> bool {
        if let Value::Var(v_nr) = code {
            let mut is_text = matches!(self.vars.tp(*v_nr), Type::Text(_));
            if let Type::RefVar(i) = self.vars.tp(*v_nr)
                && matches!(**i, Type::Text(_))
            {
                is_text = true;
            }
            if !is_text || *tp != Type::Character {
                self.change_var_type(*v_nr, tp);
            }
            true
        } else {
            false
        }
    }

    fn change_var_type(&mut self, v_nr: u16, tp: &Type) {
        let chg = self
            .vars
            .change_var_type(v_nr, tp, &self.data, &mut self.lexer);
        if chg
            && !tp.is_unknown()
            && let Type::Vector(elm, _) = tp
        {
            self.data.vector_def(&mut self.lexer, elm);
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
                let var_nr = if let Value::Var(v_nr) = *code {
                    v_nr
                } else if op == "+=" && matches!(f_type, Type::Text(_)) {
                    let v = self
                        .vars
                        .unique("field", &Type::Text(vec![]), &mut self.lexer);
                    *code = Value::Var(v);
                    parent_tp = Type::Null;
                    v
                } else {
                    u16::MAX
                };
                let mut s_type = self.parse_operators(&f_type, code, &mut parent_tp, 0);
                if let Type::Rewritten(tp) = s_type {
                    s_type = *tp;
                }
                self.change_var(&to, &s_type);
                if matches!(f_type, Type::Text(_)) {
                    self.assign_text(code, &s_type, &to, op, var_nr);
                    return Type::Void;
                }
                if let Type::RefVar(t) = &f_type
                    && matches!(**t, Type::Text(_))
                {
                    if matches!(code, Value::Insert(_)) {
                        // nothing
                    } else if op == "=" {
                        *code = v_set(var_nr, code.clone());
                    } else if s_type == Type::Character {
                        *code =
                            self.cl("OpAppendRefCharacter", &[Value::Var(var_nr), code.clone()]);
                    } else {
                        *code = self.cl("OpAppendRefText", &[Value::Var(var_nr), code.clone()]);
                    }
                    return Type::Void;
                }
                if var_nr != u16::MAX && self.create_vector(code, &f_type, op, var_nr) {
                    return Type::Void;
                }
                if !matches!(code, Value::Insert(_)) {
                    *code = self.towards_set(&to, code, &f_type, &op[0..1]);
                }
                return Type::Void;
            }
        }
        *code = to;
        f_type
    }

    fn assign_text(&mut self, code: &mut Value, tp: &Type, to: &Value, op: &str, var_nr: u16) {
        if let Value::Call(_, parms) = to.clone() {
            if op == "=" {
                let mut p = parms.clone();
                p.push(code.clone());
                *code = self.cl("OpSetText", &p);
            } else {
                let mut ls = Vec::new();
                ls.push(v_set(var_nr, to.clone()));
                if let Value::Insert(cd) = code {
                    for c in cd {
                        ls.push(c.clone());
                    }
                } else if *tp == Type::Character {
                    ls.push(self.cl("OpAppendCharacter", &[Value::Var(var_nr), code.clone()]));
                } else {
                    ls.push(self.cl("OpAppendText", &[Value::Var(var_nr), code.clone()]));
                }
                let mut p = parms.clone();
                p.push(Value::Var(var_nr));
                ls.push(self.cl("OpSetText", &p));
                *code = Value::Insert(ls);
            }
        } else if let Value::Insert(ls) = code {
            if op == "=" {
                ls.insert(0, v_set(var_nr, Value::Text(String::new())));
            }
        } else if op == "=" {
            *code = v_set(var_nr, code.clone());
        } else if *tp == Type::Character {
            *code = self.cl("OpAppendCharacter", &[Value::Var(var_nr), code.clone()]);
        } else {
            *code = self.cl("OpAppendText", &[Value::Var(var_nr), code.clone()]);
        }
    }

    fn create_vector(&mut self, code: &mut Value, f_type: &Type, op: &str, var_nr: u16) -> bool {
        if let (Value::Insert(ls), Type::Vector(tp, _)) = (code, f_type) {
            if op == "=" {
                for (s_nr, s) in self.vector_db(tp, var_nr).iter().enumerate() {
                    ls.insert(s_nr, s.clone());
                }
            }
            true
        } else {
            false
        }
    }

    fn copy_ref(&mut self, to: &Value, code: &Value, f_type: &Type) -> Value {
        let d_nr = self.data.type_def_nr(f_type);
        let tp = self.data.def(d_nr).known_type;
        // println!("here! f_type:{f_type} pass:{} to:{to:?} at {}", self.first_pass, self.lexer.pos());
        self.cl(
            "OpCopyRecord",
            &[code.clone(), to.clone(), Value::Int(i32::from(tp))],
        )
    }

    /** Mutate current code when it reads a value into writing it. This is needed for assignments.
     */
    fn towards_set(&mut self, to: &Value, val: &Value, f_type: &Type, op: &str) -> Value {
        if matches!(f_type, Type::Enum(_, true, _) | Type::Reference(_, _))
            && op == "="
            && !matches!(to, Value::Var(_))
        {
            return self.copy_ref(to, val, f_type);
        }
        if matches!(
            *f_type,
            Type::Vector(_, _)
                | Type::Sorted(_, _, _)
                | Type::Hash(_, _, _)
                | Type::Index(_, _, _)
                | Type::Spacial(_, _, _)
        ) {
            if let Value::Var(nr) = to {
                return v_set(*nr, val.clone());
            }
            return val.clone();
        }
        if let Type::RefVar(tp) = f_type
            && matches!(**tp, Type::Vector(_, _) | Type::Sorted(_, _, _))
        {
            if let Value::Var(nr) = to {
                if self.vars.uses(*nr) > 0 {
                    return val.clone();
                }
            } else {
                return val.clone();
            }
        }
        if *f_type == Type::Boolean
            && let Value::Call(_, a) = &to
            && let Value::Call(_, args) = &a[0]
        {
            let conv = Value::If(
                Box::new(val.clone()),
                Box::new(Value::Int(1)),
                Box::new(Value::Int(0)),
            );
            return self.cl(
                "OpSetByte",
                &[args[0].clone(), args[1].clone(), args[2].clone(), conv],
            );
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
                _ => panic!("Unknown {op} for {name} at {}", self.lexer.pos()),
            }
        } else if let Value::Var(nr) = to {
            // This variable was created here and thus not yet used.
            self.var_usages(*nr, false);
            v_set(*nr, code)
        } else {
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Not implemented operation {op} for type {}",
                    f_type.show(&self.data, &self.vars)
                );
            }
            Value::Null
        }
    }

    // <block> ::= '}' | <expression> {';' <expression} '}'
    fn parse_block(&mut self, context: &str, val: &mut Value, result: &Type) -> Type {
        if let Value::Var(v) = val
            && let Type::Reference(r, _) = self.vars.tp(*v).clone()
            && context == "block"
        {
            // We actually scan a record here instead of a block of statement
            self.parse_object(r, val);
            return Type::Reference(r, Vec::new());
        }
        if self.lexer.has_token("}") {
            *val = v_block(Vec::new(), Type::Void, "empty block");
            return Type::Void;
        }
        let mut t = Type::Void;
        let mut l = Vec::new();
        loop {
            let line = self.lexer.pos().line;
            if line > self.line {
                if matches!(l.last(), Some(Value::Line(_))) {
                    l.pop();
                }
                l.push(Value::Line(line));
                self.line = line;
            }
            if self.lexer.has_token(";") {
                continue;
            }
            if self.lexer.peek_token("}") {
                break;
            }
            let mut n = Value::Null;
            t = self.expression(&mut n);
            if let Value::Insert(ls) = n {
                Self::move_insert_elements(&mut l, ls);
                t = Type::Void;
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
        if matches!(l.last(), Some(Value::Line(_))) {
            l.pop();
        }
        t = self.block_result(context, result, &t, &mut l);
        *val = v_block(l, t.clone(), "block");
        t
    }

    fn move_insert_elements(l: &mut Vec<Value>, elms: Vec<Value>) {
        for el in elms {
            if let Value::Insert(ls) = el {
                Self::move_insert_elements(l, ls);
            } else {
                l.push(el);
            }
        }
    }

    fn block_result(&mut self, context: &str, result: &Type, t: &Type, l: &mut [Value]) -> Type {
        let mut tp = t.clone();
        if *result != Type::Void && !matches!(*result, Type::Unknown(_)) {
            let last = l.len() - 1;
            let ignore = *t == Type::Void && matches!(l[last], Value::Return(_));
            if !self.convert(&mut l[last], t, result) && !ignore {
                self.validate_convert(context, t, result);
            }
            tp = result.clone();
        }
        if let Type::Text(ls) = t {
            self.text_return(ls);
        } else if let Type::Reference(_, ls) | Type::Vector(_, ls) = t {
            self.ref_return(ls);
        }
        tp
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
        var_tp: &Type,
        code: &mut Value,
        parent_tp: &mut Type,
        precedence: usize,
    ) -> Type {
        let mut ls = Vec::new();
        if precedence >= OPERATORS.len() {
            let mut t = self.parse_single(var_tp, code, parent_tp);
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
        let orig_var = if let Value::Var(nr) = code {
            *nr
        } else {
            u16::MAX
        };
        let mut current_type = self.parse_operators(var_tp, code, parent_tp, precedence + 1);
        loop {
            let mut operator = "";
            for op in OPERATORS[precedence] {
                if self.lexer.has_token(op) {
                    operator = op;
                    break;
                }
            }
            if operator.is_empty() {
                if !ls.is_empty() {
                    if matches!(current_type, Type::Text(_) | Type::Character) {
                        return self.parse_append_text(code, &current_type, &ls, orig_var);
                    } else if matches!(current_type, Type::Vector(_, _)) {
                        return self.parse_append_vector(code, &current_type, &ls, orig_var);
                    }
                }
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
            } else if operator == "+"
                && matches!(
                    current_type,
                    Type::Text(_) | Type::Character | Type::Vector(_, _)
                )
            {
                let mut second_code = Value::Null;
                let tp = self.parse_operators(var_tp, &mut second_code, parent_tp, precedence + 1);
                ls.push((second_code, tp));
            } else {
                let mut second_code = Value::Null;
                let second_type =
                    self.parse_operators(var_tp, &mut second_code, parent_tp, precedence + 1);
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

    fn parse_append_vector(
        &mut self,
        code: &mut Value,
        tp: &Type,
        parts: &[(Value, Type)],
        orig_var: u16,
    ) -> Type {
        let mut ls = Vec::new();
        let rec_tp = if let Type::Vector(cont, _) = tp {
            i32::from(self.data.def(self.data.type_def_nr(cont)).known_type)
        } else {
            i32::MIN
        };
        let var_nr = if orig_var == u16::MAX {
            let vec = self.create_unique("vec", tp);
            let elm_tp = tp.content();
            for l in self.vector_db(&elm_tp, vec) {
                ls.push(l);
            }
            ls.push(self.cl(
                "OpAppendVector",
                &[Value::Var(vec), code.clone(), Value::Int(rec_tp)],
            ));
            vec
        } else if let Value::Insert(elms) = code {
            for e in elms {
                ls.push(e.clone());
            }
            orig_var
        } else {
            ls.push(v_set(orig_var, code.clone()));
            orig_var
        };
        for (val, _) in parts {
            ls.push(self.cl(
                "OpAppendVector",
                &[Value::Var(var_nr), val.clone(), Value::Int(rec_tp)],
            ));
        }
        if orig_var == u16::MAX {
            let res = self.vars.tp(var_nr).clone();
            ls.push(Value::Var(var_nr));
            *code = v_block(ls, res.clone(), "Append Vector");
            return res;
        }
        *code = Value::Insert(ls);
        Type::Rewritten(Box::new(tp.clone()))
    }

    fn parse_append_text(
        &mut self,
        code: &mut Value,
        tp: &Type,
        parts: &[(Value, Type)],
        orig_var: u16,
    ) -> Type {
        let mut ls = Vec::new();
        let var_nr = if orig_var == u16::MAX {
            let v = self.vars.work_text(&mut self.lexer);
            if matches!(self.vars.tp(v), Type::RefVar(_)) {
                ls.push(self.cl("OpClearRefText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendRefText", &[Value::Var(v), code.clone()]));
            } else if tp == &Type::Character {
                ls.push(self.cl("OpClearText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendCharacter", &[Value::Var(v), code.clone()]));
            } else {
                ls.push(self.cl("OpClearText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendText", &[Value::Var(v), code.clone()]));
            }
            v
        } else if matches!(self.vars.tp(orig_var), Type::RefVar(_)) {
            ls.push(self.cl("OpAppendRefText", &[Value::Var(orig_var), code.clone()]));
            orig_var
        } else {
            ls.push(self.cl("OpAppendText", &[Value::Var(orig_var), code.clone()]));
            orig_var
        };
        for (val, tp) in parts {
            if matches!(self.vars.tp(var_nr), Type::RefVar(_)) {
                if *tp == Type::Character {
                    ls.push(self.cl("OpAppendRefCharacter", &[Value::Var(var_nr), val.clone()]));
                } else {
                    ls.push(self.cl("OpAppendRefText", &[Value::Var(var_nr), val.clone()]));
                }
            } else if *tp == Type::Character {
                ls.push(self.cl("OpAppendCharacter", &[Value::Var(var_nr), val.clone()]));
            } else {
                ls.push(self.cl("OpAppendText", &[Value::Var(var_nr), val.clone()]));
            }
        }
        let tp = Type::Text(vec![var_nr]);
        if orig_var == u16::MAX {
            ls.push(Value::Var(var_nr));
            *code = v_block(ls, tp.clone(), "Add text");
            return tp;
        }
        *code = Value::Insert(ls);
        Type::Rewritten(Box::new(tp))
    }

    /// Rewrite boolean operators into an `IF` statement to prevent the calculation of the second
    /// expression when it is unneeded.
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
    #[allow(clippy::cloned_ref_to_slice_refs)]
    fn parse_single(&mut self, var_tp: &Type, val: &mut Value, parent_tp: &mut Type) -> Type {
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
            self.parse_vector(var_tp, val, parent_tp)
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
            self.parse_string(val, &s)
        } else if let Some(nr) = self.lexer.has_char() {
            *val = Value::Int(nr as i32);
            Type::Character
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

    /**
    Fill a structure (vector) with values. This can be done in different situations:
    - On a new variable, this creates a variable pointing to a structure with the vector.
    - As a stand-alone expression, this creates a new structure of type vector.
    - On an existing variable, this fills (or replaces) the vector with more elements.
    - On a field inside a structure, this fills any data structure with more elements.
    */
    // <vector> ::= '[' <expr> [ ';' <size-expr>]{ ',' <expr> [ ';' <size-expr> } ']'
    fn parse_vector(&mut self, var_tp: &Type, val: &mut Value, parent_tp: &Type) -> Type {
        let assign_tp = var_tp.content();
        let is_field = self.is_field(val);
        let is_var = matches!(val, Value::Var(_));
        if self.lexer.has_token("]") {
            return if is_var {
                *val = Value::Insert(vec![]);
                Type::Rewritten(Box::new(var_tp.clone()))
            } else {
                *val = Value::Insert(vec![val.clone()]);
                var_tp.clone()
            };
        }
        let block = !is_field && !matches!(val, Value::Var(_));
        let vec = if is_field {
            u16::MAX
        } else if let Value::Var(nr) = val {
            *nr
        } else {
            self.create_unique(
                "vec",
                &Type::Vector(Box::new(assign_tp.clone()), parent_tp.depend()),
            )
        };
        let mut in_t = assign_tp.clone();
        let mut res = Vec::new();
        let elm = self.unique_elm_var(parent_tp, &assign_tp, vec);
        loop {
            if let Some(value) = self.parse_item(elm, &mut in_t, &mut res) {
                return value;
            }
            if self.lexer.has_token(";")
                && let Some(value) = self.parse_multiply(&mut res)
            {
                return value;
            }
            if !self.lexer.has_token(",") {
                break;
            }
            if self.lexer.peek_token("]") {
                break;
            }
        }
        // convert parts to the common type
        if in_t == Type::Null {
            return in_t;
        }
        let struct_tp = Type::Vector(Box::new(in_t.clone()), parent_tp.depend());
        if !is_field {
            self.vars
                .change_var_type(vec, &struct_tp, &self.data, &mut self.lexer);
            self.data.vector_def(&mut self.lexer, &in_t);
        }
        let mut tp = Type::Vector(Box::new(in_t.clone()), parent_tp.depend());
        let mut ls = Vec::new();
        if block {
            for l in self.vector_db(&in_t, vec) {
                ls.push(l);
            }
        }
        for l in self.new_record(val, parent_tp, elm, vec, &res, &in_t) {
            ls.push(l);
        }
        if is_var
            && in_t != Type::Void
            && self.vars.tp(vec).depend().is_empty()
            && !matches!(self.vars.tp(vec), Type::RefVar(_))
        {
            let db = self.insert_new(vec, elm, &in_t, &mut ls);
            self.vars.depend(vec, db);
            tp = tp.depending(db);
        } else if !is_field && !is_var && *val != Value::Null {
            ls.insert(0, v_set(vec, val.clone()));
        }
        if !is_var && !is_field {
            ls.push(Value::Var(vec));
        }
        self.lexer.token("]");
        if block {
            *val = v_block(ls, tp.clone(), "Vector");
        } else {
            *val = Value::Insert(ls);
        }
        tp
    }

    fn unique_elm_var(&mut self, parent_tp: &Type, assign_tp: &Type, vec: u16) -> u16 {
        let c_tp = parent_tp.content();
        let was = Type::Reference(
            if c_tp.is_unknown() {
                0
            } else {
                self.data.type_def_nr(&c_tp)
            },
            parent_tp.depend(),
        );
        let elm = self.create_unique(
            "elm",
            if let Type::Reference(_, _) = assign_tp {
                assign_tp
            } else {
                &was
            },
        );
        self.vars.depend(elm, vec);
        for on in parent_tp.depend() {
            self.vars.depend(elm, on);
        }
        elm
    }

    fn parse_multiply(&mut self, res: &mut Vec<Value>) -> Option<Type> {
        let mut code = Value::Null;
        let tp = self.parse_operators(&Type::Unknown(0), &mut code, &mut Type::Null, 0);
        if !matches!(tp, Type::Integer(_, _)) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Expect a number as the object multiplier"
            );
            return Some(Type::Unknown(0));
        }
        res.push(Value::Return(Box::new(code)));
        None
    }

    // <item> ::== ['for' | <expr> ]
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
        if let Type::Rewritten(tp) = in_t {
            *in_t = *tp.clone();
        }
        if let Type::Rewritten(tp) = t {
            t = *tp.clone();
        }
        if in_t.is_unknown() {
            *in_t = t.clone();
        }
        if t.is_unknown() {
            t = in_t.clone();
        }
        // double conversion check: can t become in_t or vice versa
        if !self.convert(&mut p, &t, in_t) {
            if self.convert(&mut p, in_t, &t) {
                *in_t = t.clone();
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
        if let Type::Enum(td_nr, true, _) = t
            && let Value::Enum(enum_nr, _) = &p
            && self.lexer.has_token("{")
        {
            let mut ls = Vec::new();
            self.parse_enum_field(&mut ls, Value::Var(elm), td_nr, 0, *enum_nr);
            ls.push(p.clone());
            p = Value::Insert(ls);
        }
        res.push(p.clone());
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
        for p in res {
            let known = Value::Int(i32::from(
                if ed_nr == u32::from(u16::MAX) || self.data.def(ed_nr).known_type == u16::MAX {
                    0
                } else {
                    self.database.vector(self.data.def(ed_nr).known_type)
                },
            ));
            if let Value::Return(multiply) = p {
                let to = if let Value::Call(_, ps) = val {
                    ps[0].clone()
                } else {
                    Value::Var(vec)
                };
                ls.push(self.cl("OpAppendCopy", &[to, *multiply.clone(), known]));
                continue;
            }
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
            ls.push(v_set(elm, app_v));
            if let Type::Reference(_, _) = in_t {
                ls.push(p.clone());
            } else if let Value::Insert(steps) = p {
                for (l_nr, l) in steps.iter().enumerate() {
                    if l_nr + 1 == steps.len() {
                        ls.push(self.cl("OpSetEnum", &[Value::Var(elm), Value::Int(0), l.clone()]));
                    } else {
                        ls.push(l.clone());
                    }
                }
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

    fn vector_db(&mut self, assign_tp: &Type, vec: u16) -> Vec<Value> {
        if self.first_pass || self.vars.is_argument(vec) {
            Vec::new()
        } else {
            let mut ls = Vec::new();
            let vec_def = self.data.vector_def(&mut self.lexer, assign_tp);
            let db = self
                .vars
                .work_refs(&Type::Reference(vec_def, Vec::new()), &mut self.lexer);
            self.vars.depend(vec, db);
            let tp = self.data.def(vec_def).known_type;
            debug_assert_ne!(
                tp,
                u16::MAX,
                "Undefined type {} at {}",
                self.data.def(vec_def).name,
                self.lexer.pos()
            );
            ls.push(self.cl("OpDatabase", &[Value::Var(db), Value::Int(i32::from(tp))]));
            // Reference to the vector field.
            ls.push(v_set(vec, self.get_field(vec_def, 0, Value::Var(db))));
            // Write 0 into this reference.
            ls.push(self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
            ls
        }
    }

    fn insert_new(&mut self, vec: u16, elm: u16, in_t: &Type, ls: &mut Vec<Value>) -> u16 {
        // determine the element size by the resulting type
        let vec_def = self.data.vector_def(&mut self.lexer, in_t);
        let db = self
            .vars
            .work_refs(&Type::Reference(vec_def, Vec::new()), &mut self.lexer);
        self.vars.depend(elm, db);
        self.vars.depend(vec, db);
        let known = Value::Int(i32::from(self.data.def(vec_def).known_type));
        ls.insert(0, self.cl("OpDatabase", &[Value::Var(db), known]));
        // Reference to the vector field.
        ls.insert(1, v_set(vec, self.get_field(vec_def, 0, Value::Var(db))));
        // Write 0 into this reference.
        ls.insert(2, self.set_field(vec_def, 0, Value::Var(db), Value::Int(0)));
        db
    }

    fn type_info(&self, in_t: &Type) -> Value {
        Value::Int(i32::from(self.get_type(in_t)))
    }

    fn get_type(&self, in_t: &Type) -> u16 {
        if self.first_pass {
            return u16::MAX;
        }
        match in_t {
            Type::Reference(r, _) | Type::Enum(r, _, _) => self.data.def(*r).known_type,
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
                let typedef: &Type = if self.data.def_nr(&vec_name) == u32::MAX {
                    &Type::Unknown(0)
                } else {
                    tp
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
            assert_ne!(
                enr,
                u32::MAX,
                "Unknown type {}",
                t.show(&self.data, &self.vars)
            );
            let e_size = i32::from(self.database.size(self.data.def(enr).known_type));
            if let Type::RefVar(tp) = t {
                t = *tp;
            }
            let dnr = self.data.type_def_nr(&t);
            if let Type::Vector(et, _) = t.clone() {
                self.vector_operations(code, &field, e_size, &et);
            }
            let fnr = self.data.attr(dnr, &field);
            if self.first_pass && fnr == usize::MAX && self.lexer.has_token("(") {
                loop {
                    if self.lexer.has_token(")") {
                        break;
                    }
                    let mut p = Value::Null;
                    self.expression(&mut p);
                    if !self.lexer.has_token(",") {
                        break;
                    }
                }
                return Type::Unknown(0);
            }
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
                }
                *code = new;
                let dep = t.depend();
                t = self.data.attr_type(dnr, fnr);
                for on in dep {
                    t = t.depending(on);
                }
            } else {
                let dep = t.depend();
                let last_t = t.clone();
                t = self.data.attr_type(dnr, fnr);
                for on in dep {
                    t = t.depending(on);
                }
                if let Type::Enum(_, _, _) = last_t.clone() {
                    // TODO do something with enum fields
                } else {
                    if let Value::Var(nr) = code {
                        t = t.depending(*nr);
                    }
                    *code = self.get_field(dnr, fnr, code.clone());
                }
            }
            self.data.attr_used(dnr, fnr);
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect a field name");
        }
        t
    }

    fn vector_operations(&mut self, code: &mut Value, field: &str, e_size: i32, et: &Type) {
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
            if !self.convert(&mut vl, &tps[1], et) {
                diagnostic!(self.lexer, Level::Error, "Invalid value in insert");
            }
            *code = self.cl("OpInsertVector", &[code.clone(), Value::Int(e_size), cd]);
            // TODO copy vl into newly created vector element
            // Inner should be different from Reference
        }
    }

    fn parse_index(&mut self, code: &mut Value, t: &Type) -> Type {
        let mut elm_type = self.index_type(t);
        for on in t.depend() {
            elm_type = elm_type.depending(on);
        }
        /*let nr = if self.types.exists("$") {
            self.types.var_nr("$")
        } else {
            self.create_var("$".to_string(), elm_type.clone())
        };
        self.data.definitions[self.context as usize].variables[nr as usize].uses = 0;
         */
        let mut p = Value::Null;
        if let Type::Vector(etp, _) = &t {
            if let Some(value) = self.parse_vector_index(code, &elm_type, etp) {
                return value;
            }
        } else if matches!(*t, Type::Text(_)) {
            let index_t = self.expression(&mut p);
            if self.parse_text_index(code, &mut p, &index_t) == Type::Character {
                elm_type = Type::Character;
            }
        } else if let Type::RefVar(tp) = &t {
            if matches!(**tp, Type::Text(_)) {
                let index_t = self.expression(&mut p);
                *code = self.cl("OpVarRef", std::slice::from_ref(code));
                *code = self.cl("OpGetRefText", std::slice::from_ref(code));
                if self.parse_text_index(code, &mut p, &index_t) == Type::Character {
                    elm_type = Type::Character;
                }
            } else {
                panic!("Unknown type to index");
            }
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
        } else if !self.first_pass {
            panic!("Unknown type to index");
        }
        elm_type
    }

    fn index_type(&mut self, t: &Type) -> Type {
        if let Type::Vector(v_t, _) = t {
            *v_t.clone()
        } else if let Type::Sorted(d_nr, _, _)
        | Type::Hash(d_nr, _, _)
        | Type::Index(d_nr, _, _)
        | Type::Spacial(d_nr, _, _) = t
        {
            self.data.def(*d_nr).returned.clone()
        } else if matches!(t, Type::Text(_)) {
            t.clone()
        } else if let Type::RefVar(tp) = t {
            *tp.clone()
        } else {
            diagnostic!(self.lexer, Level::Error, "Indexing a non vector");
            Type::Unknown(0)
        }
    }

    fn parse_vector_index(
        &mut self,
        code: &mut Value,
        elm_type: &Type,
        etp: &Type,
    ) -> Option<Type> {
        let mut p = Value::Null;
        let index_t = self.parse_in_range(&mut p, code, "$");
        let elm_td = self.data.type_elm(etp);
        let known = self.data.def(elm_td).known_type;
        let elm_size = i32::from(self.database.size(known));
        if let Value::Iter(var, init, next) = p {
            if matches!(*next, Value::Block(_)) {
                let mut op = self.cl(
                    "OpGetVector",
                    &[code.clone(), Value::Int(elm_size), *next.clone()],
                );
                if self.database.is_base(known) || self.database.is_linked(known) {
                    op = self.get_val(etp, true, 0, op);
                }
                *code = Value::Iter(
                    var,
                    init,
                    Box::new(v_block(vec![op], etp.clone(), "Vector Index")),
                );
                return Some(Type::Iterator(
                    Box::new(elm_type.clone()),
                    Box::new(Type::Null),
                ));
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
        None
    }

    fn parse_text_index(&mut self, code: &mut Value, p: &mut Value, index_t: &Type) -> Type {
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
            Type::Text(Vec::new())
        } else {
            *code = self.cl("OpTextCharacter", &[code.clone(), p.clone()]);
            Type::Character
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
            let inclusive = self.lexer.has_token("=");
            let iter = self.create_unique("iter", &Type::Long);
            let mut ls = Vec::new();
            if !self.first_pass {
                self.fill_iter(&mut ls, code, typedef, true, inclusive);
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
            let start = v_set(iter, self.cl("OpIterate", &ls));
            let mut ls = vec![Value::Var(iter)];
            self.fill_iter(&mut ls, code, typedef, false, inclusive);
            *code = Value::Iter(
                u16::MAX,
                Box::new(start),
                Box::new(v_block(
                    vec![self.cl("OpStep", &ls)],
                    typedef.clone(),
                    "Iterate keys",
                )),
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

    fn fill_iter(
        &mut self,
        ls: &mut Vec<Value>,
        code: &mut Value,
        typedef: &Type,
        add_keys: bool,
        inclusive: bool,
    ) {
        let known = self.get_type(typedef);
        if known == u16::MAX {
            return;
        }
        let mut on;
        let arg;
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
        if inclusive {
            on += 128;
        }
        ls.push(code.clone());
        ls.push(Value::Int(i32::from(on)));
        ls.push(Value::Int(i32::from(arg)));
        self.vars.set_loop(on, arg, code);
        if add_keys {
            ls.push(Value::Keys(
                self.database.types[known as usize].keys.clone(),
            ));
        }
    }

    // <var> ::= <object> | [ <call> | <var> | <enum> ] <children> }
    fn parse_var(&mut self, code: &mut Value, name: &str) -> Type {
        let mut source = u16::MAX;
        let nm = if self.lexer.has_token("::") {
            source = self.data.get_source(name);
            if let Some(id) = self.lexer.has_identifier() {
                id
            } else {
                diagnostic!(self.lexer, Level::Error, "Expecting identifier after ::");
                name.to_string()
            }
        } else {
            name.to_string()
        };
        let mut t = self.parse_constant_value(code, source, &nm);
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
            let index_var = self.vars.var(name);
            if self.lexer.has_token("#") {
                self.iter_op(code, name, &mut t, index_var);
            } else if let Value::Var(into) = code {
                let v_nr = self.vars.var(name);
                if matches!(self.vars.tp(v_nr), Type::Text(_)) {
                    t = self.vars.tp(v_nr).clone();
                } else {
                    t = self.vars.tp(v_nr).depending(v_nr);
                }
                self.var_usages(v_nr, true);
                if let Type::Reference(d_nr, _) = self.vars.tp(*into)
                    && let Type::Reference(vd_nr, _) = self.vars.tp(v_nr)
                    && d_nr == vd_nr
                {
                    let tp = self.data.def(*d_nr).known_type;
                    *code = self.cl(
                        "OpCopyRecord",
                        &[
                            Value::Var(v_nr),
                            Value::Var(*into),
                            Value::Int(i32::from(tp)),
                        ],
                    );
                    return t;
                }
                *code = Value::Var(v_nr);
            } else {
                let v_nr = self.vars.var(name);
                t = self.vars.tp(v_nr).depending(v_nr);
                self.var_usages(v_nr, true);
                *code = Value::Var(v_nr);
            }
        } else if self.data.def_nr(name) != u32::MAX {
            let dnr = self.data.def_nr(name);
            if self.data.def_type(dnr) == DefType::Enum {
                t = self.data.def(dnr).returned.clone();
            } else if self.data.def_type(dnr) == DefType::EnumValue {
                t = Type::Enum(self.data.def(dnr).parent, true, Vec::new());
            } else {
                t = Type::Null;
            }
        } else if matches!(
            self.data.def_type(self.context),
            DefType::Struct | DefType::Main
        ) && self.data.attr(self.context, name) != usize::MAX
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

    fn iter_op(&mut self, code: &mut Value, name: &str, t: &mut Type, index_var: u16) {
        if self.lexer.has_keyword("index") {
            let i_name = &format!("{name}#index");
            if self.vars.name_exists(i_name) {
                let v = self.vars.var(i_name);
                *t = self.vars.tp(v).clone();
                *code = Value::Var(v);
            } else {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Incorrect #index variable on {}",
                    name
                );
                *t = Type::Unknown(0);
            }
        } else if self.lexer.has_token("break") {
            if !self.in_loop {
                diagnostic!(self.lexer, Level::Error, "Cannot continue outside a loop");
            }
            *code = Value::Break(self.vars.loop_nr(name));
            *t = Type::Void;
        } else if self.lexer.has_token("continue") {
            if !self.in_loop {
                diagnostic!(self.lexer, Level::Error, "Cannot continue outside a loop");
            }
            *code = Value::Continue(self.vars.loop_nr(name));
            *t = Type::Void;
        } else if self.lexer.has_keyword("count") {
            let count_var = format!("{name}#count");
            let count = if self.vars.name_exists(&count_var) {
                self.vars.var(&count_var)
            } else {
                self.create_var(&count_var, &I32)
            };
            self.vars.loop_count(count);
            *code = Value::Var(count);
            *t = I32.clone();
        } else if self.lexer.has_keyword("first") {
            let count_var = format!("{name}#count");
            let count = if self.vars.name_exists(&count_var) {
                self.vars.var(&count_var)
            } else {
                self.create_var(&count_var, &I32)
            };
            self.vars.loop_count(count);
            *code = self.cl("OpEqInt", &[Value::Var(count), Value::Int(0)]);
            *t = Type::Boolean;
        } else if self.lexer.has_keyword("remove") {
            *code = self.cl(
                "OpRemove",
                &[
                    Value::Var(self.vars.var(&format!("{name}#index"))),
                    self.vars.loop_value(index_var).clone(),
                    Value::Int(i32::from(self.vars.loop_on(index_var))),
                    Value::Int(i32::from(self.vars.loop_db_tp(index_var))),
                ],
            );
            *t = Type::Void;
        } else {
            diagnostic!(self.lexer, Level::Error, "Incorrect # variable on {}", name);
            *t = Type::Unknown(0);
        }
    }

    fn parse_constant_value(&mut self, code: &mut Value, source: u16, name: &str) -> Type {
        let mut t;
        let d_nr = if source == u16::MAX {
            self.data.def_nr(name)
        } else {
            self.data.source_nr(source, name)
        };
        if d_nr != u32::MAX {
            self.data.def_used(d_nr);
            t = self.data.def(d_nr).returned.clone();
            if self.data.def_type(d_nr) == DefType::Function {
                t = Type::Routine(d_nr);
            } else if matches!(
                self.data.def_type(d_nr),
                DefType::Struct | DefType::Main
            ) && self.lexer.has_token("{") {
                return self.parse_object(d_nr, code);
            } else if self.data.def_type(d_nr) == DefType::Constant {
                *code = self.data.def(d_nr).code.clone();
                return self.data.def(d_nr).returned.clone();
            }
            if let Type::Enum(en, _, _) = t {
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

    fn parse_string(&mut self, code: &mut Value, string: &str) -> Type {
        let mut append_value = u16::MAX;
        *code = Value::str(string);
        let mut var = u16::MAX;
        let mut list = vec![];
        if self.lexer.mode() == Mode::Formatting {
            // Define a new variable to append to
            var = self.vars.work_text(&mut self.lexer);
            list.push(v_set(var, code.clone()));
        }
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
                return Type::Void;
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
                self.string_states(&mut state);
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
                    let call = if matches!(self.vars.tp(var), Type::RefVar(_)) {
                        "OpAppendRefText"
                    } else {
                        "OpAppendText"
                    };
                    list.push(self.cl(call, &[Value::Var(var), Value::str(&text)]));
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Formatter error");
                return Type::Void;
            }
        }
        if var < u16::MAX {
            list.push(Value::Var(var));
            *code = v_block(list, Type::Text(vec![var]), "Formatted string");
            Type::Text(vec![var])
        } else {
            Type::Text(Vec::new())
        }
    }

    fn string_states(&mut self, state: &mut OutputState) {
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
            let iter_var = self.create_var(&format!("{id}#index"), &I32);
            self.lexer.token("in");
            let loop_nr = self.vars.start_loop();
            let mut expr = Value::Null;
            let in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            let var_tp = self.for_type(&in_type);
            *append_value = self.create_unique("val", &Type::Unknown(0));
            let for_var = self.create_var(&id, &var_tp);
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut create_iter = expr;
            let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var);
            if !self.first_pass && iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return Type::Null;
            }
            let for_next = v_set(for_var, iter_next);
            self.vars.loop_var(for_var);
            let in_loop = self.in_loop;
            self.in_loop = true;
            let mut block = Value::Null;
            let format_type = self.parse_block("for", &mut block, &Type::Unknown(0));
            self.change_var_type(*append_value, &format_type);
            self.in_loop = in_loop;
            let mut lp = vec![for_next];
            if !matches!(in_type, Type::Iterator(_, _)) {
                lp.push(v_if(
                    self.single_op("!", Value::Var(for_var), var_tp.clone()),
                    v_block(vec![Value::Break(0)], Type::Void, "break"),
                    Value::Null,
                ));
            }
            if if_step != Value::Null {
                lp.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            let result_tp = if let Value::Block(bl) = &block {
                bl.result.clone()
            } else {
                var_tp.clone()
            };
            lp.push(block);
            let tp = Type::Iterator(Box::new(format_type), Box::new(Type::Null));
            *val = Value::Iter(
                for_var,
                Box::new(create_iter),
                Box::new(v_block(lp, result_tp, "Iter For")),
            );
            self.vars.finish_loop(loop_nr);
            return tp;
        }
        diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        Type::Null
    }

    // range ::= rev(<expr> '..' ['='] <expr>) | <expr> [ '..' ['='] <expr> ]
    fn parse_in_range(&mut self, expr: &mut Value, data: &Value, name: &str) -> Type {
        let mut reverse = false;
        if let LexItem::Identifier(rev) = self.lexer.peek().has
            && &rev == "rev"
        {
            self.lexer.has_identifier();
            self.lexer.token("(");
            reverse = true;
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
                self.cl("OpLengthVector", std::slice::from_ref(data))
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
            u16::MAX,
            Box::new(v_set(ivar, self.null(&in_type))),
            Box::new(v_block(ls, in_type.clone(), "Iter range")),
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
        let start = (if matches!(self.vars.tp(append), Type::RefVar(_)) {
            "OpFormatRef"
        } else {
            "OpFormat"
        })
        .to_owned();
        match tp {
            Type::Integer(_, _) => {
                let value = self.cl("OpConvLongFromInt", std::slice::from_ref(format));
                list.push(self.cl(
                    &(start + "Long"),
                    &[
                        var,
                        value,
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
                    &(start + "Long"),
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
                let value = self.cl("OpCastTextFromBool", std::slice::from_ref(format));
                list.push(self.cl(
                    &(start + "Text"),
                    &[
                        var,
                        value,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                    ],
                ));
            }
            Type::Text(_) => {
                let fmt = format.clone();
                list.push(self.cl(
                    &(start + "Text"),
                    &[
                        var,
                        fmt,
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(i32::from(state.token.as_bytes()[0])),
                    ],
                ));
            }
            Type::Character => {
                list.push(self.cl("OpAppendCharacter", &[var, format.clone()]));
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
                list.push(self.cl(&(start + "Float"), &[var, fmt, a_width, p_rec]));
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
                list.push(self.cl(&(start + "Single"), &[var, fmt, a_width, p_rec]));
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
                    &(start + "Database"),
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
                    &(start + "Database"),
                    &[
                        var,
                        fmt,
                        Value::Int(i32::from(db_tp)),
                        Value::Boolean(state.note),
                    ],
                ));
            }
            Type::Enum(d_nr, is_ref, _) => {
                let fmt = format.clone();
                let e_tp = self.data.def(d_nr).known_type;
                if e_tp == u16::MAX || !is_ref {
                    let e_val = self.cl("OpCastTextFromEnum", &[fmt, Value::Int(i32::from(e_tp))]);
                    list.push(self.cl(
                        &(start + "Text"),
                        &[
                            var,
                            e_val,
                            state.width,
                            Value::Int(state.dir),
                            Value::Int(i32::from(state.token.as_bytes()[0])),
                        ],
                    ));
                } else {
                    list.push(self.cl(
                        &(start + "Database"),
                        &[
                            var,
                            fmt,
                            Value::Int(i32::from(e_tp)),
                            Value::Boolean(state.note),
                        ],
                    ));
                }
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
        if let Value::Iter(var, init, next) = value
            && matches!(**next, Value::Block(_))
        {
            let count = if *var == u16::MAX {
                self.create_unique("count", &I32)
            } else {
                let count_name = format!("{}#count", self.vars.name(*var));
                let c = self.vars.var(&count_name);
                if c == u16::MAX {
                    self.create_var(&count_name, &I32)
                } else {
                    c
                }
            };
            list.push(self.cl("OpAppendText", &[Value::Var(append), Value::str("[")]));
            list.push(*init.clone());
            list.push(v_set(count, Value::Int(0)));
            let mut append_var = append_value;
            if append_value == u16::MAX {
                append_var = self.create_unique("val", var_type);
            }
            let mut steps = Vec::new();
            steps.push(v_set(append_var, *next.clone()));
            steps.push(v_if(
                self.cl("OpGtInt", &[Value::Var(count), Value::Int(0)]),
                self.cl("OpAppendText", &[Value::Var(append), Value::str(",")]),
                Value::Null,
            ));
            steps.push(v_set(
                count,
                self.cl("OpAddInt", &[Value::Var(count), Value::Int(1)]),
            ));
            self.append_data(
                var_type.clone(),
                &mut steps,
                append,
                append_var,
                &Value::Var(append_var),
                state,
            );
            list.push(v_loop(steps, "Append Iter"));
            list.push(self.cl("OpAppendText", &[Value::Var(append), Value::str("]")]));
        }
    }

    // <object> ::= [ <identifier> ':' <expression> { ',' <identifier> ':' <expression> } ] '}'
    fn parse_object(&mut self, td_nr: u32, code: &mut Value) -> Type {
        let link = self.lexer.link();
        let mut list = Vec::new();
        let mut new_object = false;
        let v = if let Value::Var(v_nr) = code {
            if self.vars.is_independent(*v_nr)
                && self.data.definitions[td_nr as usize].def_type != DefType::EnumValue
            {
                self.data.definitions[td_nr as usize].def_type = DefType::Main;
                if !self.vars.is_argument(*v_nr) {
                    list.push(v_set(*v_nr, Value::Null));
                }
                self.data.set_referenced(td_nr, self.context, Value::Null);
                let tp = i32::from(self.data.def(td_nr).known_type);
                list.push(self.cl("OpDatabase", &[Value::Var(*v_nr), Value::Int(tp)]));
            }
            *v_nr
        } else if self.first_pass {
            new_object = true;
            if self.data.definitions[td_nr as usize].def_type != DefType::EnumValue {
                self.data.definitions[td_nr as usize].def_type = DefType::Main;
            }
            u16::MAX
        } else {
            new_object = true;
            self.data.set_referenced(td_nr, self.context, Value::Null);
            let ret = &self.data.def(td_nr).returned;
            let w = self.vars.work_refs(ret, &mut self.lexer);
            let tp = i32::from(self.data.def(td_nr).known_type);
            list.push(self.cl("OpDatabase", &[Value::Var(w), Value::Int(tp)]));
            w
        };
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
                    if v != u16::MAX {
                        parent_tp = parent_tp.depending(v);
                    }
                    let exp_tp = self.parse_operators(&td, &mut value, &mut parent_tp, 0);
                    if let Type::Vector(_, _)
                    | Type::Sorted(_, _, _)
                    | Type::Hash(_, _, _)
                    | Type::Spacial(_, _, _)
                    | Type::Index(_, _, _) = td
                    {
                        list.push(value);
                    } else {
                        self.convert(&mut value, &exp_tp, &td);
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
                default = to_default(&self.data.attr_type(td_nr, aid), &self.data);
            }
            list.push(self.set_field(td_nr, aid, Value::Var(v), default));
        }
        if new_object {
            list.push(Value::Var(v));
            *code = v_block(list, Type::Reference(td_nr, vec![v]), "Object");
            Type::Reference(td_nr, Vec::new())
        } else {
            *code = Value::Insert(list);
            Type::Rewritten(Box::new(Type::Reference(td_nr, Vec::new())))
        }
    }

    // <if> ::= <expression> '{' <block> [ 'else' ( 'if' <if> | '{' <block> ) ]
    fn parse_if(&mut self, code: &mut Value) -> Type {
        let mut test = Value::Null;
        let tp = self.expression(&mut test);
        self.convert(&mut test, &tp, &Type::Boolean);
        self.lexer.token("{");
        let mut true_code = Value::Null;
        let mut true_type = self.parse_block("if", &mut true_code, &Type::Unknown(0));
        let mut false_type = Type::Void;
        let mut false_code = Value::Null;
        if self.lexer.has_token("else") {
            if self.lexer.has_token("if") {
                self.parse_if(&mut false_code);
            } else {
                self.lexer.token("{");
                if true_type == Type::Null {
                    true_type = Type::Unknown(0);
                }
                false_type = self.parse_block("if", &mut false_code, &true_type);
                if true_type == Type::Unknown(0) {
                    true_code = v_block(vec![self.null(&false_type)], false_type.clone(), "if");
                    true_type = false_type.clone();
                }
            }
        }
        *code = v_if(test, true_code, false_code);
        merge_dependencies(&true_type, &false_type)
    }

    // <for> ::= <identifier> 'in' <expression> '{' <block>
    fn parse_for(&mut self, code: &mut Value) {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("in");
            let loop_nr = self.vars.start_loop();
            let mut expr = Value::Null;
            let mut in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            let mut fill = Value::Null;
            if matches!(in_type, Type::Vector(_, _)) {
                let vec_var = self.create_unique("vector", &in_type);
                in_type = in_type.depending(vec_var);
                fill = v_set(vec_var, expr);
                expr = Value::Var(vec_var);
            }
            let var_tp = self.for_type(&in_type);
            let iter_var = self.create_var(&format!("{id}#index"), &I32);
            let for_var = self.create_var(&id, &var_tp);
            if matches!(var_tp, Type::Integer(_, _)) {
                self.vars.in_use(for_var, true);
            }
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut create_iter = expr;
            let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var);
            if !self.first_pass && iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return;
            }
            let for_next = v_set(for_var, iter_next);
            self.vars.loop_var(for_var);
            let in_loop = self.in_loop;
            self.in_loop = true;
            let mut block = Value::Null;
            self.parse_block("for", &mut block, &Type::Void);
            let count = self.vars.loop_counter();
            self.in_loop = in_loop;
            self.vars.finish_loop(loop_nr);
            let mut for_steps = Vec::new();
            if fill != Value::Null {
                for_steps.push(fill);
            }
            for_steps.push(create_iter);
            let mut lp = vec![for_next];
            if !matches!(in_type, Type::Iterator(_, _)) {
                let mut test_for = Value::Var(for_var);
                self.convert(&mut test_for, &var_tp, &Type::Boolean);
                test_for = self.cl("OpNot", &[test_for]);
                lp.push(v_if(
                    test_for,
                    v_block(vec![Value::Break(0)], Type::Void, "break"),
                    Value::Null,
                ));
            }
            if if_step != Value::Null {
                lp.push(v_if(if_step, Value::Null, Value::Continue(0)));
            }
            lp.push(block);
            if count != u16::MAX {
                for_steps.insert(0, v_set(count, Value::Int(0)));
                lp.push(v_set(
                    count,
                    self.cl("OpAddInt", &[Value::Var(count), Value::Int(1)]),
                ));
            }
            for_steps.push(v_loop(lp, "For loop"));
            *code = v_block(for_steps, Type::Void, "For block");
        } else {
            diagnostic!(self.lexer, Level::Error, "Expect variable after for");
        }
    }

    fn for_type(&mut self, in_type: &Type) -> Type {
        if let Type::Vector(t_nr, dep) = &in_type {
            let mut t = *t_nr.clone();
            for d in dep {
                t = t.depending(*d);
            }
            t
        } else if let Type::Sorted(dnr, _, dep) = &in_type {
            Type::Reference(*dnr, dep.clone())
        } else if let Type::Iterator(i_tp, _) = &in_type {
            if **i_tp == Type::Null {
                I32.clone()
            } else {
                *i_tp.clone()
            }
        } else if let Type::Text(_) = in_type {
            Type::Character
        } else if let Type::Reference(_, _) | Type::Integer(_, _) | Type::Long = in_type {
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
        }
    }

    pub fn null(&mut self, tp: &Type) -> Value {
        match tp {
            Type::Integer(_, _) | Type::Character => self.cl("OpConvIntFromNull", &[]),
            Type::Boolean => self.cl("OpConvBoolFromNull", &[]),
            Type::Enum(tp, _, _) => self.cl(
                "OpConvEnumFromNull",
                &[Value::Int(i32::from(self.data.def(*tp).known_type))],
            ),
            Type::Long => self.cl("OpConvLongFromNull", &[]),
            Type::Float => self.cl("OpConvFloatFromNull", &[]),
            Type::Single => self.cl("OpConvSingleFromNull", &[]),
            Type::Text(_) => self.cl("OpConvTextFromNull", &[]),
            Type::RefVar(tp) if matches!(**tp, Type::Text(_)) => self.cl("OpConvTextFromNull", &[]),
            Type::Reference(_, _) => self.cl("OpConvRefFromNull", &[]),
            _ => Value::Null,
        }
    }

    // For now, assume that returned texts are always related to internal variables
    fn text_return(&mut self, ls: &[u16]) {
        if let Type::Text(cur) = &self.data.definitions[self.context as usize].returned {
            let mut dep = cur.clone();
            for v in ls {
                let n = self.vars.name(*v);
                let tp = self.vars.tp(*v);
                // skip related variables that are already attributes
                if let Some(a) = self.data.def(self.context).attr_names.get(n) {
                    if !dep.contains(&(*a as u16)) {
                        dep.push(*a as u16);
                    }
                    continue;
                }
                if matches!(tp, Type::Text(_)) {
                    // create a new attribute with this name
                    let a = self.data.add_attribute(
                        &mut self.lexer,
                        self.context,
                        n,
                        Type::RefVar(Box::new(Type::Text(Vec::new()))),
                    );
                    self.vars.become_argument(*v);
                    dep.push(a as u16);
                    self.vars
                        .set_type(*v, Type::RefVar(Box::new(Type::Text(Vec::new()))));
                } else {
                    let a = self
                        .data
                        .add_attribute(&mut self.lexer, self.context, n, tp.clone());
                    self.vars.become_argument(*v);
                    dep.push(a as u16);
                }
            }
            self.data.definitions[self.context as usize].returned = Type::Text(dep);
        }
    }

    fn ref_return(&mut self, ls: &[u16]) {
        let ret = self.data.definitions[self.context as usize]
            .returned
            .clone();
        if let Type::Vector(_, cur) | Type::Reference(_, cur) = &ret {
            let mut dep = cur.clone();
            for v in ls {
                let n = self.vars.name(*v);
                // skip related variables that are already attributes
                if let Some(a) = self.data.def(self.context).attr_names.get(n) {
                    if !dep.contains(&(*a as u16)) {
                        dep.push(*a as u16);
                    }
                    continue;
                }
                // create a new attribute with this name
                let a = self
                    .data
                    .add_attribute(&mut self.lexer, self.context, n, ret.clone());
                self.vars.become_argument(*v);
                dep.push(a as u16);
            }
            self.data.definitions[self.context as usize].returned = if let Type::Vector(it, _) = ret
            {
                Type::Vector(it, dep)
            } else if let Type::Reference(td, _) = ret {
                Type::Reference(td, dep)
            } else {
                panic!("Unknown return type");
            };
        }
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
            if t == Type::Null {
                v = self.null(&r_type);
            } else if !self.convert(&mut v, &t, &r_type) {
                self.validate_convert("return", &t, &r_type);
            }
            if let Type::Text(ls) = t {
                self.text_return(&ls);
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
            let t = self.expression(&mut p);
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
            let message = if list.len() > 1 {
                list[1].clone()
            } else {
                Value::str("assert failure")
            };
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
