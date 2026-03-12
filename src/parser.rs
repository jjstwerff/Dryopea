// Copyright (c) 2022-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Parse scripts and create internal code from it.
//! Including type checking.
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

use crate::data::{
    Argument, Context, Data, DefType, I32, Type, Value, to_default, v_block, v_if, v_loop, v_set,
};
use crate::database::{Parts, Stores};
use crate::diagnostics::{Diagnostics, Level, diagnostic_format};
use crate::lexer::{LexItem, LexResult, Lexer, Link, Mode, Position};
use crate::variables::{Function, size as var_size};
use crate::{scopes, typedef};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::env;
use std::fs::{File, metadata, read_dir};
use std::io::Write;
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
// The parser holds several independent boolean mode flags (in_loop, default, first_pass,
// reverse_iterator) that each track a distinct parse phase or context.  Combining them into
// an enum or state machine would add complexity without benefit.
#[allow(clippy::struct_excessive_bools)]
pub struct Parser {
    pub todo_files: Vec<(String, u16)>,
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
    /// Extra library directories for 'use' resolution (from --lib / --project flags)
    pub lib_dirs: Vec<String>,
    /// Is this the first pass on parsing:
    /// - Do not assume that all struct / enum types are already parsed.
    /// - Define variables, try to determine their type (can become clear from later code).
    /// - Claim working text variables for expressions that gather text data outside variables.
    /// - Links between memory allocations (text, stores) their type knows the variable numbers.
    /// - Move variables to a lower scope if an expression still links to their content.
    /// - Determine mutations to stores and administer these in arguments.
    ///
    /// The second pass:
    /// - Creates code, assumes that all types are known.
    first_pass: bool,
    /// Set by `parse_in_range` when `rev(collection)` (without a `..` range) is parsed.
    /// Consumed by `fill_iter` to add the reverse bit (64) into the `on` byte of OpIterate/OpStep.
    reverse_iterator: bool,
    vars: Function,
    /// Last seen line inside the source code, an increase inserts it in the internal code.
    line: u32,
}

// Operators ordered on their precedence
static OPERATORS: &[&[&str]] = &[
    &["??"],
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
static SKIP_WIDTH: [&str; 10] = ["}", ".", "x", "X", "o", "b", "e", "j", "d", "f"];

struct OutputState<'a> {
    radix: i32,
    width: Value,
    token: &'a str,
    plus: bool,
    note: bool,
    dir: i32,
    float: bool,
}

impl OutputState<'_> {
    fn db_format(&self) -> i32 {
        i32::from(self.note) + if self.radix < 0 { 2 } else { 0 }
    }
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

/// Validate function, attribute, value, and field names
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
            todo_files: Vec::new(),
            data: Data::new(),
            database: Stores::new(),
            lexer: Lexer::default(),
            in_loop: false,
            file: 1,
            diagnostics: Diagnostics::new(),
            default: false,
            context: u32::MAX,
            first_pass: true,
            reverse_iterator: false,
            vars: Function::new("", "none"),
            line: 0,
            lib_dirs: Vec::new(),
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
        self.lexer.switch(filename);
        self.first_pass = true;
        self.data.reset();
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl != Level::Error && lvl != Level::Fatal {
            self.first_pass = false;
            self.data.reset();
            self.lexer.switch(filename);
            self.parse_file();
        }
        self.diagnostics.fill(self.lexer.diagnostics());
        self.diagnostics.is_empty()
    }

    /// Parse all .loft files found in a directory tree in alphabetical ordering.
    /// # Errors
    /// With filesystem problems.
    pub fn parse_dir(&mut self, dir: &str, default: bool, debug: bool) -> std::io::Result<()> {
        let paths = read_dir(dir)?;
        let mut files: BTreeSet<String> = BTreeSet::new();
        for path in paths {
            let p = path?;
            let own_file = p
                .path()
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("loft"));
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
                self.parse_dir(&f, default, debug)?;
            } else if !self.parse(&f, default) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("{}", self.diagnostics),
                ));
            }
            scopes::check(&mut self.data);
            if debug {
                self.output(&f, types, from)?;
            }
        }
        Ok(())
    }

    fn output(&mut self, f: &str, types: usize, from: u32) -> std::io::Result<()> {
        let file = if let Some(p) = f.rfind('/') {
            &f[p + 1..]
        } else {
            f
        };
        let to = format!("tests/dumps/{file}.txt");
        if let Ok(mut w) = File::create(to.clone()) {
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
        } else {
            diagnostic!(self.lexer, Level::Error, "Could not write: {to}");
        }
        Ok(())
    }

    /// Only parse a specific string, only useful for parser tests.
    #[allow(dead_code)]
    pub fn parse_str(&mut self, text: &str, filename: &str, logging: bool) {
        self.first_pass = true;
        self.default = false;
        self.vars.logging = logging;
        self.lexer.parse_string(text, filename);
        self.data.reset();
        self.parse_file();
        let lvl = self.lexer.diagnostics().level();
        if lvl == Level::Error || lvl == Level::Fatal {
            self.diagnostics.fill(self.lexer.diagnostics());
            return;
        }
        self.data.reset();
        self.lexer.parse_string(text, filename);
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
    #[allow(clippy::large_types_passed_by_value)] // Option<u16> is Copy; ref adds noise at all call sites
    fn iterator(
        &mut self,
        code: &mut Value,
        is_type: &Type,
        should: &Type,
        iter_var: u16,
        pre_var: Option<u16>,
    ) -> Value {
        if let Value::Iter(_, start, next, _) = code.clone() {
            if matches!(*next, Value::Block(_)) {
                *code = *start;
                return *next.clone();
            }
            panic!("Incorrect Iter");
        }
        if matches!(*is_type, Type::Text(_)) {
            // iter_var is {id}#next — the post-advance byte position (loop driver).
            // pre_var  is {id}#index — saved to the start position of the current char.
            let index_var = pre_var.unwrap();
            let res_var = self
                .vars
                .unique("for_result", &Type::Character, &mut self.lexer);
            let l = self.cl("OpLengthCharacter", &[Value::Var(res_var)]);
            let next = vec![
                // Save current position as #index before advancing.
                v_set(index_var, Value::Var(iter_var)),
                v_set(
                    res_var,
                    self.cl("OpTextCharacter", &[code.clone(), Value::Var(iter_var)]),
                ),
                v_set(iter_var, self.cl("OpAddInt", &[Value::Var(iter_var), l])),
                Value::Var(res_var),
            ];
            // Initialise the loop driver at the outer scope.
            // The caller must separately initialise index_var at the same scope level.
            *code = v_set(iter_var, Value::Int(0));
            return v_block(next, Type::Character, "for text next");
        }
        if is_type == should {
            // there was already an iterator.
            let orig = code.clone();
            *code = Value::Null; // there is no iterator to create, we got it already
            return orig;
        }
        if self.first_pass {
            self.reverse_iterator = false;
            return Value::Null;
        }
        if let Type::Iterator(_, _) = should {
            match is_type {
                Type::Vector(vtp, dep) => {
                    let i = Value::Var(iter_var);
                    let vec_tp = self.data.type_def_nr(vtp);
                    let db_tp = self.data.def(vec_tp).known_type;
                    let size = if self.database.is_linked(db_tp) {
                        4
                    } else {
                        self.database.size(db_tp)
                    };
                    let mut ref_expr = self.cl(
                        "OpGetVector",
                        &[code.clone(), Value::Int(i32::from(size)), i.clone()],
                    );
                    if let Type::Reference(_, _) = *vtp.clone() {
                        if self.database.is_linked(db_tp) {
                            ref_expr = self.cl("OpVectorRef", &[code.clone(), i.clone()]);
                        }
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
                    // Reset the reverse flag after both fill_iter calls so the second call
                    // also picks up the bit (fill_iter does not reset it itself).
                    self.reverse_iterator = false;
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
        // Struct-literal inline constructors are typed as Rewritten(Reference(...)); strip
        // the wrapper so method calls chained on the constructor are accepted correctly.
        if let Type::Rewritten(inner) = is_type {
            return self.convert(code, inner, should);
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
            *code = self.cl("OpCreateStack", std::slice::from_ref(code));
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
        if let Type::Reference(tp, _) = should
            && self.data.def(*tp).returned.is_equal(is_type)
            && matches!(is_type, Type::Enum(_, true, _))
        {
            let get_e = self.cl("OpGetEnum", &[code.clone(), Value::Int(0)]);
            let get = self.cl("OpConvIntFromEnum", &[get_e]);
            if let Value::Enum(nr, _) = self.data.def(*tp).attributes[0].value {
                *code = v_if(
                    self.cl("OpEqInt", &[get, Value::Int(i32::from(nr))]),
                    code.clone(),
                    self.cl("OpConvRefFromNull", &[]),
                );
            }
            return true;
        }
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

    /// Validate that two types are equal
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
            if let (Type::Reference(r_nr, _), Type::Enum(e_nr, true, _)) = (test_type, should)
                && e_nr == r_nr
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
    fn call(
        &mut self,
        code: &mut Value,
        source: u16,
        name: &str,
        list: &[Value],
        types: &[Type],
    ) -> Type {
        // Create a new list of parameters based on the current ones
        // We still need to know the types.
        let d_nr = if self.default && is_op(name) {
            self.data.def_nr(name)
        } else {
            self.data.find_fn(
                source,
                name,
                if types.is_empty() || types[0] == Type::Null {
                    &Type::Unknown(0)
                } else {
                    &types[0]
                },
            )
        };
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
            Type::Reference(_, _) => {
                // This should only count for OpGetVector
                if let Value::Call(_, _) = code {
                    self.cl("OpGetRef", &[code, p])
                } else {
                    self.cl("OpGetField", &[code, p, self.type_info(tp)])
                }
            }
            _ => panic!(
                "Get not implemented on '{}' at {}",
                tp.name(&self.data),
                self.lexer.pos()
            ),
        }
    }

    fn set_field(
        &mut self,
        d_nr: u32,
        f_nr: usize,
        d_pos: u16,
        ref_code: Value,
        val_code: Value,
    ) -> Value {
        let tp = self.data.attr_type(d_nr, f_nr);
        let nm = self.data.attr_name(d_nr, f_nr);
        let pos = self.database.position(self.data.def(d_nr).known_type, &nm);
        let pos_val = Value::Int(if f_nr == usize::MAX {
            i32::from(d_pos)
        } else {
            i32::from(pos + d_pos)
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
            | Type::Sorted(_, _, _)
            | Type::Character => self.cl("OpSetInt", &[ref_code, pos_val, val_code]),
            Type::Reference(inner_tp, _) => {
                // The value is a 12-byte DbRef; OpSetInt would only read 4 bytes of it.
                // Copy the struct bytes into the embedded field instead.
                let type_nr = if self.first_pass {
                    Value::Int(i32::from(u16::MAX))
                } else {
                    Value::Int(i32::from(self.data.def(inner_tp).known_type))
                };
                let field_ref = self.cl("OpGetField", &[ref_code, pos_val, type_nr.clone()]);
                self.cl("OpCopyRecord", &[val_code, field_ref, type_nr])
            }
            Type::Enum(_, false, _) => self.cl("OpSetEnum", &[ref_code, pos_val, val_code]),
            Type::Enum(nr, true, _) => self.cl(
                "OpCopyRecord",
                &[
                    val_code,
                    ref_code,
                    Value::Int(i32::from(self.data.def(nr).known_type)),
                ],
            ),
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
                        // But for compares we can expect to be a constant Enum value.
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
                            let context =
                                format!("call to {}", self.data.def(d_nr).original_name());
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
                    ls.push(self.cl("OpCreateStack", &[Value::Var(vr)]));
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
                    let cur = &self.lexer.pos().file;
                    self.todo_files.push((cur.clone(), self.data.source));
                    self.data.use_add(&id);
                    self.lexer.switch(&f);
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
            if self.lexer.peek_token("use") {
                diagnostic!(
                    self.lexer,
                    Level::Fatal,
                    "use statements must appear before all definitions"
                );
            } else {
                diagnostic!(self.lexer, Level::Fatal, "Syntax error");
            }
        }
        typedef::actual_types(
            &mut self.data,
            &mut self.database,
            &mut self.lexer,
            start_def,
        );
        typedef::fill_all(&mut self.data, &mut self.database, start_def);
        self.database.finish();
        self.enum_fn();
        let lvl = self.lexer.diagnostics().level();
        if lvl == Level::Error || lvl == Level::Fatal {
            return;
        }
        // Parse all files left in the todo_files list, as they are halted to parse a use file.
        while let Some((t, s)) = self.todo_files.pop() {
            self.lexer.switch(&t);
            self.data.source = s;
            self.parse_file();
        }
    }

    fn lib_path(&mut self, id: &String) -> String {
        // - a source file, the lib directory in the project (project-supplied)
        let mut f = format!("lib/{id}.loft");
        if !std::path::Path::new(&f).exists() {
            f = format!("{id}.loft");
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
            f = format!("{cur_dir}/lib/{id}.loft");
        }
        // - a lib directory relative to the base directory when inside /tests/
        if !base_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{base_dir}/lib/{id}.loft");
        }
        // - a directory with the same name of the current script
        if !std::path::Path::new(&f).exists() {
            f = format!("{}/{id}.loft", &cur_script[0..cur_script.len() - 5]);
        }
        // - extra library directories from --lib / --project command-line flags
        if !std::path::Path::new(&f).exists() {
            for l in &self.lib_dirs {
                let candidate = format!("{l}/{id}.loft");
                if std::path::Path::new(&candidate).exists() {
                    f = candidate;
                    break;
                }
            }
        }
        // - a user-defined lib directory (externally downloaded)
        if !std::path::Path::new(&f).exists()
            && let Some(v) = env::var_os("LOFT_LIB")
        {
            let libs = v.to_str().unwrap();
            for l in libs.split(':') {
                f = format!("{l}/{id}.loft");
                if std::path::Path::new(&f).exists() {
                    break;
                }
            }
        }
        // - the current directory (beside the parsed file)
        if !cur_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{cur_dir}/{id}.loft");
        }
        // - the base directory when inside /tests/
        if !base_dir.is_empty() && !std::path::Path::new(&f).exists() {
            f = format!("{base_dir}/{id}.loft");
        }
        f
    }

    // Determine if there need to be special enum functions that call enum_value variants.
    fn enum_fn(&mut self) {
        if !self.first_pass {
            return;
        }
        let mut todo = HashMap::new();
        for (d_nr, d) in self.data.definitions.iter().enumerate() {
            if d.def_type != DefType::Function || d.attributes.is_empty() {
                continue;
            }
            if let Type::Reference(e_tp, _) = &d.attributes[0].typedef
                && matches!(self.data.def(*e_tp).returned, Type::Enum(_, true, _))
                && self
                    .data
                    .find_fn(u16::MAX, &d.original_name(), &self.data.def(*e_tp).returned)
                    == u32::MAX
                && let Type::Enum(e_nr, true, _) = self.data.def(*e_tp).returned
            {
                todo.entry(e_nr).or_insert(vec![]).push(d_nr);
            }
        }
        for (e_nr, nrs) in todo {
            let from_nr = nrs[0] as u32;
            let name = self.data.def(from_nr).original_name().clone();
            let attrs = self.data.def(from_nr).attributes[1..].to_vec();
            let mut common = attrs.len();
            for nr in &nrs[1..] {
                let mut c = 0;
                for a in &self.data.def(*nr as u32).attributes[1..] {
                    for o in &attrs {
                        if a.name == o.name && a.typedef == o.typedef {
                            c += 1;
                        }
                    }
                }
                if c < common {
                    common = c;
                }
            }
            for nr in &nrs {
                if self.data.def(*nr as u32).attributes.len() > common + 1 {
                    for a in &self.data.def(*nr as u32).attributes[common + 1..] {
                        if a.value == Value::Null {
                            return;
                        }
                    }
                }
            }
            let mut args = Vec::new();
            args.push(Argument {
                name: "self".to_string(),
                typedef: Type::Enum(e_nr, true, vec![]),
                default: Value::Null,
                constant: false,
            });
            for a in &attrs[..common] {
                args.push(Argument {
                    name: a.name.clone(),
                    typedef: a.typedef.clone(),
                    default: a.value.clone(),
                    constant: false,
                });
            }
            let fn_nr = self.data.add_fn(&mut self.lexer, &name, &args);
            self.context = fn_nr;
            self.vars = Function::new(&name, &self.data.def(from_nr).position.file);
            self.data
                .set_returned(fn_nr, self.data.def(from_nr).returned.clone());
            for a in &args {
                let v_nr = self.create_var(&a.name, &a.typedef);
                if v_nr != u16::MAX {
                    self.vars.become_argument(v_nr);
                }
            }
            // Build forwarding args for extra (non-self) attributes (e.g. RefVar(Text) buffers).
            // Variant calls must write into the dispatcher's own text-buffer argument, not a
            // freshly-allocated work_text that has no stack slot yet.
            let mut extra_call_args: Vec<Value> = Vec::new();
            let mut extra_call_types: Vec<Type> = Vec::new();
            for a in &args[1..] {
                let v = self.vars.var(&a.name);
                if v != u16::MAX {
                    extra_call_args.push(Value::Var(v));
                    extra_call_types.push(a.typedef.clone());
                }
            }
            let mut ls = Vec::new();
            let get_enum = self.cl("OpGetEnum", &[Value::Var(0), Value::Int(0)]);
            let get_int = self.cl("OpConvIntFromEnum", &[get_enum]);
            self.enum_numbers(
                nrs.clone(),
                &name,
                &mut ls,
                &get_int,
                &extra_call_args,
                &extra_call_types,
            );
            ls.push(Value::Null);
            self.data.definitions[fn_nr as usize].code =
                v_block(ls, self.data.def(from_nr).returned.clone(), "dynamic_fn");
            self.data.definitions[self.context as usize].variables = self.vars.clone();
            // Warn about enum variants that have no implementation for this function.
            let implemented: HashSet<u32> = nrs
                .iter()
                .filter_map(|nr| {
                    if let Type::Reference(a_nr, _) =
                        self.data.def(*nr as u32).attributes[0].typedef
                    {
                        Some(a_nr)
                    } else {
                        None
                    }
                })
                .collect();
            let missing: Vec<(String, Position)> = self
                .data
                .definitions
                .iter()
                .enumerate()
                .filter(|(_, v)| v.def_type == DefType::EnumValue && v.parent == e_nr)
                .filter(|(v_nr, _)| !implemented.contains(&(*v_nr as u32)))
                .map(|(_, v)| (v.name.clone(), v.position.clone()))
                .collect();
            for (variant_name, pos) in &missing {
                self.lexer.pos_diagnostic(
                    Level::Warning,
                    pos,
                    &format!("Warning: no implementation of '{name}' for variant '{variant_name}'"),
                );
            }
        }
    }

    fn enum_numbers(
        &mut self,
        nrs: Vec<usize>,
        name: &str,
        ls: &mut Vec<Value>,
        get_int: &Value,
        extra_args: &[Value],
        extra_types: &[Type],
    ) {
        for nr in nrs {
            let d_nr = nr as u32;
            let a_nr = if let Type::Reference(nr, _) = self.data.def(d_nr).attributes[0].typedef {
                nr
            } else {
                0
            };
            let e_nr = if let Value::Enum(nr, _) = self.data.def(a_nr).attributes[0].value {
                nr
            } else {
                0
            };
            let self_type = self.data.def(d_nr).attributes[0].typedef.clone();
            let mut call_args = vec![Value::Var(0)];
            call_args.extend_from_slice(extra_args);
            let mut call_types = vec![self_type];
            call_types.extend_from_slice(extra_types);
            let mut code = Value::Null;
            self.call(&mut code, u16::MAX, name, &call_args, &call_types);
            let ret_call = v_block(
                vec![Value::Return(Box::new(code.clone()))],
                Type::Void,
                "ret",
            );
            ls.push(v_if(
                self.cl("OpEqInt", &[get_int.clone(), Value::Int(i32::from(e_nr))]),
                ret_call,
                Value::Null,
            ));
        }
    }

    // <enum> ::= 'enum' <identifier> '{' <value> {, <value>} '}' [';']
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
                    self.data.definitions[d_nr as usize].returned =
                        Type::Enum(d_nr, true, Vec::new());
                    self.data
                        .set_returned(v_nr, Type::Enum(d_nr, true, Vec::new()));
                    self.data.add_attribute(
                        &mut self.lexer,
                        d_nr,
                        &value_name,
                        Type::Enum(d_nr, true, Vec::new()),
                    );
                    self.data.definitions[d_nr as usize].attributes[nr as usize].constant = true;
                    // Enum values start with 1 as 0 is de null/undefined value.
                    self.data
                        .set_attr_value(d_nr, nr as usize, Value::Enum(nr + 1, u16::MAX));
                    // Create an "enum" field inside the new structure
                    let e_attr = self.data.add_attribute(
                        &mut self.lexer,
                        v_nr,
                        "enum",
                        Type::Enum(self.data.def_nr("enumerate"), false, Vec::new()),
                    );
                    // Enum values start with 1 as 0 is de null/undefined value.
                    self.data
                        .set_attr_value(v_nr, e_attr, Value::Enum(nr + 1, u16::MAX));
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
                self.data.definitions[d_nr as usize].attributes[nr as usize].constant = true;
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

    // <typedef> ::= 'type' <identifier> '=' <type_def> [ 'size' '(' <integer> ')' ] ';'
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
                    diagnostic!(self.lexer, Level::Error, "'{type_name}' is not a type");
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Expected a type after =");
            }
        }
        if self.lexer.has_keyword("size") {
            self.lexer.token("(");
            self.lexer.has_integer();
            self.lexer.token(")");
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

    /// After parsing a function body, check that each `&` (`RefVar`) argument is actually
    /// mutated somewhere in the body. If not, emit a compile error suggesting to drop the `&`.
    fn check_ref_mutations(&mut self, arguments: &[Argument]) {
        let code = self.data.def(self.context).code.clone();
        let mut written: HashSet<u16> = HashSet::new();
        find_written_vars(&code, &self.data, &mut written);
        for (a_nr, a) in arguments.iter().enumerate() {
            if matches!(a.typedef, Type::RefVar(_))
                && !a.constant
                && !written.contains(&(a_nr as u16))
            {
                let src = self.vars.var_source(a_nr as u16);
                self.lexer.to(&src);
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Parameter '{}' has & but is never modified; remove the &",
                    a.name
                );
            }
        }
    }

    // <function> ::= 'fn' <identifier> '(' <attributes> ] [ '->' <type> ] (';' <rust> | <code>)
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
        } else if self.default && is_op(&fn_name) {
            self.data.def_nr(&fn_name)
        } else {
            self.data.get_fn(&fn_name, &arguments)
        };
        if self.context == u32::MAX {
            return false;
        }
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
        if self.first_pass {
            self.data.set_returned(self.context, result);
        }
        if !self.lexer.has_token(";") {
            for (a_nr, a) in arguments.iter().enumerate() {
                if self.first_pass {
                    let v_nr = self.create_var(&a.name, &a.typedef);
                    if v_nr != u16::MAX {
                        self.vars.become_argument(v_nr);
                        self.var_usages(v_nr, false);
                    }
                } else {
                    self.change_var_type(a_nr as u16, &a.typedef);
                    if a.constant {
                        self.vars.set_const_param(a_nr as u16);
                    }
                }
            }
            self.parse_code();
            if !self.first_pass {
                self.check_ref_mutations(&arguments);
            }
        }
        if !self.first_pass {
            // Stub functions with an empty body `{ }` and a `self` parameter are intentional
            // skips (e.g. to silence the "no implementation for variant" warning).
            // Don't warn about unused parameters in that case.
            let is_stub = {
                let def = &self.data.definitions[self.context as usize];
                let body_empty = matches!(&def.code, Value::Block(bl) if bl.operators.is_empty());
                let first_is_self = def.attributes.first().is_some_and(|a| a.name == "self");
                body_empty && first_is_self
            };
            if !is_stub {
                self.vars.test_used(&mut self.lexer, &self.data);
            }
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
                        } else {
                            if !self.first_pass {
                                diagnostic!(
                                    self.lexer,
                                    Level::Error,
                                    "'{type_name}' is not a type"
                                );
                            }
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

    // <type> ::= <identifier> [::<identifier>] [ '<' ( <sub_type> | <type> ) '>' ] [ <depend> ]
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
                DefType::Type | DefType::Enum | DefType::EnumValue | DefType::Struct
            )
        {
            if matches!(dt, DefType::EnumValue)
                || (self.first_pass && matches!(dt, DefType::Struct))
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
                        self.parse_fields(true, &mut fields);
                        Type::Index(self.data.type_def_nr(&tp), fields, Vec::new())
                    }
                    "hash" => {
                        self.parse_fields(false, &mut fields);
                        self.data.set_referenced(sub_nr, on_d, Value::Null);
                        let mut f = Vec::new();
                        for (field, _) in fields {
                            f.push(field);
                        }
                        Type::Hash(sub_nr, f, Vec::new())
                    }
                    "vector" => {
                        self.lexer.token(">");
                        Type::Vector(Box::new(tp), Vec::new())
                    }
                    "sorted" => {
                        self.parse_fields(true, &mut fields);
                        Type::Sorted(sub_nr, fields, Vec::new())
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

    // <depend> ::= '[' { <field> [ ',' ] } ']'
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

    fn parse_fields(&mut self, directions: bool, result: &mut Vec<(String, bool)>) {
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
                result.push((field, !desc));
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
            self.lexer.has_token("pub");
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
            if !self.lexer.has_token(",") || self.lexer.peek_token("}") {
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
        loop {
            if self.lexer.has_keyword("not") {
                // This field cannot be null, this allows for 256 values in a byte
                self.lexer.token("null");
                nullable = false;
            }
            self.parse_field_default(&mut value, &mut a_type, d_nr, a_name, &mut defined);
            /* TODO for now ignore this, we have to properly implement this in the future
            if self.lexer.has_keyword("check") {
                self.lexer.token("(");
                let tp = self.expression(&mut check);
                self.convert(&mut check, &tp, &Type::Boolean);
                self.lexer.token(")");
            }
            */
            if let Some(id) = self.lexer.has_identifier() {
                if id == "CHECK" {
                    // CHECK(condition, message) constraint — parse and discard for now
                    self.lexer.token("(");
                    let mut p = Value::Null;
                    self.expression(&mut p);
                    if self.lexer.has_token(",") {
                        let mut q = Value::Null;
                        self.expression(&mut q);
                    }
                    self.lexer.token(")");
                } else if let Some(tp) = self.parse_type(d_nr, &id, false) {
                    defined = true;
                    a_type = tp;
                    // '= expr' shorthand for a field default value
                    if self.lexer.has_token("=") {
                        let tp = self.expression(&mut value);
                        if a_type.is_unknown() {
                            a_type = tp;
                        }
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
            self.data.set_attr_nullable(d_nr, a, nullable);
            self.data.set_attr_value(d_nr, a, value);
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
            // In debug builds: auto-lock the stores for every const Reference/Vector argument
            // at the very start of the function body (after work-variable initialisations).
            #[cfg(debug_assertions)]
            if !self.first_pass {
                let n_vars = self.vars.next_var();
                let lock_fn = self.data.def_nr("n_set_store_lock");
                if lock_fn != u32::MAX {
                    let mut inserts = Vec::new();
                    for v_nr in 0..n_vars {
                        if self.vars.is_argument(v_nr)
                            && self.vars.is_const_param(v_nr)
                            && matches!(
                                self.vars.tp(v_nr),
                                Type::Reference(_, _) | Type::Vector(_, _)
                            )
                        {
                            inserts.push(Value::Call(
                                lock_fn,
                                vec![Value::Var(v_nr), Value::Boolean(true)],
                            ));
                        }
                    }
                    // Insert in reverse order so index-0 inserts keep the right sequence.
                    inserts.reverse();
                    for ins in inserts {
                        ls.insert(0, ins);
                    }
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
        } else if self.lexer.peek_token("{") {
            self.parse_block("block", val, &Type::Void)
        } else {
            // `const x = expr` — mark the resulting local variable as const after initialisation.
            let const_decl = self.lexer.has_keyword("const");
            let res = self.parse_assign(val);
            if const_decl && !self.first_pass {
                let v_nr = match val {
                    Value::Set(nr, _) => Some(*nr),
                    Value::Insert(ls) => ls.iter().find_map(|v| {
                        if let Value::Set(nr, _) = v {
                            Some(*nr)
                        } else {
                            None
                        }
                    }),
                    _ => None,
                };
                if let Some(v_nr) = v_nr {
                    self.vars.set_const_param(v_nr);
                    // In debug builds: auto-lock the store right after initialisation.
                    #[cfg(debug_assertions)]
                    if matches!(
                        self.vars.tp(v_nr),
                        Type::Reference(_, _) | Type::Vector(_, _)
                    ) {
                        let lock_call = self.cl(
                            "n_set_store_lock",
                            &[Value::Var(v_nr), Value::Boolean(true)],
                        );
                        *val = Value::Insert(vec![val.clone(), lock_call]);
                    }
                } else if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "const keyword requires a variable assignment"
                    );
                }
            }
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
            if op == "="
                && let Value::Var(v_nr) = code
                && !self.first_pass
                && self.vars.exists(*v_nr)
            {
                self.vars.defined(*v_nr);
            }
            if self.lexer.has_token(op) {
                let var_nr = self.assign_var_nr(code, op, &f_type, &mut parent_tp);
                // Handle `f += X` for File variables before type-changing logic.
                if op == "+="
                    && self.is_file_var_type(&f_type)
                    && let Value::Var(file_v) = to
                {
                    self.append_to_file(code, file_v);
                    return Type::Void;
                }
                // Guard: forbid adding elements to a collection that is currently
                // being iterated by a surrounding for-loop.  Adding elements during
                // iteration is never safe:
                //   • vector   — get_vector re-reads the length each step, so new
                //                elements ARE visited, risking an infinite loop.
                //   • sorted   — insertions shift positions in the backing array,
                //                corrupting the pre-computed finish boundary.
                //   • index    — B-tree insertions trigger rotations/splits; stored
                //                record numbers become stale.
                // Use `e#remove` (which adjusts the iterator) for safe removal.
                if !self.first_pass
                    && op == "+="
                    && matches!(
                        f_type,
                        Type::Vector(_, _)
                            | Type::Sorted(_, _, _)
                            | Type::Index(_, _, _)
                            | Type::Spacial(_, _, _)
                    )
                {
                    if let Value::Var(lhs_nr) = to
                        && self.vars.is_iterated_var(lhs_nr)
                    {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Cannot add elements to '{}' while it is being iterated — \
use a separate collection or add after the loop",
                            self.vars.name(lhs_nr)
                        );
                    } else if !matches!(to, Value::Var(_))
                        && self.vars.is_iterated_value(&to)
                    {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Cannot add elements to a collection while it is being iterated — \
use a separate collection or add after the loop"
                        );
                    }
                }
                // Save parent struct type before the RHS parse overwrites parent_tp.
                let lhs_parent_tp = parent_tp.clone();
                let mut s_type = self.parse_operators(&f_type, code, &mut parent_tp, 0);
                if let Type::Rewritten(tp) = s_type {
                    s_type = *tp;
                }
                if var_nr == u16::MAX {
                    self.validate_write(&to, &parent_tp);
                }
                self.change_var(&to, &s_type);
                if matches!(f_type, Type::Text(_)) {
                    self.assign_text(code, &s_type, &to, op, var_nr);
                    return Type::Void;
                }
                if self.assign_refvar_text(code, &f_type, &s_type, op, var_nr) {
                    return Type::Void;
                }
                if self.assign_refvar_vector(code, &f_type, op, var_nr) {
                    return Type::Void;
                }
                if var_nr != u16::MAX && self.create_vector(code, &f_type, op, var_nr) {
                    return Type::Void;
                }
                // `lhs += other_vec` where both sides are vectors: append all elements
                // in-place via OpAppendVector.  This handles both variable LHS (a += more)
                // and field LHS (bx.pts += more) — both produce a DbRef as their first arg.
                if !self.first_pass
                    && op == "+="
                    && let Type::Vector(elm_tp, _) = &f_type.clone()
                    && matches!(s_type, Type::Vector(_, _))
                    && !matches!(code, Value::Insert(_))
                {
                    let rec_tp =
                        i32::from(self.data.def(self.data.type_def_nr(elm_tp)).known_type);
                    *code = Value::Insert(vec![self.cl(
                        "OpAppendVector",
                        &[to.clone(), code.clone(), Value::Int(rec_tp)],
                    )]);
                    return Type::Void;
                }
                // Scalar `field += elem` where field is a vector field (var_nr == u16::MAX).
                // Route through new_record so that a null vector field is allocated in place,
                // the same as `field += [elem]` (bracket form) does via parse_vector.
                if !self.first_pass
                    && var_nr == u16::MAX
                    && op == "+="
                    && self.is_field(&to)
                    && let Type::Vector(elm_tp, _) = &f_type
                    && !matches!(code, Value::Insert(_))
                {
                    let elm_tp = (**elm_tp).clone();
                    let elm = self.unique_elm_var(&lhs_parent_tp, &elm_tp, u16::MAX);
                    let scalar = code.clone();
                    let ls = self.new_record(
                        &mut to.clone(),
                        &lhs_parent_tp,
                        elm,
                        u16::MAX,
                        &[scalar],
                        &elm_tp,
                    );
                    *code = Value::Insert(ls);
                    return Type::Void;
                }
                // Auto-convert integer to long for a long-typed LHS assignment.
                if matches!(f_type, Type::Long)
                    && matches!(s_type, Type::Integer(_, _))
                    && op == "="
                    && !self.first_pass
                {
                    *code = self.cl("OpConvLongFromInt", std::slice::from_ref(code));
                }
                // Validate d#lock = <expr> assignments.
                if !self.first_pass
                    && let Value::Call(lock_nr, lock_args) = &to
                    && self.data.def(*lock_nr).name == "n_get_store_lock"
                {
                    // Only constant boolean literals are allowed.
                    if !matches!(code, Value::Boolean(_)) {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "d#lock can only be assigned a constant boolean (true or false)"
                        );
                        return Type::Void;
                    }
                    // Setting a const variable's lock to false is illegal.
                    if matches!(code, Value::Boolean(false))
                        && let Some(Value::Var(v_nr)) = lock_args.first()
                        && self.vars.is_const_param(*v_nr)
                    {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Cannot unlock const variable '{}' via d#lock = false",
                            self.vars.name(*v_nr)
                        );
                        return Type::Void;
                    }
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

    fn append_to_text(&mut self, code: &mut Value, op: &str, var_nr: u16, s_type: &Type) {
        if !self.first_pass && self.vars.is_const_param(var_nr) && !matches!(code, Value::Insert(_))
        {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Cannot modify {} '{}'",
                self.vars.const_kind(var_nr),
                self.vars.name(var_nr)
            );
        }
        if matches!(code, Value::Insert(_)) {
            // nothing
        } else if op == "=" {
            *code = v_set(var_nr, code.clone());
        } else if s_type == &Type::Character {
            *code = self.cl(
                "OpAppendStackCharacter",
                &[Value::Var(var_nr), code.clone()],
            );
        } else {
            *code = self.cl("OpAppendStackText", &[Value::Var(var_nr), code.clone()]);
        }
    }

    fn append_to_file(&mut self, code: &mut Value, file_v: u16) {
        let mut rhs_code = Value::Null;
        let mut unused = Type::Null; // parent_tp, this is normally used to unpack the vector fill
        let mut rhs_type = self.parse_operators(&Type::Unknown(0), &mut rhs_code, &mut unused, 0);
        if let Type::Rewritten(tp) = rhs_type {
            rhs_type = *tp;
        }
        *code = self.write_to_file(file_v, rhs_code, &rhs_type);
    }

    /// Determine the variable number for an assignment target.
    /// For text `+=`, creates a unique temporary variable.
    fn assign_var_nr(
        &mut self,
        code: &mut Value,
        op: &str,
        f_type: &Type,
        parent_tp: &mut Type,
    ) -> u16 {
        if let Value::Var(v_nr) = *code {
            v_nr
        } else if op == "+=" && matches!(f_type, Type::Text(_)) {
            let v = self
                .vars
                .unique("field", &Type::Text(vec![]), &mut self.lexer);
            *code = Value::Var(v);
            *parent_tp = Type::Null;
            v
        } else {
            u16::MAX
        }
    }

    /// Handle assignment into a `RefVar(Text)` target; returns true if handled.
    fn assign_refvar_text(
        &mut self,
        code: &mut Value,
        f_type: &Type,
        s_type: &Type,
        op: &str,
        var_nr: u16,
    ) -> bool {
        let Type::RefVar(t) = f_type else {
            return false;
        };
        if !matches!(**t, Type::Text(_)) {
            return false;
        }
        self.append_to_text(code, op, var_nr, s_type);
        true
    }

    /// Handle `v += expr` where `v: &vector<T>`; returns true if handled.
    /// NOTE: does NOT intercept `Value::Insert` — bracket-form `[elem]` literals are already
    /// handled by the Insert-expansion in `parse_block` → `OpFinishRecord`.
    fn assign_refvar_vector(
        &mut self,
        code: &mut Value,
        f_type: &Type,
        op: &str,
        var_nr: u16,
    ) -> bool {
        let Type::RefVar(inner) = f_type else {
            return false;
        };
        let Type::Vector(elm_tp, _) = inner.as_ref() else {
            return false;
        };
        if op != "+=" {
            return false;
        }
        // Bracket-form [elem] and vector comprehensions produce Insert/Block; leave those
        // to the existing parse_block expansion path which uses OpFinishRecord.
        if matches!(code, Value::Insert(_) | Value::Block(_)) {
            return false;
        }
        if self.first_pass {
            return true;
        }
        let rec_tp = i32::from(self.data.def(self.data.type_def_nr(elm_tp)).known_type);
        *code = self.cl(
            "OpAppendVector",
            &[Value::Var(var_nr), code.clone(), Value::Int(rec_tp)],
        );
        true
    }

    fn validate_write(&mut self, to: &Value, parent_tp: &Type) {
        if let Value::Call(_, vars) = to
            && vars.len() > 1
            && let Value::Int(pos) = vars[1]
        {
            let d_nr = self.data.type_def_nr(parent_tp);
            if d_nr != u32::MAX {
                let known = self.data.def(d_nr).known_type;
                if known != u16::MAX
                    && let Parts::Struct(fields) = &self.database.types[known as usize].parts
                {
                    for (f_nr, f) in fields.iter().enumerate() {
                        if f.position == pos as u16 && !self.data.def(d_nr).attributes[f_nr].mutable
                        {
                            diagnostic!(
                                self.lexer,
                                Level::Error,
                                "Cannot write to key field {}.{} create a record instead",
                                self.data.def(d_nr).name,
                                f.name
                            );
                        }
                    }
                }
            }
        }
    }

    fn assign_text(&mut self, code: &mut Value, tp: &Type, to: &Value, op: &str, var_nr: u16) {
        if !self.first_pass && var_nr != u16::MAX && self.vars.is_const_param(var_nr) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Cannot modify {} '{}'",
                self.vars.const_kind(var_nr),
                self.vars.name(var_nr)
            );
        }
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
        } else if op == "=" && var_nr != u16::MAX {
            *code = v_set(var_nr, code.clone());
        } else if *tp == Type::Character {
            *code = self.cl("OpAppendCharacter", &[Value::Var(var_nr), code.clone()]);
        } else {
            *code = self.cl("OpAppendText", &[Value::Var(var_nr), code.clone()]);
        }
    }

    fn create_vector(&mut self, code: &mut Value, f_type: &Type, op: &str, var_nr: u16) -> bool {
        if let (Value::Insert(ls), Type::Vector(tp, _)) = (code, f_type) {
            if !self.first_pass && self.vars.is_const_param(var_nr) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Cannot modify {} '{}'",
                    self.vars.const_kind(var_nr),
                    self.vars.name(var_nr)
                );
            }
            if op == "=" {
                for (s_nr, s) in self.vector_db(tp, var_nr).iter().enumerate() {
                    ls.insert(s_nr, s.clone());
                }
                if ls.is_empty()
                    && !self.first_pass
                    && var_nr != u16::MAX
                    && matches!(f_type, Type::Vector(_, _))
                {
                    ls.push(self.cl("OpClearVector", &[Value::Var(var_nr)]));
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
        // Intercept `h[key] = null` → remove the key from hash/index/sorted
        if !self.first_pass
            && *val == Value::Null
            && op == "="
            && let Value::Call(get_nr, get_args) = to
            && self.data.def(*get_nr).name == "OpGetRecord"
            && let Some(Value::Int(db_tp_val)) = get_args.get(1)
            && (*db_tp_val as usize) < self.database.types.len()
            && matches!(
                self.database.types[*db_tp_val as usize].parts,
                Parts::Hash(_, _) | Parts::Index(_, _, _) | Parts::Sorted(_, _)
            )
        {
            let db_tp = *db_tp_val;
            let get_args = get_args.clone();
            let get_rec = self.cl("OpGetRecord", &get_args);
            return self.cl(
                "OpHashRemove",
                &[get_args[0].clone(), get_rec, Value::Int(db_tp)],
            );
        }
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
                if !self.first_pass && self.vars.is_const_param(*nr) {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Cannot modify {} '{}'",
                        self.vars.const_kind(*nr),
                        self.vars.name(*nr)
                    );
                }
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
        let code = self.compute_op_code(op, to, val, f_type);
        if let Value::Call(d_nr, args) = &to {
            let name = self.data.def(*d_nr).name.clone();
            let args = args.clone();
            self.call_to_set_op(&name, &args, code, op)
        } else if let Value::Var(nr) = to {
            if !self.first_pass && self.vars.is_const_param(*nr) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Cannot modify {} '{}'",
                    self.vars.const_kind(*nr),
                    self.vars.name(*nr)
                );
            }
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

    /// Compute the RHS value after applying `op` to `to` and `val`.
    fn compute_op_code(&mut self, op: &str, to: &Value, val: &Value, f_type: &Type) -> Value {
        if op == "=" {
            val.clone()
        } else if op == ">" {
            self.op("Lt", val.clone(), to.clone(), f_type.clone())
        } else if op == ">=" {
            self.op("Le", val.clone(), to.clone(), f_type.clone())
        } else {
            self.op(rename(op), to.clone(), val.clone(), f_type.clone())
        }
    }

    /// Dispatch an `OpGetX` getter name to the corresponding `OpSetX` setter call.
    fn call_to_set_op(&mut self, name: &str, args: &[Value], code: Value, op: &str) -> Value {
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
            "OpGetLong" => {
                // f#next = pos: seek the file AND update the stored field.
                if args[1] == Value::Int(16)
                    && let Value::Var(v_nr) = &args[0]
                    && self.is_file_var(*v_nr)
                {
                    let seek = self.cl("OpSeekFile", &[args[0].clone(), code.clone()]);
                    let set = self.cl(
                        "OpSetLong",
                        &[args[0].clone(), args[1].clone(), code.clone()],
                    );
                    return Value::Insert(vec![seek, set]);
                }
                self.cl("OpSetLong", &[args[0].clone(), args[1].clone(), code])
            }
            "OpGetFloat" => self.cl("OpSetFloat", &[args[0].clone(), args[1].clone(), code]),
            "OpGetSingle" => self.cl("OpSetSingle", &[args[0].clone(), args[1].clone(), code]),
            "OpGetField" => code,
            "n_get_store_lock" => {
                // d#lock = val — validation enforced in parse_assign before this call.
                self.cl("n_set_store_lock", &[args[0].clone(), code])
            }
            "OpSizeFile" => {
                // f#size = n: delegate to set_file_size which validates format and sign.
                let fn_nr = self.data.def_nr("t_4File_set_file_size");
                if fn_nr == u32::MAX {
                    if !self.first_pass {
                        diagnostic!(self.lexer, Level::Error, "set_file_size is not defined");
                    }
                    Value::Null
                } else {
                    Value::Call(fn_nr, vec![args[0].clone(), code])
                }
            }
            _ => {
                if !self.first_pass {
                    diagnostic!(self.lexer, Level::Error, "Unknown {op} for {name}");
                }
                Value::Null
            }
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
        self.lexer.token("{");
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
        if matches!(t, Type::RefVar(_)) {
            let mut code = l.pop().unwrap().clone();
            self.un_ref(&mut t, &mut code);
            l.push(code);
        }
        t = self.block_result(context, result, &t, &mut l);
        *val = v_block(l, t.clone(), "block");
        t
    }

    fn un_ref(&mut self, t: &mut Type, code: &mut Value) {
        if let Type::RefVar(tp) = t.clone() {
            self.convert(code, t, &tp);
            *t = *tp;
            for on in t.depend() {
                *t = t.depending(on);
            }
        }
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
            let t = self.parse_part(var_tp, code, parent_tp);
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
                    } else if let Type::RefVar(inner) = &current_type
                        && matches!(**inner, Type::Vector(_, _))
                    {
                        return self.parse_append_vector(code, inner, &ls, orig_var);
                    }
                }
                return current_type;
            }
            self.known_var_or_type(code);
            if operator == "+"
                && matches!(
                    current_type,
                    Type::Text(_) | Type::Character | Type::Vector(_, _)
                )
            {
                let mut second_code = Value::Null;
                let tp = self.parse_operators(var_tp, &mut second_code, parent_tp, precedence + 1);
                ls.push((second_code, tp));
            } else if let Some(value) = self.handle_operator(
                var_tp,
                code,
                parent_tp,
                precedence,
                &mut current_type,
                operator,
            ) {
                return value;
            }
        }
    }

    fn parse_part(&mut self, var_tp: &Type, code: &mut Value, parent_tp: &mut Type) -> Type {
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
        t
    }

    fn handle_operator(
        &mut self,
        var_tp: &Type,
        code: &mut Value,
        parent_tp: &mut Type,
        precedence: usize,
        ctp: &mut Type,
        operator: &str,
    ) -> Option<Type> {
        if operator == "??" {
            // Null-coalescing: `x ?? default` evaluates to `x` if x is not null,
            // otherwise to `default`.  Compiles as: if (x != null_sentinel) { x } else { default }.
            // Note: `x` is evaluated twice for non-trivial expressions (known V1 limitation).
            // Returns None so the outer loop in parse_operators continues, allowing chaining.
            let lhs_type = ctp.clone();
            let mut rhs = Value::Null;
            let rhs_type = self.parse_operators(var_tp, &mut rhs, parent_tp, precedence + 1);
            self.known_var_or_type(&rhs);
            if matches!(lhs_type, Type::Null) {
                // LHS is an untyped null literal: always use the RHS.
                *code = rhs;
                *ctp = rhs_type;
            } else {
                if !self.convert(&mut rhs, &rhs_type, &lhs_type) && !self.first_pass {
                    self.can_convert(&rhs_type, &lhs_type);
                }
                let lhs = code.clone();
                let mut null_check = code.clone();
                self.call_op(
                    &mut null_check,
                    "!=",
                    &[lhs.clone(), Value::Null],
                    &[lhs_type.clone(), Type::Null],
                );
                *code = v_if(null_check, lhs, rhs);
                *ctp = lhs_type;
            }
        } else if operator == "as" {
            if let Some(tps) = self.lexer.has_identifier() {
                let Some(tp) = self.parse_type(u32::MAX, &tps, false) else {
                    diagnostic!(self.lexer, Level::Error, "Expect type");
                    return Some(Type::Null);
                };
                if !self.convert(code, ctp, &tp) && !self.cast(code, ctp, &tp) {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown cast from {} to {tps}",
                        &ctp.name(&self.data),
                    );
                }
                let mut rt = tp;
                for d in ctp.depend() {
                    rt = rt.depending(d);
                }
                return Some(rt);
            }
            diagnostic!(self.lexer, Level::Error, "Expect type after as");
        } else if operator == "or" || operator == "||" {
            self.boolean_operator(code, ctp, precedence, true);
            *ctp = Type::Boolean;
        } else if operator == "and" || operator == "&&" {
            self.boolean_operator(code, ctp, precedence, false);
            *ctp = Type::Boolean;
        } else if operator == "=="
            || operator == "!="
            || operator == "<"
            || operator == "<="
            || operator == ">"
            || operator == ">="
        {
            let mut second_code = Value::Null;
            let tp = parent_tp.clone();
            *parent_tp = ctp.clone();
            let second_type =
                self.parse_operators(var_tp, &mut second_code, parent_tp, precedence + 1);
            self.known_var_or_type(&second_code);
            if operator == ">" {
                *ctp = self.call_op(
                    code,
                    "<",
                    &[second_code, code.clone()],
                    &[second_type, ctp.clone()],
                );
            } else if operator == ">=" {
                *ctp = self.call_op(
                    code,
                    "<=",
                    &[second_code, code.clone()],
                    &[second_type, ctp.clone()],
                );
            } else {
                *ctp = self.call_op(
                    code,
                    operator,
                    &[code.clone(), second_code],
                    &[ctp.clone(), second_type],
                );
            }
            *parent_tp = tp;
        } else {
            let mut second_code = Value::Null;
            let second_type =
                self.parse_operators(var_tp, &mut second_code, parent_tp, precedence + 1);
            self.known_var_or_type(&second_code);
            *ctp = self.call_op(
                code,
                operator,
                &[code.clone(), second_code],
                &[ctp.clone(), second_type],
            );
        }
        None
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
        } else if matches!(self.vars.tp(orig_var), Type::RefVar(t) if matches!(**t, Type::Vector(_, _)))
        {
            // RefVar(Vector): append directly without an identity Set(v, Var(v)).
            // find_written_vars detects the write via the OpAppendVector in the parts loop.
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
                ls.push(self.cl("OpClearStackText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendStackText", &[Value::Var(v), code.clone()]));
            } else if tp == &Type::Character {
                ls.push(self.cl("OpClearText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendCharacter", &[Value::Var(v), code.clone()]));
            } else {
                ls.push(self.cl("OpClearText", &[Value::Var(v)]));
                ls.push(self.cl("OpAppendText", &[Value::Var(v), code.clone()]));
            }
            v
        } else if matches!(self.vars.tp(orig_var), Type::RefVar(_)) {
            ls.push(self.cl("OpAppendStackText", &[Value::Var(orig_var), code.clone()]));
            orig_var
        } else {
            ls.push(self.cl("OpAppendText", &[Value::Var(orig_var), code.clone()]));
            orig_var
        };
        for (val, tp) in parts {
            if matches!(self.vars.tp(var_nr), Type::RefVar(_)) {
                if *tp == Type::Character {
                    ls.push(self.cl("OpAppendStackCharacter", &[Value::Var(var_nr), val.clone()]));
                } else {
                    ls.push(self.cl("OpAppendStackText", &[Value::Var(var_nr), val.clone()]));
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
    //              'true' | 'false' | 'null'
    fn parse_single(&mut self, var_tp: &Type, val: &mut Value, parent_tp: &mut Type) -> Type {
        if self.lexer.has_token("!") {
            let t = self.parse_part(var_tp, val, parent_tp);
            let arg = val.clone();
            self.call_op(val, "Not", &[arg], &[t])
        } else if self.lexer.has_token("-") {
            let t = self.parse_part(var_tp, val, parent_tp);
            let arg = val.clone();
            self.call_op(val, "Min", &[arg], &[t])
        } else if self.lexer.has_token("(") {
            let t = self.expression(val);
            self.lexer.token(")");
            t
        } else if self.lexer.peek_token("{") {
            self.parse_block("block", val, &Type::Unknown(0))
        } else if self.lexer.has_token("[") {
            self.parse_vector(var_tp, val, parent_tp)
        } else if self.lexer.has_token("if") {
            self.parse_if(val)
        } else if self.lexer.has_token("fn") {
            self.parse_fn_ref(val)
        } else if let Some(name) = self.lexer.has_identifier() {
            self.parse_var(val, &name, parent_tp)
        } else if self.lexer.has_token("$") {
            self.parse_var(val, "$", parent_tp)
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

    // <fn-ref> ::= 'fn' <identifier>
    // Produces a Type::Function value whose runtime representation is the
    // definition number (d_nr) of the named function stored as an i32.
    fn parse_fn_ref(&mut self, code: &mut Value) -> Type {
        let Some(name) = self.lexer.has_identifier() else {
            if !self.first_pass {
                diagnostic!(self.lexer, Level::Error, "Expect function name after fn");
            }
            return Type::Unknown(0);
        };
        // Try user function (n_<name>) first, then fall back to bare name.
        let d_nr = {
            let prefixed = format!("n_{name}");
            let nr = self.data.def_nr(&prefixed);
            if nr == u32::MAX {
                self.data.def_nr(&name)
            } else {
                nr
            }
        };
        if d_nr == u32::MAX {
            if !self.first_pass {
                diagnostic!(self.lexer, Level::Error, "Unknown function '{name}'");
            }
            return Type::Unknown(0);
        }
        if !self.first_pass && !matches!(self.data.def_type(d_nr), DefType::Function) {
            diagnostic!(self.lexer, Level::Error, "'{name}' is not a function");
            return Type::Unknown(0);
        }
        *code = Value::Int(d_nr as i32);
        self.data.def_used(d_nr);
        let n_args = self.data.attributes(d_nr);
        let arg_types: Vec<Type> = (0..n_args).map(|a| self.data.attr_type(d_nr, a)).collect();
        let ret_type = self.data.def(d_nr).returned.clone();
        Type::Function(arg_types, Box::new(ret_type))
    }

    // <for-vector> ::= 'for' <id> 'in' <range> ['if' <cond>] '{' <expr> '}'
    // Implements [for n in range { body }] vector comprehensions.
    #[allow(clippy::too_many_arguments)] // parser helper threading IR-construction params alongside &mut self; no sensible grouping reduces the count
    fn parse_vector_for(
        &mut self,
        vec: u16,
        elm: u16,
        in_t: &mut Type,
        val: &mut Value,
        is_var: bool,
        is_field: bool,
        block: bool,
        parent_tp: &Type,
    ) -> Type {
        let Some(id) = self.lexer.has_identifier() else {
            diagnostic!(self.lexer, Level::Error, "Expect variable after for");
            return Type::Null;
        };
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
        let (iter_var, pre_var) = if matches!(in_type, Type::Text(_)) {
            let pos_var = self.create_var(&format!("{id}#next"), &I32);
            self.vars.defined(pos_var);
            let index_var = self.create_var(&format!("{id}#index"), &I32);
            self.vars.defined(index_var);
            (pos_var, Some(index_var))
        } else {
            let iv = self.create_var(&format!("{id}#index"), &I32);
            self.vars.defined(iv);
            (iv, None)
        };
        let for_var = self.create_var(&id, &var_tp);
        self.vars.defined(for_var);
        let if_step = if self.lexer.has_token("if") {
            let mut if_expr = Value::Null;
            self.expression(&mut if_expr);
            if_expr
        } else {
            Value::Null
        };
        let mut create_iter = expr;
        let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
        let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var, pre_var);
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
        // Parse body as an expression-returning block: [for n in range { expr }]
        let mut body = Value::Null;
        let body_type = self.parse_block("for", &mut body, &Type::Unknown(0));
        *in_t = body_type.clone();
        self.in_loop = in_loop;
        self.vars.finish_loop(loop_nr);
        // Finalise vector element type (same as parse_vector post-loop)
        let struct_tp = Type::Vector(Box::new(in_t.clone()), parent_tp.depend());
        if !is_field {
            self.vars
                .change_var_type(vec, &struct_tp, &self.data, &mut self.lexer);
            self.data.vector_def(&mut self.lexer, in_t);
        }
        let tp = Type::Vector(Box::new(in_t.clone()), parent_tp.depend());
        if self.first_pass {
            return tp;
        }
        // Second pass: build the append-in-loop bytecode.
        self.build_comprehension_code(
            vec,
            elm,
            in_t,
            &in_type,
            &var_tp,
            for_var,
            for_next,
            pre_var,
            fill,
            create_iter,
            if_step,
            body,
            val,
            is_var,
            is_field,
            block,
            tp,
        )
    }

    /// Build the second-pass bytecode for a `[for ... { body }]` vector comprehension.
    #[allow(clippy::too_many_arguments)] // parser helper threading IR-construction params alongside &mut self; no sensible grouping reduces the count
    #[allow(clippy::large_types_passed_by_value)] // Option<u16> is Copy; ref adds noise at call sites
    fn build_comprehension_code(
        &mut self,
        vec: u16,
        elm: u16,
        in_t: &Type,
        in_type: &Type,
        var_tp: &Type,
        for_var: u16,
        for_next: Value,
        pre_var: Option<u16>,
        fill: Value,
        create_iter: Value,
        if_step: Value,
        body: Value,
        val: &mut Value,
        is_var: bool,
        is_field: bool,
        block: bool,
        tp: Type,
    ) -> Type {
        // Per-iteration: OpNewRecord / set_field / OpFinishRecord pattern.
        let ed_nr = self.data.type_def_nr(in_t);
        let known = Value::Int(i32::from(
            if ed_nr == u32::MAX || self.data.def(ed_nr).known_type == u16::MAX {
                0
            } else {
                self.database.vector(self.data.def(ed_nr).known_type)
            },
        ));
        let fld = Value::Int(i32::from(u16::MAX));
        let comp_var = self.create_unique("comp", in_t);
        let mut lp = vec![for_next];
        if !matches!(in_type, Type::Iterator(_, _)) {
            let mut test_for = Value::Var(for_var);
            self.convert(&mut test_for, var_tp, &Type::Boolean);
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
        lp.push(v_set(comp_var, body));
        lp.push(v_set(
            elm,
            self.cl(
                "OpNewRecord",
                &[Value::Var(vec), known.clone(), fld.clone()],
            ),
        ));
        lp.push(self.set_field(ed_nr, usize::MAX, 0, Value::Var(elm), Value::Var(comp_var)));
        lp.push(self.cl(
            "OpFinishRecord",
            &[Value::Var(vec), Value::Var(elm), known, fld],
        ));
        let mut for_steps: Vec<Value> = Vec::new();
        if fill != Value::Null {
            for_steps.push(fill);
        }
        if let Some(idx_var) = pre_var {
            for_steps.push(v_set(idx_var, Value::Int(0)));
        }
        for_steps.push(create_iter);
        for_steps.push(v_loop(lp, "For comprehension"));
        let mut ls: Vec<Value> = Vec::new();
        if block {
            ls.extend(self.vector_db(in_t, vec));
        }
        ls.extend(for_steps);
        if self.vector_needs_db(vec, in_t, is_var) {
            let db = self.insert_new(vec, elm, in_t, &mut ls);
            self.vars.depend(vec, db);
        } else if !is_field && !is_var && *val != Value::Null {
            ls.insert(0, v_set(vec, val.clone()));
        }
        if !is_var && !is_field {
            ls.push(Value::Var(vec));
        }
        *val = if block || (!is_var && !is_field) {
            v_block(ls, tp.clone(), "Vector comprehension")
        } else {
            Value::Insert(ls)
        };
        tp
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
            } else if is_field {
                // Empty `[]` on a struct field: the field is already zero-initialized by
                // OpDatabase; there is nothing to emit.  Wrapping the OpGetField result in
                // Value::Insert would leave a dangling 12-byte DbRef on the expression stack.
                *val = Value::Insert(vec![]);
                var_tp.clone()
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
        // Handle [for n in range [if cond] { body }] vector comprehension
        if self.lexer.peek_token("for") {
            self.lexer.has_token("for");
            let tp =
                self.parse_vector_for(vec, elm, &mut in_t, val, is_var, is_field, block, parent_tp);
            self.lexer.token("]");
            return tp;
        }
        if let Some(early) = self.collect_vector_items(elm, &mut in_t, &mut res) {
            return early;
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
        let tp = Type::Vector(Box::new(in_t.clone()), parent_tp.depend());
        let (tp, ls) =
            self.build_vector_list(val, parent_tp, elm, vec, &res, &in_t, tp, is_var, is_field);
        self.lexer.token("]");
        if block {
            *val = v_block(ls, tp.clone(), "Vector");
        } else {
            *val = Value::Insert(ls);
        }
        tp
    }

    /// Parse comma-separated vector items inside `[...]`, returning an early error type on failure.
    fn collect_vector_items(
        &mut self,
        elm: u16,
        in_t: &mut Type,
        res: &mut Vec<Value>,
    ) -> Option<Type> {
        loop {
            if let Some(value) = self.parse_item(elm, in_t, res) {
                return Some(value);
            }
            if self.lexer.has_token(";")
                && let Some(value) = self.parse_multiply(res)
            {
                return Some(value);
            }
            if !self.lexer.has_token(",") {
                break;
            }
            if self.lexer.peek_token("]") {
                break;
            }
        }
        None
    }

    /// Build the instruction list for a parsed vector literal; returns `(tp, ls)`.
    #[allow(clippy::too_many_arguments)] // parser helper threading IR-construction params alongside &mut self; no sensible grouping reduces the count
    fn build_vector_list(
        &mut self,
        val: &mut Value,
        parent_tp: &Type,
        elm: u16,
        vec: u16,
        res: &[Value],
        in_t: &Type,
        mut tp: Type,
        is_var: bool,
        is_field: bool,
    ) -> (Type, Vec<Value>) {
        let mut ls = Vec::new();
        // Only create a fresh database record here when the variable has no existing
        // one (dep is empty).  For `v += [...]` the variable already has a dep from
        // the initial `=` assignment; calling vector_db again would reset v to an
        // empty record and discard the existing elements.  create_vector handles
        // the `=` re-assignment case by calling vector_db unconditionally.
        if self.vars.tp(vec).depend().is_empty() {
            ls.extend(self.vector_db(in_t, vec));
        }
        ls.extend(self.new_record(val, parent_tp, elm, vec, res, in_t));
        if self.vector_needs_db(vec, in_t, is_var) {
            let db = self.insert_new(vec, elm, in_t, &mut ls);
            self.vars.depend(vec, db);
            tp = tp.depending(db);
        } else if !is_field && !is_var && *val != Value::Null {
            ls.insert(0, v_set(vec, val.clone()));
        }
        if !is_var && !is_field {
            ls.push(Value::Var(vec));
            for d in self.vars.tp(vec).depend() {
                tp = tp.depending(d);
            }
        }
        (tp, ls)
    }

    fn vector_needs_db(&self, vec: u16, in_t: &Type, is_var: bool) -> bool {
        is_var
            && *in_t != Type::Void
            && self.vars.tp(vec).depend().is_empty()
            && !matches!(self.vars.tp(vec), Type::RefVar(_))
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
        if let (Type::Reference(t_nr, _), Type::Reference(in_nr, _)) = (&t, &in_t.clone())
            && let (Type::Enum(t_e, true, _), Type::Enum(in_e, true, _)) = (
                &self.data.def(*t_nr).returned,
                &self.data.def(*in_nr).returned,
            )
            && *t_e == *in_e
        {
            *in_t = Type::Enum(*t_e, true, Vec::new());
        } else if !self.convert(&mut p, &t, in_t) {
            // double conversion check: can't become in_t or vice versa
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
            && self.lexer.peek_token("{")
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
            if let Type::Reference(inner_nr, _) = in_t {
                if let Value::Insert(steps) = p {
                    // Inline struct initialization: the steps already write fields into elm.
                    for l in steps {
                        ls.push(l.clone());
                    }
                } else {
                    // Source is a variable, field access, or function call — the struct bytes
                    // must be explicitly copied into the new element slot.
                    let type_nr = if self.first_pass {
                        Value::Int(i32::from(u16::MAX))
                    } else {
                        Value::Int(i32::from(self.data.def(*inner_nr).known_type))
                    };
                    ls.push(self.cl("OpCopyRecord", &[p.clone(), Value::Var(elm), type_nr]));
                }
            } else if let Value::Insert(steps) = p {
                for l in steps {
                    ls.push(l.clone());
                }
            } else {
                ls.push(self.set_field(ed_nr, usize::MAX, 0, Value::Var(elm), p.clone()));
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
        if self.first_pass || vec == u16::MAX || self.vars.is_argument(vec) {
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
            ls.push(self.set_field(vec_def, 0, 0, Value::Var(db), Value::Int(0)));
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
        ls.insert(
            2,
            self.set_field(vec_def, 0, 0, Value::Var(db), Value::Int(0)),
        );
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
            Type::Integer(min, _) => match in_t.size(false) {
                1 if *min == 0 => self.database.name("byte"),
                1 => self.database.name(&format!("byte<{min},false>")),
                2 => self.database.name(&format!("short<{min},false>")),
                _ => self.database.name("integer"),
            },
            Type::Character => self.database.name("integer"),
            Type::Long => self.database.name("long"),
            Type::Float => self.database.name("float"),
            Type::Single => self.database.name("single"),
            Type::Text(_) => self.database.name("text"),
            Type::Reference(r, _) | Type::Enum(r, _, _) => self.data.def(*r).known_type,
            Type::Hash(tp, key, _) => {
                let mut name = "hash<".to_string() + &self.data.def(*tp).name + "[";
                self.database
                    .field_name(self.data.def(*tp).known_type, key, &mut name);
                self.database.name(&name)
            }
            Type::Sorted(tp, key, _) => {
                let mut name = "sorted<".to_string() + &self.data.def(*tp).name + "[";
                field_id(key, &mut name);
                let r = self.database.name(&name);
                if r == u16::MAX {
                    name = "ordered<".to_string() + &self.data.def(*tp).name + "[";
                    field_id(key, &mut name);
                }
                self.database.name(&name)
            }
            Type::Index(tp, key, _) => {
                let mut name = "index<".to_string() + &self.data.def(*tp).name + "[";
                field_id(key, &mut name);
                let r = self.database.name(&name);
                if r == u16::MAX {
                    name = "index<".to_string() + &self.data.def(*tp).name + "[";
                    field_id(key, &mut name);
                }
                self.database.name(&name)
            }
            Type::Vector(tp, _) => {
                let elem_tp = self.get_type(tp);
                let vec_name = if elem_tp == u16::MAX {
                    "vector".to_string()
                } else {
                    format!("vector<{}>", self.database.types[elem_tp as usize].name)
                };
                self.database.name(&vec_name)
            }
            _ => u16::MAX,
        }
    }

    // <children> ::=
    fn field(&mut self, code: &mut Value, tp: Type) -> Type {
        if let Type::Unknown(_) = tp {
            diagnostic!(self.lexer, Level::Error, "Field of unknown variable");
            return tp;
        }
        let mut t = tp;
        let Some(field) = self.lexer.has_identifier() else {
            diagnostic!(self.lexer, Level::Error, "Expect a field name");
            return t;
        };
        let enr = self.data.type_elm(&t);
        if enr == u32::MAX {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Unknown type {}",
                t.show(&self.data, &self.vars)
            );
            return Type::Unknown(0);
        }
        let e_size = i32::from(self.database.size(self.data.def(enr).known_type));
        if let Type::RefVar(tp) = t {
            t = *tp;
        }
        let dnr = self.data.type_def_nr(&t);
        if matches!(t, Type::Vector(_, _)) && self.vector_operations(code, &field, e_size) {
            return Type::Void;
        }
        let fnr = self.data.attr(dnr, &field);
        if fnr == usize::MAX {
            if self.first_pass && self.lexer.has_token("(") {
                self.skip_remaining_args();
            } else if !self.first_pass {
                // For polymorphic enums, this field may be in a struct (not the enum itself).
                if let Type::Enum(enum_d_nr, true, _) = &t
                    && let Some((found_d_nr, found_fnr)) =
                        self.find_poly_enum_field(*enum_d_nr, &field)
                {
                    let dep = t.depend();
                    t = self.data.attr_type(found_d_nr, found_fnr);
                    for on in dep {
                        t = t.depending(on);
                    }
                    if let Value::Var(nr) = code {
                        t = t.depending(*nr);
                    }
                    *code = self.get_field(found_d_nr, found_fnr, code.clone());
                    self.data.attr_used(found_d_nr, found_fnr);
                    return t;
                }
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown field {}.{field}",
                    self.data.def(dnr).name
                );
                // Consume a trailing `(…)` to avoid cascading parse errors.
                if self.lexer.has_token("(") {
                    self.skip_remaining_args();
                }
            }
            return Type::Unknown(0);
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
        } else if self.data.def(dnr).attributes[fnr].constant {
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
            t = self.data.attr_type(dnr, fnr);
            for on in dep {
                t = t.depending(on);
            }
            if let Value::Var(nr) = code {
                t = t.depending(*nr);
            }
            *code = self.get_field(dnr, fnr, code.clone());
        }
        self.data.attr_used(dnr, fnr);
        t
    }

    /// Consume remaining function call arguments after `(` has already been consumed.
    fn skip_remaining_args(&mut self) {
        loop {
            if self.lexer.peek_token(")") {
                break;
            }
            let mut p = Value::Null;
            self.expression(&mut p);
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token(")");
    }

    /// Search for `field` in the variant structs of a polymorphic enum.
    /// Returns `(variant_d_nr, attr_nr)` if found.
    fn find_poly_enum_field(&self, enum_d_nr: u32, field: &str) -> Option<(u32, usize)> {
        for a_nr in 0..self.data.attributes(enum_d_nr) {
            let a_name = self.data.attr_name(enum_d_nr, a_nr);
            let variant_d_nr = self.data.def_nr(&a_name);
            if variant_d_nr == u32::MAX {
                continue;
            }
            if !matches!(self.data.def_type(variant_d_nr), DefType::EnumValue) {
                continue;
            }
            let f = self.data.attr(variant_d_nr, field);
            if f != usize::MAX {
                return Some((variant_d_nr, f));
            }
        }
        None
    }

    fn vector_operations(&mut self, code: &mut Value, field: &str, e_size: i32) -> bool {
        if field == "remove" {
            self.lexer.token("(");
            let (tps, ls) = self.parse_parameters();
            let mut cd = ls[0].clone();
            // validate types
            if tps.len() != 1 || !self.convert(&mut cd, &tps[0], &I32) {
                diagnostic!(self.lexer, Level::Error, "Invalid index in remove");
            }
            *code = self.cl("OpRemoveVector", &[code.clone(), Value::Int(e_size), cd]);
            true
        } else {
            false
        }
    }

    fn parse_index(&mut self, code: &mut Value, tp: &Type) -> Type {
        let mut t = tp.clone();
        let mut p = Value::Null;
        self.un_ref(&mut t, &mut p);
        let mut elm_type = self.index_type(&t);
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
        if let Type::Vector(etp, _) = &t {
            if let Some(value) = self.parse_vector_index(code, &elm_type, etp) {
                return value;
            }
        } else if matches!(t, Type::Text(_)) {
            let index_t = if self.lexer.peek_token("..") {
                p = Value::Int(0);
                I32.clone()
            } else {
                self.expression(&mut p)
            };
            if self.parse_text_index(code, &mut p, &index_t) == Type::Character {
                elm_type = Type::Character;
            }
        } else if let Type::Hash(el, keys, _) | Type::Spacial(el, keys, _) = &t {
            let mut key_types = Vec::new();
            for k in keys {
                key_types.push(self.data.attr_type(*el, self.data.attr(*el, k)).clone());
            }
            self.parse_key(code, &t, &key_types);
        } else if let Type::Sorted(el, keys, _) | Type::Index(el, keys, _) = &t {
            let mut key_types = Vec::new();
            for (k, _) in keys {
                key_types.push(self.data.attr_type(*el, self.data.attr(*el, k)).clone());
            }
            self.parse_key(code, &t, &key_types);
        } else {
            // index_type() already emitted a diagnostic; consume the inner expression
            // so that the caller can still parse the closing `]` without cascading errors.
            let mut p = Value::Null;
            self.expression(&mut p);
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
        if let Value::Iter(var, init, next, extra_init) = p {
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
                    extra_init,
                );
                return Some(Type::Iterator(
                    Box::new(elm_type.clone()),
                    Box::new(Type::Null),
                ));
            }
            unreachable!("Value::Iter with non-Block next field");
        }
        if !self.first_pass && !self.convert(&mut p, &index_t, &I32) {
            diagnostic!(
                self.lexer,
                Level::Error,
                "Invalid index type {} on vector",
                index_t.show(&self.data, &self.vars)
            );
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
                Box::new(Value::Null),
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
            Parts::Hash(_, _) => {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Cannot iterate a hash directly — a hash has no stable element order, \
so #index and #remove are not supported; \
pair the hash with a vector to iterate in insertion order"
                );
                return;
            }
            _ => {
                diagnostic!(self.lexer, Level::Error, "Cannot iterate");
                return;
            }
        }
        if inclusive {
            on += 128;
        }
        if self.reverse_iterator {
            on += 64;
            // Do not reset here — `iterator()` calls fill_iter twice and resets after both.
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
    fn parse_var(&mut self, code: &mut Value, name: &str, parent_tp: &mut Type) -> Type {
        // '$' refers to the current record in struct field default expressions
        if name == "$" && matches!(self.data.def_type(self.context), DefType::Struct) {
            *code = Value::Var(0);
            return Type::Reference(self.context, Vec::new());
        }
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
            } else if name == "typedef" {
                let mut p = Value::Null;
                let et = self.expression(&mut p);
                self.lexer.token(")");
                let tp = self.data.def(self.data.type_def_nr(&et)).known_type;
                t = Type::Integer(0, 65536);
                *code = Value::Int(i32::from(tp));
            } else {
                t = self.parse_call(code, source, &nm);
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
                    // Don't create OpCopyRecord here: generate_set handles the copy when
                    // value=Var(src). Using Var(v_nr) directly lets method calls like
                    // `d = c.double()` pass c as `self` without the broken CopyRecord-as-self
                    // pattern that was causing garbage store_nr crashes (Issue 1).
                    let d_nr = *d_nr;
                    let into_var = *into;
                    self.vars.make_independent(into_var, v_nr);
                    *code = Value::Var(v_nr);
                    return Type::Reference(d_nr, Vec::new());
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
        } else if matches!(self.data.def_type(self.context), DefType::Struct)
            && self.data.attr(self.context, name) != usize::MAX
        {
            let fnr = self.data.attr(self.context, name);
            *code = self.get_field(self.context, fnr, Value::Var(0));
            t = self.data.attr_type(self.context, fnr);
        } else if let Type::Enum(enr, _, _) = parent_tp
            && let Some(a_nr) = self.data.def(*enr).attr_names.get(name)
        {
            *code = self.data.attr_value(*enr, *a_nr);
            t = parent_tp.clone();
        } else {
            *code = Value::Var(self.create_var(name, &Type::Unknown(0)));
            t = Type::Unknown(0);
        }
        t
    }

    fn iter_op(&mut self, code: &mut Value, name: &str, t: &mut Type, index_var: u16) {
        // File variables handle their own # operations before iterator operations.
        if self.is_file_var(index_var) {
            self.file_op(code, t, index_var);
            return;
        }
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
        } else if self.lexer.has_keyword("next") {
            let n_name = format!("{name}#next");
            if self.vars.name_exists(&n_name) {
                let v = self.vars.var(&n_name);
                *t = self.vars.tp(v).clone();
                *code = Value::Var(v);
            } else {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Incorrect #next variable on {} (only valid in text loops)",
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
        } else if self.lexer.has_keyword("lock") {
            // d#lock — read the lock state of the store containing a reference or vector variable.
            // Assignment d#lock = true/false is resolved in towards_set.
            if !self.first_pass
                && !matches!(
                    self.vars.tp(index_var),
                    Type::Reference(_, _) | Type::Vector(_, _)
                )
            {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "#lock is only valid on reference or vector variables, not on '{}'",
                    name
                );
                *t = Type::Unknown(0);
            } else {
                *code = self.cl("n_get_store_lock", &[Value::Var(index_var)]);
                *t = Type::Boolean;
            }
        } else {
            diagnostic!(self.lexer, Level::Error, "Incorrect # variable on {}", name);
            *t = Type::Unknown(0);
        }
    }

    fn is_file_var(&self, var_nr: u16) -> bool {
        let file_def = self.data.def_nr("File");
        matches!(self.vars.tp(var_nr), Type::Reference(d, _) if *d == file_def)
    }

    fn file_op(&mut self, code: &mut Value, t: &mut Type, var_nr: u16) {
        self.vars.in_use(var_nr, true);
        if self.lexer.has_keyword("format") {
            let file_ref = Value::Var(var_nr);
            *code = self.cl("OpGetEnum", &[file_ref, Value::Int(32)]);
            let fmt_def = self.data.def_nr("Format");
            *t = Type::Enum(fmt_def, false, Vec::new());
        } else if self.lexer.has_keyword("size") {
            *code = self.cl("OpSizeFile", &[Value::Var(var_nr)]);
            *t = Type::Long;
        } else if self.lexer.has_keyword("index") {
            // Read the current field at offset 8
            *code = self.cl("OpGetLong", &[Value::Var(var_nr), Value::Int(8)]);
            *t = Type::Long;
        } else if self.lexer.has_keyword("next") {
            // Read the next field at offset 16
            *code = self.cl("OpGetLong", &[Value::Var(var_nr), Value::Int(16)]);
            *t = Type::Long;
        } else if self.lexer.has_keyword("read") {
            self.lexer.token("(");
            let mut n_code = Value::Null;
            self.expression(&mut n_code);
            self.lexer.token(")");
            // Determine read type from optional "as T"
            let (read_type, db_tp) = if self.lexer.has_token("as") {
                if let Some(type_name) = self.lexer.has_identifier() {
                    let tp = self
                        .parse_type(u32::MAX, &type_name, false)
                        .unwrap_or(Type::Text(vec![]));
                    self.ensure_io_type(&tp.clone());
                    let id = self.get_type(&tp);
                    (tp, id)
                } else {
                    let text_tp = Type::Text(vec![]);
                    let id = self.get_type(&text_tp);
                    (text_tp, id)
                }
            } else {
                let text_tp = Type::Text(vec![]);
                let id = self.get_type(&text_tp);
                (text_tp, id)
            };
            let mut ls = Vec::new();
            let temp_var = if let Type::Text(_) = read_type {
                self.vars.work_text(&mut self.lexer)
            } else {
                let t = self.vars.unique("read", &read_type, &mut self.lexer);
                ls.push(v_set(t, self.null(&read_type)));
                t
            };
            let var_ref = self.cl("OpCreateStack", &[Value::Var(temp_var)]);
            ls.push(self.cl(
                "OpReadFile",
                &[
                    Value::Var(var_nr),
                    var_ref,
                    n_code,
                    Value::Int(i32::from(db_tp)),
                ],
            ));
            ls.push(Value::Var(temp_var));
            *code = v_block(ls, read_type.clone(), "reading file");
            *t = read_type;
        } else {
            if !self.first_pass {
                diagnostic!(self.lexer, Level::Error, "Unknown # operation on File");
            }
            *t = Type::Unknown(0);
        }
    }

    fn is_file_var_type(&self, tp: &Type) -> bool {
        let file_def = self.data.def_nr("File");
        matches!(tp, Type::Reference(d, _) if *d == file_def)
    }

    /// Ensure byte/short integer types used in file I/O are registered in the database.
    fn ensure_io_type(&mut self, t: &Type) {
        match t {
            Type::Integer(min, _) => match t.size(false) {
                1 => {
                    self.database.byte(*min, false);
                }
                2 => {
                    self.database.short(*min, false);
                }
                _ => {}
            },
            Type::Vector(tp, _) => {
                let tp = tp.clone();
                self.ensure_io_type(&tp);
            }
            _ => {}
        }
    }

    fn write_to_file(&mut self, file_var: u16, val: Value, val_type: &Type) -> Value {
        let val_type_clone = val_type.clone();
        self.ensure_io_type(&val_type_clone);
        let db_tp = self.get_type(val_type);
        let temp_var = self.vars.unique("wf", val_type, &mut self.lexer);
        for d in val_type.depend() {
            self.vars.depend(temp_var, d);
        }
        let assign = v_set(temp_var, val);
        let var_ref = self.cl("OpCreateStack", &[Value::Var(temp_var)]);
        let write = self.cl(
            "OpWriteFile",
            &[Value::Var(file_var), var_ref, Value::Int(i32::from(db_tp))],
        );
        Value::Insert(vec![assign, write])
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
                DefType::Struct | DefType::EnumValue
            ) && !matches!(self.data.def(d_nr).returned, Type::Enum(_, false, _))
                && self.lexer.peek_token("{")
            {
                let tp = self.parse_object(d_nr, code);
                if tp != Type::Unknown(0) {
                    return tp;
                }
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
            if !self.vars.exists(*nr) {
                return;
            }
            if self.default && matches!(self.vars.tp(*nr), Type::Vector(_, _)) {
                return;
            }
            if !self.first_pass && (self.vars.tp(*nr).is_unknown() || !self.vars.is_defined(*nr)) {
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
            let mut tp = if self.lexer.has_token("for") {
                self.iter_for(&mut format, &mut append_value)
            } else {
                self.expression(&mut format)
            };
            self.un_ref(&mut tp, &mut format);
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
                        "OpAppendStackText"
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
            if id.to_lowercase() == "j" || id.to_lowercase() == "json" {
                -1
            } else if id == "x" || id == "X" {
                16
            } else if id == "b" {
                2
            } else if id == "o" {
                8
            } else if id == "e" {
                1
            } else if id == "d" || id == "f" {
                10
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
            // Create {id}#index first (always needed, regardless of type).
            let index_var = self.create_var(&format!("{id}#index"), &I32);
            self.vars.defined(index_var);
            self.lexer.token("in");
            let loop_nr = self.vars.start_loop();
            let mut expr = Value::Null;
            let in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            // For text loops: {id}#next drives the loop; {id}#index is saved per-iteration.
            let (iter_var, pre_var) = if matches!(in_type, Type::Text(_)) {
                let pos_var = self.create_var(&format!("{id}#next"), &I32);
                self.vars.defined(pos_var);
                (pos_var, Some(index_var))
            } else {
                (index_var, None)
            };
            let var_tp = self.for_type(&in_type);
            *append_value = self.create_unique("val", &Type::Unknown(0));
            let for_var = self.create_var(&id, &var_tp);
            self.vars.defined(for_var);
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            let mut create_iter = expr;
            let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var, pre_var);
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
            // For text loops, extra_init holds v_set(index_var, 0) which must be emitted at
            // the same scope level as the iterator init (outside the loop) so the slot
            // assigner sees {id}#index as live across the entire loop body.
            let extra_init = if let Some(idx_var) = pre_var {
                Box::new(v_set(idx_var, Value::Int(0)))
            } else {
                Box::new(Value::Null)
            };
            *val = Value::Iter(
                for_var,
                Box::new(create_iter),
                Box::new(v_block(lp, result_tp, "Iter For")),
                extra_init,
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
        let in_type = if self.lexer.peek_token("..") || self.lexer.peek_token("..=") {
            // Open-start range: treat missing start as 0.
            *expr = Value::Int(0);
            I32.clone()
        } else {
            self.expression(expr)
        };
        if !self.lexer.has_token("..") {
            if reverse {
                // rev() wrapping a collection (not a range): set the reverse-iterator flag so
                // that fill_iter adds bit 64 into the OpIterate/OpStep `on` byte.
                if matches!(in_type, Type::Sorted(_, _, _) | Type::Index(_, _, _)) {
                    self.reverse_iterator = true;
                } else if !matches!(in_type, Type::Null) {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "rev() on a non-range expression must wrap a sorted or index collection"
                    );
                }
                self.lexer.token(")");
            }
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
                if incl { "<" } else { "<=" },
                till,
                Value::Var(ivar),
                till_tp,
                in_type.clone(),
            )
        };
        ls.push(v_if(test, Value::Break(0), Value::Null));
        ls.push(Value::Var(ivar));
        *expr = Value::Iter(
            u16::MAX,
            Box::new(v_set(ivar, self.null(&in_type))),
            Box::new(v_block(ls, in_type.clone(), "Iter range")),
            Box::new(Value::Null),
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
            "OpFormatStack"
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
                        Value::Int(state.db_format()),
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
                        Value::Int(state.db_format()),
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
                            Value::Int(state.db_format()),
                        ],
                    ));
                }
            }
            _ => {
                if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Cannot format type {}",
                        tp.name(&self.data)
                    );
                }
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
        if let Value::Iter(var, init, next, extra_init) = value
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
            if !matches!(**extra_init, Value::Null) {
                list.push(*extra_init.clone());
            }
            list.push(v_set(count, Value::Int(0)));
            let mut append_var = append_value;
            if append_value == u16::MAX {
                append_var = self.create_unique("val", var_type);
            }
            let mut steps = Vec::new();
            steps.push(v_set(append_var, *next.clone()));
            steps.push(v_if(
                self.cl("OpLtInt", &[Value::Int(0), Value::Var(count)]),
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
        if !self.lexer.has_token("{") {
            self.lexer.revert(link);
            return Type::Unknown(0);
        }
        let mut list = Vec::new();
        let mut new_object = false;
        let work = self.vars.work_ref();
        if let Value::Var(v_nr) = code {
            let var_tp = self.vars.tp(*v_nr).clone();
            let type_matches =
                var_tp.is_unknown() || matches!(&var_tp, Type::Reference(d, _) if *d == td_nr);
            if self.vars.is_independent(*v_nr) && type_matches {
                if !self.vars.is_argument(*v_nr) {
                    list.push(v_set(*v_nr, Value::Null));
                }
                self.data.set_referenced(td_nr, self.context, Value::Null);
                let tp = i32::from(self.data.def(td_nr).known_type);
                list.push(self.cl("OpDatabase", &[Value::Var(*v_nr), Value::Int(tp)]));
            } else if !type_matches && !self.first_pass {
                // LHS variable already has an incompatible type (e.g. integer from a prior
                // pass). Fall through to new_object so the struct gets a fresh work ref and
                // the result is a proper Value::Block — not a Value::Insert — which can be
                // used safely as a method-call argument.
                new_object = true;
                self.data.set_referenced(td_nr, self.context, Value::Null);
                let ret = &self.data.def(td_nr).returned;
                let w = self.vars.work_refs(ret, &mut self.lexer);
                let tp = i32::from(self.data.def(td_nr).known_type);
                list.push(v_set(w, Value::Null));
                list.push(self.cl("OpDatabase", &[Value::Var(w), Value::Int(tp)]));
                *code = Value::Var(w);
            }
        } else if !self.first_pass && !self.is_field(code) {
            new_object = true;
            self.data.set_referenced(td_nr, self.context, Value::Null);
            let ret = &self.data.def(td_nr).returned;
            let w = self.vars.work_refs(ret, &mut self.lexer);
            let tp = i32::from(self.data.def(td_nr).known_type);
            list.push(v_set(w, Value::Null));
            list.push(self.cl("OpDatabase", &[Value::Var(w), Value::Int(tp)]));
            *code = Value::Var(w);
        }
        let mut found_fields = HashSet::new();
        loop {
            if self.lexer.peek_token("}") {
                break;
            }
            if let Some(field) = self.lexer.has_identifier() {
                if !self.lexer.has_token(":") {
                    self.lexer.revert(link);
                    self.vars.clean_work_refs(work);
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
                    let td = self.data.attr_type(td_nr, nr);
                    let pos = self
                        .database
                        .position(self.data.def(td_nr).known_type, &field);
                    found_fields.insert(field.clone());
                    let mut value = if let Type::Vector(_, _)
                    | Type::Sorted(_, _, _)
                    | Type::Hash(_, _, _)
                    | Type::Spacial(_, _, _)
                    | Type::Enum(_, true, _)
                    | Type::Index(_, _, _) = td
                    {
                        list.push(self.cl(
                            "OpSetInt",
                            &[code.clone(), Value::Int(i32::from(pos)), Value::Int(0)],
                        ));
                        self.cl(
                            "OpGetField",
                            &[
                                code.clone(),
                                Value::Int(i32::from(pos)),
                                self.type_info(&td),
                            ],
                        )
                    } else {
                        Value::Null
                    };
                    let mut parent_tp = Type::Reference(td_nr, Vec::new());
                    if let Value::Var(v) = code {
                        parent_tp = parent_tp.depending(*v);
                    }
                    let exp_tp = self.parse_operators(&td, &mut value, &mut parent_tp, 0);
                    self.handle_field(td_nr, code, &mut list, &field, &mut value, &exp_tp);
                }
            } else {
                // We have not encountered an identifier
                self.lexer.revert(link);
                self.vars.clean_work_refs(work);
                return Type::Unknown(0);
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token("}");
        if !self.first_pass {
            self.object_init(&mut list, td_nr, 0, code, &found_fields);
        }
        if new_object && let Value::Var(v) = code {
            list.push(Value::Var(*v));
            *code = v_block(list, Type::Reference(td_nr, vec![*v]), "Object");
            Type::Reference(td_nr, Vec::new())
        } else {
            *code = Value::Insert(list);
            Type::Rewritten(Box::new(Type::Reference(td_nr, Vec::new())))
        }
    }

    /// Recursively replace `Value::Var(0)` (the record placeholder used in field default
    /// expressions) with the actual record reference from the calling context.
    fn replace_record_ref(val: Value, record: &Value) -> Value {
        match val {
            Value::Var(0) => record.clone(),
            Value::Call(nr, args) => Value::Call(
                nr,
                args.into_iter()
                    .map(|a| Self::replace_record_ref(a, record))
                    .collect(),
            ),
            Value::If(cond, t, f) => Value::If(
                Box::new(Self::replace_record_ref(*cond, record)),
                Box::new(Self::replace_record_ref(*t, record)),
                Box::new(Self::replace_record_ref(*f, record)),
            ),
            Value::Block(bl) => Value::Block(Box::new(crate::data::Block {
                name: bl.name,
                operators: bl
                    .operators
                    .into_iter()
                    .map(|v| Self::replace_record_ref(v, record))
                    .collect(),
                result: bl.result,
                scope: bl.scope,
            })),
            other => other,
        }
    }

    // fill the not mentioned fields with their default value
    fn object_init(
        &mut self,
        list: &mut Vec<Value>,
        td_nr: u32,
        pos: u16,
        code: &Value,
        found_fields: &HashSet<String>,
    ) {
        for aid in 0..self.data.attributes(td_nr) {
            let tp = self.data.attr_type(td_nr, aid);
            let nm = self.data.attr_name(td_nr, aid);
            let fld = self.database.position(self.data.def(td_nr).known_type, &nm);
            if found_fields.contains(&nm) || matches!(tp, Type::Routine(_)) {
                continue;
            }
            let mut default = self.data.attr_value(td_nr, aid);
            if let Type::Reference(tp, _) = tp
                && default == Value::Null
            {
                self.object_init(list, tp, pos + fld, code, &HashSet::new());
                continue;
            } else if default == Value::Null {
                default = to_default(&tp, &self.data);
            } else {
                default = Self::replace_record_ref(default, code);
            }
            list.push(self.set_field(td_nr, aid, pos, code.clone(), default));
        }
    }

    fn handle_field(
        &mut self,
        td_nr: u32,
        code: &mut Value,
        list: &mut Vec<Value>,
        field: &str,
        value: &mut Value,
        exp_tp: &Type,
    ) {
        let nr = self.data.attr(td_nr, field);
        let td = self.data.attr_type(td_nr, nr);
        if matches!(
            td,
            Type::Vector(_, _)
                | Type::Sorted(_, _, _)
                | Type::Hash(_, _, _)
                | Type::Spacial(_, _, _)
                | Type::Index(_, _, _)
        ) {
            list.push(value.clone());
        } else if let Value::Insert(ops) = value {
            for o in ops {
                list.push(o.clone());
            }
        } else {
            if !self.convert(value, exp_tp, &td) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Cannot write {} on field {}.{field}:{}",
                    td.show(&self.data, &self.vars),
                    self.data.def(td_nr).name,
                    exp_tp.show(&self.data, &self.vars)
                );
            }
            list.push(self.set_field(td_nr, nr, 0, code.clone(), value.clone()));
        }
    }

    fn parse_enum_field(
        &mut self,
        list: &mut Vec<Value>,
        into: Value,
        d_nr: u32,
        pos: u16,
        enum_nr: u8,
    ) {
        let e_nr = self
            .data
            .def_nr(&self.data.def(d_nr).attributes[enum_nr as usize - 1].name);
        let tp = self.data.def(e_nr).returned.clone();
        let v = self.create_unique("enum", &tp);
        let mut cd = if pos != 0 {
            list.push(v_set(
                v,
                self.cl("OpGetField", &[into, Value::Int(i32::from(pos))]),
            ));
            Value::Var(v)
        } else {
            into.clone()
        };
        self.parse_object(e_nr, &mut cd);
        if let Value::Insert(ls) = &cd {
            for l in ls {
                list.push(l.clone());
            }
        }
    }

    // <if> ::= <expression> '{' <block> [ 'else' ( 'if' <if> | '{' <block> ) ]
    fn parse_if(&mut self, code: &mut Value) -> Type {
        let mut test = Value::Null;
        let tp = self.expression(&mut test);
        self.convert(&mut test, &tp, &Type::Boolean);
        let mut true_code = Value::Null;
        let mut true_type = self.parse_block("if", &mut true_code, &Type::Unknown(0));
        let mut false_type = Type::Void;
        let mut false_code = Value::Null;
        if self.lexer.has_token("else") {
            if self.lexer.has_token("if") {
                self.parse_if(&mut false_code);
            } else {
                if true_type == Type::Null {
                    true_type = Type::Unknown(0);
                }
                false_type = self.parse_block("else", &mut false_code, &true_type);
                if true_type == Type::Unknown(0) {
                    if let Value::Block(bl) = &mut true_code {
                        let p = bl.operators.len() - 1;
                        bl.operators[p] = self.null(&false_type);
                        bl.result = false_type.clone();
                    }
                    true_type = false_type.clone();
                }
            }
        } else if true_type != Type::Void {
            false_code = v_block(vec![self.null(&true_type)], true_type.clone(), "else");
        }
        *code = v_if(test, true_code, false_code);
        merge_dependencies(&true_type, &false_type)
    }

    // <for> ::= <identifier> 'in' <expression> [ 'par' '(' <id> '=' <worker> ',' <threads> ')' ] '{' <block>
    //
    // The optional parallel clause `par(b=worker(a), N)` desugars to a parallel map
    // followed by an index-based loop over the results.  Three worker call forms
    // are supported — see `parse_parallel_for_loop` for details.
    fn parse_for(&mut self, code: &mut Value) {
        if let Some(id) = self.lexer.has_identifier() {
            self.lexer.token("in");
            let loop_nr = self.vars.start_loop();
            let mut expr = Value::Null;
            let mut in_type = self.parse_in_range(&mut expr, &Value::Null, &id);
            let mut fill = Value::Null;
            // For vector loops, the iterator runs on a unique temp copy so that the loop
            // variable does not alias the user-visible collection.  Record the original
            // variable number so that mutation of the original can be detected later.
            let orig_coll_var = if let Value::Var(v) = &expr {
                *v
            } else {
                u16::MAX
            };
            // Save the original collection expression before the vector temp-copy substitution
            // so that is_iterated_value() can match field-access patterns like `db.items`.
            let orig_coll_expr = expr.clone();
            if matches!(in_type, Type::Vector(_, _)) {
                let vec_var = self.create_unique("vector", &in_type);
                in_type = in_type.depending(vec_var);
                fill = v_set(vec_var, expr);
                expr = Value::Var(vec_var);
            }
            // Optional parallel clause: par(result=worker(elem), threads)
            if let LexItem::Identifier(kw) = &self.lexer.peek().has
                && kw == "par"
            {
                self.lexer.has_identifier(); // consume "par"
                self.parse_parallel_for_loop(code, &id, &in_type, expr, fill, loop_nr);
                return;
            }
            let var_tp = self.for_type(&in_type);
            // For text loops: {id}#next drives the loop; {id}#index is saved per-iteration.
            let (iter_var, pre_var) = if matches!(in_type, Type::Text(_)) {
                let pos_var = self.create_var(&format!("{id}#next"), &I32);
                self.vars.defined(pos_var);
                let index_var = self.create_var(&format!("{id}#index"), &I32);
                self.vars.defined(index_var);
                (pos_var, Some(index_var))
            } else {
                let iv = self.create_var(&format!("{id}#index"), &I32);
                self.vars.defined(iv);
                (iv, None)
            };
            let for_var = self.create_var(&id, &var_tp);
            self.vars.defined(for_var);
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
            let mut create_iter = expr;
            let it = Type::Iterator(Box::new(var_tp.clone()), Box::new(Type::Null));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it, iter_var, pre_var);
            // For vector loops: set_loop stores the temp-copy var; override with the
            // original so that `orig += elem` is correctly identified as a mutation.
            if matches!(in_type, Type::Vector(_, _)) {
                if orig_coll_var != u16::MAX {
                    self.vars.set_coll_var(orig_coll_var);
                }
                // Always restore the original collection expression so that
                // is_iterated_value() can match field-access forms like `db.items`.
                self.vars.set_coll_value(orig_coll_expr);
            }
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
            // For text loops, initialise {id}#index at the FOR block scope so its live
            // interval covers the entire loop (not just the inner "for text next" block).
            if let Some(idx_var) = pre_var {
                for_steps.push(v_set(idx_var, Value::Int(0)));
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

    // Desugar a parallel for loop:
    //   for a in <vec> par(b=worker(a), N) { body }
    // into an index-based loop over the parallel_for result.
    //
    // Supported worker call forms:
    //   Form 1: func(a)         — global/user function; a is the element variable
    //   Form 2: a.method()      — method on the element type; a is the loop variable
    //   Form 3: c.method(a)     — NOT YET SUPPORTED (captured receiver + element arg)
    //
    // Limitations:
    //   • Input must be a vector<T>; integer ranges (1..10) are not supported.
    //   • The worker must return a primitive type: integer, long, float, or boolean.
    //     text and reference return types require store-merging (deferred).
    //   • Form 3 (captured receiver) requires IR-level wrapper synthesis (deferred).
    //   • The element type T must be a struct (reference) or enum for form 2.
    //
    // The desugared IR:
    //   par_len#N   = len(input_vec)
    //   par_results#N = parallel_for(input_vec, elem_size, return_size, threads, fn_d_nr)
    //   b#index     = 0
    //   loop {
    //     if par_len#N <= b#index { break }
    //     b = parallel_get_T(par_results#N, b#index)
    //     <body>
    //     b#index += 1
    //   }
    fn parse_parallel_for_loop(
        &mut self,
        code: &mut Value,
        elem_var: &str,
        in_type: &Type,
        vec_expr: Value,
        fill: Value,
        loop_nr: u16,
    ) {
        // Consume opening '('.
        self.lexer.token("(");

        // Validate: parallel syntax requires a vector input.
        let elem_tp = if let Type::Vector(_, _) = in_type {
            self.for_type(in_type)
        } else {
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "par(...) requires a vector<T> input, not {}",
                    in_type.name(&self.data)
                );
            }
            self.skip_to_parallel_body();
            self.vars.finish_loop(loop_nr);
            return;
        };

        // Parse: result_name = worker_call , threads )
        let Some(result_name) = self.lexer.has_identifier() else {
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect result variable name after 'par('"
                );
            }
            self.skip_to_parallel_body();
            self.vars.finish_loop(loop_nr);
            return;
        };
        if !self.lexer.has_token("=") {
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect '=' after result name '{}' in par(...)",
                    result_name
                );
            }
            self.skip_to_parallel_body();
            self.vars.finish_loop(loop_nr);
            return;
        }

        // Create the element variable so the worker call expression can resolve it.
        // (e.g. `calc(a)` needs `a` in scope during parsing even though the body
        // never runs `a` directly — the parallel map handles that.)
        let elem_var_nr = self.create_var(elem_var, &elem_tp);
        self.vars.defined(elem_var_nr);
        if matches!(elem_tp, Type::Integer(_, _)) {
            self.vars.in_use(elem_var_nr, true);
        }

        // Resolve worker function: consumes the worker call tokens up to the ','.
        let (fn_d_nr, ret_type) = self.parse_parallel_worker(elem_var, &elem_tp);

        // Comma separating worker from thread count.
        self.lexer.token(",");
        let mut threads_expr = Value::Null;
        self.expression(&mut threads_expr);
        // Closing ')'.
        self.lexer.token(")");

        // Map return type to sizes and get function names.
        let (return_size, get_fn_name): (i32, &str) = match &ret_type {
            Type::Integer(_, _) | Type::Character => (4, "n_parallel_get_int"),
            Type::Long => (8, "n_parallel_get_long"),
            Type::Float => (8, "n_parallel_get_float"),
            Type::Boolean => (1, "n_parallel_get_bool"),
            _ => {
                if !self.first_pass && fn_d_nr != u32::MAX {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Parallel worker return type '{}' must be integer, long, float, or boolean",
                        ret_type.name(&self.data)
                    );
                }
                (4, "n_parallel_get_int") // fallback; fn_d_nr will be u32::MAX on error
            }
        };
        // Use the actual inline element size from the database (e.g. 4 for Score{value:integer},
        // 8 for Range{lo,hi:integer}).  var_size() returns size_of::<DbRef>() for reference types,
        // which is wrong for inline vector element storage.
        let elem_size = {
            let elm_td = self.data.type_elm(&elem_tp);
            let known = self.data.def(elm_td).known_type;
            let db_size = i32::from(self.database.size(known));
            if db_size > 0 {
                db_size
            } else {
                i32::from(var_size(&elem_tp, &Context::Argument))
            }
        };

        let ref_d_nr = self.data.def_nr("reference");
        let results_ref_type = Type::Reference(ref_d_nr, Vec::new());
        let par_for_d_nr = self.data.def_nr("n_parallel_for");
        let get_fn_d_nr = self.data.def_nr(get_fn_name);

        // Create result-reference variable.
        let results_var = self.create_unique("par_results", &results_ref_type);
        self.vars.defined(results_var);

        // Create index variable (b#index).
        let idx_var = self.create_var(&format!("{result_name}#index"), &I32);
        self.vars.defined(idx_var);
        self.vars.in_use(idx_var, true);

        // Create length variable (par_len#N).
        let len_var = self.create_unique("par_len", &I32);
        self.vars.defined(len_var);
        self.vars.in_use(len_var, true);

        // Create the result element variable (b) with the worker's return type.
        let b_type = if fn_d_nr == u32::MAX || matches!(ret_type, Type::Unknown(_)) {
            I32.clone()
        } else {
            ret_type.clone()
        };
        let b_var = self.create_var(&result_name, &b_type);
        self.vars.defined(b_var);
        if matches!(b_type, Type::Integer(_, _)) {
            self.vars.in_use(b_var, true);
        }

        // Parse the body block.
        self.vars.loop_var(b_var);
        let in_loop = self.in_loop;
        self.in_loop = true;
        let mut block = Value::Null;
        self.parse_block("parallel for", &mut block, &Type::Void);
        let count = self.vars.loop_counter();
        self.in_loop = in_loop;
        self.vars.finish_loop(loop_nr);

        // Build IR only when we have a valid function reference.
        if fn_d_nr == u32::MAX || par_for_d_nr == u32::MAX || get_fn_d_nr == u32::MAX {
            // Errors already reported; emit nothing useful.
            *code = Value::Null;
            return;
        }

        // parallel_for(input, elem_size, return_size, threads, fn_d_nr)
        let pf_call = Value::Call(
            par_for_d_nr,
            vec![
                vec_expr.clone(),
                Value::Int(elem_size),
                Value::Int(return_size),
                threads_expr,
                Value::Int(fn_d_nr as i32),
            ],
        );

        // len(input_vec) — compute once before the loop.
        let len_call = self.cl("OpLengthVector", &[vec_expr]);

        // Loop body:
        //   if par_len <= b#index { break }
        //   b = parallel_get_T(par_results, b#index)
        //   <user body>
        //   b#index += 1
        let stop_cond = self.cl("OpLeInt", &[Value::Var(len_var), Value::Var(idx_var)]);
        let stop = v_if(
            stop_cond,
            v_block(vec![Value::Break(0)], Type::Void, "break"),
            Value::Null,
        );
        let get_call = Value::Call(
            get_fn_d_nr,
            vec![Value::Var(results_var), Value::Var(idx_var)],
        );
        let b_assign = v_set(b_var, get_call);
        let idx_inc = v_set(
            idx_var,
            self.cl("OpAddInt", &[Value::Var(idx_var), Value::Int(1)]),
        );

        let mut lp = vec![stop, b_assign, block, idx_inc];
        if count != u16::MAX {
            lp.insert(
                3,
                v_set(
                    count,
                    self.cl("OpAddInt", &[Value::Var(count), Value::Int(1)]),
                ),
            );
        }

        let mut for_steps = Vec::new();
        if count != u16::MAX {
            for_steps.push(v_set(count, Value::Int(0)));
        }
        if fill != Value::Null {
            for_steps.push(fill);
        }
        for_steps.push(v_set(len_var, len_call));
        for_steps.push(v_set(results_var, pf_call));
        for_steps.push(v_set(idx_var, Value::Int(0)));
        for_steps.push(v_loop(lp, "Parallel for loop"));
        *code = v_block(for_steps, Type::Void, "Parallel for block");
    }

    // Consume the remaining `par(...)` tokens and then the body block so the
    // parser can recover after an error in the parallel clause.
    // Called after '(' has already been consumed, so this drains to ')'.
    fn skip_to_parallel_body(&mut self) {
        let mut depth = 1i32;
        loop {
            if self.lexer.peek_token("(") {
                depth += 1;
            } else if self.lexer.peek_token(")") {
                depth -= 1;
                if depth == 0 {
                    self.lexer.has_token(")");
                    break;
                }
            } else if self.lexer.peek_token("") {
                break;
            }
            let mut dummy = Value::Null;
            self.expression(&mut dummy);
        }
        let mut dummy = Value::Null;
        self.parse_block("parallel for", &mut dummy, &Type::Void);
    }

    // Resolve the worker function from a parallel call pattern inside par(...).
    //
    // Recognised forms (elem_var is the name of the loop element, e.g. "a"):
    //   Form 1  func(a)         — global/user function
    //   Form 2  a.method()      — method whose receiver is the element
    //   Form 3  c.method(a)     — captured receiver + element arg (deferred)
    //
    // All tokens for the call are consumed; the caller reads ',' next.
    // On error or first pass returns (u32::MAX, Unknown).
    fn parse_parallel_worker(&mut self, elem_var: &str, elem_tp: &Type) -> (u32, Type) {
        let Some(first_id) = self.lexer.has_identifier() else {
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect function name or '{elem_var}.method' inside |..|"
                );
            }
            return (u32::MAX, Type::Unknown(0));
        };

        if first_id == elem_var {
            // ── Form 2: a.method() ────────────────────────────────────────────
            if !self.lexer.has_token(".") {
                if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Expect '.' after '{elem_var}' in parallel clause (use a.method() or func(a))"
                    );
                }
                return (u32::MAX, Type::Unknown(0));
            }
            let Some(method_name) = self.lexer.has_identifier() else {
                if !self.first_pass {
                    diagnostic!(self.lexer, Level::Error, "Expect method name after '.'");
                }
                return (u32::MAX, Type::Unknown(0));
            };
            self.lexer.token("(");
            self.lexer.token(")");

            // Resolve the method on the element type.
            let type_name = match elem_tp {
                Type::Reference(d, _) | Type::Enum(d, _, _) => self.data.def(*d).name.clone(),
                _ => {
                    if !self.first_pass {
                        diagnostic!(
                            self.lexer,
                            Level::Error,
                            "Parallel method call (form 2) requires a struct element type, not {}",
                            elem_tp.name(&self.data)
                        );
                    }
                    return (u32::MAX, Type::Unknown(0));
                }
            };
            // Method internal name: t_<len><TypeName>_<method>
            let internal = format!("t_{}{type_name}_{method_name}", type_name.len());
            let d_nr = {
                let nr = self.data.def_nr(&internal);
                if nr == u32::MAX {
                    self.data.def_nr(&method_name)
                } else {
                    nr
                }
            };
            if d_nr == u32::MAX {
                if !self.first_pass {
                    diagnostic!(
                        self.lexer,
                        Level::Error,
                        "Unknown method '{method_name}' on type '{type_name}'"
                    );
                }
                return (u32::MAX, Type::Unknown(0));
            }
            if !self.first_pass && !matches!(self.data.def_type(d_nr), DefType::Function) {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "'{method_name}' is not a function"
                );
                return (u32::MAX, Type::Unknown(0));
            }
            self.data.def_used(d_nr);
            let ret_type = self.data.def(d_nr).returned.clone();
            (d_nr, ret_type)
        } else if self.lexer.peek_token(".") {
            // ── Form 3: c.method(a) — deferred ───────────────────────────────
            // Consume the rest of the call so parsing can continue.
            self.lexer.has_token(".");
            if self.lexer.has_identifier().is_some() && self.lexer.has_token("(") {
                let mut depth = 1i32;
                loop {
                    if self.lexer.peek_token("(") {
                        depth += 1;
                    } else if self.lexer.peek_token(")") {
                        depth -= 1;
                        if depth == 0 {
                            self.lexer.has_token(")");
                            break;
                        }
                    } else if self.lexer.peek_token("") {
                        break;
                    }
                    let mut dummy = Value::Null;
                    self.expression(&mut dummy);
                    self.lexer.has_token(",");
                }
            }
            if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Parallel form '{first_id}.method({elem_var})' (captured receiver) \
                     is not yet supported; define a wrapper function and use func({elem_var}) instead"
                );
            }
            (u32::MAX, Type::Unknown(0))
        } else {
            // ── Form 1: func(a) ───────────────────────────────────────────────
            // Resolve function name: try n_<name> first (user function convention).
            let d_nr = {
                let prefixed = format!("n_{first_id}");
                let nr = self.data.def_nr(&prefixed);
                if nr == u32::MAX {
                    self.data.def_nr(&first_id)
                } else {
                    nr
                }
            };
            // Consume the argument list.
            if self.lexer.has_token("(") {
                let mut depth = 1i32;
                loop {
                    if self.lexer.peek_token("(") {
                        depth += 1;
                    } else if self.lexer.peek_token(")") {
                        depth -= 1;
                        if depth == 0 {
                            self.lexer.has_token(")");
                            break;
                        }
                    } else if self.lexer.peek_token("") {
                        break;
                    }
                    let mut dummy = Value::Null;
                    self.expression(&mut dummy);
                    self.lexer.has_token(",");
                }
            } else if !self.first_pass {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Expect '(' after function name '{first_id}' in parallel clause"
                );
            }
            if d_nr == u32::MAX {
                if !self.first_pass {
                    diagnostic!(self.lexer, Level::Error, "Unknown function '{first_id}'");
                }
                return (u32::MAX, Type::Unknown(0));
            }
            if !self.first_pass && !matches!(self.data.def_type(d_nr), DefType::Function) {
                diagnostic!(self.lexer, Level::Error, "'{first_id}' is not a function");
                return (u32::MAX, Type::Unknown(0));
            }
            self.data.def_used(d_nr);
            let ret_type = self.data.def(d_nr).returned.clone();
            (d_nr, ret_type)
        }
    }

    fn for_type(&mut self, in_type: &Type) -> Type {
        if let Type::Vector(t_nr, dep) = &in_type {
            let mut t = *t_nr.clone();
            if let Type::Enum(nr, true, _) = t {
                t = Type::Reference(nr, vec![]);
            }
            for d in dep {
                t = t.depending(*d);
            }
            t
        } else if let Type::Sorted(dnr, _, dep) | Type::Index(dnr, _, dep) = &in_type {
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
            self.data.definitions[self.context as usize].returned = match ret {
                Type::Vector(it, _) => Type::Vector(it, dep),
                Type::Reference(td, _) => Type::Reference(td, dep),
                _ => unreachable!("ref_return called with non-Vector/Reference return type"),
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
    fn parse_call(&mut self, val: &mut Value, source: u16, name: &str) -> Type {
        let call_pos = self.lexer.pos().clone();
        let mut list = Vec::new();
        let mut types = Vec::new();
        if self.lexer.has_token(")") {
            return self.call(val, source, name, &list, &Vec::new());
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
            if self.first_pass {
                *val = Value::Null;
                return Type::Void;
            }
            let d_nr = self.data.def_nr("n_assert");
            let file_v = Value::str(&call_pos.file);
            let line_v = Value::Int(call_pos.line as i32);
            *val = Value::Call(d_nr, vec![test, message, file_v, line_v]);
            Type::Void
        } else if name == "panic" {
            let message = if list.is_empty() {
                Value::str("panic")
            } else {
                list[0].clone()
            };
            if self.first_pass {
                *val = Value::Null;
                return Type::Void;
            }
            let d_nr = self.data.def_nr("n_panic");
            let file_v = Value::str(&call_pos.file);
            let line_v = Value::Int(call_pos.line as i32);
            *val = Value::Call(d_nr, vec![message, file_v, line_v]);
            Type::Void
        } else if matches!(name, "log_info" | "log_warn" | "log_error" | "log_fatal") {
            let message = if list.is_empty() {
                Value::str("")
            } else {
                list[0].clone()
            };
            if self.first_pass {
                *val = Value::Null;
                return Type::Void;
            }
            let fn_name = format!("n_{name}");
            let d_nr = self.data.def_nr(&fn_name);
            let file_v = Value::str(&call_pos.file);
            let line_v = Value::Int(call_pos.line as i32);
            *val = Value::Call(d_nr, vec![message, file_v, line_v]);
            Type::Void
        } else if name == "parallel_for" {
            self.parse_parallel_for(val, &list, &types)
        } else {
            self.call(val, source, name, &list, &types)
        }
    }

    // Validate and rewrite a user-friendly `parallel_for(fn f, vec, threads)` call
    // into a `Value::Call(n_parallel_for_d_nr, [input, elem_size, return_size, threads, func])`.
    //
    // The parser intercepts calls by name "parallel_for" before normal overload
    // resolution.  Compile-time checks performed here:
    // - First arg must be `Type::Function(args, ret)` (produced by `fn <name>` expression).
    // - Second arg must be `Type::Vector(T, _)`.
    // - Worker's first parameter must be a reference to T (type checked by name).
    // - Return type must be a primitive: integer, long, float, or boolean.
    // - Extra arg count must match the worker's extra parameters (args[1..]).
    fn parse_parallel_for(&mut self, val: &mut Value, list: &[Value], types: &[Type]) -> Type {
        let ref_d_nr = self.data.def_nr("reference");
        let result_ref_type = Type::Reference(ref_d_nr, Vec::new());
        if self.first_pass {
            return result_ref_type;
        }
        if list.len() < 3 {
            diagnostic!(
                self.lexer,
                Level::Error,
                "parallel_for requires at least 3 arguments: fn worker, input_vector, threads"
            );
            return Type::Unknown(0);
        }
        let (worker_arg_types, worker_ret_type) = if let Type::Function(args, ret) = &types[0] {
            (args.clone(), (**ret).clone())
        } else {
            diagnostic!(
                self.lexer,
                Level::Error,
                "parallel_for: first argument must be a function reference (use fn <name>)"
            );
            return Type::Unknown(0);
        };
        let elem_tp = if let Type::Vector(elem, _) = &types[1] {
            (**elem).clone()
        } else {
            diagnostic!(
                self.lexer,
                Level::Error,
                "parallel_for: second argument must be a vector"
            );
            return Type::Unknown(0);
        };
        // Validate return type is a supported primitive.
        let return_size: u32 = match &worker_ret_type {
            Type::Integer(_, _) | Type::Character => 4,
            Type::Boolean => 1,
            Type::Long | Type::Float => 8,
            _ => {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "parallel_for: worker return type '{}' must be integer, long, float, or boolean",
                    worker_ret_type.name(&self.data)
                );
                return Type::Unknown(0);
            }
        };
        // Validate extra arg count matches worker's extra params.
        let n_extra = list.len().saturating_sub(3);
        let n_worker_extra = worker_arg_types.len().saturating_sub(1);
        if n_extra != n_worker_extra {
            diagnostic!(
                self.lexer,
                Level::Error,
                "parallel_for: wrong number of extra arguments: worker expects {n_worker_extra}, got {n_extra}"
            );
            return Type::Unknown(0);
        }
        // Compute element size from T — use the actual inline database size, not the IR size.
        // var_size() returns size_of::<DbRef>() for reference types, which is wrong for inline
        // vector element storage (e.g. Score{value:integer} is 4 bytes inline, not 12).
        let elem_size = {
            let elm_td = self.data.type_elm(&elem_tp);
            let known = self.data.def(elm_td).known_type;
            let db_size = i32::from(self.database.size(known));
            if db_size > 0 {
                db_size
            } else {
                i32::from(var_size(&elem_tp, &Context::Argument))
            }
        };
        // Look up internal n_parallel_for.
        let par_for_d_nr = self.data.def_nr("n_parallel_for");
        if par_for_d_nr == u32::MAX {
            diagnostic!(
                self.lexer,
                Level::Error,
                "internal error: n_parallel_for not found"
            );
            return Type::Unknown(0);
        }
        // Build augmented call: [input, element_size, return_size, threads, func].
        // (input first so gather_key reads it, not the integer func d_nr.)
        let mut augmented = vec![
            list[1].clone(),                // input: vector<T>
            Value::Int(elem_size),          // element_size: synthesized
            Value::Int(return_size as i32), // return_size: synthesized
            list[2].clone(),                // threads: integer
            list[0].clone(),                // func: d_nr as integer
        ];
        // Append any extra args (verified count above; types passed through).
        for extra in list.iter().skip(3) {
            augmented.push(extra.clone());
        }
        *val = Value::Call(par_for_d_nr, augmented);
        result_ref_type
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
            let e_tp = self.data.type_elm(&tp);
            if e_tp != u32::MAX {
                found = true;
                if matches!(tp, Type::Enum(_, true, _) | Type::Reference(_, _)) && !self.first_pass
                {
                    // Polymorphic enum or reference: size depends on runtime variant.
                    *val = self.cl("OpSizeofRef", &[drop]);
                } else {
                    *val = Value::Int(i32::from(
                        self.database.size(self.data.def(e_tp).known_type),
                    ));
                }
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

fn field_id(key: &[(String, bool)], name: &mut String) {
    for (k_nr, (k, asc)) in key.iter().enumerate() {
        if k_nr > 0 {
            *name += ",";
        }
        if !asc {
            *name += "-";
        }
        *name += k;
    }
    *name += "]>";
}

/// Collect all `Value::Var` indices reachable anywhere in `val`.
fn collect_vars_in(val: &Value, result: &mut HashSet<u16>) {
    match val {
        Value::Var(v) => {
            result.insert(*v);
        }
        Value::Set(_, body) => collect_vars_in(body, result),
        Value::Call(_, args) => {
            for a in args {
                collect_vars_in(a, result);
            }
        }
        Value::Block(b) | Value::Loop(b) => {
            for op in &b.operators {
                collect_vars_in(op, result);
            }
        }
        Value::Insert(list) => {
            for item in list {
                collect_vars_in(item, result);
            }
        }
        Value::If(c, t, e) => {
            collect_vars_in(c, result);
            collect_vars_in(t, result);
            collect_vars_in(e, result);
        }
        Value::Return(v) | Value::Drop(v) => collect_vars_in(v, result),
        Value::Iter(_, a, b, c) => {
            collect_vars_in(a, result);
            collect_vars_in(b, result);
            collect_vars_in(c, result);
        }
        _ => {}
    }
}

/// Recursively walk a Value IR tree and collect all variable indices that are written.
/// A variable is considered written if:
/// - It appears as the target of `Value::Set(v, ...)`,
/// - It is passed as a `RefVar`-typed argument to a `Value::Call`, or
/// - It appears anywhere in the first argument of a field-write operator (`OpSet*`),
///   which covers the pattern `v[idx].field = val` where `v: &vector<T>`.
fn find_written_vars(code: &Value, data: &Data, written: &mut HashSet<u16>) {
    match code {
        Value::Set(v, body) => {
            written.insert(*v);
            find_written_vars(body, data, written);
        }
        Value::Call(fn_nr, args) => {
            let def = data.def(*fn_nr);
            let attrs = &def.attributes;
            // Stack-text mutation operators (OpAppendStackText, OpAppendStackCharacter,
            // OpClearStackText) write to their first argument via a const-u16 position.
            // OpAppendVector mutates the vector pointed to by its first Var argument.
            let stack_write = def.name.starts_with("OpAppendStack")
                || def.name.starts_with("OpClearStack")
                || def.name == "OpAppendVector";
            // Field-write and vector-append operators: any Var appearing in the first
            // argument is being mutated (e.g. v[idx].field = val, r += [x]).
            let field_write = def.name.starts_with("OpSet")
                || def.name == "OpNewRecord"
                || def.name == "OpAppendCopy";
            for (i, arg) in args.iter().enumerate() {
                if i < attrs.len()
                    && matches!(attrs[i].typedef, Type::RefVar(_))
                    && let Value::Var(v) = arg
                {
                    written.insert(*v);
                }
                if i == 0
                    && stack_write
                    && let Value::Var(v) = arg
                {
                    written.insert(*v);
                }
                if i == 0 && field_write {
                    collect_vars_in(arg, written);
                }
                find_written_vars(arg, data, written);
            }
        }
        Value::Block(block) | Value::Loop(block) => {
            for item in &block.operators {
                find_written_vars(item, data, written);
            }
        }
        Value::Insert(list) => {
            for item in list {
                find_written_vars(item, data, written);
            }
        }
        Value::If(cond, then, els) => {
            find_written_vars(cond, data, written);
            find_written_vars(then, data, written);
            find_written_vars(els, data, written);
        }
        Value::Return(v) | Value::Drop(v) => {
            find_written_vars(v, data, written);
        }
        Value::Iter(_, create, next, extra) => {
            find_written_vars(create, data, written);
            find_written_vars(next, data, written);
            find_written_vars(extra, data, written);
        }
        _ => {}
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
        "^" => "Eor",
        "<<" => "SLeft",
        ">>" => "SRight",
        "==" => "Eq",
        "!=" => "Ne",
        "<" => "Lt",
        "<=" => "Le",
        "%" => "Rem",
        "!" => "Not",
        "+=" => "Append",
        _ => op,
    }
}
