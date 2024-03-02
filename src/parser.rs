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
    /// The function that is currently parsed
    current_fn: u32,
    first_pass: bool,
}

// Operators ordered on their precedence
static OPERATORS: &[&[&str]] = &[
    &[".."],
    &["as"],
    &["||"],
    &["&&"],
    &["==", "!="],
    &["<", "<=", ">", ">="],
    &["-", "+"],
    &["*", "/", "%"],
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
            current_fn: u32::MAX,
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
        if !default {
            let lvl = self.lexer.diagnostics().level();
            if lvl != Level::Error && lvl != Level::Fatal {
                self.lexer = Lexer::lines(
                    BufReader::new(File::open(filename).unwrap()).lines(),
                    filename,
                );
                self.parse_file();
            }
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
    fn iterator(&mut self, code: &mut Value, is_type: &Type, should: &Type) -> Value {
        if is_type == should {
            // there was already an iterator.
            let orig = code.clone();
            *code = Value::Null; // there is no iterator to create, we got it already
            return orig;
        }
        if let Type::Iterator(_) = should {
            if let Type::Vector(tp) = is_type {
                let iter_var = self.types.create_var(
                    format!("iter_{}", self.var_nr),
                    Type::Integer,
                    self.lexer.pos(),
                );
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
                    format!("iter_{}", self.var_nr),
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
                        format!("count_{}", self.var_nr),
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
                    } else if self.data.returned(dnr) == Type::Reference(0) {
                        if let Type::Reference(_) = *should {
                            *code = Value::Null;
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
        for &dnr in self.types.get_possible("Cast") {
            if self.data.attributes(dnr) > 0
                && self.data.attr_type(dnr, 0) == *is_type
                && self.data.returned(dnr) == *should
            {
                *code = Value::Call(dnr, vec![code.clone()]);
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
        if self.first_pass && !self.default {
            return Type::Unknown(0);
        }
        let d_nr = self.data.def_nr(name);
        if d_nr != u32::MAX {
            self.call_nr(code, d_nr, list, types, true)
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
        self.get_val(&self.data.attr_type(d_nr, f_nr), pos, code)
    }

    fn get_val(&mut self, tp: &Type, pos: i32, code: Value) -> Value {
        match tp {
            Type::Integer => self.cl("OpGetInt", &[code, Value::Int(pos)]),
            Type::Boolean => self.cl("OpGetByte", &[code, Value::Int(pos), Value::Int(0)]),
            Type::Long => self.cl("OpGetLong", &[code, Value::Int(pos)]),
            Type::Float => self.cl("OpGetFloat", &[code, Value::Int(pos)]),
            Type::Single => self.cl("OpGetSingle", &[code, Value::Int(pos)]),
            Type::Text => self.cl("OpGetText", &[code, Value::Int(pos)]),
            Type::Vector(_) => self.cl("OpGet", &[code, Value::Int(pos)]),
            Type::Reference(_) => self.cl("OpGet", &[code, Value::Int(pos)]),
            _ => panic!("Get not implemented on {}", tp),
        }
    }

    fn set_field(&mut self, d_nr: u32, f_nr: u16, ref_code: Value, val_code: Value) -> Value {
        let pos = self.data.attr_pos(d_nr, f_nr) as i32;
        match self.data.attr_type(d_nr, f_nr) {
            Type::Integer
            | Type::Vector(_)
            | Type::Hash(_, _)
            | Type::Index(_, _)
            | Type::Radix(_, _)
            | Type::Inner(_)
            | Type::Reference(_)
            | Type::Sorted(_, _) => self.cl("OpSetInt", &[ref_code, Value::Int(pos), val_code]),
            Type::Boolean => self.cl(
                "OpSetByte",
                &[ref_code, Value::Int(pos), Value::Int(0), val_code],
            ),
            Type::Long => self.cl("OpSetLong", &[ref_code, Value::Int(pos), val_code]),
            Type::Float => self.cl("OpSetFloat", &[ref_code, Value::Int(pos), val_code]),
            Type::Single => self.cl("OpSetSingle", &[ref_code, Value::Int(pos), val_code]),
            Type::Text => self.cl("OpSetText", &[ref_code, Value::Int(pos), val_code]),
            _ => panic!(
                "Set not implemented on {}/{}",
                self.data.attr_name(d_nr, f_nr),
                self.data.attr_type(d_nr, f_nr)
            ),
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
                    && !self.parse_struct())
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
        let mut nr = 1;
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
            let v_nr = self
                .data
                .add_def(value_name.clone(), self.lexer.pos(), DefType::EnumValue);
            self.data.set_returned(v_nr, Type::Enum(d_nr));
            self.data.def_set_size(v_nr, 1, 1);
            let a_nr =
                self.data
                    .add_attribute(&mut self.lexer, d_nr, &value_name, Type::Enum(d_nr));
            if self.first_pass {
                self.data.set_attr_value(d_nr, a_nr, Value::Int(nr));
            }
            nr += 1;
            if !self.lexer.has_token(",") {
                break;
            }
        }
        complete_definition(&mut self.lexer, &mut self.data, d_nr);
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
        let d_nr = self
            .data
            .add_def(type_name, self.lexer.pos(), DefType::Type);
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
        complete_definition(&mut self.lexer, &mut self.data, d_nr);
        self.lexer.token(";");
        true
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
        self.current_fn = if self.default && is_op(&fn_name) {
            self.data
                .add_op(&mut self.lexer, &mut self.types, fn_name, arguments)
        } else if self.default || self.first_pass {
            self.data.add_fn(&mut self.lexer, fn_name, arguments)
        } else {
            self.data.def_nr(&fn_name)
        };
        let mut attributes = 0;
        if self.current_fn != u32::MAX {
            if self.default || self.first_pass {
                self.data.set_returned(self.current_fn, result);
            }
            attributes = self.data.attributes(self.current_fn);
        }
        if !self.default || !self.lexer.has_token(";") {
            self.parse_code();
        }
        if self.default || !self.first_pass {
            self.types.test_used(attributes, &mut self.diagnostics);
        }
        self.types.clear();
        self.lexer.has_token(";");
        if self.default && self.lexer.has_token("#") {
            if self.lexer.has_identifier() == Some("rust".to_string()) {
                if let Some(c) = self.lexer.has_cstring() {
                    self.data.set_rust(self.current_fn, c);
                } else {
                    diagnostic!(self.lexer, Level::Error, "Expect rust string");
                }
            } else {
                diagnostic!(self.lexer, Level::Error, "Expect #rust");
            }
        }
        self.current_fn = u32::MAX;
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

    // <type> ::= <identifier> [ '<' ( <sub_type> | <type> ) '>' [ '[' <identifier> { ',' <identifier> } ']' ]]
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
                        "vector" => {
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Vector(Box::new(tp))
                        }
                        "sorted" => {
                            self.data.set_referenced(sub_nr, on_d, Value::Null);
                            Type::Sorted(sub_nr, vec![])
                        }
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
                diagnostic!(self.lexer, Level::Error, "Expected a type between <>");
            }
        }
        let dt = self.data.def_type(tp_nr);
        if tp_nr != u32::MAX
            && (dt == DefType::Type || dt == DefType::Enum || dt == DefType::Struct)
        {
            Some(self.data.returned(tp_nr))
        } else {
            None
        }
    }

    // <attributes> = <identifier> [ ':' <type> ] ['[' param { ',' param } ']'] [ '=' <expression> ] '}'
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
        self.lexer.token("{");
        loop {
            let mut defined = false;
            let mut a_type: Type = Type::Unknown(0);
            let Some(a_name) = self.lexer.has_identifier() else {
                diagnostic!(self.lexer, Level::Error, "Expect attribute");
                return true;
            };
            if self.lexer.has_token(":") {
                defined = true;
                if let Some(id) = self.lexer.has_identifier() {
                    if let Some(tp) = self.parse_type(d_nr, &id) {
                        a_type = tp;
                    }
                }
            }
            if self.lexer.has_token("[") {
                loop {
                    let Some(_name) = self.lexer.has_identifier() else {
                        diagnostic!(self.lexer, Level::Error, "Expect attribute");
                        return true;
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
            } else if self.lexer.has_keyword("limit") {
                self.lexer.has_token("-");
                let _min = if let Some(nr) = self.lexer.has_long() {
                    nr
                } else {
                    u64::MAX
                };
                let _incl = if self.lexer.has_token("..=") {
                    true
                } else {
                    self.lexer.token("..");
                    false
                };
                let _max = if let Some(nr) = self.lexer.has_long() {
                    nr
                } else {
                    u64::MAX
                };
            } else if self.lexer.has_keyword("not") {
                self.lexer.token("null");
            }
            if self.lexer.has_token("=") {
                defined = true;
                // Define a default value on an attribute
                let mut t = Value::Null;
                // TODO EXT_0005 allows field to be read during the expression = give the current definition as context
                let tp = self.expression(&mut t);
                if tp.is_unknown() {
                    diagnostic!(self.lexer, Level::Error, "Expecting a clear type");
                }
                a_type = tp.clone();
            }
            if self.first_pass {
                self.data
                    .add_attribute(&mut self.lexer, d_nr, &a_name, a_type);
            }
            if !defined {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Attribute {a_name} needs type or definition"
                );
            }
            if !self.lexer.has_token(",") {
                break;
            }
        }
        self.lexer.token("}");
        self.lexer.has_token(";");
        true
    }

    // <code> = '{' <block> '}'
    /// Parse the code on the last inserted definition.
    /// This way we can use recursion with the definition itself.
    fn parse_code(&mut self) -> Type {
        self.lexer.token("{");
        let mut v = Value::Null;
        let result = if self.current_fn != u32::MAX {
            self.types.parameters(&self.data, self.current_fn)
        } else {
            Type::Void
        };
        let t = self.parse_block(&mut v, result);
        if self.current_fn != u32::MAX && (self.default || !self.first_pass) {
            self.data.set_code(self.current_fn, v);
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
            self.test_var(&res, val);
            res
        }
    }

    // <parse_operators> [ '=' | '+=' | '-=' | '*=' | '%=' | '/=' <parse_operators> ]
    fn parse_assign(&mut self, code: &mut Value) -> Type {
        let f_type = self.parse_operators(code, 0);
        let to = code.clone();
        for op in ["=", "+=", "-=", "*=", "%=", "/="] {
            if self.lexer.has_token(op) {
                let s_type = self.parse_operators(code, 0);
                let new = self.types.change_var_type(&mut self.lexer, &to, &s_type);
                *code = self.towards_set(to, code, &f_type, &op[0..1], new);
                return Type::Void;
            }
        }
        *code = to;
        f_type
    }

    fn towards_set(&mut self, to: Value, val: &Value, f_type: &Type, op: &str, new: bool) -> Value {
        if let Type::Vector(elm_tp) = f_type {
            let mut blk = Vec::new();
            if op == "=" {
                blk.push(self.cl("OpClearVector", &[to.clone()]));
            } else if op != "+=" {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unsupported operator '{}' on a structure",
                    op
                );
                return Value::Null;
            }
            let elm_td = self.data.type_elm(elm_tp);
            let elm_size = self.data.def_size(elm_td) as i32;
            if let Value::Block(vec) = val {
                for v in vec {
                    let app = self.cl("OpAppendVector", &[to.clone(), Value::Int(elm_size)]);
                    blk.push(self.op(op, app, v.clone(), *elm_tp.clone()));
                }
            } else if let Value::Var(nr) = val {
                let len = self.cl("OpLengthVector", &[Value::Var(*nr)]);
                blk.push(self.cl("OpAppendVector", &[to.clone(), len]));
                // TODO actually write the data
                // Loop through all elements
            } else {
                // TODO Test with an array typed variable
                panic!("Unknown type on Vector {:?}", val);
            }
            return Value::Block(blk);
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

    // <operator> ::= '..' |
    //                '||' |
    //                '&&' |
    //                '==' | '!=' | '<' | '<=' | '>' | '>=' |
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
            self.test_var(&current_type, code);
            if operator == "as" {
                if let Some(tps) = self.lexer.has_identifier() {
                    let tp = self.data.name_type(&tps);
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
            } else {
                let mut second_code = Value::Null;
                let second_type = self.parse_operators(&mut second_code, precedence + 1);
                self.test_var(&second_type, &second_code);
                if operator == ".." {
                    // Force the creation of an actual iterator later, only then it will become an integer iterator.
                    current_type = Type::Iterator(Box::new(Type::Null));
                    *code = Value::Range(Box::new(code.clone()), Box::new(second_code));
                } else {
                    current_type = self.call_op(
                        code,
                        operator,
                        vec![code.clone(), second_code],
                        vec![current_type, second_type],
                    );
                }
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
            *val = Value::Int(1);
            Type::Boolean
        } else if self.lexer.has_token("false") {
            *val = Value::Int(0);
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
            &Value::Var(vec),
            &Type::Vector(Box::new(in_t.clone())),
        );
        let mut ls = Vec::new();
        let ed_nr = self.data.type_def_nr(&in_t);
        let attr = self.data.def_type(ed_nr) == DefType::Type;
        let inline = if attr {
            false
        } else {
            self.data.def_referenced(ed_nr)
        };
        for p in res {
            let vec_tp = self.data.type_def_nr(&in_t);
            let vec_size = self.data.def_size(vec_tp) as i32;
            let app_v = self.cl("OpAppendVector", &[Value::Var(vec), Value::Int(vec_size)]);
            if attr {
                ls.push(v_set(elm, app_v));
                ls.push(self.set_field(vec_tp, u16::MAX, Value::Var(elm), p.clone()));
            } else if inline {
                ls.push(v_set(elm, app_v));
                Self::push(&mut ls, &p);
            } else {
                ls.push(v_set(
                    elm,
                    self.cl("OpAppend", &[Value::Var(vec), Value::Int(vec_size)]),
                ));
                Self::push(&mut ls, &p);
                ls.push(self.cl("OpSetReference", &[app_v, Value::Int(0), Value::Var(elm)]));
            }
        }
        if new_store {
            let rec_td = self.data.type_elm(&in_t);
            let vec_def = self.data.vector_def(&mut self.lexer, rec_td, &in_t);
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
    fn field(&mut self, code: &mut Value, t: Type) -> Type {
        let mut t = t;
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
                let last_t = t.clone();
                t = self.data.attr_type(dnr, fnr);
                if let Type::Enum(_) = last_t.clone() {
                    // do something with enum fields
                } else if let Type::Routine(r_nr) = t {
                    if self.lexer.has_token("(") {
                        t = self.parse_method(code, r_nr, last_t);
                    }
                } else {
                    let pos = self.data.attr_pos(dnr, fnr);
                    *code = if pos == 0 {
                        self.data.attr_value(dnr, fnr)
                    } else {
                        self.get_field(dnr, fnr, code.clone())
                    };
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
            *code = self.get_val(&etp, 0, code.clone());
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
            t = tp;
            *code = Value::Var(var_nr);
        } else if self.data.def_nr(&name) != u32::MAX {
            let dnr = self.data.def_nr(&name);
            if self.data.def_type(dnr) == DefType::Enum {
                t = Type::Enum(dnr);
            } else if self.data.def_type(dnr) == DefType::EnumValue {
                t = Type::Enum(self.data.def_parent(dnr));
            } else {
                t = Type::Null;
            };
        } else {
            *code = Value::Var(
                self.types
                    .create_var(name, Type::Unknown(0), self.lexer.pos()),
            );
            t = Type::Unknown(0);
        }
        while self.lexer.peek_token(".") || self.lexer.peek_token("[") {
            self.test_var(&t, code);
            if self.lexer.has_token(".") {
                t = self.field(code, t)
            } else if self.lexer.has_token("[") {
                t = self.index(code, t);
                self.lexer.token("]");
            }
        }
        t
    }

    fn test_var(&mut self, tp: &Type, code: &Value) {
        if !self.first_pass && tp.is_unknown() {
            if let Value::Var(nr) = *code {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown variable {}",
                    self.types.name(nr)
                );
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
        let iter_next = self.iterator(&mut create_iter, &in_type, &it);
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
            if tp.is_unknown() {
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
            self.append_data(tp, &mut list, var, &mut format, state);
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
                .create_var(id, var_type.clone(), self.lexer.pos());
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let in_loop = self.in_loop;
            self.in_loop = true;
            let block_type = self.parse_block(&mut block, Type::Void);
            self.in_loop = in_loop;
            let mut create_iter = in_expr;
            let it = Type::Iterator(Box::new(var_type.clone()));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it);
            if iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return Value::Null;
            }
            *val = create_iter;
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
        let is = tp.clone();
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
                        Value::Int(if state.plus { 1 } else { 0 }),
                        Value::Int(if state.note { 1 } else { 0 }),
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
                        Value::Int(if state.plus { 1 } else { 0 }),
                        Value::Int(if state.note { 1 } else { 0 }),
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
                    "OpFormatText",
                    &[
                        v_if(fmt, text("true"), text("false")),
                        state.width,
                        Value::Int(state.dir),
                        Value::Int(state.token.as_bytes()[0] as i32),
                    ],
                    &[Type::Text, Type::Integer, Type::Integer, Type::Integer],
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
            Type::Vector(vtp) => {
                let mut step = format.clone();
                let it = Type::Iterator(vtp.clone());
                let next = self.iterator(&mut step, &is, &it);
                list.push(step);
                append = false;
                self.append_iter(list, var, vtp.as_ref(), next);
            }
            Type::Iterator(vtp) => {
                append = false;
                self.append_iter(list, var, vtp.as_ref(), format.clone());
            }
            Type::Reference(d_nr) => {
                list.push(v_set(
                    var,
                    self.cl(
                        "OpAddText",
                        &[
                            Value::Var(var),
                            Value::Text(self.data.def_name(d_nr) + " {"),
                        ],
                    ),
                ));
                let attrs = self.data.attributes(d_nr);
                // let first = true
                let first_var = self.types.create_var(
                    format!("iter_{}", self.var_nr),
                    Type::Boolean,
                    self.lexer.pos(),
                );
                self.var_nr += 1;
                list.push(v_set(first_var, Value::Int(1)));
                for a in 0..attrs {
                    let tp = self.data.attr_type(d_nr, a);
                    let a_name = self.data.attr_name(d_nr, a);
                    if let Type::Routine(_) = tp {
                        continue;
                    }
                    // let val = rec.attribute()
                    let val_var = self.types.create_var(
                        format!("iter_{}", self.var_nr),
                        Type::Boolean,
                        self.lexer.pos(),
                    );
                    self.var_nr += 1;
                    list.push(v_set(
                        val_var,
                        Value::Null, //TODO Field(Box::new(format.clone()), a as u16)),
                    ));
                    // if val != none {
                    //   if first { first = false } else { Append(", ") }
                    //   Append(name + ": ")
                    let mut write = vec![
                        v_if(
                            Value::Var(first_var),
                            v_set(first_var, Value::Int(0)),
                            v_set(var, self.cl("OpAddText", &[Value::Var(var), text(", ")])),
                        ),
                        v_set(
                            var,
                            self.cl(
                                "OpAddText",
                                &[Value::Var(var), Value::Text(a_name.clone() + ":")],
                            ),
                        ),
                    ];
                    //   Append(val)
                    let mut val = Value::Var(val_var);
                    self.append_data(tp.clone(), &mut write, var, &mut val, OUTPUT_DEFAULT);
                    // }
                    list.push(v_if(
                        self.op("!=", Value::Var(val_var), Value::Null, tp.clone()),
                        Value::Block(write),
                        Value::Null,
                    ));
                }
                list.push(v_set(
                    var,
                    self.cl("OpAddText", &[Value::Var(var), text("}")]),
                ));
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
        let v = self.types.create_var(
            "val".to_string(),
            self.data.returned(td_nr),
            self.lexer.pos(),
        );
        let mut list = vec![];
        let rec_size = Value::Int(self.data.def_size(td_nr) as i32);
        list.push(v_let(
            v,
            if let Value::Reference(_, _, _) = code {
                self.cl(
                    "OpAppend",
                    &[Value::Int(self.data.def_size(td_nr) as i32), code.clone()],
                )
            } else {
                self.data
                    .set_referenced(td_nr, self.current_fn, Value::Null);
                self.cl("OpDatabase", &[rec_size])
            },
        ));
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
            if found_fields.contains(&self.data.attr_name(td_nr, aid)) {
                continue;
            }
            list.push(self.set_field(td_nr, aid, Value::Var(v), self.data.attr_value(td_nr, aid)));
        }
        list.push(Value::Var(v));
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
            } else {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Unknown in expression type {}",
                    self.data.show_type(&in_type)
                );
                Type::Null
            };
            let for_var = self.types.create_var(id, var_tp.clone(), self.lexer.pos());
            let if_step = if self.lexer.has_token("if") {
                let mut if_expr = Value::Null;
                self.expression(&mut if_expr);
                if_expr
            } else {
                Value::Null
            };
            self.lexer.token("{");
            let mut block = Value::Null;
            let in_loop = self.in_loop;
            self.in_loop = true;
            self.parse_block(&mut block, Type::Void);
            self.in_loop = in_loop;
            let mut create_iter = in_expr;
            let it = Type::Iterator(Box::new(var_tp.clone()));
            let iter_next = self.iterator(&mut create_iter, &in_type, &it);
            if iter_next == Value::Null {
                diagnostic!(
                    self.lexer,
                    Level::Error,
                    "Need an iterable expression in a for statement"
                );
                return;
            }
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
        let r_type = self.data.returned(self.current_fn);
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
                    if let Type::Reference(_) = tp {
                        found = true;
                        *val = Value::Int(4);
                    } else {
                        found = true;
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
