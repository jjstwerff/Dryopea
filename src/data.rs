// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Hold all definitions
//! Those are the combinations of types, records and routines.
//! Many definitions can hold fields of their own, a routine
//! has parameters that behave very similar to fields.

// These structures are rather inefficient right now, but they are be the basis
// for a far more efficient database design later.
#![allow(dead_code)]

extern crate strum_macros;
use crate::diagnostics::{Diagnostics, Level, diagnostic_format};
use crate::lexer::{Lexer, Position};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::io::{Result, Write};

lazy_static::lazy_static! {
    static ref OPERATORS: Vec<&'static str> = vec!(
        "OpAdd", "OpMin", "OpMul", "OpDiv", "OpRem", "OpPow",
        "OpNot", "OpLand", "OpLor", "OpEor", "OpSLeft", "OpSRight",
        "OpEq", "OpNe", "OpLt", "OpLe", "OpGt", "OpGe",
        "OpAppend", "OpConv", "OpCast",
    );
}

pub static I32: Type = Type::Integer(i32::MIN + 1, i32::MAX as u32);

/// A value that can be assigned to attributes on a definition of instance
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Null,
    Int(i32),
    // value and database type
    Enum(u8, u16),
    Boolean(bool),
    /// A range
    Float(f64),
    Long(i64),
    Single(f32),
    Text(String),
    /// Call an outside routine with values.
    Call(u32, Vec<Value>),
    /// Call a closure function that allows access to the original stack
    // CCall(Box<Value>, Vec<Value>),
    /// Block with steps and last variable claimed before it.
    Block(Vec<Value>),
    /// Read variable or parameter from stack (nr relative to current function start).
    Var(u32),
    /// Set a variable with an expressions
    Set(u32, Box<Value>),
    // / Read a variable from the closure stack instead of the current function
    // CVar(u32),
    // / Set a closure variable outside the current function
    // CSet(u32, Box<Value>),
    /// Set a new variable with an expressions
    Let(u32, Box<Value>),
    /// Return from a routine with optionally a Value
    Return(Box<Value>),
    /// Break out of the n-th loop
    Break(u16),
    /// Continue the n-th loop
    Continue(u16),
    /// Conditional statement
    If(Box<Value>, Box<Value>, Box<Value>),
    /// Loop through the block till Break is encountered
    Loop(Vec<Value>),
    // / Closure function value with a def-nr and
    // Closure(u32, u32),
    /// Drop the returned value of a call
    Drop(Box<Value>),
    /// The creation of the iterator and the next expression. Not able to revert.
    Iter(Box<Value>, Box<Value>),
}

#[allow(dead_code)]
impl Value {
    #[must_use]
    pub fn str(s: &str) -> Value {
        Value::Text(s.to_string())
    }

    #[must_use]
    pub fn is_op(&self, op: u32) -> bool {
        if let Value::Call(func, _) = self {
            return *func == op;
        }
        false
    }
}

#[must_use]
pub fn to_default(tp: &Type) -> Value {
    match tp {
        Type::Integer(_, _)
        | Type::Boolean
        | Type::Enum(_)
        | Type::Vector(_)
        | Type::Sorted(_, _)
        | Type::Index(_, _)
        | Type::Hash(_, _)
        | Type::Spacial(_, _) => Value::Int(0),
        Type::Long => Value::Long(0),
        Type::Single => Value::Single(0.0),
        Type::Float => Value::Float(0.0),
        Type::Text => Value::Text(String::new()),
        _ => Value::Null,
    }
}

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    /// The type of this parse result is unknown, but linked to a given definition unless 0.
    Unknown(u32),
    /// The type of this result is specifically undefined.
    Null,
    /// Result of a function without return type.
    Void,
    /// The given definition might hold restrictions on this number.
    /// (minimum, maximum).
    Integer(i32, u32),
    /// A store with the given base record type. (nullable)
    Boolean,
    Long,
    Float,
    Single,
    Text,
    /// An enum value. There is always a single parent definition with enum type itself.
    Enum(u32),
    /// A readonly reference to a record instance in a store.
    Reference(u32),
    /// A dynamic vector of a specific type
    Vector(Box<Type>),
    /// A dynamic routine, from a routine definition without code.
    /// The actual code is a routine with this routine as a parent or just a Block for a lambda function.
    Routine(u32),
    /// Linked to the n-th subtype on the first defined parameter
    Subtype(u32),
    /// Iterator with a certain result
    Iterator(Box<Type>),
    /// An ordered vector on a record, the second structure is the (attribute number, descending), third LT function.
    Sorted(u32, Vec<(u16, bool)>),
    /// An index towards other records. The third is the LT function.
    Index(u32, Vec<(u16, bool)>),
    /// An index towards other records. The third is the LT function.
    Spacial(u32, Vec<u16>),
    /// A hash table towards other records. The third is the hash function.
    Hash(u32, Vec<u16>),
    /// A function reference allowing for closures. Argument types and result.
    Function(Vec<Type>, Box<Type>),
}

impl Type {
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown(_))
    }

    #[must_use]
    pub fn is_same(&self, other: &Type) -> bool {
        self == other
            || (matches!(self, Type::Enum(_)) && matches!(other, Type::Enum(_)))
            || (matches!(self, Type::Reference(_)) && matches!(other, Type::Reference(_)))
            || (matches!(self, Type::Vector(_)) && matches!(other, Type::Vector(_)))
            || (matches!(self, Type::Integer(_, _)) && matches!(other, Type::Integer(_, _)))
    }

    #[must_use]
    pub fn size(&self, nullable: bool) -> u8 {
        if let Type::Integer(min, max) = self {
            let c_min = i64::from(*min);
            let c_max = i64::from(*max);
            if c_max - c_min < 256 || (nullable && c_max - c_min == 256) {
                1
            } else if c_max - c_min < 65536 || (nullable && c_max - c_min == 65536) {
                2
            } else {
                4
            }
        } else {
            0
        }
    }

    #[must_use]
    pub fn show(&self, data: &Data) -> String {
        match self {
            Type::Enum(t) | Type::Reference(t) => data.def(*t).name.clone(),
            Type::Vector(tp) if matches!(tp as &Type, Type::Unknown(_)) => "vector".to_string(),
            Type::Vector(tp) => format!("vector<{}>", tp.show(data)),
            Type::Sorted(tp, key) => format!("sorted<{},{key:?}>", data.def(*tp).name),
            Type::Hash(tp, key) => format!("hash<{},{key:?}>", data.def(*tp).name),
            Type::Index(tp, key) => format!("index<{},{key:?}>", data.def(*tp).name),
            Type::Spacial(tp, key) => format!("spacial<{},{key:?}>", data.def(*tp).name),
            Type::Routine(tp) => format!("fn {}[{tp}]", data.def(*tp).name),
            _ => self.to_string(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer(min, max) if *min == i32::MIN + 1 && *max == i32::MAX as u32 => {
                f.write_str("integer")
            }
            Type::Integer(min, max) if *min == 0 && *max == 256 => f.write_str("byte"),
            Type::Vector(tp) if matches!(tp as &Type, Type::Unknown(_)) => f.write_str("vector"),
            _ => f.write_str(&format!("{self:?}").to_lowercase()),
        }
    }
}

pub struct Argument {
    pub name: String,
    pub typedef: Type,
    pub default: Value,
    pub constant: bool,
    pub reference: bool,
}

#[derive(Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct Attribute {
    /// Name of the attribute for this definition
    pub name: String,
    pub typedef: Type,
    /// Is this attribute mutable.
    pub mutable: bool,
    /// Is this a reference to a variable instead of a value
    pub reference: bool,
    /// Is this attribute allowed to be null in the sub-structure.
    pub nullable: bool,
    /// Is this attribute holding the primary reference of its records.
    primary: bool,
    /// The initial value of this attribute if it is not given.
    value: Value,
    /// A test on the validity of this attribute.
    check: Value,
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.name, &self.typedef))
    }
}

/// Per defined variable
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
    /// Definition position of this variable
    pub position: Position,
    /// Is the content of this variable read?
    pub uses: u32,
    pub is_new: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DefType {
    // Not yet known, must be filled in after the first parse pass.
    Unknown,
    // A normal function, cannot be defined twice.
    Function,
    // Dynamic function, where all arguments hold references to multiple implementations we can choose
    Dynamic,
    // The possible values are EnumValue definitions in the childs.
    Enum,
    // The parent is the Enum.
    EnumValue,
    // A structure, with possibly conditional fields in the childs.
    Struct,
    // A vector with a unique content (can be a base Type, Struct, Enum or Vector)
    Vector,
    // A reference to a base type.
    Reference,
    // A hash table definition
    Hash,
    // An index definition
    Index,
    // A radix index definition
    Radix,
    // A type definition, for now only the base types.
    Type,
    // A static constant.
    Constant,
}

impl Display for DefType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{self:?}"))
    }
}

/// Game definition, the data cannot be changed, there can be instances with differences
#[derive(Clone)]
pub struct Definition {
    pub name: String,
    /// Type of definition.
    pub def_type: DefType,
    /// Parent definition for `EnumValue` or `StructPart`. Initial `u32::MAX`.
    pub parent: u32,
    /// The source file position where this is defined, only allow redefinitions within the same file.
    /// This might eventually also limit access to protected internals.
    pub position: Position,
    /// Allowed attributes
    pub attributes: Vec<Attribute>,
    /// Allowed attributes on name
    pub attr_names: HashMap<String, usize>,
    /// Possible code associated with this definition. The attributes are parameters.
    pub code: Value,
    /// Related type for fields, and the return type for functions
    pub returned: Type,
    /// Rust code
    pub rust: String,
    /// Interpreter operator code
    pub op_code: u8,
    /// Position inside the generated code
    pub code_position: u32,
    /// Code length for this function
    pub code_length: u32,
    /// Entry in the known types for the database
    pub known_type: u16,
    /// Known variables inside this definition
    pub variables: Vec<Variable>,
}

impl Definition {
    #[must_use]
    pub fn is_operator(&self) -> bool {
        matches!(self.def_type, DefType::Function)
            && self.name.len() > 2
            && self.name.starts_with("Op")
            && self.name[2..3]
                .chars()
                .next()
                .unwrap_or_default()
                .is_uppercase()
    }
}

#[derive(PartialEq)]

pub enum Context {
    Argument,
    Reference,
    Result,
    Constant,
    Variable,
}

#[allow(dead_code)]
/// The immutable data of a game
pub struct Data {
    pub definitions: Vec<Definition>,
    /// Index on definitions on name
    pub def_names: HashMap<String, u32>,
    used_definitions: HashSet<u32>,
    used_attributes: HashSet<(u32, usize)>,
    /// This definition is referenced by a specific definition, the code is used to update this
    referenced: HashMap<u32, (u32, Value)>,
    /// Static data
    statics: Vec<u8>,
    op_codes: u8,
    possible: HashMap<String, Vec<u32>>,
    operators: HashMap<u8, u32>,
}

impl Data {
    /// A new local variable gets its type via assignment instead of explicit.
    pub fn change_var_type(
        &mut self,
        lexer: &mut Lexer,
        def_nr: u32,
        val: &Value,
        tp: &Type,
    ) -> bool {
        if def_nr == u32::MAX {
            return false;
        }
        // do not expect a field because that should already be a correct value
        let mut new = false;
        if let Value::Var(vnr) = val {
            if self.definitions[def_nr as usize].variables[*vnr as usize].is_new {
                new = true;
                self.definitions[def_nr as usize].variables[*vnr as usize].is_new = false;
            }
            if tp.is_unknown() {
                return new;
            }
            if let Type::Vector(inner) =
                &self.definitions[def_nr as usize].variables[*vnr as usize].var_type
            {
                if let Type::Unknown(_) = **inner {
                    self.definitions[def_nr as usize].variables[*vnr as usize].var_type =
                        tp.clone();
                    if let Type::Vector(inner) = tp {
                        self.vector_def(lexer, inner);
                    }
                }
            } else if self.definitions[def_nr as usize].variables[*vnr as usize]
                .var_type
                .is_unknown()
            {
                self.definitions[def_nr as usize].variables[*vnr as usize].var_type = tp.clone();
            } else if !self.definitions[def_nr as usize].variables[*vnr as usize]
                .var_type
                .is_same(tp)
            {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Cannot change type of variable '{}' from '{}' to '{}'",
                    self.definitions[def_nr as usize].variables[*vnr as usize].name,
                    self.definitions[def_nr as usize].variables[*vnr as usize]
                        .var_type
                        .show(self),
                    tp.show(self)
                );
            }
        }
        new
    }

    pub fn test_used(&self, def_nr: u32, diagnostics: &mut Diagnostics) {
        let parameters = self.def(def_nr).attributes.len();
        for (nr, var) in self.def(def_nr).variables.iter().enumerate() {
            if var.uses == 0 && !var.name.starts_with('_') {
                // TODO: add the location of the definition of this variable.
                diagnostics.add(
                    Level::Warning,
                    &format!(
                        "{} {} is never read in {} line {}:{}",
                        if nr < parameters {
                            "Parameter"
                        } else {
                            "Variable"
                        },
                        var.name,
                        var.position.file,
                        var.position.line,
                        var.position.pos
                    ),
                );
            }
        }
    }
}

#[must_use]
pub fn v_if(test: Value, t: Value, f: Value) -> Value {
    Value::If(Box::new(test), Box::new(t), Box::new(f))
}

#[must_use]
pub fn v_set(var: u32, value: Value) -> Value {
    Value::Set(var, Box::new(value))
}

#[must_use]
pub fn v_let(var: u32, value: Value) -> Value {
    Value::Let(var, Box::new(value))
}

impl Display for Definition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", &self.name, &self.def_type)
    }
}

impl Default for Data {
    fn default() -> Self {
        Self::new()
    }
}

struct Into {
    str: String,
}

impl Write for Into {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        self.str += &String::from_utf8_lossy(buf);
        Ok(self.str.len())
    }

    fn flush(&mut self) -> Result<()> {
        Ok(())
    }

    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        self.write(buf)?;
        Ok(())
    }
}

#[allow(dead_code)]
impl Data {
    #[must_use]
    pub fn new() -> Data {
        Data {
            definitions: Vec::new(),
            def_names: HashMap::new(),
            used_definitions: HashSet::new(),
            used_attributes: HashSet::new(),
            referenced: HashMap::new(),
            statics: Vec::new(),
            op_codes: 0,
            possible: HashMap::new(),
            operators: HashMap::new(),
        }
    }

    /// Allow a new attribute on a definition with a specified type.
    pub fn add_attribute(
        &mut self,
        lexer: &mut Lexer,
        on_def: u32,
        name: &str,
        typedef: Type,
        reference: bool,
    ) -> usize {
        if self.def(on_def).attr_names.contains_key(name) {
            let orig_attr = self.def(on_def).attr_names[name];
            let attr = &self.def(on_def).attributes[orig_attr];
            if attr.typedef.is_unknown() {
                if attr.typedef == typedef {
                    diagnostic!(
                        lexer,
                        Level::Error,
                        "Double attribute '{}.{name}'",
                        self.def(on_def).name
                    );
                } else {
                    diagnostic!(
                        lexer,
                        Level::Error,
                        "Cannot change the type of attribute: {}.{name}",
                        self.def(on_def).name
                    );
                }
            }
            return orig_attr;
        }
        let attr = Attribute {
            name: name.to_string(),
            typedef,
            mutable: true,
            nullable: true,
            primary: false,
            reference,
            value: Value::Null,
            check: Value::Null,
        };
        let next_attr = self.def(on_def).attributes.len();
        let def = &mut self.definitions[on_def as usize];
        def.attr_names.insert(name.to_string(), next_attr);
        def.attributes.push(attr);
        next_attr
    }

    /**
        Add a definitions.
        # Panics
        Will panic if a definition with the same name already exists.
    */
    pub fn add_def(&mut self, name: &str, position: &Position, def_type: DefType) -> u32 {
        let rec = self.definitions();
        assert!(
            !self.def_names.contains_key(name),
            "Dual definition of {name} at {position}"
        );
        self.def_names.insert(name.to_string(), rec);
        let mut new_def = Definition {
            name: name.to_string(),
            position: position.clone(),
            def_type,
            parent: u32::MAX,
            attributes: Vec::default(),
            attr_names: HashMap::default(),
            code: Value::Null,
            returned: Type::Unknown(rec),
            rust: String::new(),
            op_code: 255,
            known_type: u16::MAX,
            code_position: 0,
            code_length: 0,
            variables: Vec::new(),
        };
        if new_def.is_operator() {
            new_def.op_code = self.op_codes;
            self.op_codes += 1;
            self.operators.insert(new_def.op_code, rec);
        }
        self.definitions.push(new_def);
        rec
    }

    #[must_use]
    /// # Panics
    /// When an operator is searched that is currently not known.
    pub fn get_possible(&self, start: &str, lexer: &Lexer) -> &Vec<u32> {
        assert!(
            self.possible.contains_key(start),
            "Unknown operator {start} at {}",
            lexer.pos()
        );
        &self.possible[start]
    }

    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn definitions(&self) -> u32 {
        self.definitions.len() as u32
    }

    #[must_use]
    pub fn def_referenced(&self, d_nr: u32) -> bool {
        self.referenced.contains_key(&d_nr)
    }

    pub fn set_referenced(&mut self, d_nr: u32, t_nr: u32, change: Value) {
        if d_nr != u32::MAX {
            self.referenced.insert(d_nr, (t_nr, change));
        }
    }

    #[must_use]
    pub fn def_type(&self, d_nr: u32) -> DefType {
        if d_nr == u32::MAX {
            DefType::Unknown
        } else {
            self.def(d_nr).def_type.clone()
        }
    }

    /**
    Set the return type on a definition.
    # Panics
    When the return type was already set before.
    */
    pub fn set_returned(&mut self, d_nr: u32, tp: Type) {
        assert!(
            self.def(d_nr).returned.is_unknown(),
            "Cannot change returned type on [{d_nr}]{} to {tp} twice was {} at {:?}",
            self.def(d_nr).name,
            self.def(d_nr).returned,
            self.def(d_nr).position
        );
        self.definitions[d_nr as usize].returned = tp;
    }

    #[must_use]
    pub fn attributes(&self, d_nr: u32) -> usize {
        self.def(d_nr).attributes.len()
    }

    #[must_use]
    pub fn attr(&self, d_nr: u32, name: &str) -> usize {
        if let Some(nr) = self.def(d_nr).attr_names.get(name) {
            *nr
        } else {
            usize::MAX
        }
    }

    #[must_use]
    pub fn attr_name(&self, d_nr: u32, a_nr: usize) -> String {
        self.def(d_nr).attributes[a_nr].name.clone()
    }

    #[must_use]
    pub fn attr_type(&self, d_nr: u32, a_nr: usize) -> Type {
        if a_nr == usize::MAX {
            self.def(d_nr).returned.clone()
        } else {
            self.def(d_nr).attributes[a_nr].typedef.clone()
        }
    }

    /**
    Write the type on an attribute of a definition.
    # Panics
    When the type was already set before.
    */
    pub fn set_attr_type(&mut self, d_nr: u32, a_nr: usize, tp: Type) {
        if a_nr == usize::MAX || !self.attr_type(d_nr, a_nr).is_unknown() {
            panic!(
                "Cannot set attribute type {}.{} twice was {} to {tp}",
                self.def(d_nr).name,
                self.attr_name(d_nr, a_nr),
                self.attr_type(d_nr, a_nr)
            );
        } else {
            self.definitions[d_nr as usize].attributes[a_nr].typedef = tp;
        }
    }

    #[must_use]
    pub fn attr_value(&self, d_nr: u32, a_nr: usize) -> Value {
        self.def(d_nr).attributes[a_nr].value.clone()
    }

    /**
    Write the default value of an attribute of a definition.
    # Panics
    When the value was already set before.
    */
    pub fn set_attr_value(&mut self, d_nr: u32, a_nr: usize, val: Value) {
        self.definitions[d_nr as usize].attributes[a_nr].value = val;
    }

    #[must_use]
    pub fn attr_check(&self, d_nr: u32, a_nr: u16) -> Value {
        self.def(d_nr).attributes[a_nr as usize].check.clone()
    }

    /**
    Write the check value of an attribute of a definition.
    # Panics
    When the value was already set before.
    */
    pub fn set_attr_check(&mut self, d_nr: u32, a_nr: usize, check: Value) {
        assert_eq!(
            self.def(d_nr).attributes[a_nr].value,
            Value::Null,
            "Cannot set attribute value twice"
        );
        self.definitions[d_nr as usize].attributes[a_nr].check = check;
    }

    #[must_use]
    pub fn attr_nullable(&self, d_nr: u32, a_nr: usize) -> bool {
        if a_nr == usize::MAX {
            return false;
        }
        self.definitions[d_nr as usize].attributes[a_nr].nullable
    }

    pub fn set_attr_nullable(&mut self, d_nr: u32, a_nr: usize, nullable: bool) {
        self.definitions[d_nr as usize].attributes[a_nr].nullable = nullable;
    }

    #[must_use]
    pub fn attr_mutable(&self, d_nr: u32, a_nr: usize) -> bool {
        self.definitions[d_nr as usize].attributes[a_nr].mutable
    }

    pub fn set_attr_mutable(&mut self, d_nr: u32, a_nr: usize, mutable: bool) {
        self.definitions[d_nr as usize].attributes[a_nr].mutable = mutable;
    }

    /**
    Add a new function to the definitions.
    # Panics
    When the return type cannot be parsed.
    */
    pub fn add_fn(&mut self, lexer: &mut Lexer, fn_name: &str, arguments: &[Argument]) -> u32 {
        let mut name = fn_name.to_string();
        let is_self = !arguments.is_empty() && arguments[0].name == "self";
        let is_both = !arguments.is_empty() && arguments[0].name == "both";
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].typedef);
            if type_nr == u32::MAX {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Unknown type {:?} on {fn_name}",
                    arguments[0].name
                );
            } else {
                name = format!("_tp_{}_{fn_name}", self.def(type_nr).name);
            }
        }
        let mut d_nr = self.def_nr(&name);
        if d_nr != u32::MAX {
            diagnostic!(
                lexer,
                Level::Error,
                "Cannot redefine {:?} {fn_name}",
                self.def_type(d_nr)
            );
            return u32::MAX;
        }
        d_nr = self.add_def(&name, lexer.pos(), DefType::Function);
        for a in arguments {
            let a_nr = self.add_attribute(lexer, d_nr, &a.name, a.typedef.clone(), a.reference);
            self.set_attr_value(d_nr, a_nr, a.default.clone());
            self.set_attr_mutable(d_nr, a_nr, !a.constant);
        }
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].typedef);
            let a_nr = self.add_attribute(lexer, type_nr, fn_name, Type::Routine(d_nr), false);
            self.set_attr_mutable(type_nr, a_nr, false);
        }
        if is_both {
            let mut main = self.def_nr(fn_name);
            if main == u32::MAX {
                main = self.add_def(fn_name, lexer.pos(), DefType::Dynamic);
            }
            let type_nr = self.type_def_nr(&arguments[0].typedef);
            assert_ne!(
                type_nr,
                u32::MAX,
                "Unknown type {}: {:?} at {}",
                arguments[0].name,
                arguments[0].typedef,
                lexer.pos()
            );
            let name = &self.def(type_nr).name.clone();
            let a_nr = self.add_attribute(lexer, main, name, Type::Routine(d_nr), false);
            self.set_attr_mutable(main, a_nr, false);
        }
        d_nr
    }

    #[must_use]
    pub fn get_fn(&self, fn_name: &str, arguments: &[Argument]) -> u32 {
        let is_self = !arguments.is_empty() && arguments[0].name == "self";
        let is_both = !arguments.is_empty() && arguments[0].name == "both";
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].typedef);
            let name = format!("_tp_{}_{fn_name}", self.def(type_nr).name);
            self.def_nr(&name)
        } else {
            self.def_nr(fn_name)
        }
    }

    /**
    Add a new operator
    # Panics
    When operators are not scanned correctly.
    */
    pub fn add_op(&mut self, lexer: &mut Lexer, fn_name: &str, arguments: &[Argument]) -> u32 {
        let d_nr = self.add_def(fn_name, lexer.pos(), DefType::Function);
        for a in arguments {
            let a_nr = self.add_attribute(lexer, d_nr, &a.name, a.typedef.clone(), a.reference);
            self.set_attr_mutable(d_nr, a_nr, !a.constant);
            self.set_attr_value(d_nr, a_nr, a.default.clone());
        }
        if self.def(d_nr).is_operator() {
            for op in OPERATORS.iter() {
                if self.def(d_nr).name.starts_with("OpGen") {
                    continue;
                }
                if self.def(d_nr).name.starts_with(op) {
                    if !self.possible.contains_key(*op) {
                        self.possible.insert((*op).to_string(), Vec::new());
                    }
                    self.possible.get_mut(*op).unwrap().push(d_nr);
                }
            }
        }
        d_nr
    }

    /// Get a vector definition. This is a record with a single field pointing towards this vector.
    /// We need this definition as the primary record of a database holding a vector and its child records/vectors.
    pub fn vector_def(&mut self, lexer: &mut Lexer, tp: &Type) -> u32 {
        let name = format!("main_vector<{}>", tp.show(self));
        let d_nr = self.def_nr(&name);
        if d_nr == u32::MAX {
            let vd = self.add_def(&name, lexer.pos(), DefType::Struct);
            self.add_attribute(
                lexer,
                vd,
                "vector",
                Type::Vector(Box::new(tp.clone())),
                false,
            );
            vd
        } else {
            d_nr
        }
    }

    pub fn check_vector(&mut self, d_nr: u32, vec_tp: u16, pos: &Position) -> u32 {
        let vec_name = format!("vector<{}>", self.def(d_nr).name);
        let mut v_nr = self.def_nr(&vec_name);
        if v_nr == u32::MAX {
            v_nr = self.add_def(&vec_name, pos, DefType::Vector);
            self.definitions[v_nr as usize].parent = d_nr;
            self.definitions[v_nr as usize].known_type = vec_tp;
        };
        v_nr
    }

    /// Get the corresponding number of a definition on name.
    #[must_use]
    pub fn def_nr(&self, name: &str) -> u32 {
        let Some(nr) = self.def_names.get(name) else {
            return u32::MAX;
        };
        *nr
    }

    /** Get the definition by name
    # Panics
    When an unknown definition is requested
    */
    #[must_use]
    pub fn def_name(&self, name: &str) -> &Definition {
        let Some(nr) = self.def_names.get(name) else {
            panic!("Unknown definition {name}");
        };
        &self.definitions[*nr as usize]
    }

    #[must_use]
    pub fn def(&self, dnr: u32) -> &Definition {
        &self.definitions[dnr as usize]
    }

    #[must_use]
    pub fn has_op(&self, op: u8) -> bool {
        self.operators.contains_key(&op)
    }

    #[must_use]
    pub fn operator(&self, op: u8) -> &Definition {
        self.def(self.operators[&op])
    }

    pub fn attr_used(&mut self, d_nr: u32, a_nr: usize) {
        self.used_attributes.insert((d_nr, a_nr));
    }

    pub fn def_used(&mut self, d_nr: u32) {
        self.used_definitions.insert(d_nr);
    }

    #[must_use]
    pub fn show_type(&self, show: &Type) -> String {
        match show {
            Type::Reference(dnr) | Type::Enum(dnr) => self.def(*dnr).name.clone(),
            Type::Vector(sub) => format!("vector<{}>", self.show_type(sub)),
            Type::Routine(sub) => format!("fn {}", self.def(*sub).name),
            _ => format!("{show}"),
        }
    }

    #[must_use]
    pub fn type_def_nr(&self, tp: &Type) -> u32 {
        match tp {
            Type::Integer(_, _) => self.def_nr("integer"),
            Type::Long => self.def_nr("long"),
            Type::Boolean => self.def_nr("boolean"),
            Type::Float => self.def_nr("float"),
            Type::Text => self.def_nr("text"),
            Type::Single => self.def_nr("single"),
            Type::Routine(d_nr)
            | Type::Enum(d_nr)
            | Type::Reference(d_nr)
            | Type::Unknown(d_nr) => *d_nr,
            Type::Vector(_) => self.def_nr("vector"),
            Type::Sorted(_, _) => self.def_nr("reference"),
            Type::Index(_, _) => self.def_nr("index"),
            Type::Hash(_, _) => self.def_nr("hash"),
            _ => u32::MAX,
        }
    }

    #[must_use]
    /// Get the definition number for the given type.
    pub fn type_elm(&self, tp: &Type) -> u32 {
        match tp {
            Type::Integer(_, _) => self.def_nr("integer"),
            Type::Long => self.def_nr("long"),
            Type::Boolean => self.def_nr("boolean"),
            Type::Float => self.def_nr("float"),
            Type::Text => self.def_nr("text"),
            Type::Routine(d_nr) | Type::Enum(d_nr) | Type::Reference(d_nr) => *d_nr,
            Type::Vector(tp) => {
                if let Type::Reference(td) = **tp {
                    td
                } else {
                    self.type_def_nr(tp)
                }
            }
            Type::Sorted(_, _) | Type::Index(_, _) | Type::Hash(_, _) => self.def_nr("reference"),
            _ => u32::MAX,
        }
    }

    /**
    Return the rust type on a definitions.
    # Panics
    When the rust type cannot be determined.
    */
    #[must_use]
    #[allow(clippy::needless_pass_by_value)]
    pub fn rust_type(tp: &Type, context: Context) -> String {
        if context == Context::Reference {
            let mut result = String::new();
            result += "&";
            result += &Self::rust_type(tp, Context::Argument);
            return result;
        }
        match tp {
            Type::Integer(from, to)
                if i64::from(*to) - i64::from(*from) <= 255 && i64::from(*from) >= 0 =>
            {
                "u8"
            }
            Type::Integer(from, to)
                if i64::from(*to) - i64::from(*from) <= 65536 && i64::from(*from) >= 0 =>
            {
                "u16"
            }
            Type::Integer(from, to) if i64::from(*to) - i64::from(*from) <= 255 => "i8",
            Type::Integer(from, to) if i64::from(*to) - i64::from(*from) <= 65536 => "i16",
            Type::Integer(_, _) => "i32",
            Type::Enum(_) => "u8",
            Type::Text if context == Context::Variable => "String",
            Type::Text => "Str",
            Type::Long => "i64",
            Type::Boolean => "bool",
            Type::Float => "f64",
            Type::Single => "f32",
            Type::Reference(_)
            | Type::Vector(_)
            | Type::Hash(_, _)
            | Type::Sorted(_, _)
            | Type::Index(_, _) => "DbRef",
            Type::Routine(_) => "u32",
            Type::Subtype(_) => "T",
            Type::Unknown(_) => "??",
            _ => panic!("Incorrect type {tp}"),
        }
        .to_string()
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn find_unused(&self, diagnostics: &mut Diagnostics) {
        for (d_nr, def) in self.definitions.iter().enumerate() {
            if self.used_definitions.contains(&(d_nr as u32)) {
                for (a_nr, attr) in def.attributes.iter().enumerate() {
                    if !self.used_attributes.contains(&(d_nr as u32, a_nr)) {
                        diagnostics.add(
                            Level::Warning,
                            &format!("Unused field {}.{} {:?}", def.name, attr.name, def.position),
                        );
                    }
                }
            } else {
                diagnostics.add(
                    Level::Warning,
                    &format!("Unused definition {} {:?}", def.name, def.position),
                );
            }
        }
    }

    /**
    Dump the internal parse tree to the standard output.
    # Panics
    Will not, this is to internal data structures instead of a file.
    */
    pub fn dump(&self, value: &Value, d_nr: u32) {
        let mut s = Into { str: String::new() };
        self.show_code(&mut s, d_nr, value, 0, true).unwrap();
        println!("{}", s.str);
    }

    /**
    Dump the internal parse tree to file.
    # Errors
    When the file cannot be written.
    */
    pub fn show_code(
        &self,
        write: &mut dyn Write,
        d_nr: u32,
        value: &Value,
        indent: u32,
        start: bool,
    ) -> Result<()> {
        if start {
            for _i in 0..indent {
                write!(write, "  ")?;
            }
        }
        match value {
            Value::Null => write!(write, "null"),
            Value::Int(i) => write!(write, "{i}i32"),
            Value::Enum(e, tp) => write!(write, "{e}u8({tp})"),
            Value::Boolean(true) => write!(write, "true"),
            Value::Boolean(_) => write!(write, "false"),
            Value::Float(f) => write!(write, "{f}f64"),
            Value::Long(l) => write!(write, "{l}i64"),
            Value::Single(f) => write!(write, "{f}f32"),
            Value::Text(t) => write!(write, "\"{t}\""),
            Value::Iter(start, next) => {
                self.show_code(write, d_nr, start, indent + 1, true)?;
                write!(write, "loop {{")?;
                self.show_code(write, d_nr, next, indent + 1, false)?;
                write!(write, "}}")
            }
            Value::Call(t, ex) => {
                write!(write, "{}(", self.def(*t).name)?;
                for (v_nr, v) in ex.iter().enumerate() {
                    if v_nr > 0 {
                        write!(write, ", ")?;
                    }
                    self.show_code(write, d_nr, v, indent, false)?;
                }
                write!(write, ")")
            }
            Value::Block(v) => {
                writeln!(write, "{{")?;
                for val in v {
                    self.show_code(write, d_nr, val, indent + 1, true)?;
                    writeln!(write, ";")?;
                }
                for _i in 0..indent {
                    write!(write, "  ")?;
                }
                write!(write, "}}")
            }
            Value::Var(v) => write!(write, "{}", self.def(d_nr).variables[*v as usize].name),
            Value::Set(v, to) => {
                write!(write, "{} = ", self.def(d_nr).variables[*v as usize].name)?;
                self.show_code(write, d_nr, to, indent, false)
            }
            Value::Let(v, to) => {
                write!(
                    write,
                    "let {} = ",
                    self.def(d_nr).variables[*v as usize].name
                )?;
                self.show_code(write, d_nr, to, indent, false)
            }
            Value::Return(ex) => {
                write!(write, "return ")?;
                self.show_code(write, d_nr, ex, indent, false)
            }
            Value::Break(v) => write!(write, "break({v})"),
            Value::Continue(v) => write!(write, "continue({v})"),
            Value::If(test, t, f) => {
                write!(write, "if ")?;
                self.show_code(write, d_nr, test, indent, false)?;
                write!(write, " {{")?;
                self.show_code(write, d_nr, t, indent, false)?;
                write!(write, "}} else {{")?;
                self.show_code(write, d_nr, f, indent, false)?;
                write!(write, "}}")
            }
            Value::Loop(v) => {
                writeln!(write, "loop {{")?;
                for val in v {
                    self.show_code(write, d_nr, val, indent + 1, true)?;
                    writeln!(write, ";")?;
                }
                for _i in 0..indent {
                    write!(write, "  ")?;
                }
                write!(write, "}}")
            }
            Value::Drop(v) => {
                write!(write, "drop ")?;
                self.show_code(write, d_nr, v, indent, false)
            }
        }
    }

    pub fn show(&self) {
        for d in &self.definitions {
            if d.position.file == "default/01_code.gcp" {
                continue;
            }
            print!("{} {}", d.position.file, d.name);
            if !d.attributes.is_empty() {
                print!("(");
                for a in &d.attributes {
                    print!("{}:{:?}, ", a.name, a.typedef);
                }
                print!(")");
            }
            if !d.returned.is_unknown() {
                print!(" -> {:?}", d.returned);
            }
            println!();
        }
    }
}
