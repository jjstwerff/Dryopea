// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Hold all definitions
//! Those are the combinations of types, records and routines.
//! Many definitions can hold fields of their own, a routine
//! has parameters that behave very similar to fields.

// These structures are rather inefficient right now, but they are be the basis
// for a far more efficient database design later.

extern crate strum_macros;
use crate::database::{DbRef, KnownTypes};
use crate::diagnostics::*;
use crate::lexer::{Lexer, Position};
use crate::types::Types;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::io::{Result, Write};

/// A value that can be assigned to attributes on a definition of instance
#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Value {
    Null,
    Int(i32),
    Boolean(bool),
    /// A record or field with 1:database, 2:allocation, 3:field_position.
    /// The position is in bytes relative to the allocation start. Updated on insert/remove of vector elements.
    Reference(DbRef),
    /// A range
    Range(Box<Value>, Box<Value>),
    Float(f64),
    Long(i64),
    Single(f32),
    /// Dynamic text with an efficient appender.
    Text(String),
    /// Call an outside routine with values.
    Call(u32, Vec<Value>),
    /// Call a closure function that allows access to the original stack
    // CCall(Box<Value>, Vec<Value>),
    /// Block with number of variables and steps.
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
}

#[allow(dead_code)]
impl Value {
    pub fn str(s: &str) -> Value {
        Value::Text(s.to_string())
    }
}

pub fn to_default(tp: &Type) -> Value {
    match tp {
        Type::Integer
        | Type::Boolean
        | Type::Enum(_)
        | Type::Vector(_)
        | Type::Sorted(_, _)
        | Type::Index(_, _)
        | Type::Hash(_, _)
        | Type::Radix(_, _) => Value::Int(0),
        Type::Long => Value::Long(0),
        Type::Single => Value::Single(0.0),
        Type::Float => Value::Float(0.0),
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
    Integer,
    /// A store with the given base record type.
    Boolean,
    Long,
    Float,
    Single,
    Text,
    /// A part of the static program data, often a text but possibly a structure.
    Data,
    /// An enum value. There is always a single parent definition with enum type itself.
    Enum(u32),
    /// A readonly reference to a record instance in a store.
    Reference(u32),
    /// A record that is placed inside another object. Like compact vectors or parent objects.
    Inner(u32),
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
    /// Inside index records the links to the next & previous element.
    Link,
    /// An index towards other records. The third is the LT function.
    Radix(u32, Vec<u16>),
    /// A hash table towards other records. The third is the hash function.
    Hash(u32, Vec<u16>),
    /// A function reference allowing for closures. Argument types and result.
    Function(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown(_))
    }

    pub fn is_same(&self, other: &Type) -> bool {
        self == other
            || (matches!(self, Type::Enum(_)) && matches!(other, Type::Enum(_)))
            || (matches!(self, Type::Reference(_)) && matches!(other, Type::Reference(_)))
            || (matches!(self, Type::Vector(_)) && matches!(other, Type::Vector(_)))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{self:?}").to_lowercase())
    }
}

pub type Arguments = Vec<(String, Type, Value)>;

#[derive(Clone)]
#[allow(dead_code)]
struct Attribute {
    /// Name of the attribute for this definition
    name: String,
    typedef: Type,
    /// Position of this Attribute relative to the start of the definition.
    /// Initial: u32::MAX
    position: u32,
    /// Is this attribute mutable.
    mutable: bool,
    /// Is this attribute allowed to be null in the sub-structure.
    nullable: bool,
    /// Is this attribute holding the primary reference of its records.
    primary: bool,
    /// The initial value of this attribute if it is not given.
    value: Value,
    /// A test on the validity of this attribute.
    check: Value,
    /// Restrictions: Value::Null is nothing, on text it is the allowed size as Value::Integer
    min: i32,
    max: i32,
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.position == u32::MAX {
            f.write_str(&format!("{}:{}", self.name, &self.typedef))
        } else {
            f.write_str(&format!(
                "{}:{}[{}]",
                self.name, &self.typedef, self.position
            ))
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum DefType {
    // Not yet known, must be filled in after the first parse pass.
    Unknown,
    // A normal function, cannot be defined twice.
    // With parent>0 this is a member implementation with name <struct>::<method> and the first argument is the struct.
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
    // A type definition, for now only the base types.
    Type,
    // A static constant.
    Constant,
}

impl Display for DefType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}

/// Game definition, the data cannot be changed, there can be instances with differences
#[derive(Clone)]
struct Definition {
    name: String,
    /// Type of definition.
    def_type: DefType,
    /// Parent definition for EnumValue or StructPart. Initial u32::MAX.
    parent: u32,
    /// The source file position where this is defined, only allow redefinitions within the same file.
    /// This might eventually also limit access to protected internals.
    position: Position,
    /// Allowed attributes
    attributes: Vec<Attribute>,
    /// Allowed attributes on name
    attr_names: HashMap<String, u16>,
    /// Possible code associated with this definition. The attributes are parameters.
    code: Value,
    /// The needed alignment of the fields of this definition in bytes
    alignment: u8,
    /// The total size needed for this definition in 8 byte words
    size: u32,
    /// Related type for fields, and the return type for functions
    returned: Type,
    /// Rust code
    rust: String,
    /// Entry in the known types for the database
    known_type: u16,
}

#[allow(dead_code)]
/// The immutable data of a game
pub struct Data {
    definitions: Vec<Definition>,
    /// Index on definitions on name
    def_names: HashMap<String, u32>,
    used_definitions: HashSet<u32>,
    used_attributes: HashSet<(u32, u16)>,
    /// This definition is referenced by a specific definition, the code is used to update this
    referenced: HashMap<u32, (u32, Value)>,
    /// Static data
    data: Vec<u8>,
    pub known_types: KnownTypes,
}

pub fn v_if(test: Value, t: Value, f: Value) -> Value {
    Value::If(Box::new(test), Box::new(t), Box::new(f))
}

pub fn v_set(var: u32, val: Value) -> Value {
    Value::Set(var, Box::new(val))
}

pub fn v_let(var: u32, val: Value) -> Value {
    Value::Let(var, Box::new(val))
}

pub fn text(s: &str) -> Value {
    Value::Text(s.to_string())
}

impl Debug for Definition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Definition")
            .field("name", &self.name)
            .field("type", &self.def_type)
            .field("attributes", &self.attributes)
            .field("return", &self.returned)
            .finish()
    }
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

#[allow(dead_code)]
impl Data {
    pub fn new() -> Data {
        Data {
            definitions: Vec::new(),
            def_names: HashMap::new(),
            used_definitions: HashSet::new(),
            used_attributes: HashSet::new(),
            referenced: HashMap::new(),
            data: Vec::new(),
            known_types: KnownTypes::new(),
        }
    }

    /// Allow a new attribute on a definition with a specified type.
    pub fn add_attribute(
        &mut self,
        lexer: &mut Lexer,
        on_def: u32,
        name: &str,
        typedef: Type,
    ) -> u16 {
        if self.def(on_def).attr_names.contains_key(name) {
            let orig_attr = self.def(on_def).attr_names[name];
            let attr = &self.def(on_def).attributes[orig_attr as usize];
            if attr.typedef.is_unknown() {
                if attr.typedef != typedef {
                    diagnostic!(
                        lexer,
                        Level::Error,
                        "Cannot change the type of attribute: {}.{name}",
                        self.def(on_def).name
                    );
                } else {
                    diagnostic!(
                        lexer,
                        Level::Error,
                        "Double attribute '{}.{name}'",
                        self.def(on_def).name
                    );
                }
            }
            return orig_attr;
        }
        let attr = Attribute {
            name: name.to_string(),
            typedef,
            position: u32::MAX,
            mutable: true,
            nullable: true,
            primary: false,
            value: Value::Null,
            check: Value::Null,
            min: i32::MIN,
            max: i32::MAX,
        };
        let next_attr = self.def(on_def).attributes.len() as u16;
        let def = &mut self.definitions[on_def as usize];
        def.attr_names.insert(name.to_string(), next_attr);
        def.attributes.push(attr);
        next_attr
    }

    pub fn add_def(&mut self, name: String, position: Position, def_type: DefType) -> u32 {
        let rec = self.definitions.len() as u32;
        if self.def_names.contains_key(&name) {
            panic!("Dual definition of {name}");
        }
        self.def_names.insert(name.clone(), rec);
        self.definitions.push(Definition {
            name,
            position,
            def_type,
            parent: u32::MAX,
            attributes: Default::default(),
            attr_names: Default::default(),
            code: Value::Null,
            alignment: 1,
            size: 0,
            returned: Type::Unknown(rec),
            rust: "".to_string(),
            known_type: u16::MAX,
        });
        rec
    }

    pub fn definitions(&self) -> u32 {
        self.definitions.len() as u32
    }

    pub fn def_name(&self, d_nr: u32) -> String {
        self.def(d_nr).name.clone()
    }

    pub fn def_parent(&self, d_nr: u32) -> u32 {
        self.def(d_nr).parent
    }

    pub fn set_parent(&mut self, d_nr: u32, p_nr: u32) {
        self.definitions[d_nr as usize].parent = p_nr;
    }

    pub fn def_referenced(&self, d_nr: u32) -> bool {
        self.referenced.contains_key(&d_nr)
    }

    pub fn set_referenced(&mut self, d_nr: u32, t_nr: u32, change: Value) {
        if d_nr != u32::MAX {
            self.referenced.insert(d_nr, (t_nr, change));
        }
    }

    // Is this object an inner type
    pub fn def_inner(&self, d_nr: u32) -> bool {
        matches!(self.def(d_nr).returned, Type::Inner(_))
    }

    pub fn def_type(&self, d_nr: u32) -> DefType {
        if d_nr == u32::MAX {
            DefType::Unknown
        } else {
            self.def(d_nr).def_type.clone()
        }
    }

    pub fn set_def_type(&mut self, d_nr: u32, val: DefType) {
        if self.def_type(d_nr) != DefType::Unknown {
            panic!("Cannot change def_type");
        }
        self.definitions[d_nr as usize].def_type = val;
    }

    pub fn def_pos(&self, d_nr: u32) -> Position {
        self.def(d_nr).position.clone()
    }

    pub fn set_def_pos(&mut self, d_nr: u32, pos: Position) {
        self.definitions[d_nr as usize].position = pos;
    }

    pub fn def_set_size(&mut self, d_nr: u32, size: u32, align: u8) {
        if self.def(d_nr).size != 0 {
            panic!("Cannot change definition size twice");
        }
        self.definitions[d_nr as usize].size = size;
        self.definitions[d_nr as usize].alignment = align;
    }

    /// Size of this definition in 8 byte words
    pub fn def_size(&self, d_nr: u32) -> u32 {
        self.def(d_nr).size
    }

    pub fn def_align(&self, d_nr: u32) -> u8 {
        self.def(d_nr).alignment
    }

    pub fn set_known_type(&mut self, d_nr: u32, known_type: u16) {
        self.definitions[d_nr as usize].known_type = known_type;
    }

    pub fn def_known_type(&self, d_nr: u32) -> u16 {
        self.def(d_nr).known_type
    }

    pub fn rust(&mut self, d_nr: u32) -> String {
        self.def(d_nr).rust.clone()
    }

    pub fn set_rust(&mut self, d_nr: u32, rust: String) {
        self.definitions[d_nr as usize].rust = rust;
    }

    pub fn set_returned(&mut self, d_nr: u32, tp: Type) {
        if !self.def(d_nr).returned.is_unknown() {
            panic!(
                "Cannot change returned type on [{d_nr}]{} to {tp} twice was {} at {:?}",
                self.def(d_nr).name,
                self.def(d_nr).returned,
                self.def_pos(d_nr)
            );
        }
        self.definitions[d_nr as usize].returned = tp;
    }

    pub fn returned(&self, d_nr: u32) -> Type {
        self.def(d_nr).returned.clone()
    }

    pub fn attributes(&self, d_nr: u32) -> u16 {
        self.def(d_nr).attributes.len() as u16
    }

    pub fn attr(&self, d_nr: u32, name: &str) -> u16 {
        if let Some(nr) = self.def(d_nr).attr_names.get(name) {
            *nr
        } else {
            u16::MAX
        }
    }

    pub fn attr_name(&self, d_nr: u32, a_nr: u16) -> String {
        self.def(d_nr).attributes[a_nr as usize].name.clone()
    }

    pub fn attr_pos(&self, d_nr: u32, a_nr: u16) -> u32 {
        if a_nr == u16::MAX {
            0
        } else {
            self.def(d_nr).attributes[a_nr as usize].position
        }
    }

    pub fn set_attr_pos(&mut self, d_nr: u32, a_nr: u16, pos: u32) {
        if self.def(d_nr).attributes[a_nr as usize].position != u32::MAX {
            panic!("Cannot set attribute position twice");
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].position = pos;
    }

    pub fn attr_type(&self, d_nr: u32, a_nr: u16) -> Type {
        if a_nr == u16::MAX {
            self.def(d_nr).returned.clone()
        } else {
            self.def(d_nr).attributes[a_nr as usize].typedef.clone()
        }
    }

    pub fn set_attr_type(&mut self, d_nr: u32, a_nr: u16, tp: Type) {
        if a_nr == u16::MAX || !self.attr_type(d_nr, a_nr).is_unknown() {
            panic!(
                "Cannot set attribute type {}.{} twice was {} to {tp}",
                self.def_name(d_nr),
                self.attr_name(d_nr, a_nr),
                self.attr_type(d_nr, a_nr)
            );
        } else {
            self.definitions[d_nr as usize].attributes[a_nr as usize].typedef = tp;
        }
    }

    pub fn attr_value(&self, d_nr: u32, a_nr: u16) -> Value {
        self.def(d_nr).attributes[a_nr as usize].value.clone()
    }

    pub fn set_attr_value(&mut self, d_nr: u32, a_nr: u16, val: Value) {
        if self.def(d_nr).attributes[a_nr as usize].value != Value::Null {
            panic!("Cannot set attribute value twice");
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].value = val;
    }

    pub fn attr_check(&self, d_nr: u32, a_nr: u16) -> Value {
        self.def(d_nr).attributes[a_nr as usize].check.clone()
    }

    pub fn set_attr_check(&mut self, d_nr: u32, a_nr: u16, check: Value) {
        if self.def(d_nr).attributes[a_nr as usize].value != Value::Null {
            panic!("Cannot set attribute value twice");
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].check = check;
    }

    pub fn attr_min(&mut self, d_nr: u32, a_nr: u16) -> i32 {
        if a_nr == u16::MAX {
            return i32::MIN;
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].min
    }

    pub fn set_attr_min(&mut self, d_nr: u32, a_nr: u16, min: i32) {
        self.definitions[d_nr as usize].attributes[a_nr as usize].min = min;
    }

    pub fn attr_max(&mut self, d_nr: u32, a_nr: u16) -> i32 {
        if a_nr == u16::MAX {
            return i32::MAX;
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].max
    }

    pub fn set_attr_max(&mut self, d_nr: u32, a_nr: u16, max: i32) {
        self.definitions[d_nr as usize].attributes[a_nr as usize].max = max;
    }

    pub fn attr_nullable(&mut self, d_nr: u32, a_nr: u16) -> bool {
        if a_nr == u16::MAX {
            return false;
        }
        self.definitions[d_nr as usize].attributes[a_nr as usize].nullable
    }

    pub fn set_attr_nullable(&mut self, d_nr: u32, a_nr: u16, nullable: bool) {
        self.definitions[d_nr as usize].attributes[a_nr as usize].nullable = nullable;
    }

    pub fn attr_mutable(&mut self, d_nr: u32, a_nr: u16) -> bool {
        self.definitions[d_nr as usize].attributes[a_nr as usize].mutable
    }

    pub fn set_attr_mutable(&mut self, d_nr: u32, a_nr: u16, mutable: bool) {
        self.definitions[d_nr as usize].attributes[a_nr as usize].mutable = mutable;
    }

    pub fn add_fn(&mut self, lexer: &mut Lexer, fn_name: String, arguments: Arguments) -> u32 {
        let mut name = fn_name.clone();
        let is_self = !arguments.is_empty() && arguments[0].0 == "self";
        let is_both = !arguments.is_empty() && arguments[0].0 == "both";
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].1);
            if type_nr != u32::MAX {
                name = format!("_tp_{}_{fn_name}", self.def_name(type_nr));
            } else {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Unknown type {:?} on {fn_name}",
                    arguments[0].1
                );
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
        } else {
            d_nr = self.add_def(name, lexer.pos(), DefType::Function);
            for (a_name, a_type, a_value) in &arguments {
                let a_nr = self.add_attribute(lexer, d_nr, a_name, a_type.clone());
                self.set_attr_value(d_nr, a_nr, a_value.clone());
                self.set_attr_mutable(d_nr, a_nr, false);
            }
        }
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].1);
            let a_nr = self.add_attribute(lexer, type_nr, &fn_name, Type::Routine(d_nr));
            self.set_attr_mutable(type_nr, a_nr, false);
        }
        if is_both {
            let mut main = self.def_nr(&fn_name);
            if main == u32::MAX {
                main = self.add_def(fn_name, lexer.pos(), DefType::Dynamic);
            }
            let type_nr = self.type_def_nr(&arguments[0].1);
            if type_nr == u32::MAX {
                panic!("Unknown type {:?} at {}", arguments[0].1, lexer.pos());
            }
            let a_nr =
                self.add_attribute(lexer, main, &self.def_name(type_nr), Type::Routine(d_nr));
            self.set_attr_mutable(main, a_nr, false);
        }
        d_nr
    }

    pub fn get_fn(&self, fn_name: &str, arguments: &Arguments) -> u32 {
        let d_nr = self.def_nr(fn_name);
        let is_self = !arguments.is_empty() && arguments[0].0 == "self";
        let is_both = !arguments.is_empty() && arguments[0].0 == "both";
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].1);
            let name = format!("_tp_{}_{fn_name}", self.def_name(type_nr));
            self.def_nr(&name)
        } else {
            d_nr
        }
    }

    pub fn add_op(
        &mut self,
        lexer: &mut Lexer,
        types: &mut Types,
        fn_name: String,
        arguments: Arguments,
    ) -> u32 {
        let d_nr = self.add_def(fn_name.clone(), lexer.pos(), DefType::Function);
        for (a_name, a_type, a_value) in arguments {
            let a_nr = self.add_attribute(lexer, d_nr, &a_name, a_type);
            self.set_attr_value(d_nr, a_nr, a_value);
        }
        if !types.add_possible(self, fn_name.clone()) {
            diagnostic!(
                lexer,
                Level::Error,
                "Not matching operator external {}",
                fn_name
            );
        }
        d_nr
    }

    /// Get a vector definition. This is a record with a single field pointing towards this vector.
    /// We need this definition as the primary record of a database holding a vector and its child records/vectors.
    pub fn vector_def(&mut self, lexer: &mut Lexer, tp: &Type) -> u32 {
        let name = format!("{tp:?}");
        let d_nr = self.def_nr(&name);
        if d_nr != u32::MAX {
            d_nr
        } else {
            let vd = self.add_def(name, lexer.pos(), DefType::Struct);
            self.add_attribute(lexer, vd, "vector", Type::Vector(Box::new(tp.clone())));
            vd
        }
    }

    pub fn check_vector(&mut self, d_nr: u32, vec_tp: u16, pos: Position) -> u32 {
        let vec_name = format!("vector<{}>", self.def_name(d_nr));
        let mut v_nr = self.def_nr(&vec_name);
        if v_nr == u32::MAX {
            v_nr = self.add_def(vec_name, pos, DefType::Vector);
            self.set_parent(v_nr, d_nr);
            self.set_known_type(v_nr, vec_tp);
        };
        v_nr
    }

    /// Get the corresponding number of a definition on name.
    pub fn def_nr(&self, name: &str) -> u32 {
        let Some(nr) = self.def_names.get(name) else {
            return u32::MAX;
        };
        *nr
    }

    fn def(&self, dnr: u32) -> &Definition {
        &self.definitions[dnr as usize]
    }

    pub fn code(&self, d_nr: u32) -> &Value {
        &self.def(d_nr).code
    }

    pub fn set_code(&mut self, d_nr: u32, v: Value) {
        if self.def(d_nr).code != Value::Null {
            panic!("Cannot set code on '{}' twice", self.def(d_nr).name);
        }
        self.definitions[d_nr as usize].code = v;
    }

    pub fn attr_used(&mut self, d_nr: u32, a_nr: u16) {
        self.used_attributes.insert((d_nr, a_nr));
    }

    pub fn def_used(&mut self, d_nr: u32) {
        self.used_definitions.insert(d_nr);
    }

    pub fn show_type(&self, show: &Type) -> String {
        match show {
            Type::Reference(dnr) => self.def_name(*dnr),
            Type::Inner(dnr) => self.def_name(*dnr),
            Type::Enum(dnr) => self.def_name(*dnr),
            Type::Vector(sub) => format!("vector<{}>", self.show_type(sub)),
            Type::Routine(sub) => format!("fn {}", self.def_name(*sub)),
            _ => format!("{}", show),
        }
    }

    #[allow(dead_code)]
    /// Use to get the size on an array element. Possibly also store in Definition?
    pub fn align(self, data: &Data, d_nr: u32) -> u32 {
        let mut anr = data.def_align(d_nr) as u32;
        if anr == 0 {
            anr = 1;
        }
        let size = data.def_size(d_nr);
        if size % anr > 0 {
            size + anr - size % anr
        } else {
            size
        }
    }

    pub fn type_def_nr(&self, tp: &Type) -> u32 {
        match tp {
            Type::Integer => self.def_nr("integer"),
            Type::Long => self.def_nr("long"),
            Type::Boolean => self.def_nr("boolean"),
            Type::Float => self.def_nr("float"),
            Type::Text => self.def_nr("text"),
            Type::Single => self.def_nr("single"),
            Type::Routine(d_nr) => *d_nr,
            Type::Enum(d_nr) => *d_nr,
            Type::Inner(d_nr) => *d_nr,
            Type::Reference(d_nr) => *d_nr,
            Type::Vector(_) => self.def_nr("vector"),
            Type::Sorted(_, _) => self.def_nr("reference"),
            Type::Index(_, _) => self.def_nr("index"),
            Type::Hash(_, _) => self.def_nr("hash"),
            Type::Unknown(nr) => *nr,
            _ => u32::MAX,
        }
    }

    /// Get the definition number for the given type.
    pub fn type_elm(&self, tp: &Type) -> u32 {
        match tp {
            Type::Integer => self.def_nr("integer"),
            Type::Long => self.def_nr("long"),
            Type::Boolean => self.def_nr("boolean"),
            Type::Float => self.def_nr("float"),
            Type::Text => self.def_nr("text"),
            Type::Routine(d_nr) => *d_nr,
            Type::Enum(d_nr) => *d_nr,
            Type::Inner(d_nr) => *d_nr,
            Type::Reference(d_nr) => *d_nr,
            Type::Vector(tp) => {
                if let Type::Reference(td) = **tp {
                    td
                } else {
                    self.type_def_nr(tp)
                }
            }
            Type::Sorted(_, _) => self.def_nr("reference"),
            Type::Index(_, _) => self.def_nr("reference"),
            Type::Hash(_, _) => self.def_nr("reference"),
            _ => u32::MAX,
        }
    }

    pub fn name_type(&self, def_name: &str) -> Type {
        if def_name == "integer" {
            Type::Integer
        } else if def_name == "long" {
            Type::Long
        } else if def_name == "float" {
            Type::Float
        } else if def_name == "single" {
            Type::Single
        } else if def_name == "boolean" {
            Type::Boolean
        } else if def_name == "text" {
            Type::Text
        } else {
            Type::Null
        }
    }

    pub fn rust_type(&self, tp: &Type) -> &str {
        match tp {
            Type::Integer | Type::Enum(_) => "i32",
            Type::Text => "String",
            Type::Long => "i64",
            Type::Boolean => "bool",
            Type::Float => "f64",
            Type::Single => "f32",
            Type::Reference(_)
            | Type::Inner(_)
            | Type::Vector(_)
            | Type::Hash(_, _)
            | Type::Sorted(_, _)
            | Type::Index(_, _) => "DbRef",
            Type::Routine(_) => "u32",
            Type::Subtype(_) => "T",
            Type::Unknown(_) => "??",
            _ => panic!("Incorrect type {}", tp),
        }
    }

    pub fn output_webassembly(&self, dir: &str) -> Result<()> {
        let program = "code/".to_string() + dir;
        let source = "code/".to_string() + dir + "/src";
        std::fs::create_dir_all(&source)?;
        std::fs::copy(
            "webassembly/src/external.rs",
            source.clone() + "/external.rs",
        )?;
        std::fs::copy("webassembly/src/store.rs", source.clone() + "/store.rs")?;
        let cw = &mut std::fs::File::create(program + "/Cargo.toml")?;
        cw.write_all(
            "[package]
name = \"scriptlib\"
version = \"0.1.0\"
"
            .as_bytes(),
        )?;
        let w = &mut std::fs::File::create(source + "/main.rs")?;
        w.write_all(
            "#![allow(unused_parens)]
mod external;
mod store;

use external::*;
"
            .as_bytes(),
        )?;
        self.output(w, 0, self.definitions())
    }

    pub fn output(&self, w: &mut dyn Write, from: u32, till: u32) -> Result<()> {
        writeln!(
            w,
            "\
#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;"
        )?;
        writeln!(w, "use dryopea::database::{{Stores, KnownTypes, DbRef}};")?;
        writeln!(w, "use dryopea::external::*;\n")?;
        writeln!(w, "fn init(db: &mut KnownTypes) {{")?;
        for dnr in from..till {
            let def = self.def(dnr);
            if def.def_type == DefType::Struct {
                self.output_struct(w, def)?;
            } else if def.def_type == DefType::Enum {
                self.output_enum(w, def)?;
            } else if def.def_type == DefType::Vector {
                writeln!(w, "    db.vector({});", self.def_known_type(def.parent))?;
            }
        }
        writeln!(w, "}}\n")?;
        for dnr in from..till {
            let def = self.def(dnr);
            if def.def_type == DefType::Function {
                self.output_function(w, def)?;
            }
        }
        Ok(())
    }

    fn output_struct(&self, w: &mut dyn Write, def: &Definition) -> Result<()> {
        let p = if def.parent == u32::MAX {
            u16::MAX
        } else {
            def.parent as u16
        };
        writeln!(
            w,
            "    let s = db.structure(\"{}\".to_string(), {}, {p}); // {}",
            def.name, def.size, def.known_type
        )?;
        for a in &def.attributes {
            if !a.mutable {
                continue;
            }
            let pos = if a.position == u32::MAX {
                u16::MAX
            } else {
                a.position as u16
            };
            let nm = a.name.clone();
            let d_nr = self.type_def_nr(&a.typedef);
            if d_nr == u32::MAX {
                panic!("Unknown def_nr for {:?}", a.typedef);
            }
            let tp = self.def_known_type(d_nr);
            let mut done = false;
            if let Type::Vector(c) = &a.typedef {
                let content = self.def_known_type(self.type_def_nr(c));
                writeln!(
                    w,
                    "    db.field(s, \"{nm}\".to_string(), db.vector({content}), {pos});",
                )?;
                done = true;
            } else if a.typedef == Type::Integer && a.min != i32::MIN && a.max != i32::MAX {
                if a.max - a.min < 256 || (!a.nullable && a.max - a.min == 256) {
                    writeln!(
                        w,
                        "    db.field(s, \"{nm}\".to_string(), db.byte({}, {}), {pos});",
                        a.min, a.nullable
                    )?;
                    done = true;
                } else if a.max - a.min < 65536 || (!a.nullable && a.max - a.min == 65536) {
                    writeln!(
                        w,
                        "    db.field(s, \"{nm}\".to_string(), db.short({}, {}), {pos});",
                        a.min, a.nullable
                    )?;
                    done = true;
                }
            }
            if !done && tp != u16::MAX {
                writeln!(w, "    db.field(s, \"{nm}\".to_string(), {tp}, {pos});")?;
            }
        }
        Ok(())
    }

    fn output_enum(&self, w: &mut dyn Write, def: &Definition) -> Result<()> {
        writeln!(w, "    let e = db.enumerate(\"{}\".to_string());", def.name)?;
        for a in &def.attributes {
            writeln!(w, "    db.value(e, \"{}\".to_string());", a.name)?;
        }
        Ok(())
    }

    fn output_function(&self, w: &mut dyn Write, def: &Definition) -> Result<()> {
        if def.position.file == "default/01_code.gcp"
            && def.name.starts_with("Op")
            && def.code == Value::Null
        {
            return Ok(());
        }
        write!(w, "fn {}(stores: &mut Stores", def.name)?;
        for (anr, a) in def.attributes.iter().enumerate() {
            write!(w, ", var_{}: {}", anr, self.rust_type(&a.typedef))?
        }
        write!(w, ") ")?;
        if def.returned != Type::Void {
            write!(w, "-> {} ", self.rust_type(&def.returned))?
        }
        if let Value::Block(_) = def.code {
            self.output_code(w, &def.code, 0)?
        } else {
            writeln!(w, "{{")?;
            if def.code != Value::Null {
                self.output_code(w, &def.code, 0)?;
            }
            writeln!(w, "\n}}")?
        }
        writeln!(w, "\n")
    }

    pub fn output_code(&self, w: &mut dyn Write, code: &Value, indent: u32) -> Result<()> {
        match code {
            Value::Text(txt) => {
                write!(w, "\"{}\".to_string()", txt)?;
            }
            Value::Long(v) => {
                write!(w, "{}_i64", v)?;
            }
            Value::Int(v) => {
                write!(w, "{}_i32", v)?;
            }
            Value::Boolean(v) => {
                write!(w, "{}", v)?;
            }
            Value::Float(v) => {
                write!(w, "{}_f64", v)?;
            }
            Value::Single(v) => {
                write!(w, "{}_f32", v)?;
            }
            Value::Reference(_) => {
                // This cannot be a constant in the language
                write!(w, "DbRef")?;
            }
            Value::Block(vals) => {
                writeln!(w, "{{")?;
                for (vnr, v) in vals.iter().enumerate() {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code(w, v, indent + 1)?;
                    if vnr < vals.len() - 1 {
                        writeln!(w, ";")?;
                    } else {
                        writeln!(w)?;
                    }
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(w, "}}")?
            }
            Value::Loop(vals) => {
                writeln!(w, "loop {{")?;
                for v in vals {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code(w, v, indent + 1)?;
                    writeln!(w, ";")?;
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(w, "}}")?;
            }
            Value::Set(var, to) => {
                write!(w, "var_{} = ", var)?;
                self.output_code(w, to, indent)?;
            }
            Value::Let(var, to) => {
                write!(w, "let mut var_{} = ", var)?;
                self.output_code(w, to, indent)?;
            }
            Value::Var(v) => {
                write!(w, "var_{}", v)?;
            }
            Value::If(test, true_v, false_v) => {
                self.output_if(w, test, true_v, false_v, indent)?;
            }
            Value::Call(d_nr, vals) => {
                let def_fn = self.def(*d_nr);
                if def_fn.rust.is_empty() {
                    write!(w, "{}(stores", self.def(*d_nr).name)?;
                    for v in vals {
                        write!(w, ", ")?;
                        self.output_code(w, v, indent)?;
                    }
                    write!(w, ")")?;
                } else {
                    let mut res = def_fn.rust.clone();
                    for (a_nr, a) in def_fn.attributes.iter().enumerate() {
                        let name = "@".to_string() + &a.name;
                        let mut val_code = std::io::BufWriter::new(Vec::new());
                        if a_nr < vals.len() {
                            self.output_code(&mut val_code, &vals[a_nr], indent)?;
                            let with = String::from_utf8(val_code.into_inner().unwrap()).unwrap();
                            // TODO if with == "Null" { println!("Here! {} {code:?}", self.def_name(*d_nr));}
                            res = res.replace(&name, &format!("({with})"));
                        } else {
                            println!(
                                "Problem def_fn {def_fn} attributes {:?} vals {:?}",
                                def_fn.attributes, vals
                            );
                            break;
                        }
                    }
                    write!(w, "{}", res)?;
                }
            }
            Value::Return(val) => {
                write!(w, "return ")?;
                self.output_code(w, val, indent)?;
            }
            _ => write!(w, "{:?}", code)?,
        }
        Ok(())
    }

    fn output_if(
        &self,
        w: &mut dyn Write,
        test: &Value,
        true_v: &Value,
        false_v: &Value,
        indent: u32,
    ) -> Result<()> {
        write!(w, "if ")?;
        let b_true = matches!(*true_v, Value::Block(_));
        let b_false = matches!(*false_v, Value::Block(_));
        self.output_code(w, test, indent)?;
        if b_true {
            write!(w, " ")?;
        } else {
            write!(w, " {{")?;
        };
        self.output_code(w, true_v, indent + if b_true { 0 } else { 1 })?;
        if let Value::Block(_) = *true_v {
            write!(w, " else ")?;
        } else {
            write!(w, "}} else ")?;
        }
        if !b_false {
            write!(w, "{{")?;
        }
        self.output_code(w, false_v, indent + if b_false { 0 } else { 1 })?;
        if !b_false {
            write!(w, "}}")?;
        }
        Ok(())
    }

    pub fn find_unused(&self, diagnostics: &mut Diagnostics) {
        for (d_nr, def) in self.definitions.iter().enumerate() {
            if !self.used_definitions.contains(&(d_nr as u32)) {
                diagnostics.add(
                    Level::Warning,
                    &format!("Unused definition {} {:?}", def.name, def.position),
                );
            } else {
                for (a_nr, attr) in def.attributes.iter().enumerate() {
                    if !self.used_attributes.contains(&(d_nr as u32, a_nr as u16)) {
                        diagnostics.add(
                            Level::Warning,
                            &format!("Unused field {}.{} {:?}", def.name, attr.name, def.position),
                        );
                    }
                }
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
