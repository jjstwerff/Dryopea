// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Hold all definitions
//! Those are the combinations of types, records and routines.
//! Many definitions can hold fields of their own, a routine
//! has parameters that behave very similar to fields.

// These structures are rather inefficient right now, but they are be the basis
// for a far more efficient database later.

extern crate strum_macros;
use crate::diagnostics::*;
use crate::lexer::{Lexer, Position};
use crate::types::Types;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};

/// A value that can be assigned to attributes on a definition of instance
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Null,
    Int(i32),
    /// An record or field with 1:database, 2:allocation, 3:field_position.
    /// The position is in bytes relative the the allocation start. Updated on insert/remove of vector elements.
    Reference(u16, u32, u32),
    /// A range
    Range(Box<Value>, Box<Value>),
    Float(f64),
    Long(i64),
    Single(f32),
    /// Dynamic text with an efficient appender.
    Text(String),
    /// Part of the static data with position and length.
    Data(u32, u32),
    /// Call an outside routine with values.
    Call(u32, Vec<Value>),
    /// Block with number of variables and steps.
    Block(Vec<Value>),
    /// Read variable or parameter from stack (nr relative to current function start).
    Var(u32),
    /// Set a variable with an expressions
    Set(u32, Box<Value>),
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
}

#[derive(Clone, Debug, PartialEq)]
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
    /// A reference to a record instance in a store.
    Reference(u32),
    /// A record that is placed inside another object. Like compact vectors or parent objects.
    Inner(u32),
    /// A dynamic vector of a specific type
    Vector(Box<Type>),
    /// A dynamic routine, from a routine definition without code.
    /// The actual code is a routine with this routine as a parent or just a Block for a lambda function.
    Routine(u32),
    /// Linked to the n-th sub-type on the first defined parameter
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
    /// An hash table towards other records. The third is the hash function.
    Hash(u32, Vec<u16>),
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown(_))
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
    /// Is this attribute holding the primary reference of it's records.
    primary: bool,
    /// The initial value of this attribute if it is not given.
    value: Value,
    /// Restrictions: Value::Null is nothing, on text it is the saved size as Value::Integer
    min: Value,
    max: Value,
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{}:{}[{}]",
            self.name, &self.typedef, self.position
        ))
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
    // A set of fields on the parent struct.
    StructPart,
    // A type definition, for now only the base types.
    Type,
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
    /// Related type for in fields, and return type of a function
    returned: Type,
    /// Wasm code
    wasm: String,
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
}

pub fn v_if(test: Value, t: Value, f: Value) -> Value {
    Value::If(Box::new(test), Box::new(t), Box::new(f))
}

pub fn v_set(var: u32, val: Value) -> Value {
    Value::Set(var, Box::new(val))
}

pub fn text(s: &str) -> Value {
    Value::Text(s.to_string())
}

impl Debug for Definition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("Definition")
            .field("name", &self.name)
            .field("type", &self.def_type)
            .field("attributes", &self.attributes)
            .field("return", &self.returned)
            .finish()
    }
}

impl Default for Data {
    fn default() -> Self {
        Self::new()
    }
}

impl Data {
    pub fn new() -> Data {
        Data {
            definitions: Vec::new(),
            def_names: HashMap::new(),
            used_definitions: HashSet::new(),
            used_attributes: HashSet::new(),
            referenced: HashMap::new(),
            data: Vec::new(),
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
        if self.def(on_def).attr_names.get(name).is_some() {
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
            min: Value::Null,
            max: Value::Null,
        };
        let next_attr = self.def(on_def).attributes.len() as u16;
        let def = &mut self.definitions[on_def as usize];
        def.attr_names.insert(name.to_string(), next_attr);
        def.attributes.push(attr);
        next_attr
    }

    pub fn add_def(&mut self, name: String, position: Position, def_type: DefType) -> u32 {
        let rec = self.definitions.len() as u32;
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
            wasm: "".to_string(),
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

    pub fn def_referenced(&self, d_nr: u32) -> bool {
        self.referenced.contains_key(&d_nr)
    }

    pub fn set_referenced(&mut self, d_nr: u32, t_nr: u32, change: Value) {
        self.referenced.insert(d_nr, (t_nr, change));
    }

    pub fn def_type(&self, d_nr: u32) -> DefType {
        self.def(d_nr).def_type.clone()
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

    pub fn def_size(&self, d_nr: u32) -> u32 {
        self.def(d_nr).size
    }

    pub fn def_align(&self, d_nr: u32) -> u8 {
        self.def(d_nr).alignment
    }

    pub fn wasm(&mut self, d_nr: u32) -> String {
        self.def(d_nr).wasm.clone()
    }

    pub fn set_wasm(&mut self, d_nr: u32, wasm: String) {
        self.definitions[d_nr as usize].wasm = wasm;
    }

    pub fn set_returned(&mut self, d_nr: u32, tp: Type) {
        if !self.def(d_nr).returned.is_unknown() {
            panic!(
                "Cannot change returned type on {} to {tp} twice was {}",
                self.def(d_nr).name,
                self.def(d_nr).returned
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
            panic!("Cannot set attribute type twice");
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

    pub fn add_fn(&mut self, lexer: &mut Lexer, fn_name: String, arguments: Arguments) -> u32 {
        let mut name = fn_name.clone();
        let is_self = !arguments.is_empty() && arguments[0].0 == "self";
        let is_both = !arguments.is_empty() && arguments[0].0 == "both";
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].1);
            if type_nr != u32::MAX {
                name = format!("{}::{fn_name}", self.def_name(type_nr));
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
            }
        }
        if is_self || is_both {
            let type_nr = self.type_def_nr(&arguments[0].1);
            self.add_attribute(lexer, type_nr, &fn_name, Type::Routine(d_nr));
        }
        if is_both {
            let mut main = self.def_nr(&fn_name);
            if main == u32::MAX {
                main = self.add_def(fn_name, lexer.pos(), DefType::Dynamic);
            }
            let type_nr = self.type_def_nr(&arguments[0].1);
            self.add_attribute(lexer, main, &self.def_name(type_nr), Type::Routine(d_nr));
        }
        d_nr
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
    pub fn vector_def(&mut self, lexer: &mut Lexer, dnr: u32, tp: &Type) -> u32 {
        let name = format!("{tp:?}");
        let d_nr = self.def_nr(&name);
        if d_nr != u32::MAX {
            d_nr
        } else {
            let vd = self.add_def(name, lexer.pos(), DefType::Struct);
            self.add_attribute(lexer, vd, "vector", Type::Vector(Box::new(tp.clone())));
            let vector = &mut self.definitions[vd as usize];
            vector.attributes[0].position = 4;
            vector.size = 1;
            vector.returned = Type::Reference(dnr);
            vd
        }
    }

    /// Get the corresponding number of a definition on name.
    pub fn def_nr(&self, name: &str) -> u32 {
        let Some(nr) = self.def_names.get(name) else {
            return u32::MAX
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
            _ => u32::MAX,
        }
    }

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
            Type::Integer => "i32",
            Type::Text => "String",
            Type::Long => "i64",
            Type::Enum(_) => "u8",
            Type::Reference(_) => "(u32, u32)",
            Type::Inner(_) => "(u32, u32)",
            Type::Vector(_) => "(u32, u32)",
            Type::Boolean => "bool",
            Type::Float => "f64",
            Type::Single => "f32",
            Type::Hash(_, _) => "(u32, u32)",
            Type::Sorted(_, _) => "(u32, u32)",
            Type::Index(_, _) => "(u32, u32)",
            Type::Routine(_) => "u32",
            Type::Subtype(_) => "T",
            Type::Unknown(_) => "??",
            _ => panic!("Incorrect type {}", tp),
        }
    }

    pub fn output_def(&self, w: &mut dyn std::io::Write, dnr: u32) {
        let def = self.def(dnr);
        if def.def_type == DefType::Function {
            write!(w, "fn {}(", def.name).unwrap();
            for a in &def.attributes {
                write!(w, "{}: {}, ", a.name, self.rust_type(&a.typedef)).unwrap()
            }
            write!(w, ") ").unwrap();
            if def.returned != Type::Void {
                write!(w, "-> {} ", self.rust_type(&def.returned)).unwrap()
            }
            if def.code == Value::Null {
                write!(w, " {{}}").unwrap();
            } else {
                self.output_code(w, &def.code, 0);
            }
            write!(w, "\n\n").unwrap();
        } else if def.def_type == DefType::Struct {
            writeln!(w, "{{").unwrap();
            writeln!(w, "  let d = Definition::new(\"{}\");", def.name).unwrap();
            if def.parent != u32::MAX {
                writeln!(w, "  d.parent.push({});", self.def(def.parent).name).unwrap();
            }
            if !def.attributes.is_empty() {
                for (e, a) in def.attributes.iter().enumerate() {
                    write!(
                        w,
                        "  d.add_attribute(\"{}\", Type::{}{}{}",
                        a.name,
                        self.show_type(&a.typedef),
                        if a.mutable { " mutable" } else { "" },
                        if a.primary { " primary" } else { "" },
                    )
                    .unwrap();
                    if a.value != Value::Null {
                        write!(w, " = ").unwrap();
                        self.output_code(w, &a.value, 2);
                    }
                    writeln!(w, ");").unwrap();

                    if a.position != u32::MAX {
                        writeln!(w, "  d.attributes[{}].position = {};", e, a.position).unwrap();
                    }
                }
            }
            if !def.returned.is_unknown() {
                writeln!(w, "  d.returned = Type::{};", self.show_type(&def.returned)).unwrap();
            }
            writeln!(w, "  d.size = {};", def.size).unwrap();
            if def.code != Value::Null {
                write!(w, "  d.code = ").unwrap();
                self.output_code(w, &def.code, 2);
                writeln!(w, ";").unwrap();
            }
            writeln!(w, "}}").unwrap();
        }
    }

    pub fn output_code(&self, w: &mut dyn std::io::Write, code: &Value, indent: u32) {
        match code {
            Value::Reference(_, rec, pos) => {
                write!(w, "{}.{}", rec, pos).unwrap();
            }
            Value::Block(vals) => {
                writeln!(w, "{{").unwrap();
                for v in vals {
                    for _i in 0..=indent {
                        write!(w, "  ").unwrap();
                    }
                    self.output_code(w, v, indent + 1);
                    writeln!(w, ";").unwrap();
                }
                for _i in 0..indent {
                    write!(w, "  ").unwrap();
                }
                write!(w, "}}").unwrap()
            }
            Value::Int(i) => write!(w, "{}", i).unwrap(),
            Value::Loop(vals) => {
                writeln!(w, "loop {{").unwrap();
                for v in vals {
                    for _i in 0..=indent {
                        write!(w, "  ").unwrap();
                    }
                    self.output_code(w, v, indent + 1);
                    writeln!(w, ";").unwrap();
                }
                for _i in 0..indent {
                    write!(w, "  ").unwrap();
                }
                write!(w, "}}").unwrap();
            }
            Value::Set(var, to) => {
                write!(w, "var_{} = ", var).unwrap();
                self.output_code(w, to, indent);
            }
            Value::Var(v) => {
                write!(w, "var_{}", v).unwrap();
            }
            Value::If(test, true_v, false_v) => {
                write!(w, "if ").unwrap();
                let b_true = matches!(**true_v, Value::Block(_));
                let b_false = matches!(**false_v, Value::Block(_));
                self.output_code(w, test, indent);
                if b_true {
                    write!(w, " ").unwrap();
                } else {
                    write!(w, " {{").unwrap();
                };
                self.output_code(w, true_v, indent + if b_true { 0 } else { 1 });
                if let Value::Block(_) = **true_v {
                    write!(w, " else ").unwrap();
                } else {
                    write!(w, "}} else ").unwrap();
                }
                if !b_false {
                    write!(w, "{{").unwrap();
                }
                self.output_code(w, false_v, indent + if b_false { 0 } else { 1 });
                if !b_false {
                    write!(w, "}}").unwrap();
                }
            }
            Value::Call(d_nr, vals) => {
                write!(w, "{}(", self.def(*d_nr).name).unwrap();
                let mut first = true;
                for v in vals {
                    if first {
                        first = false;
                    } else {
                        write!(w, ", ").unwrap();
                    }
                    self.output_code(w, v, indent);
                }
                write!(w, ")").unwrap();
            }
            Value::Return(val) => {
                write!(w, "return ").unwrap();
                self.output_code(w, val, indent);
            }
            _ => write!(w, "{:?}", code).unwrap(),
        }
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
