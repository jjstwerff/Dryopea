// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Database operations on stores
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(dead_code)]

use crate::calc;
use crate::keys::DbRef;
use crate::store::Store;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Write};
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Str {
    pub ptr: *const u8,
    pub len: u32,
}

impl Str {
    #[must_use]
    pub fn new(v: &str) -> Str {
        Str {
            ptr: v.as_ptr(),
            len: v.len() as u32,
        }
    }

    #[must_use]
    pub fn str<'a>(&self) -> &'a str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.ptr, self.len as usize))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Relation {
    Main(u16, u16),       // this index is this field of the given record, may not exist
    Current(u16),         // referenced directly from the current record
    Referenced(u16, u16), // this field references a record, and this field holds the index
    None,                 // no primary reference found yet or only part of Vectors/Others
}

#[derive(Clone)]
pub enum Content {
    Long(i64),
    Float(f64),
    Single(f32),
    Text(String),
    Str(Str),
}

#[derive(Debug, Clone, PartialEq)]
struct Field {
    name: String,
    content: u16,
    position: u16,
    default: Content,
    other_indexes: Vec<u16>, // For now only fields on the same record
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    name: String,
    parts: Parts,
    complex: bool,
    linked: bool,
    size: u16,
    align: u8,
}

impl Type {
    fn new(name: &str, parts: Parts, size: u16) -> Type {
        Type {
            name: name.to_string(),
            parts,
            complex: false,
            linked: false,
            size,
            align: size as u8,
        }
    }

    fn data(name: &str, parts: Parts) -> Type {
        Type {
            name: name.to_string(),
            parts,
            complex: true,
            linked: false,
            size: 4,
            align: 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Parts {
    Base,
    Struct(Vec<Field>), // The fields and the primary parent of this record (the first).
    Sub(Vec<u16>),      // Polymorphic structure with different subtypes
    Enum(Vec<String>),
    Byte(i32, bool),                // start number and nullable flag
    Short(i32, bool),               // start number and nullable flag
    Vector(u16),                    // The records are part of the vector
    Array(u16),                     // The array holds references for each record
    Sorted(u16, Vec<(u16, bool)>),  // Sorted vector on fields with ascending flag
    Ordered(u16, Vec<(u16, bool)>), // Sorted array on fields with ascending flag
    Reference(u16, Relation), // reference and how to get there from here (Primary not allowed)
    Hash(u16, Vec<u16>),      // A hash table, listing the field numbers that define its key
    // An index to a table, listing the key fields and the left field (with +1 right and +2 color)
    Index(u16, Vec<(u16, bool)>, u16),
    Spacial(u16, Vec<u16>), // A spacial index with the listed coordinate fields as key
}

impl PartialEq for Content {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Content::Long(l), Content::Long(r)) => l == r,
            (Content::Float(l), Content::Float(r)) => l == r,
            (Content::Single(l), Content::Single(r)) => l == r,
            (Content::Text(l), Content::Text(r)) => l == r,
            (Content::Str(s), Content::Str(o)) => s.str() == o.str(),
            _ => false,
        }
    }
}

impl Debug for Content {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Content::Long(l) => f.write_fmt(format_args!("Long({l})"))?,
            Content::Float(v) => f.write_fmt(format_args!("Float({v})"))?,
            Content::Single(s) => f.write_fmt(format_args!("Single({s})"))?,
            Content::Text(t) => {
                f.write_char('"')?;
                f.write_str(t)?;
                f.write_char('"')?;
            }
            Content::Str(t) => {
                f.write_char('"')?;
                f.write_str(t.str())?;
                f.write_char('"')?;
            }
        }
        Ok(())
    }
}

impl Content {
    fn hash(&self, hasher: &mut DefaultHasher) {
        match self {
            Content::Long(l) => l.hash(hasher),
            Content::Text(t) => t.hash(hasher),
            Content::Str(s) => s.str().hash(hasher),
            _ => panic!("Incorrect hash key field"),
        }
    }
}

// This is just a subset of all combining characters, but it is those for most generally used
// languages. Source https://ftp.unicode.org/Public/16.0.0/ucd/extracted/DerivedJoiningType.txt
// Sorry for the many languages omitted from this specific list.
fn is_combining(ch: u32) -> bool {
    if let 0xAD | 0x300..=0x36F // Latin
        | 0x483..=0x487 | 0x488..=0x489 // Cyrillic
        | 0x591..=0x5BD | 0x5BF | 0x5C1..=0x5C2 | 0x5C4..=0x5C5 | 0x5C7 // Hebrew
        | 0x610..=0x61A | 0x61C | 0x64B..=0x65F | 0x670 | 0x6D6..=0x6DC | 0x6DF..=0x6E4 | 0x6E7..=0x6E8 | 0x6EA..=0x6ED // Arabic
        | 0x70F | 0x711 | 0x730..=0x74A // Syriac
        = ch {
        true
    } else {
        false
    }
}

fn compare(a: &Content, b: &Content) -> Ordering {
    match (a, b) {
        (Content::Long(a), Content::Long(b)) => i64::cmp(a, b),
        (Content::Single(a), Content::Single(b)) => {
            if a > b {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }
        (Content::Float(a), Content::Float(b)) => {
            if a > b {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }
        (Content::Str(a), Content::Str(b)) => str::cmp(a.str(), b.str()),
        _ => panic!("Undefined compare {a:?} vs {b:?}"),
    }
}

#[allow(dead_code)]
pub struct Stores {
    pub types: Vec<Type>,
    pub names: HashMap<String, u16>,
    pub allocations: Vec<Store>,
    max: u16,
}

impl Default for Stores {
    fn default() -> Self {
        Self::new()
    }
}

struct Key {
    // The current line on the source data. Only relevant if that has a pretty print format.
    line: u32,
    // The position on the current line in utf-8 characters. We count zero width characters.
    line_pos: u32,
    // The current key: holds positions of key identifiers or vector steps when negative.
    current: Vec<i64>,
    // The current step on the key, can decrease due to finished structures.
    step: u32,
}

fn parse_key(text: &str, pos: &mut usize, result: usize, key: &mut Key) {
    if *pos >= result {
        return;
    }
    skip_empty(text, pos, key);
    if match_token(text, pos, b'[') {
        key.line_pos += 1;
        if key.step == key.current.len() as u32 {
            key.current.push(-1);
        } else {
            key.current[key.step as usize] = -1;
        }
        key.step += 1;
        skip_empty(text, pos, key);
        loop {
            parse_key(text, pos, result, key);
            if match_token(text, pos, b',') {
                key.current[key.step as usize] -= 1;
                key.line_pos += 1;
                skip_empty(text, pos, key);
            } else {
                break;
            }
        }
        if !match_token(text, pos, b']') {
            *pos = usize::MAX;
            return;
        }
        key.line_pos += 1;
        key.step -= 1;
    } else if match_token(text, pos, b'{') {
        key.line_pos += 1;
        if key.step == key.current.len() as u32 {
            key.current.push(*pos as i64);
        } else {
            key.current[key.step as usize] = *pos as i64;
        }
        key.step += 1;
        skip_empty(text, pos, key);
        loop {
            let mut val = String::new();
            match_identifier(text, pos, &mut val);
            key.line_pos += val.len() as u32;
            skip_empty(text, pos, key);
            if match_token(text, pos, b':') {
                key.line_pos += 1;
            } else {
                *pos = usize::MAX;
                return;
            }
            parse_key(text, pos, result, key);
            if match_token(text, pos, b',') {
                key.line_pos += 1;
            } else {
                break;
            }
            skip_empty(text, pos, key);
        }
        if !match_token(text, pos, b'}') {
            *pos = usize::MAX;
            return;
        }
        key.line_pos += 1;
        key.step -= 1;
    } else {
        let mut p = skip_float(text, pos);
        if p > *pos {
            *pos = p;
            return;
        }
        let mut val = String::new();
        p = match_text(text, pos, &mut val);
        // allow for constant strings
        if p > *pos {
            *pos = p;
            return;
        }
        p = match_identifier(text, pos, &mut val);
        // allow for 'true', 'false', 'null' etc
        if p > *pos {
            *pos = p;
            return;
        }
        *pos = usize::MAX;
    }
}

fn skip_empty(text: &str, pos: &mut usize, key: &mut Key) {
    let mut c = *pos;
    let bytes = text.as_bytes();
    while c < bytes.len() && (bytes[c] == b' ' || bytes[c] == b'\t' || bytes[c] == b'\n') {
        if bytes[c] == b'\n' {
            key.line += 1;
            key.line_pos = 0;
        } else {
            key.line_pos += 1;
        }
        c += 1;
        *pos = c;
    }
}

fn show_key(text: &str, key: &Key) -> String {
    let mut result = format!("line {}:{} path:", key.line, key.line_pos);
    for k in 0..key.step {
        let p = key.current[k as usize];
        if p < 0 {
            result += &format!("[{}]", 1 - p);
        } else {
            let mut pos = key.current[k as usize] as usize;
            let mut val = String::new();
            match_identifier(text, &mut pos, &mut val);
            if k > 0 {
                result += ".";
            }
            result += &val;
        }
    }
    result
}

#[allow(dead_code)]
impl Stores {
    #[must_use]
    pub fn new() -> Stores {
        let mut result = Stores {
            types: Vec::new(),
            names: HashMap::new(),
            allocations: Vec::new(),
            max: 0,
        };
        result.base_type("integer", 4); // 0
        result.base_type("long", 8); // 1
        result.base_type("single", 4); // 2
        result.base_type("float", 8); // 3
        result.base_type("boolean", 1); // 4
        result.base_type("text", 4); // 5
        result
    }

    #[must_use]
    pub fn show_type(&self, tp: u16, pretty: bool) -> String {
        if tp > self.types.len() as u16 {
            return format!("Unknown type({tp})");
        }
        let typedef = &self.types[tp as usize];
        let mut res = format!("{}[{}]:", typedef.name, typedef.size);
        if let Parts::Struct(v) = &typedef.parts {
            if pretty {
                res += "\n";
            } else {
                res += "{";
            }
            for (f_nr, p) in v.iter().enumerate() {
                let name = &self.types[p.content as usize].name;
                if pretty {
                    res += "    ";
                } else if f_nr > 0 {
                    res += ", ";
                }
                res += &format!("{}:{name}[{}]", p.name, p.position);
                if !p.other_indexes.is_empty() {
                    res += &format!(" other {:?}", p.other_indexes);
                }
                if pretty {
                    res += "\n";
                }
            }
            if !pretty {
                res += "}";
            }
        } else {
            res += &format!("{:?}", &typedef.parts);
            if pretty {
                res += "\n";
            }
        }
        res
    }

    #[must_use]
    pub fn size(&self, tp: u16) -> u16 {
        if tp == u16::MAX {
            0
        } else {
            self.types[tp as usize].size
        }
    }

    #[must_use]
    pub fn position(&self, tp: u16, field: u16) -> u16 {
        if tp == u16::MAX {
            u16::MAX
        } else if let Parts::Struct(f) = &self.types[tp as usize].parts {
            if (field as usize) < f.len() {
                f[field as usize].position
            } else {
                u16::MAX
            }
        } else {
            u16::MAX
        }
    }

    fn base_type(&mut self, name: &str, size: u8) {
        self.names.insert(name.to_string(), self.types.len() as u16);
        self.types
            .push(Type::new(name, Parts::Base, u16::from(size)));
    }

    /**
    Allow multiple structures for polymorphism.
    */
    pub fn sub(&mut self, name: &str) -> u16 {
        let num = self.types.len() as u16;
        self.names.insert(name.to_string(), num);
        self.types.push(Type::new(name, Parts::Sub(Vec::new()), 0));
        num
    }

    /**
        Make a class an alternative of a Sub.
        # Panics
        When the given type is not a Sub.
    */
    pub fn alternative(&mut self, structure: u16, sub: u16) {
        if let Parts::Sub(s) = &mut self.types[sub as usize].parts {
            s.push(structure);
        } else {
            panic!(
                "Adding a child structures to a non structure type {}",
                self.types[sub as usize].name
            );
        }
    }

    /**
    Define a new database structure (record).
    # Panics
    When childs are added to a type that is not a structure.
    */
    pub fn structure(&mut self, name: &str) -> u16 {
        let num = self.types.len() as u16;
        self.names.insert(name.to_string(), num);
        let mut tp = Type::new(name, Parts::Struct(Vec::new()), u16::MAX);
        tp.align = u8::MAX;
        self.types.push(tp);
        num
    }

    /**
    Add a new field to a structure
    # Panics
    When the field has a position outside the structure size or on a non structure type.
    */
    pub fn field(&mut self, structure: u16, name: &str, content: u16) -> u16 {
        if content == u16::MAX {
            return 0;
        }
        let mut others = Vec::new();
        let mut linked = HashMap::new();
        if let Parts::Struct(fld) = &self.types[structure as usize].parts {
            for (f_nr, f) in fld.iter().enumerate() {
                let fld_content = self.content(f.content);
                if fld_content != u16::MAX && fld_content == self.content(content) {
                    if others.is_empty() {
                        others.push(u16::MAX);
                    }
                    linked.insert(f_nr as u16, fld.len() as u16);
                }
            }
        };
        if let Parts::Struct(s) = &mut self.types[structure as usize].parts {
            for (f_nr, f) in s.iter_mut().enumerate() {
                if let Some(add) = linked.get(&(f_nr as u16)) {
                    f.other_indexes.push(*add);
                }
            }
            let num = s.len() as u16;
            s.push(Field {
                name: name.to_string(),
                content,
                position: u16::MAX,
                default: Content::Str(Str::new("")),
                other_indexes: others,
            });
            if num > 8
                || self.types[content as usize].complex
                || matches!(self.types[content as usize].parts, Parts::Struct(_))
            {
                self.types[structure as usize].complex = true;
            }
            num
        } else {
            panic!(
                "Adding field {name} to a non structure type {}",
                self.types[structure as usize].name
            );
        }
    }

    #[must_use]
    pub fn content(&self, tp: u16) -> u16 {
        match self.types[tp as usize].parts {
            Parts::Vector(c)
            | Parts::Array(c)
            | Parts::Ordered(c, _)
            | Parts::Sorted(c, _)
            | Parts::Index(c, _, _)
            | Parts::Hash(c, _)
            | Parts::Spacial(c, _) => c,
            _ => u16::MAX,
        }
    }

    #[must_use]
    pub fn is_linked(&self, tp: u16) -> bool {
        tp != u16::MAX && self.types[tp as usize].linked
    }

    #[must_use]
    pub fn is_base(&self, tp: u16) -> bool {
        tp != u16::MAX && matches!(self.types[tp as usize].parts, Parts::Base | Parts::Enum(_))
    }

    #[must_use]
    pub fn field_type(&self, rec: u16, fld: u16) -> u16 {
        if let Parts::Struct(fields) = &self.types[rec as usize].parts {
            fields[fld as usize].content
        } else {
            u16::MAX
        }
    }

    pub fn finish(&mut self) {
        let mut vectors = HashSet::new();
        let mut linked = HashSet::new();
        let mut parts = HashSet::new();
        for t_nr in 0..self.types.len() {
            if let Parts::Sub(subs) = &self.types[t_nr].parts {
                for s in subs {
                    parts.insert(*s);
                }
            }
            if let Parts::Struct(fields) = &self.types[t_nr].parts {
                for f in fields {
                    if let Parts::Vector(v) = self.types[f.content as usize].parts {
                        vectors.insert(v);
                    }
                    if let Parts::Reference(r, _) | Parts::Hash(r, _) =
                        self.types[f.content as usize].parts
                    {
                        linked.insert(r);
                    }
                }
            }
        }
        let mut sizes = Vec::new();
        for t_nr in 0..self.types.len() {
            if self.types[t_nr].size != u16::MAX
                || !matches!(self.types[t_nr].parts, Parts::Struct(_))
            {
                continue;
            }
            sizes.clear();
            if let Parts::Struct(fields) = self.types[t_nr].parts.clone() {
                for (field_nr, f) in fields.iter().enumerate() {
                    sizes.push((
                        self.types[f.content as usize].size,
                        self.types[f.content as usize].align,
                    ));
                    if let Parts::Vector(c) = self.types[f.content as usize].parts {
                        if linked.contains(&c) {
                            self.types[c as usize].linked = true;
                            self.types[fields[field_nr].content as usize].parts = Parts::Array(c);
                            self.types[fields[field_nr].content as usize].name =
                                format!("array<{}>", self.types[c as usize].name);
                        }
                    }
                    if let Parts::Sorted(c, key) = self.types[f.content as usize].parts.clone() {
                        if linked.contains(&c) {
                            let mut name = format!("ordered<{}[", self.types[c as usize].name);
                            self.key_name(c, &key, &mut name);
                            self.types[fields[field_nr].content as usize].parts =
                                Parts::Ordered(c, key.clone());
                            self.types[fields[field_nr].content as usize].name = name;
                        }
                    }
                }
            }
            if let Parts::Struct(fields) = &mut self.types[t_nr].parts {
                let mut size = 0;
                let mut alignment = 0;
                let t = t_nr as u16;
                let vector = vectors.contains(&t) && !linked.contains(&t);
                let sub = parts.contains(&t);
                let pos = calc::calculate_positions(&sizes, vector, sub, &mut size, &mut alignment);
                for (field_nr, pos) in pos.iter().enumerate() {
                    fields[field_nr].position = *pos;
                }
                self.types[t_nr].size = size;
                self.types[t_nr].align = alignment;
            }
        }
        // self.dump_types();
    }

    pub fn dump_types(&self) {
        for t_nr in 0..self.types.len() {
            print!("{}", self.show_type(t_nr as u16, true));
        }
    }

    #[must_use]
    pub fn dump_type(&self, name: &str) -> String {
        for t in 0..self.types.len() {
            if self.types[t].name == name {
                return self.show_type(t as u16, false);
            }
        }
        String::new()
    }

    pub fn vector(&mut self, content: u16) -> u16 {
        let name = "vector<".to_string() + &self.types[content as usize].name + ">";
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types.push(Type::data(&name, Parts::Vector(content)));
            self.names.insert(name, num);
            num
        }
    }

    pub fn reference(&mut self, content: u16, relation: Relation) -> u16 {
        let name = "reference<".to_string() + &self.types[content as usize].name + ">";
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::data(&name, Parts::Reference(content, relation)));
            self.names.insert(name, num);
            num
        }
    }

    pub fn hash(&mut self, content: u16, key: &[u16]) -> u16 {
        let mut name = "hash<".to_string() + &self.types[content as usize].name + "[";
        self.field_name(content, key, &mut name);
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::data(&name, Parts::Hash(content, Vec::from(key))));
            self.names.insert(name, num);
            num
        }
    }

    pub fn spacial(&mut self, content: u16, key: &[u16]) -> u16 {
        let mut name = "spacial<".to_string() + &self.types[content as usize].name + "[";
        self.field_name(content, key, &mut name);
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::data(&name, Parts::Spacial(content, Vec::from(key))));
            self.names.insert(name, num);
            num
        }
    }

    pub fn field_name(&self, content: u16, key: &[u16], name: &mut String) {
        if let Parts::Struct(fields) = &self.types[content as usize].parts {
            for (k_nr, k) in key.iter().enumerate() {
                if k_nr > 0 {
                    *name += ",";
                }
                *name += &fields[*k as usize].name;
            }
        }
        *name += "]>";
    }

    pub fn field_id(&self, content: u16, key: &[(u16, bool)], name: &mut String) {
        if let Parts::Struct(fields) = &self.types[content as usize].parts {
            for (k_nr, (k, asc)) in key.iter().enumerate() {
                if k_nr > 0 {
                    *name += ",";
                }
                if !asc {
                    *name += "-";
                }
                *name += &fields[*k as usize].name;
            }
        }
        *name += "]>";
    }

    #[must_use]
    pub fn field_nr(&self, record: u16, position: i32) -> u16 {
        if let Parts::Struct(fields) = &self.types[record as usize].parts {
            for (f_nr, f) in fields.iter().enumerate() {
                if f.position == position as u16 {
                    return f_nr as u16;
                }
            }
        };
        0
    }

    /**
    Keys with field number and ascending flag.
    */
    pub fn sorted(&mut self, content: u16, key: &[(u16, bool)]) -> u16 {
        let mut name = "sorted<".to_string() + &self.types[content as usize].name + "[";
        self.key_name(content, key, &mut name);
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::new(&name, Parts::Sorted(content, Vec::from(key)), 4));
            self.names.insert(name, num);
            num
        }
    }

    pub fn index(&mut self, content: u16, key: &[(u16, bool)]) -> u16 {
        let mut name = "index<".to_string() + &self.types[content as usize].name + "[";
        self.key_name(content, key, &mut name);
        let int_c = self.name("integer");
        let bool_c = self.name("boolean");
        let mut nr = 1;
        if let Parts::Struct(fields) = &self.types[content as usize].parts {
            for f in fields {
                if f.name.starts_with("_left_") {
                    nr += 1;
                }
            }
        }
        let left = if let Parts::Struct(fields) = &mut self.types[content as usize].parts {
            let left = fields.len();
            fields.push(Field {
                name: format!("_left_{nr}"),
                content: int_c,
                position: 0,
                default: Content::Long(0),
                other_indexes: Vec::new(),
            });
            fields.push(Field {
                name: format!("_right_{nr}"),
                content: int_c,
                position: 0,
                default: Content::Long(0),
                other_indexes: Vec::new(),
            });
            fields.push(Field {
                name: format!("_color_{nr}"),
                content: bool_c,
                position: 0,
                default: Content::Long(0),
                other_indexes: Vec::new(),
            });
            left as u16
        } else {
            u16::MAX
        };
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types.push(Type::new(
                &name,
                Parts::Index(content, Vec::from(key), left),
                4,
            ));
            self.names.insert(name, num);
            num
        }
    }

    fn key_name(&mut self, content: u16, key: &[(u16, bool)], name: &mut String) {
        if let Parts::Struct(fields) = &self.types[content as usize].parts {
            for (k_nr, (k, asc)) in key.iter().enumerate() {
                if k_nr > 0 {
                    *name += ",";
                }
                if !*asc {
                    *name += "-";
                }
                *name += &fields[*k as usize].name;
            }
        }
        *name += "]>";
    }

    pub fn byte(&mut self, min: i32, nullable: bool) -> u16 {
        let name = if min == 0 && !nullable {
            "byte".to_string()
        } else {
            format!("byte<{min},{nullable}>")
        };
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::new(&name, Parts::Byte(min, nullable), 1));
            self.names.insert(name, num);
            num
        }
    }

    /**
    Retrieve a defined type number by name.
    # Panics
    When a type name doesn't exist.
    */
    #[must_use]
    pub fn name(&self, name: &str) -> u16 {
        *self.names.get(name).unwrap_or(&u16::MAX)
    }

    pub fn short(&mut self, min: i32, nullable: bool) -> u16 {
        let name = format!("short<{min},{nullable}>");
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types
                .push(Type::new(&name, Parts::Short(min, nullable), 2));
            self.names.insert(name, num);
            num
        }
    }

    pub fn enumerate(&mut self, name: &str) -> u16 {
        let num = self.types.len() as u16;
        self.types.push(Type::new(name, Parts::Enum(Vec::new()), 1));
        self.names.insert(name.to_string(), num);
        num
    }

    /**
    Add a value to an enumerate type.
    # Panics
    When adding a value to a non-enumerate.
    */
    pub fn value(&mut self, known_type: u16, name: &str) -> u16 {
        if let Parts::Enum(values) = &mut self.types[known_type as usize].parts {
            let num = values.len() as u16;
            values.push(name.to_string());
            num
        } else {
            panic!(
                "Adding a value to a non enum type {}",
                self.types[known_type as usize].name
            );
        }
    }

    #[allow(dead_code)]
    #[must_use]
    // We do not compile inter/external in main as that would result in a slew of dead_code warnings
    pub fn enum_val(&self, known_type: u16, value: u8) -> String {
        if known_type == u16::MAX {
            return format!("unknown({value})");
        }
        if let Parts::Enum(values) = &self.types[known_type as usize].parts {
            if value > 0 && (value as usize) <= values.len() {
                return values[value as usize - 1].clone();
            }
        }
        "null".to_string()
    }

    #[allow(dead_code)]
    #[must_use]
    pub fn to_enum(&self, known_type: u16, value: &str) -> u8 {
        if let Parts::Enum(values) = &self.types[known_type as usize].parts {
            for (idx, val) in values.iter().enumerate() {
                if val == value {
                    return 1 + idx as u8;
                }
            }
        }
        0u8
    }

    #[must_use]
    pub fn is_null(&self, store: &Store, rec: u32, pos: u32, known_type: u16) -> bool {
        if rec == 0 {
            return true;
        }
        if known_type < 6 {
            match known_type {
                0 => store.get_int(rec, pos) == i32::MIN,
                1 => store.get_long(rec, pos) == i64::MIN,
                2 => store.get_single(rec, pos).is_nan(),
                3 => store.get_float(rec, pos).is_nan(),
                4 => store.get_byte(rec, pos, 0) > 1,
                5 => store.get_str(store.get_int(rec, pos) as u32).is_empty(),
                _ => false,
            }
        } else if let Parts::Enum(_) = &self.types[known_type as usize].parts {
            store.get_byte(rec, pos, 0) == 0
        } else if let Parts::Struct(_) = &self.types[known_type as usize].parts {
            rec == 0
        } else if let Parts::Vector(_) = &self.types[known_type as usize].parts {
            store.get_int(rec, pos) == 0
        } else if let Parts::Byte(from, nullable) = &self.types[known_type as usize].parts {
            let v = store.get_byte(rec, pos, *from);
            *nullable && v == 255
        } else if let Parts::Short(from, nullable) = &self.types[known_type as usize].parts {
            let v = store.get_short(rec, pos, *from);
            *nullable && v == 65535
        } else {
            false
        }
    }

    pub fn database(&mut self, size: u32) -> DbRef {
        if self.max >= self.allocations.len() as u16 {
            self.allocations.push(Store::new(100));
        }
        let store = &mut self.allocations[self.max as usize];
        let rec = store.claim(size);
        self.max += 1;
        DbRef {
            store_nr: self.max - 1,
            rec,
            pos: 0,
        }
    }

    #[must_use]
    pub fn type_claim(&self, tp: u16) -> u32 {
        (u32::from(self.types[tp as usize].size) + 7) / 8
    }

    pub fn claim(&mut self, db: &DbRef, size: u32) -> DbRef {
        let store = &mut self.allocations[db.store_nr as usize];
        let rec = store.claim(size);
        DbRef {
            store_nr: db.store_nr,
            rec,
            pos: 0,
        }
    }

    #[must_use]
    pub fn null() -> DbRef {
        DbRef {
            store_nr: u16::MAX,
            rec: 0,
            pos: 0,
        }
    }

    #[must_use]
    pub fn store(&self, r: &DbRef) -> &Store {
        &self.allocations[r.store_nr as usize]
    }

    pub fn store_mut(&mut self, r: &DbRef) -> &mut Store {
        &mut self.allocations[r.store_nr as usize]
    }

    #[must_use]
    pub fn store_nr(&self, nr: u16) -> &Store {
        &self.allocations[nr as usize]
    }

    pub fn show(&self, s: &mut String, db: &DbRef, tp: u16, pretty: bool) {
        ShowDb {
            stores: self,
            store: db.store_nr,
            rec: db.rec,
            pos: db.pos,
            known_type: tp,
            pretty,
        }
        .write(s, 0);
    }

    /**
    Parse the content of a string into a new database.
    # Panics
    When this string in incorrectly parsed.
    */
    pub fn parse(&mut self, text: &str, tp: u16, result: &DbRef) {
        let mut pos = 0;
        let result = self.parsing(text, &mut pos, tp, tp, u16::MAX, result);
        pos = 0;
        let mut key = Key {
            line: 1,
            line_pos: 0,
            current: Vec::new(),
            step: 0,
        };
        parse_key(text, &mut pos, result, &mut key);
        assert_eq!(
            result,
            0,
            "Parse error on position {result} at {}",
            show_key(text, &key)
        );
    }

    pub fn parse_message(&mut self, text: &str, tp: u16) -> String {
        let db = self.database(u32::from(self.types[tp as usize].size));
        let mut pos = 0;
        let result = self.parsing(text, &mut pos, tp, tp, u16::MAX, &db);
        pos = 0;
        let mut key = Key {
            line: 1,
            line_pos: 0,
            current: Vec::new(),
            step: 0,
        };
        parse_key(text, &mut pos, result, &mut key);
        show_key(text, &key)
    }

    /**
    # Panics
    When requesting a record on a non-structure
    */
    pub fn record_new(&mut self, data: &DbRef, parent_tp: u16, field: u16) -> DbRef {
        let tp = if field == u16::MAX {
            // This case is when the top level is a data-structure
            parent_tp
        } else {
            self.field_type(parent_tp, field)
        };
        let d = self.field_ref(data, parent_tp, field);
        match self.types[tp as usize].parts {
            Parts::Sorted(c, _) => self.sorted_new(&d, u32::from(self.size(c))),
            Parts::Vector(c) => self.vector_append(&d, 1, u32::from(self.size(c))),
            Parts::Array(c)
            | Parts::Ordered(c, _)
            | Parts::Hash(c, _)
            | Parts::Index(c, _, _)
            | Parts::Spacial(c, _) => self.claim(&d, (u32::from(self.size(c)) + 7) >> 3),
            _ => panic!(
                "Cannot add to none-structure '{}'",
                self.types[tp as usize].name
            ),
        }
    }

    /**
    # Panics
    When the implementation is not yet written
    */
    pub fn record_finish(&mut self, data: &DbRef, rec: &DbRef, parent_tp: u16, field: u16) {
        let tp = if field == u16::MAX {
            // This case is when the top level is a data-structure
            parent_tp
        } else {
            self.field_type(parent_tp, field)
        };
        let d = self.field_ref(data, parent_tp, field);
        self.insert_record(&d, rec, tp);
        if field != u16::MAX {
            if let Parts::Struct(fields) = self.types[parent_tp as usize].parts.clone() {
                let f = &fields[field as usize];
                let o = &f.other_indexes;
                if !o.is_empty() && o[0] != u16::MAX {
                    for fld_nr in o {
                        let o = self.field_ref(data, parent_tp, *fld_nr);
                        self.insert_record(&o, rec, fields[*fld_nr as usize].content);
                    }
                }
            }
        }
    }

    fn field_ref(&self, data: &DbRef, parent_tp: u16, field: u16) -> DbRef {
        if field == u16::MAX {
            *data
        } else if let Parts::Struct(fields) = &self.types[parent_tp as usize].parts {
            DbRef {
                store_nr: data.store_nr,
                rec: data.rec,
                pos: data.pos + u32::from(fields[field as usize].position),
            }
        } else {
            *data
        }
    }

    fn insert_record(&mut self, data: &DbRef, rec: &DbRef, tp: u16) {
        match self.types[tp as usize].parts.clone() {
            Parts::Sorted(c, keys) => {
                let size = u32::from(self.size(c));
                self.sorted_finish(data, size, c, &keys);
            }
            Parts::Array(_) => {
                let reference = self.vector_append(data, 1, 4);
                self.store_mut(data)
                    .set_int(reference.rec, reference.pos, rec.rec as i32);
            }
            Parts::Hash(c, keys) => self.hash_add(data, rec, c, &keys),
            Parts::Ordered(c, keys) => {
                self.ordered_finish(data, rec, c, &keys);
            }
            Parts::Spacial(_, _) | Parts::Index(_, _, _) => panic!("Not implemented"),
            _ => (),
        }
    }

    fn parsing(
        &mut self,
        text: &str,
        pos: &mut usize,
        tp: u16,
        rec_tp: u16,
        field: u16,
        to: &DbRef,
    ) -> usize {
        if match_null(text, pos) {
            self.set_default_value(tp, to);
        }
        match self.types[tp as usize].parts.clone() {
            Parts::Base => {
                let result = self.parse_simple(text, pos, tp, to);
                if result != 0 {
                    return result;
                }
            }
            Parts::Sorted(c, _)
            | Parts::Vector(c)
            | Parts::Array(c)
            | Parts::Ordered(c, _)
            | Parts::Hash(c, _)
            | Parts::Spacial(c, _)
            | Parts::Index(c, _, _) => {
                match_empty(text, pos);
                if match_token(text, pos, b'[') {
                    match_empty(text, pos);
                    if match_token(text, pos, b']') {
                        return *pos;
                    }
                    loop {
                        let res = self.record_new(to, rec_tp, field);
                        let result = self.parsing(text, pos, c, c, u16::MAX, &res);
                        if result != 0 {
                            return result;
                        }
                        self.record_finish(to, &res, rec_tp, field);
                        match_empty(text, pos);
                        if !match_token(text, pos, b',') {
                            break;
                        }
                        match_empty(text, pos);
                    }
                    match_empty(text, pos);
                    if !match_token(text, pos, b']') {
                        return *pos;
                    }
                } else {
                    return *pos;
                }
            }
            Parts::Reference(_other, _) => {
                // parse it
            }
            Parts::Sub(_) => {}
            Parts::Struct(object) => {
                let result = self.parse_struct(text, pos, tp, to, &object);
                if result != 0 {
                    return result;
                }
            }
            Parts::Enum(fields) => {
                let mut value = String::new();
                let mut result = match_text(text, pos, &mut value);
                if result != 0 {
                    result = match_identifier(text, pos, &mut value);
                    if result != 0 {
                        return result;
                    }
                }
                let val = if value == "null" {
                    0
                } else {
                    let mut v = 1;
                    for (f_nr, f) in fields.iter().enumerate() {
                        if *f == value {
                            v = f_nr as i32 + 1;
                            break;
                        }
                    }
                    v
                };
                self.store_mut(to).set_byte(to.rec, to.pos, 0, val);
            }
            Parts::Byte(from, _null) => {
                let mut value = 0;
                let result = match_integer(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_byte(to.rec, to.pos, from, value);
            }
            Parts::Short(from, _null) => {
                let mut value = 0;
                let result = match_integer(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_short(to.rec, to.pos, from, value);
            }
        }
        0
    }

    fn parse_struct(
        &mut self,
        text: &str,
        pos: &mut usize,
        tp: u16,
        to: &DbRef,
        object: &[Field],
    ) -> usize {
        if match_token(text, pos, b'{') {
            match_empty(text, pos);
            if match_token(text, pos, b'}') {
                return 0;
            }
            let fld = if to.rec == 0 { 0 } else { to.pos };
            let rec = if to.rec == 0 {
                let size = self.types[tp as usize].size;
                self.store_mut(to).claim((u32::from(size) + 7) / 8)
            } else {
                to.rec
            };
            let mut found_fields = HashSet::new();
            loop {
                let mut field_name = String::new();
                let result = match_identifier(text, pos, &mut field_name);
                if result != 0 {
                    return result;
                }
                match_empty(text, pos);
                if !match_token(text, pos, b':') {
                    return *pos;
                }
                match_empty(text, pos);
                for (f_nr, f) in object.iter().enumerate() {
                    if f.name == field_name {
                        let result = if self.content(f.content) == u16::MAX {
                            let field = DbRef {
                                store_nr: to.store_nr,
                                rec,
                                pos: fld + u32::from(f.position),
                            };
                            self.parsing(text, pos, f.content, tp, f_nr as u16, &field)
                        } else {
                            self.parsing(text, pos, f.content, tp, f_nr as u16, to)
                        };
                        if result != 0 {
                            return result;
                        }
                    }
                }
                found_fields.insert(field_name);
                match_empty(text, pos);
                if !match_token(text, pos, b',') {
                    break;
                }
                match_empty(text, pos);
            }
            match_empty(text, pos);
            if !match_token(text, pos, b'}') {
                return *pos;
            }
            for f in object {
                if (f.other_indexes.is_empty() || f.other_indexes[0] != u16::MAX)
                    && !found_fields.contains(&f.name)
                {
                    let field = DbRef {
                        store_nr: to.store_nr,
                        rec,
                        pos: to.pos + u32::from(f.position),
                    };
                    self.set_default_value(f.content, &field);
                }
            }
        } else {
            return *pos;
        }
        0
    }

    // Returns 0 on success, or a position inside the original string as failure.
    fn parse_simple(&mut self, text: &str, pos: &mut usize, tp: u16, to: &DbRef) -> usize {
        match tp {
            0 => {
                let mut value = 0;
                let result = match_integer(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_int(to.rec, to.pos, value);
            }
            1 => {
                let mut value = 0;
                let result = match_long(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_long(to.rec, to.pos, value);
            }
            2 => {
                let mut value = 0.0;
                let result = match_single(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_single(to.rec, to.pos, value);
            }
            3 => {
                let mut value = 0.0;
                let result = match_float(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to).set_float(to.rec, to.pos, value);
            }
            4 => {
                let mut value = false;
                let result = match_boolean(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                self.store_mut(to)
                    .set_byte(to.rec, to.pos, 0, i32::from(value));
            }
            5 => {
                let mut value = String::new();
                let result = match_text(text, pos, &mut value);
                if result != 0 {
                    return result;
                }
                let text_pos = self.store_mut(to).set_str(&value);
                self.store_mut(to).set_int(to.rec, to.pos, text_pos as i32);
            }
            _ => {
                return *pos;
            }
        }
        0
    }

    /**
        Write default values on fields. This should normally only be done while debugging
        as all fields should be set anyway under correct code generation.
        # Panics
        On inconsistent database definitions.
    */
    pub fn set_default_value(&mut self, tp: u16, rec: &DbRef) {
        if tp < 6 {
            match tp {
                0 => {
                    self.store_mut(rec).set_int(rec.rec, rec.pos, i32::MIN);
                }
                1 => {
                    self.store_mut(rec).set_long(rec.rec, rec.pos, i64::MIN);
                }
                2 => {
                    self.store_mut(rec).set_single(rec.rec, rec.pos, f32::NAN);
                }
                3 => {
                    self.store_mut(rec).set_float(rec.rec, rec.pos, f64::NAN);
                }
                4 => {
                    self.store_mut(rec).set_byte(rec.rec, rec.pos, 0, 0);
                }
                5 => {
                    self.store_mut(rec).set_int(rec.rec, rec.pos, 0);
                }
                _ => (),
            }
            return;
        }
        match self.types[tp as usize].parts.clone() {
            Parts::Enum(_) => {
                self.store_mut(rec).set_byte(rec.rec, rec.pos, 0, 0);
            }
            Parts::Byte(_, null) => {
                self.store_mut(rec)
                    .set_byte(rec.rec, rec.pos, 0, if null { 255 } else { 0 });
            }
            Parts::Short(_, null) => {
                self.store_mut(rec)
                    .set_short(rec.rec, rec.pos, 0, if null { 65535 } else { 0 });
            }
            Parts::Struct(fields) => {
                for f in &fields {
                    self.set_default_value(
                        f.content,
                        &DbRef {
                            store_nr: rec.store_nr,
                            rec: rec.rec,
                            pos: rec.pos + u32::from(f.position),
                        },
                    );
                }
            }
            Parts::Sub(_)
            | Parts::Reference(_, _)
            | Parts::Sorted(_, _)
            | Parts::Ordered(_, _)
            | Parts::Spacial(_, _)
            | Parts::Hash(_, _)
            | Parts::Index(_, _, _)
            | Parts::Array(_)
            | Parts::Vector(_) => {
                self.store_mut(rec).set_int(rec.rec, rec.pos, 0);
            }
            Parts::Base => {
                panic!(
                    "not implemented default {:?}",
                    self.types[tp as usize].parts
                );
            }
        }
    }

    #[must_use]
    #[allow(clippy::cast_sign_loss)]
    pub fn length_vector(&self, db: &DbRef) -> u32 {
        if db.rec == 0 || db.pos == 0 {
            return 0;
        }
        let store = self.store(db);
        let v_rec = store.get_int(db.rec, db.pos) as u32;
        if v_rec == 0 {
            0
        } else {
            store.get_int(v_rec, 4) as u32
        }
    }

    #[allow(clippy::cast_sign_loss)]
    pub fn clear_vector(&mut self, db: &DbRef) {
        let store = self.store_mut(db);
        let v_rec = store.get_int(db.rec, db.pos) as u32;
        if v_rec != 0 {
            // Only set size of the vector to 0
            // TODO when the main path to a separate allocated objects: remove these
            // TODO lower string reference counts where needed
            store.set_int(v_rec, 4, 0);
        }
    }

    #[must_use]
    pub fn slice_vector(&self, db: &DbRef, size: u32, from: i32, _till: i32) -> DbRef {
        let store = self.store(db);
        if from == i32::MIN {
            return Stores::null();
        };
        let v_rec = store.get_int(db.rec, db.pos) as u32;
        let l = self.length_vector(db);
        let f = if from < 0 { from + l as i32 } else { from };
        /*
        let t = if till == i32::MIN {
            from + 1
        } else if till < from {
            from
        } else {
            till
        };*/
        DbRef {
            store_nr: db.store_nr,
            rec: v_rec,
            pos: 8 + size * f as u32,
        }
    }

    #[must_use]
    pub fn get_ref(&self, db: &DbRef, fld: u32) -> DbRef {
        let store = self.store(db);
        let res = store.get_int(db.rec, db.pos + fld) as u32;
        DbRef {
            store_nr: db.store_nr,
            rec: res,
            pos: 0,
        }
    }

    #[must_use]
    pub fn get_field(db: &DbRef, fld: u32) -> DbRef {
        DbRef {
            store_nr: db.store_nr,
            rec: db.rec,
            pos: fld,
        }
    }

    pub fn vector_append(&mut self, db: &DbRef, add: u32, size: u32) -> DbRef {
        let store = self.store_mut(db);
        let mut vec_rec = store.get_int(db.rec, db.pos) as u32;
        let new_length;
        if vec_rec == 0 {
            // claim a new array with minimal 11 elements
            vec_rec = store.claim(((add + 10) * size + 15) / 8);
            store.set_int(db.rec, db.pos, vec_rec as i32);
            new_length = add;
        } else {
            new_length = add + store.get_int(vec_rec, 4) as u32;
            let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
            if new_vec != vec_rec {
                store.set_int(db.rec, db.pos, new_vec as i32);
                vec_rec = new_vec;
            }
        };
        store.set_int(vec_rec, 4, new_length as i32);
        DbRef {
            store_nr: db.store_nr,
            rec: vec_rec,
            pos: 8 + (new_length - add) * size,
        }
    }

    pub fn copy_block(&mut self, from: &DbRef, to: &DbRef, len: u32) {
        unsafe {
            std::ptr::copy(
                self.store(from)
                    .ptr
                    .offset(from.rec as isize * 8 + from.pos as isize),
                self.store_mut(to)
                    .ptr
                    .offset(to.rec as isize * 8 + to.pos as isize),
                len as usize,
            );
        }
    }

    // TODO copy child records & strings during copy (string reference counting on same)
    pub fn vector_add(&mut self, db: &DbRef, o_db: &DbRef, size: u32) {
        let o_length = self.length_vector(o_db);
        if o_length == 0 {
            // Other vector has no data
            return;
        }
        let o_rec = self.store(o_db).get_int(o_db.rec, o_db.pos) as u32;
        let o_pos = 0;
        let new_db = self.vector_append(db, o_length, size);
        if db.store_nr == o_db.store_nr {
            self.store_mut(db).copy_block(
                o_rec,
                o_pos as isize,
                new_db.rec,
                new_db.pos as isize,
                o_length as isize * size as isize,
            );
        } else {
            let o_store: &Store;
            let db_store: &mut Store;
            unsafe {
                o_store = (*std::ptr::from_ref::<Stores>(self)).store(o_db);
                db_store = (*std::ptr::from_mut::<Stores>(self)).store_mut(db);
            }
            o_store.copy_block_between(
                o_rec,
                o_pos as isize,
                db_store,
                new_db.rec,
                new_db.pos as isize,
                o_length as isize * size as isize,
            );
        }
    }

    // TODO change slice to its own vector on updating it
    pub fn insert_vector(&mut self, db: &DbRef, size: u32, index: i32) -> DbRef {
        let len = self.length_vector(db);
        let real = if index < 0 { index + len as i32 } else { index };
        if real < 0 || real > len as i32 {
            return DbRef {
                store_nr: db.store_nr,
                rec: 0,
                pos: 0,
            };
        }
        let store = self.store_mut(db);
        let mut vec_rec = store.get_int(db.rec, db.pos) as u32;
        let new_length;
        if vec_rec == 0 {
            // claim a new array with minimal 11 elements
            vec_rec = store.claim((11 * size + 15) / 8);
            store.set_int(db.rec, db.pos, vec_rec as i32);
            new_length = 1;
        } else {
            new_length = len + 1;
            let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
            if new_vec != vec_rec {
                store.set_int(db.rec, db.pos, new_vec as i32);
                vec_rec = new_vec;
            }
            store.copy_block(
                new_vec,
                8 + size as isize * real as isize,
                new_vec,
                8 + size as isize * (real as isize + 1),
                (len as isize - real as isize) * size as isize,
            );
        };
        store.set_int(vec_rec, 4, new_length as i32);
        DbRef {
            store_nr: db.store_nr,
            rec: vec_rec,
            pos: 8 + real as u32 * size,
        }
    }

    pub fn remove_vector(&mut self, db: &DbRef, size: u32, index: u32) -> bool {
        let len = self.length_vector(db);
        let store = self.store_mut(db);
        let vec_rec = store.get_int(db.rec, db.pos) as u32;
        if index >= len || vec_rec == 0 {
            return false;
        }
        store.copy_block(
            vec_rec,
            8 + size as isize * (index as isize + 1),
            vec_rec,
            8 + size as isize * index as isize,
            (len as isize - index as isize) * size as isize,
        );
        store.set_int(vec_rec, 4, len as i32 - 1);
        true
    }

    pub fn get_file(&mut self, file: &DbRef) -> bool {
        if file.rec == 0 {
            return false;
        }
        let store = self.store_mut(file);
        let filename = store.get_str(store.get_int(file.rec, file.pos + 4) as u32);
        let path = std::path::Path::new(filename);
        fill_file(path, store, file)
    }

    pub fn get_file_text(&mut self, file: &DbRef) -> String {
        if file.rec == 0 {
            return String::new();
        }
        let store = self.store(file);
        let file_path = store.get_str(store.get_int(file.rec, file.pos + 4) as u32);
        std::fs::read_to_string(file_path).unwrap_or_default()
    }

    pub fn get_dir(&mut self, file_path: &str, result: &DbRef) -> bool {
        let path = std::path::Path::new(&file_path);
        if let Ok(iter) = std::fs::read_dir(path) {
            let vector = DbRef {
                store_nr: result.store_nr,
                rec: result.rec,
                pos: result.pos + 16,
            };
            for entry in iter.flatten() {
                let elm = self.vector_append(&vector, 1, 17);
                let path = entry.path();
                if let Some(name) = path.to_str() {
                    let store = self.store_mut(result);
                    let name_pos = store.set_str(name) as i32;
                    store.set_int(elm.rec, elm.pos + 4, name_pos);
                    if !fill_file(&path, store, &elm) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }

    /**
    Read the binary data from a png image.
    # Panics
    On file system problems
    */
    pub fn get_png(&mut self, file_path: &str, result: &DbRef) -> bool {
        let store = self.store_mut(result);
        if let Ok((img, width, height)) = crate::png_store::read(file_path, store) {
            if let Some(name) = std::path::Path::new(&file_path).file_name() {
                let name_pos = store.set_str(name.to_str().unwrap());
                store.set_int(result.rec, result.pos + 4, name_pos as i32);
                store.set_int(result.rec, result.pos + 8, width as i32);
                store.set_int(result.rec, result.pos + 12, height as i32);
                store.set_int(result.rec, result.pos + 16, img as i32);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn sorted_new(&mut self, db: &DbRef, size: u32) -> DbRef {
        // Keep an extra record between the current and the new one.
        // This is needed to allow to create a new open space to move the new record to.
        let store = self.store_mut(db);
        let mut sorted_rec = store.get_int(db.rec, db.pos) as u32;
        // Claim a record at the back of the current structure or create a new structure.
        if sorted_rec == 0 {
            sorted_rec = store.claim((12 * size + 15) / 8);
            store.set_int(db.rec, db.pos, sorted_rec as i32);
            // Set initial length to 0
            store.set_int(sorted_rec, 4, 0);
            // return the first record
            DbRef {
                store_nr: db.store_nr,
                rec: sorted_rec,
                pos: 8,
            }
        } else {
            let length = store.get_int(sorted_rec, 4) as u32;
            let new_sorted = store.resize(sorted_rec, ((length + 2) * size + 15) / 8);
            if new_sorted != sorted_rec {
                store.set_int(db.rec, db.pos, new_sorted as i32);
                sorted_rec = new_sorted;
            }
            // return the last record inside the allocation
            DbRef {
                store_nr: db.store_nr,
                rec: sorted_rec,
                pos: 8 + (length + 1) * size,
            }
        }
    }

    fn sorted_finish(&mut self, sorted: &DbRef, size: u32, db: u16, keys: &[(u16, bool)]) {
        let sorted_rec = self.store(sorted).get_int(sorted.rec, sorted.pos) as u32;
        let length = self.store(sorted).get_int(sorted_rec, 4) as u32;
        if length == 0 {
            // we do not have to reorder the first inserted record; set length to 1
            self.store_mut(sorted).set_int(sorted_rec, 4, 1);
            return;
        }
        let latest_pos = 8 + (length + 1) * size;
        let rec = DbRef {
            store_nr: sorted.store_nr,
            rec: sorted_rec,
            pos: latest_pos,
        };
        let key = self.get_key(&rec, db, keys);
        let pos = self.sorted_find(sorted, length, size, db, keys, &key);
        let store = self.store_mut(&pos);
        let end_pos = 8 + length * size;
        if pos.pos < end_pos {
            // create space to write the new record to
            store.copy_block(
                pos.rec,
                pos.pos as isize,
                pos.rec,
                (pos.pos + size) as isize,
                (end_pos - pos.pos) as isize,
            );
        }
        // move last record to the found correct position
        store.copy_block(
            pos.rec,
            latest_pos as isize,
            pos.rec,
            pos.pos as isize,
            size as isize,
        );
        store.set_int(sorted_rec, 4, (length + 1) as i32);
    }

    fn get_key(&mut self, fld: &DbRef, db: u16, keys: &[(u16, bool)]) -> Vec<Content> {
        let mut key = Vec::new();
        for (k, _) in keys {
            key.push(self.field_content(fld, db, *k));
        }
        key
    }

    fn ordered_finish(&mut self, sorted: &DbRef, rec: &DbRef, db: u16, keys: &[(u16, bool)]) {
        let rec_ref = self.sorted_new(sorted, 4);
        let sorted_rec = self.store(sorted).get_int(sorted.rec, sorted.pos) as u32;
        let length = self.store(sorted).get_int(sorted_rec, 4) as u32;
        if length == 0 {
            // we do not have to reorder the first inserted record, set length to 1
            self.store_mut(sorted).set_int(sorted_rec, 4, 1);
            self.store_mut(sorted)
                .set_int(sorted_rec, rec_ref.pos, rec.rec as i32);
            return;
        }
        let key = self.get_key(rec, db, keys);
        let mut p = 0;
        self.ordered_find(sorted, length, db, &mut p, keys, &key);
        let latest_pos = 8 + length * 4;
        if latest_pos > p {
            self.store_mut(sorted).copy_block(
                sorted_rec,
                p as isize,
                sorted_rec,
                p as isize + 4,
                (latest_pos - p) as isize,
            );
        }
        self.store_mut(&rec_ref)
            .set_int(sorted_rec, p, rec.rec as i32);
        self.store_mut(sorted)
            .set_int(sorted_rec, 4, 1 + length as i32);
    }

    fn field_content(&self, rec: &DbRef, db: u16, key: u16) -> Content {
        let store = self.store(rec);
        if let Parts::Struct(fields) = &self.types[db as usize].parts {
            let f = &fields[key as usize];
            return match f.content {
                0 => Content::Long(i64::from(
                    store.get_int(rec.rec, rec.pos + u32::from(f.position)),
                )),
                1 => Content::Long(store.get_long(rec.rec, rec.pos + u32::from(f.position))),
                2 => Content::Single(store.get_single(rec.rec, rec.pos + u32::from(f.position))),
                3 => Content::Float(store.get_float(rec.rec, rec.pos + u32::from(f.position))),
                4 => Content::Long(i64::from(store.get_byte(
                    rec.rec,
                    rec.pos + u32::from(f.position),
                    0,
                ))),
                5 => Content::Str(Str::new(
                    store.get_str(store.get_int(rec.rec, rec.pos + u32::from(f.position)) as u32),
                )),
                _ => {
                    if let Parts::Enum(_) = self.types[f.content as usize].parts {
                        Content::Long(i64::from(store.get_byte(
                            rec.rec,
                            rec.pos + u32::from(f.position),
                            0,
                        )))
                    } else {
                        panic!(
                            "Unknown key type {} of {}.{}",
                            self.types[f.content as usize].name,
                            self.types[db as usize].name,
                            f.name
                        )
                    }
                }
            };
        }
        Content::Long(0)
    }

    /**
    Find a record on a given key.
    # Panics
    When the given database type doesn't support searcher.
    */
    #[must_use]
    pub fn find(&self, data: &DbRef, db: u16, key: &[Content]) -> DbRef {
        match &self.types[db as usize].parts {
            Parts::Vector(c) => {
                if let Content::Long(v) = key[0] {
                    self.slice_vector(data, u32::from(self.types[*c as usize].size), v as i32, 0)
                } else {
                    DbRef {
                        store_nr: data.store_nr,
                        rec: if data.rec == 0 || self.store(data).get_int(data.rec, 4) == 0 {
                            0
                        } else {
                            self.store(data).get_int(data.rec, 0) as u32
                        },
                        pos: 8,
                    }
                }
            }
            Parts::Array(c) => {
                if let Content::Long(v) = key[0] {
                    let res = self.slice_vector(
                        data,
                        u32::from(self.types[*c as usize].size),
                        v as i32,
                        0,
                    );
                    DbRef {
                        store_nr: res.store_nr,
                        rec: if res.rec == 0 {
                            0
                        } else {
                            self.store(&res).get_int(res.rec, res.pos) as u32
                        },
                        pos: 0,
                    }
                } else {
                    DbRef {
                        store_nr: data.store_nr,
                        rec: if data.rec == 0 || self.store(data).get_int(data.rec, 4) == 0 {
                            0
                        } else {
                            let rec = self.store(data).get_int(data.rec, 0) as u32;
                            self.store(data).get_int(rec, 8) as u32
                        },
                        pos: 0,
                    }
                }
            }
            Parts::Sorted(c, keys) => {
                let rec = self.store(data).get_int(data.rec, data.pos) as u32;
                let length = self.store(data).get_int(rec, 4) as u32;
                self.sorted_find(
                    data,
                    length,
                    u32::from(self.types[*c as usize].size),
                    *c,
                    keys,
                    key,
                )
            }
            Parts::Ordered(c, keys) => {
                let rec = self.store(data).get_int(data.rec, data.pos) as u32;
                let length = self.store(data).get_int(rec, 4) as u32;
                let mut pos = 0;
                self.ordered_find(data, length, *c, &mut pos, keys, key)
            }
            Parts::Hash(rec, keys) => self.hash_find(data, *rec, keys, key),
            Parts::Index(_, _, _) => self.index_find(data, db, key),
            Parts::Spacial(_, _) => self.spacial_find(data, db, key),
            _ => panic!("Incorrect search"),
        }
    }

    #[must_use]
    pub fn get_keys(&self, db: u16) -> Vec<u16> {
        match &self.types[db as usize].parts {
            Parts::Vector(_) | Parts::Array(_) => vec![0],
            Parts::Sorted(c, key) | Parts::Ordered(c, key) | Parts::Index(c, key, _) => {
                let mut res = Vec::new();
                if let Parts::Struct(fields) = &self.types[*c as usize].parts {
                    for (k, _) in key {
                        res.push(fields[*k as usize].content);
                    }
                }
                res
            }
            Parts::Spacial(c, keys) | Parts::Hash(c, keys) => {
                let mut res = Vec::new();
                if let Parts::Struct(fields) = &self.types[*c as usize].parts {
                    for k in keys {
                        res.push(fields[*k as usize].content);
                    }
                }
                res
            }
            _ => Vec::new(),
        }
    }

    #[must_use]
    pub fn start(&self, data: &DbRef, db: u16, key: &[Content]) -> i32 {
        if data.rec == 0 {
            return i32::MAX;
        }
        match &self.types[db as usize].parts {
            Parts::Sorted(c, keys) => {
                if key.is_empty() {
                    i32::MAX
                } else {
                    let rec = self.store(data).get_int(data.rec, data.pos) as u32;
                    let length = self.store(data).get_int(rec, 4) as u32;
                    let f = self.sorted_find(
                        data,
                        length,
                        u32::from(self.types[*c as usize].size),
                        *c,
                        keys,
                        key,
                    );
                    f.rec as i32
                }
            }
            Parts::Ordered(c, keys) => {
                if key.is_empty() {
                    i32::MAX
                } else {
                    let rec = self.store(data).get_int(data.rec, data.pos) as u32;
                    let length = self.store(data).get_int(rec, 4) as u32;
                    let mut pos = 0;
                    self.ordered_find(data, length, *c, &mut pos, keys, key);
                    pos as i32
                }
            }
            _ => i32::MAX,
        }
    }
    /**
    Get the next record given a specific point in a structure.
    # Panics
    When not in a valid structure
    */
    pub fn next(&self, data: &DbRef, pos: &mut i32, db: u16, key: &[Content]) -> DbRef {
        match &self.types[db as usize].parts {
            Parts::Vector(c) => {
                self.vector_next(data, pos, self.types[*c as usize].size);
                self.element_reference(data, pos)
            }
            Parts::Hash(_, _) => {
                let rec = self.store(data).get_int(data.rec, data.pos) as u32;
                let max = self.store(data).get_int(rec, 0) * 8;
                if *pos == i32::MAX {
                    *pos = 8;
                } else if *pos < max - 4 {
                    *pos += 4;
                } else {
                    *pos = i32::MAX;
                }
                while *pos != i32::MAX {
                    if self.store(data).get_int(rec, *pos as u32) as u32 != 0 {
                        break;
                    }
                    if *pos < max - 4 {
                        *pos += 4;
                    } else {
                        *pos = i32::MAX;
                    }
                }
                self.db_ref(data, pos, rec)
            }
            Parts::Array(_) => {
                self.vector_next(data, pos, 4);
                let r = self.store(data).get_int(data.rec, data.pos) as u32;
                self.db_ref(data, pos, r)
            }
            Parts::Sorted(c, keys) => {
                self.vector_next(data, pos, self.types[*c as usize].size);
                let mut rec = self.element_reference(data, pos);
                if !key.is_empty()
                    && *pos != 0
                    && self.compare_key(&rec, db, keys, key) != Ordering::Equal
                {
                    rec.rec = 0;
                };
                rec
            }
            Parts::Ordered(_, keys) => {
                self.vector_next(data, pos, 4);
                if *pos == i32::MAX {
                    return DbRef {
                        store_nr: data.store_nr,
                        rec: 0,
                        pos: 0,
                    };
                }
                let r = self.store(data).get_int(data.rec, data.pos) as u32;
                let mut rec = DbRef {
                    store_nr: data.store_nr,
                    rec: self.store(data).get_int(r, *pos as u32) as u32,
                    pos: 0,
                };
                if !key.is_empty()
                    && *pos != 0
                    && self.compare_key(&rec, db, keys, key) != Ordering::Equal
                {
                    rec.rec = 0;
                };
                rec
            }
            _ => panic!("Undefined iterate on '{}'", self.types[db as usize].name),
        }
    }

    fn db_ref(&self, data: &DbRef, pos: &mut i32, r: u32) -> DbRef {
        DbRef {
            store_nr: data.store_nr,
            rec: if *pos == i32::MAX {
                0
            } else {
                self.store(data).get_int(r, *pos as u32) as u32
            },
            pos: 0,
        }
    }

    fn element_reference(&self, data: &DbRef, pos: &mut i32) -> DbRef {
        DbRef {
            store_nr: data.store_nr,
            rec: if *pos == i32::MAX {
                0
            } else {
                self.store(data).get_int(data.rec, data.pos) as u32
            },
            pos: *pos as u32,
        }
    }

    /**
    Remove a specific record from a structure.
    # Panics
    When not in a structure.
    */
    pub fn remove(&mut self, data: &DbRef, rec: &DbRef, db: u16) {
        match self.types[db as usize].parts.clone() {
            Parts::Sorted(c, _) | Parts::Vector(c) | Parts::Array(c) | Parts::Ordered(c, _) => {
                let size = u32::from(self.types[c as usize].size);
                self.remove_vector(data, size, (rec.pos - 8) / size);
            }
            Parts::Hash(c, keys) => self.hash_remove(data, rec, c, &keys),
            _ => panic!("Incorrect search"),
        }
    }

    fn vector_next(&self, data: &DbRef, pos: &mut i32, size: u16) {
        let rec = self.store(data).get_int(data.rec, data.pos) as u32;
        if rec == 0 {
            *pos = i32::MAX;
            return;
        }
        let length = self.store(data).get_int(rec, 4);
        if *pos == i32::MAX && length != 0 {
            *pos = 8;
        } else if length != 0 && *pos < 8 + (length - 1) * i32::from(size) {
            *pos += i32::from(size);
        } else {
            *pos = i32::MAX;
        }
    }

    #[allow(clippy::unused_self)]
    fn index_find(&self, data: &DbRef, _db: u16, _key: &[Content]) -> DbRef {
        *data
    }

    #[allow(clippy::unused_self)]
    fn spacial_find(&self, data: &DbRef, _db: u16, _key: &[Content]) -> DbRef {
        *data
    }

    fn sorted_find(
        &self,
        sorted: &DbRef,
        length: u32,
        size: u32,
        db: u16,
        keys: &[(u16, bool)],
        key: &[Content],
    ) -> DbRef {
        let store = self.store(sorted);
        let sorted_rec = store.get_int(sorted.rec, sorted.pos) as u32;
        let mut found = 0;
        let mut result = DbRef {
            store_nr: sorted.store_nr,
            rec: sorted_rec,
            pos: 8 + found * size,
        };
        let mut left = 0;
        let mut right = length - 1;
        while left != right {
            found = (left + right + 1) >> 1;
            result.pos = 8 + found * size;
            let cmd = self.compare_key(&result, db, keys, key);
            if cmd == Ordering::Less {
                right = found - 1;
            } else {
                left = found;
            }
        }
        result.pos = 8 + left * size;
        let cmd = self.compare_key(&result, db, keys, key);
        if cmd == Ordering::Greater {
            result.pos = 8 + (left + 1) * size;
        }
        result
    }

    fn ordered_find(
        &self,
        sorted: &DbRef,
        length: u32,
        db: u16,
        pos: &mut u32,
        keys: &[(u16, bool)],
        key: &[Content],
    ) -> DbRef {
        let store = self.store(sorted);
        let sorted_rec = store.get_int(sorted.rec, sorted.pos) as u32;
        let mut found;
        let mut result = DbRef {
            store_nr: sorted.store_nr,
            rec: 0,
            pos: 0,
        };
        if sorted_rec == 0 {
            *pos = 0;
            return result;
        }
        let mut left = 0;
        let mut right = length - 1;
        while left != right {
            found = (left + right + 1) >> 1;
            result.rec = store.get_int(sorted_rec, 8 + found * 4) as u32;
            let cmd = self.compare_key(&result, db, keys, key);
            if cmd == Ordering::Less {
                right = found - 1;
            } else {
                left = found;
            }
        }
        *pos = 8 + left * 4;
        result.rec = store.get_int(sorted_rec, *pos) as u32;
        let cmd = self.compare_key(&result, db, keys, key);
        if cmd == Ordering::Greater {
            *pos += 4;
            result.rec = store.get_int(sorted_rec, *pos) as u32;
        }
        result
    }

    // Output the hash content and validate its content.
    fn hash_dump(&mut self, hash_ref: &DbRef, db: u16, keys: &[u16]) {
        let claim = self.store(hash_ref).get_int(hash_ref.rec, hash_ref.pos) as u32;
        let length = self.store(hash_ref).get_int(claim, 4) as u32;
        let room = self.store(hash_ref).get_int(claim, 0) as u32;
        let elms = (room - 1) * 2;
        println!(
            "dump hash length:{length} elms:{elms} {:.2}%",
            100.0 * f64::from(length) / f64::from(elms)
        );
        let mut record = DbRef {
            store_nr: hash_ref.store_nr,
            rec: 0,
            pos: 0,
        };
        let mut l = 0;
        for i in 0..elms {
            let rec = self.store(hash_ref).get_int(claim, 8 + i * 4) as u32;
            if rec != 0 {
                let mut s = String::new();
                record.rec = rec;
                self.show(&mut s, &record, db, false);
                l += 1;
                println!("{i:4}:[{rec}]{s}");
                let mut k = Vec::new();
                for f in keys {
                    k.push(self.field_content(&record, db, *f));
                }
                assert_eq!(
                    self.hash_find(hash_ref, db, keys, &k).rec,
                    rec,
                    "Incorrect entry {k:?}"
                );
            }
        }
        assert_eq!(length, l, "Incorrect hash length");
    }

    fn hash_add(&mut self, hash_ref: &DbRef, rec: &DbRef, db: u16, keys: &[u16]) {
        let mut claim = self.store(hash_ref).get_int(hash_ref.rec, hash_ref.pos) as u32;
        let length = if claim == 0 {
            claim = self.store_mut(hash_ref).claim(9);
            self.store_mut(hash_ref).zero_fill(claim);
            self.store_mut(hash_ref)
                .set_int(hash_ref.rec, hash_ref.pos, claim as i32);
            0
        } else {
            self.store(hash_ref).get_int(claim, 4) as u32
        };
        let room = self.store(hash_ref).get_int(claim, 0) as u32;
        let elms = (room - 1) * 2;
        if (length * 14 / 16) + 1 >= room {
            // rehash
            let mut move_rec = DbRef {
                store_nr: hash_ref.store_nr,
                rec: 0,
                pos: 0,
            };
            let new_claim = self.store_mut(hash_ref).claim(room * 2 - 1);
            self.store_mut(hash_ref).zero_fill(new_claim);
            for i in 0..elms {
                let v = self.store(hash_ref).get_int(claim, 8 + 4 * i) as u32;
                if v == 0 {
                    continue;
                }
                move_rec.rec = v;
                self.hash_set(new_claim, &move_rec, db, keys);
            }
            claim = new_claim;
            self.store_mut(hash_ref)
                .set_int(hash_ref.rec, hash_ref.pos, claim as i32);
        }
        self.hash_set(claim, rec, db, keys);
        self.store_mut(rec).set_int(claim, 4, length as i32 + 1);
        // self.hash_dump(hash_ref, db, keys);
    }

    fn hash_remove(&mut self, hash_ref: &DbRef, rec: &DbRef, db: u16, keys: &[u16]) {
        let claim = self.store(hash_ref).get_int(hash_ref.rec, hash_ref.pos) as u32;
        let length = self.store(hash_ref).get_int(claim, 4);
        if length == 0 {
            return;
        }
        let room = self.store(hash_ref).get_int(claim, 0) as u32;
        let elms = (room - 1) * 2;
        // get the index of the element, and set it to 0
        let mut index = self.hash_free_pos(claim, rec, db, keys);
        self.store_mut(hash_ref).set_int(rec.rec, index, 0);
        let mut index_val = 0;
        let mut current = 0;
        // move from there and determine if the next elements could get to a better position
        for _ in 0..elms {
            if index_val == 0 {
                break;
            }
            let next = DbRef {
                store_nr: rec.store_nr,
                rec: index_val,
                pos: 0,
            };
            let to = (8 + (self.rec_hash(&next, db, keys) % u64::from(elms)) * 4) as u32;
            let right = if index < to {
                current < index && index <= to
            } else {
                current < index || index <= to
            };
            if !right {
                let val = self.store(hash_ref).get_int(claim, index);
                self.store_mut(hash_ref).set_int(claim, current, val);
                current = index;
                self.store_mut(hash_ref).set_int(claim, index, 0);
            }
            if index + 4 >= elms {
                index = 8;
            } else {
                index += 4;
            }
            index_val = self.store(hash_ref).get_int(claim, index) as u32;
        }
        self.store_mut(hash_ref).set_int(claim, 4, length - 1);
    }

    fn rec_hash(&self, record: &DbRef, db: u16, keys: &[u16]) -> u64 {
        let mut hasher = DefaultHasher::new();
        for f in keys {
            self.field_content(record, db, *f).hash(&mut hasher);
        }
        hasher.finish()
    }

    fn hash_free_pos(&self, claim: u32, rec: &DbRef, db: u16, keys: &[u16]) -> u32 {
        let room = self.store(rec).get_int(claim, 0) as u32;
        let elms = (room - 1) * 2;
        let hash_val = self.rec_hash(rec, db, keys);
        let mut index = (hash_val % u64::from(elms)) as u32;
        for _ in 0..elms {
            if self.store(rec).get_int(claim, 8 + index * 4) == 0 {
                break;
            }
            index += 1;
            if index >= elms {
                index = 0;
            }
        }
        8 + index * 4
    }

    fn hash_set(&mut self, claim: u32, rec: &DbRef, db: u16, keys: &[u16]) {
        let index = self.hash_free_pos(claim, rec, db, keys);
        self.store_mut(rec).set_int(claim, index, rec.rec as i32);
    }

    fn hash_find(&self, hash_ref: &DbRef, db: u16, keys: &[u16], key: &[Content]) -> DbRef {
        let store = self.store(hash_ref);
        let claim = store.get_int(hash_ref.rec, hash_ref.pos) as u32;
        let mut hasher = DefaultHasher::new();
        for k in key {
            k.hash(&mut hasher);
        }
        let hash_val = hasher.finish();
        let mut record = DbRef {
            store_nr: hash_ref.store_nr,
            rec: 0,
            pos: 0,
        };
        let room = store.get_int(claim, 0) as u32;
        if room == 0 {
            return record;
        }
        let elms = (room - 1) * 2;
        let mut index = (hash_val % u64::from(elms)) as u32;
        let mut rec_pos = store.get_int(claim, 8 + index * 4) as u32;
        'Record: for _ in 0..elms {
            if rec_pos == 0 {
                break;
            }
            record.rec = rec_pos;
            for (f_nr, f) in keys.iter().enumerate() {
                if self.field_content(&record, db, *f) != key[f_nr] {
                    index += 1;
                    if index >= elms {
                        index = 0;
                    }
                    rec_pos = store.get_int(claim, 8 + index * 4) as u32;
                    continue 'Record;
                }
            }
            break;
        }
        DbRef {
            store_nr: hash_ref.store_nr,
            rec: rec_pos,
            pos: 0,
        }
    }

    fn compare_key(&self, rec: &DbRef, db: u16, keys: &[(u16, bool)], key: &[Content]) -> Ordering {
        for (k_nr, k) in key.iter().enumerate() {
            let mut cmp = compare(k, &self.field_content(rec, db, keys[k_nr].0));
            if !keys[k_nr].1 {
                if cmp == Ordering::Less {
                    cmp = Ordering::Greater;
                } else if cmp == Ordering::Greater {
                    cmp = Ordering::Less;
                }
            }
            if cmp != Ordering::Equal {
                return cmp;
            }
        }
        Ordering::Equal
    }
}

fn match_token(text: &str, pos: &mut usize, token: u8) -> bool {
    if *pos < text.len() && text.as_bytes()[*pos] == token {
        *pos += 1;
        true
    } else {
        false
    }
}

fn match_empty(text: &str, pos: &mut usize) {
    let mut c = *pos;
    let bytes = text.as_bytes();
    while c < bytes.len() && (bytes[c] == b' ' || bytes[c] == b'\t' || bytes[c] == b'\n') {
        c += 1;
        *pos = c;
    }
}

fn match_null(text: &str, pos: &mut usize) -> bool {
    if text.len() >= *pos + 4 && &text[*pos..*pos + 4] == "null" {
        *pos += 4;
        true
    } else {
        false
    }
}

fn match_boolean(text: &str, pos: &mut usize, value: &mut bool) -> usize {
    if text.len() >= *pos + 4 && &text[*pos..*pos + 4] == "true" {
        *pos += 4;
        *value = true;
        0
    } else if text.len() >= *pos + 5 && &text[*pos..*pos + 5] == "false" {
        *pos += 4;
        *value = false;
        0
    } else {
        *pos
    }
}

fn skip_integer(text: &str, pos: &mut usize) -> usize {
    let mut c = *pos;
    let bytes = text.as_bytes();
    if c < bytes.len() && bytes[c] == b'-' {
        c += 1;
    }
    while c < bytes.len() && bytes[c] >= b'0' && bytes[c] <= b'9' {
        c += 1;
    }
    c
}

fn match_integer(text: &str, pos: &mut usize, value: &mut i32) -> usize {
    let c = skip_integer(text, pos);
    if c == *pos {
        c
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        0
    }
}

fn match_long(text: &str, pos: &mut usize, value: &mut i64) -> usize {
    let c = skip_integer(text, pos);
    if c == *pos {
        c
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        0
    }
}

fn skip_float(text: &str, pos: &mut usize) -> usize {
    let mut c = *pos;
    let bytes = text.as_bytes();
    if c < bytes.len() && bytes[c] == b'-' {
        c += 1;
    }
    while c < bytes.len()
        && ((bytes[c] >= b'0' && bytes[c] <= b'9') || bytes[c] == b'e' || bytes[c] == b'.')
    {
        c += 1;
        if c < bytes.len() && bytes[c - 1] == b'e' && bytes[c] == b'-' {
            c += 1;
        }
    }
    c
}

fn match_single(text: &str, pos: &mut usize, value: &mut f32) -> usize {
    let c = skip_float(text, pos);
    if c == *pos {
        c
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        0
    }
}

fn match_float(text: &str, pos: &mut usize, value: &mut f64) -> usize {
    let c = skip_float(text, pos);
    if c == *pos {
        c
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        0
    }
}

fn match_identifier(text: &str, pos: &mut usize, value: &mut String) -> usize {
    let mut c = *pos;
    let bytes = text.as_bytes();
    if c < bytes.len()
        && ((bytes[c] >= b'a' && bytes[c] <= b'z')
            || bytes[c] >= b'A' && bytes[c] <= b'Z'
            || bytes[c] == b'_')
    {
        c += 1;
        while c < bytes.len()
            && ((bytes[c] >= b'0' && bytes[c] <= b'9')
                || (bytes[c] >= b'a' && bytes[c] <= b'z')
                || bytes[c] >= b'A' && bytes[c] <= b'Z'
                || bytes[c] == b'_')
        {
            c += 1;
        }
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        0
    } else {
        c
    }
}

fn match_text(text: &str, pos: &mut usize, value: &mut String) -> usize {
    let mut c = *pos;
    let bytes = text.as_bytes();
    value.clear();
    if c < bytes.len() && (bytes[c] == b'"' || bytes[c] == b'\'') {
        let close = bytes[c];
        c += 1;
        while c < bytes.len() && bytes[c] != close {
            if bytes[c] == b'\\' {
                c += 1;
                if c == bytes.len() {
                    return *pos;
                }
                if bytes[c] == b'n' {
                    *value += "\n";
                } else if bytes[c] == b't' {
                    *value += "\t";
                } else if bytes[c] == b'\\' {
                    *value += "\\";
                } else if bytes[c] == b'"' {
                    *value += "\"";
                } else if bytes[c] == b'\'' {
                    *value += "\'";
                } else {
                    return *pos;
                }
            } else {
                let s = c;
                while c < bytes.len() && bytes[c] > 127 {
                    c += 1;
                }
                if c == bytes.len() || bytes[c] == close {
                    return *pos;
                }
                c += 1;
                *value += &text[s..c];
            }
        }
        if bytes[c] == close {
            *pos = c + 1;
            0
        } else {
            *pos
        }
    } else {
        *pos
    }
}

fn fill_file(path: &std::path::Path, store: &mut Store, file: &DbRef) -> bool {
    if let Ok(data) = path.metadata() {
        store.set_long(file.rec, file.pos + 8, data.len() as i64);
        store.set_byte(file.rec, file.pos + 16, 0, i32::from(data.is_dir()));
        true
    } else {
        false
    }
}

pub struct ShowDb<'a> {
    pub stores: &'a Stores,
    pub store: u16,
    pub rec: u32,
    pub pos: u32,
    pub known_type: u16,
    pub pretty: bool,
}

impl ShowDb<'_> {
    fn store(&self) -> &Store {
        let r = DbRef {
            store_nr: self.store,
            rec: 0,
            pos: 0,
        };
        self.stores.store(&r)
    }

    /**
    Write data from the database into String s.
    # Panics
    When the database is not correct.
    */
    pub fn write(&self, s: &mut String, indent: u16) {
        if self.known_type == 0 {
            s.push_str(&format!("{}", self.store().get_int(self.rec, self.pos)));
        } else if self.known_type == 1 {
            s.push_str(&format!("{}", self.store().get_long(self.rec, self.pos)));
        } else if self.known_type == 2 {
            s.push_str(&format!("{}", self.store().get_single(self.rec, self.pos)));
        } else if self.known_type == 3 {
            s.push_str(&format!("{}", self.store().get_float(self.rec, self.pos)));
        } else if self.known_type == 4 {
            s.push_str(if self.store().get_byte(self.rec, self.pos, 0) == 0 {
                "false"
            } else {
                "true"
            });
        } else if self.known_type == 5 {
            let text_nr = self.store().get_int(self.rec, self.pos) as u32;
            let text_val = self.store().get_str(text_nr);
            s.push('\"');
            s.push_str(text_val);
            s.push('\"');
        } else if (self.known_type as usize) < self.stores.types.len() {
            match &self.stores.types[self.known_type as usize].parts {
                Parts::Enum(vals) => {
                    let v = self.store().get_byte(self.rec, self.pos, 0);
                    let enum_val = if v == 0 {
                        "null"
                    } else {
                        &vals[v as usize - 1]
                    };
                    s.push_str(enum_val);
                }
                Parts::Struct(st) => {
                    self.write_struct(s, st, indent);
                }
                Parts::Vector(tp)
                | Parts::Sorted(tp, _)
                | Parts::Array(tp)
                | Parts::Ordered(tp, _)
                | Parts::Hash(tp, _)
                | Parts::Index(tp, _, _)
                | Parts::Spacial(tp, _) => {
                    self.write_list(s, *tp, indent);
                }
                Parts::Byte(from, nullable) => {
                    let v = self.store().get_byte(self.rec, self.pos, *from);
                    if *nullable && v == 255 {
                        s.push_str("null");
                    } else {
                        s.push_str(&format!("{v}"));
                    }
                }
                Parts::Short(from, nullable) => {
                    let v = self.store().get_short(self.rec, self.pos, *from);
                    if *nullable && v == 65535 {
                        s.push_str("null");
                    } else {
                        s.push_str(&format!("{v}"));
                    }
                }
                _ => {
                    panic!(
                        "Not matching parts:{:?} type:{} name:{}",
                        self.stores.types[self.known_type as usize].parts,
                        self.known_type,
                        self.stores.types[self.known_type as usize].name
                    )
                }
            }
        } else {
            panic!("Undefined known type {}", self.known_type)
        }
    }

    fn write_indent(&self, complex: bool, s: &mut String, indent: u16, zero_test: bool) {
        if complex && zero_test {
            s.push_str(&ShowDb::new_line(indent + 1));
        } else if self.pretty {
            s.push(' ');
        }
    }

    fn write_struct(&self, s: &mut String, fields: &[Field], indent: u16) {
        let complex = self.pretty && self.stores.types[self.known_type as usize].complex;
        // TODO reference to an object inside a field instead of the object itself, show the key
        s.push('{');
        if self.pretty {
            s.push(' ');
        }
        self.write_fields(s, fields, indent, complex);
        if complex {
            s.push_str(&ShowDb::new_line(indent));
        } else if self.pretty {
            s.push(' ');
        }
        s.push('}');
    }

    fn write_fields(&self, s: &mut String, fields: &[Field], indent: u16, complex: bool) {
        let mut first = true;
        for fld in fields {
            if (!fld.other_indexes.is_empty() && fld.other_indexes[0] == u16::MAX)
                || self.stores.is_null(
                    self.store(),
                    self.rec,
                    self.pos + u32::from(fld.position),
                    fld.content,
                )
            {
                continue;
            }
            if first {
                first = false;
            } else {
                s.push(',');
                self.write_indent(complex, s, indent, true);
            }
            s.push_str(&fld.name);
            s.push(':');
            if self.pretty {
                s.push(' ');
            }
            let sub = ShowDb {
                stores: self.stores,
                store: self.store,
                rec: self.rec,
                pos: self.pos + u32::from(fld.position),
                known_type: fld.content,
                pretty: self.pretty,
            };
            sub.write(s, indent + 1);
        }
    }

    fn new_line(indent: u16) -> String {
        let mut res = "\n".to_string();
        for _ in 0..indent {
            res += "  ";
        }
        res
    }

    fn write_list(&self, s: &mut String, content: u16, indent: u16) {
        let data = DbRef {
            store_nr: self.store,
            rec: self.rec,
            pos: self.pos,
        };
        let mut pos = self.stores.start(&data, self.known_type, &[]);
        let complex = self.pretty && self.stores.types[content as usize].complex;
        s.push('[');
        let mut first_elm = true;
        loop {
            let rec = self.stores.next(&data, &mut pos, self.known_type, &[]);
            if rec.rec == 0 {
                break;
            }
            if first_elm {
                if self.pretty {
                    self.write_indent(complex, s, indent, true);
                }
                first_elm = false;
            } else {
                s.push(',');
                if self.pretty {
                    if matches!(self.stores.types[content as usize].parts, Parts::Struct(_)) {
                        self.write_indent(true, s, indent, true);
                    } else {
                        self.write_indent(complex, s, indent, false);
                    }
                }
            }
            let sub = ShowDb {
                stores: self.stores,
                store: self.store,
                rec: rec.rec,
                pos: rec.pos,
                known_type: content,
                pretty: self.pretty,
            };
            sub.write(s, indent + 1);
        }
        if self.pretty {
            s.push(' ');
        }
        s.push(']');
    }
}

// These values are for amd64 or arm64 systems.
// It's not possible to test these continuously as these will fail on 32-bit systems.
#[test]
fn sizes() {
    /*
    assert_eq!(size_of::<DbRef>(), 12);
    assert_eq!(size_of::<String>(), 24);
    assert_eq!(size_of::<&str>(), 16);
    */
}
