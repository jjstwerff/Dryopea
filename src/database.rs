// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Database operations on stores
use crate::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Field {
    name: String,
    content: u16,
    position: u16,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Type {
    name: String,
    parts: Parts,
    complex: bool,
    size: u16,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Struct {
    fields: Vec<Field>,
    parent: u16,
    childs: Vec<u16>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum Parts {
    Base,
    Struct(Box<Struct>),
    Enum(Vec<String>),
    Vector(u16),
    Byte(i32, bool),
    Short(i32, bool), // TODO also allow for sorted, radix, index, hash etc
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KnownTypes {
    types: Vec<Type>,
    names: HashMap<String, u16>,
}

impl KnownTypes {
    pub fn new() -> KnownTypes {
        let mut types = KnownTypes {
            types: Vec::new(),
            names: HashMap::new(),
        };
        types.base_type("integer", 4, false); // 0
        types.base_type("long", 8, false); // 1
        types.base_type("single", 4, false); // 2
        types.base_type("float", 8, false); // 3
        types.base_type("boolean", 1, false); // 4
        types.base_type("text", 4, true); // 5
        types
    }

    fn base_type(&mut self, name: &str, size: u16, complex: bool) {
        self.names.insert(name.to_string(), self.types.len() as u16);
        self.types.push(Type {
            name: name.to_string(),
            parts: Parts::Base,
            size,
            complex,
        });
    }

    pub fn structure(&mut self, name: String, size: u16, parent: u16) -> u16 {
        let num = self.types.len() as u16;
        self.names.insert(name.clone(), num);
        self.types.push(Type {
            name,
            size,
            parts: Parts::Struct(Box::new(Struct {
                fields: Vec::new(),
                parent,
                childs: Vec::new(),
            })),
            complex: false,
        });
        if parent != u16::MAX {
            if let Parts::Struct(s) = &mut self.types[parent as usize].parts {
                s.childs.push(num);
            } else {
                panic!(
                    "Adding a child structures to a non structure type {}",
                    self.types[parent as usize].name
                );
            }
        }
        num
    }

    pub fn field(&mut self, known_type: u16, name: String, content: u16, position: u16) -> u16 {
        if content == u16::MAX {
            return 0;
        }
        let size = self.types[known_type as usize].size;
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            if position >= size {
                panic!(
                    "Field {}.{name} outside structure size {size}",
                    self.types[known_type as usize].name
                );
            }
            let num = s.fields.len() as u16;
            s.fields.push(Field {
                name,
                content,
                position,
            });
            if num > 8
                || self.types[content as usize].complex
                || matches!(self.types[content as usize].parts, Parts::Struct(_))
            {
                self.types[known_type as usize].complex = true;
            }
            num
        } else {
            panic!(
                "Adding field {name} to a non structure type {}",
                self.types[known_type as usize].name
            );
        }
    }

    pub fn vector(&mut self, content: u16) -> u16 {
        let name = "vector<".to_string() + &self.types[content as usize].name + ">";
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types.push(Type {
                name,
                size: 4,
                parts: Parts::Vector(content),
                complex: true,
            });
            num
        }
    }

    pub fn byte(&mut self, min: i32, nullable: bool) -> u16 {
        let name = format!("byte<{min},{nullable}>");
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types.push(Type {
                name,
                size: 1,
                parts: Parts::Byte(min, nullable),
                complex: false,
            });
            num
        }
    }

    pub fn short(&mut self, min: i32, nullable: bool) -> u16 {
        let name = format!("short<{min},{nullable}>");
        if let Some(nr) = self.names.get(&name) {
            *nr
        } else {
            let num = self.types.len() as u16;
            self.types.push(Type {
                name,
                size: 2,
                parts: Parts::Short(min, nullable),
                complex: false,
            });
            num
        }
    }

    pub fn enumerate(&mut self, name: String) -> u16 {
        let num = self.types.len() as u16;
        self.types.push(Type {
            name,
            size: 1,
            parts: Parts::Enum(Vec::new()),
            complex: false,
        });
        num
    }

    pub fn value(&mut self, known_type: u16, name: String) -> u16 {
        if let Parts::Enum(values) = &mut self.types[known_type as usize].parts {
            let num = values.len() as u16;
            values.push(name);
            num
        } else {
            panic!(
                "Adding a value to a non enum type {}",
                self.types[known_type as usize].name
            );
        }
    }

    #[allow(dead_code)]
    // We do not compile inter/external in main as that would result in a slew of dead_code warnings
    pub fn enum_val(&self, known_type: u16, value: u16) -> String {
        if let Parts::Enum(values) = &self.types[known_type as usize].parts {
            if (value as usize) < values.len() {
                return values[value as usize].clone();
            }
        }
        "null".to_string()
    }

    pub fn is_null(&self, store: &Store, rec: u32, pos: u32, known_type: u16) -> bool {
        if known_type == 0 {
            store.get_int(rec, pos as isize) == i32::MIN
        } else if known_type == 1 {
            store.get_long(rec, pos as isize) == i64::MIN
        } else if known_type == 2 {
            store.get_single(rec, pos as isize).is_nan()
        } else if known_type == 3 {
            store.get_float(rec, pos as isize).is_nan()
        } else if known_type == 4 {
            store.get_byte(rec, pos as isize, 0) > 1
        } else {
            // All structure including references to records
            store.get_int(rec, pos as isize) == 0
        }
    }
}

impl Default for KnownTypes {
    fn default() -> Self {
        KnownTypes::new()
    }
}

impl std::fmt::Display for KnownTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", serde_json::to_string_pretty(self).unwrap())
    }
}

#[allow(dead_code)]
pub struct Stores<'a> {
    pub types: &'a KnownTypes,
    stores: Vec<Store>,
    max: u16,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct DbRef {
    store_nr: u16,
    pub rec: u32,
    pub pos: u32,
}

#[allow(dead_code)]
impl<'a> Stores<'a> {
    pub fn new(types: &KnownTypes) -> Stores {
        Stores {
            types,
            stores: Vec::new(),
            max: 0,
        }
    }

    pub fn database(&mut self, size: u32) -> DbRef {
        if self.max >= self.stores.len() as u16 {
            self.stores.push(Store::new(100));
        }
        let store = &mut self.stores[self.max as usize];
        let rec = store.claim(size);
        self.max += 1;
        DbRef {
            store_nr: self.max - 1,
            rec,
            pos: 0,
        }
    }

    pub fn claim(&mut self, db: &DbRef, size: u32) -> DbRef {
        let store = &mut self.stores[db.store_nr as usize];
        let rec = store.claim(size);
        DbRef {
            store_nr: db.store_nr,
            rec,
            pos: 0,
        }
    }

    pub fn store(&self, r: &DbRef) -> &Store {
        &self.stores[r.store_nr as usize]
    }

    pub fn mut_store(&mut self, r: &DbRef) -> &mut Store {
        &mut self.stores[r.store_nr as usize]
    }

    pub fn show(&self, db: &DbRef, tp: u16, pretty: bool) -> String {
        let store = self.store(db);
        format!(
            "{}",
            ShowDb {
                types: self.types,
                store,
                rec: db.rec,
                pos: db.pos,
                known_type: tp,
                pretty,
            }
        )
    }

    pub fn length_vector(&self, db: &DbRef) -> u32 {
        let store = self.store(db);
        let v_rec = store.get_int(db.rec, db.pos as isize) as u32;
        if v_rec == 0 {
            0
        } else {
            store.get_int(v_rec, 4) as u32
        }
    }

    pub fn clear_vector(&mut self, db: &DbRef) {
        let store = self.mut_store(db);
        let v_rec = store.get_int(db.rec, db.pos as isize) as u32;
        if v_rec != 0 {
            // Only set size of the vector to 0
            // TODO when the main path to a separate allocated objects: remove these
            store.set_int(v_rec, 4, 0);
        }
    }

    pub fn get_ref(&self, db: &DbRef, fld: u32) -> DbRef {
        let store = self.store(db);
        let res = store.get_int(db.rec, db.pos as isize + fld as isize) as u32;
        DbRef {
            store_nr: db.store_nr,
            rec: res,
            pos: 0,
        }
    }

    pub fn get_field(&self, db: &DbRef, fld: u32) -> DbRef {
        DbRef {
            store_nr: db.store_nr,
            rec: db.rec,
            pos: fld,
        }
    }

    pub fn clear_text(&mut self, db: &DbRef) {
        let store = self.mut_store(db);
        let text_rec = store.get_int(db.rec, db.pos as isize) as u32;
        self.mut_store(db).set_int(db.rec, db.pos as isize, 0);
        self.mut_store(db).delete(text_rec);
    }

    pub fn get_vector(&self, db: &DbRef, size: u32, index: i32) -> DbRef {
        let len = self.length_vector(db) as i32;
        let store = self.store(db);
        let mut vec_rec = store.get_int(db.rec, db.pos as isize) as u32;
        let mut pos = 0;
        if vec_rec != 0 {
            let real = if index < 0 { index + len } else { index };
            if real >= 0 && real < len {
                pos = 8 + real as u32 * size
            } else {
                vec_rec = 0
            }
        }
        DbRef {
            store_nr: db.store_nr,
            rec: vec_rec,
            pos,
        }
    }

    pub fn vector_append(&mut self, db: &DbRef, add: u32, size: u32) -> DbRef {
        let store = self.mut_store(db);
        let mut vec_rec = store.get_int(db.rec, db.pos as isize) as u32;
        let new_length;
        if vec_rec == 0 {
            // claim a new array with minimal 11 elements
            vec_rec = store.claim(((add + 10) * size + 15) / 8);
            store.set_int(db.rec, db.pos as isize, vec_rec as i32);
            new_length = add;
        } else {
            new_length = add + store.get_int(vec_rec, 4) as u32;
            let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
            if new_vec != vec_rec {
                store.set_int(db.rec, db.pos as isize, new_vec as i32);
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

    pub fn vector_add(&mut self, db: &DbRef, other: &DbRef, size: u32) {
        let o_length = self.length_vector(other);
        if o_length == 0 {
            // Other vector has no data
            return;
        }
        let o_rec = self.store(other).get_int(other.rec, other.pos as isize) as u32;
        let new_db = self.vector_append(db, o_length, size);
        if db.store_nr == other.store_nr {
            self.mut_store(db).copy_block(
                o_rec,
                0,
                new_db.rec,
                new_db.pos as isize,
                o_length as isize * size as isize,
            )
        } else {
            let o_store: &Store;
            let db_store: &mut Store;
            unsafe {
                o_store = (*(self as *const Stores)).store(other);
                db_store = (*(self as *mut Stores)).mut_store(db);
            }
            o_store.copy_block_between(
                o_rec,
                0,
                db_store,
                new_db.rec,
                new_db.pos as isize,
                o_length as isize * size as isize,
            )
        }
    }

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
        let store = self.mut_store(db);
        let mut vec_rec = store.get_int(db.rec, db.pos as isize) as u32;
        let new_length;
        if vec_rec == 0 {
            // claim a new array with minimal 11 elements
            vec_rec = store.claim((11 * size + 15) / 8);
            store.set_int(db.rec, db.pos as isize, vec_rec as i32);
            new_length = 1;
        } else {
            new_length = len + 1;
            let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
            if new_vec != vec_rec {
                store.set_int(db.rec, db.pos as isize, new_vec as i32);
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

    pub fn remove_vector(&mut self, db: &DbRef, size: u32, index: i32) -> bool {
        let len = self.length_vector(db);
        let real = if index < 0 { index + len as i32 } else { index };
        let store = self.mut_store(db);
        let vec_rec = store.get_int(db.rec, db.pos as isize) as u32;
        if real < 0 || real >= len as i32 || vec_rec == 0 {
            return false;
        }
        store.copy_block(
            vec_rec,
            8 + size as isize * (real as isize + 1),
            vec_rec,
            8 + size as isize * real as isize,
            (len as isize - real as isize) * size as isize,
        );
        store.set_int(vec_rec, 4, len as i32 - 1);
        true
    }
}

pub struct ShowDb<'a> {
    types: &'a KnownTypes,
    store: &'a Store,
    rec: u32,
    pos: u32,
    known_type: u16,
    pretty: bool,
}

impl<'a> ShowDb<'a> {
    #[allow(dead_code)]
    pub fn new(
        types: &'a KnownTypes,
        store: &'a Store,
        rec: u32,
        pos: u32,
        known_type: u16,
        pretty: bool,
    ) -> ShowDb<'a> {
        ShowDb {
            types,
            store,
            rec,
            pos,
            known_type,
            pretty,
        }
    }

    fn write(&self, f: &mut Formatter<'_>, indent: u16) -> Result {
        if self.known_type == 0 {
            return write!(f, "{}", self.store.get_int(self.rec, self.pos as isize));
        } else if self.known_type == 1 {
            return write!(f, "{}", self.store.get_long(self.rec, self.pos as isize));
        } else if self.known_type == 2 {
            return write!(f, "{}", self.store.get_single(self.rec, self.pos as isize));
        } else if self.known_type == 3 {
            return write!(f, "{}", self.store.get_float(self.rec, self.pos as isize));
        } else if self.known_type == 4 {
            let val = if self.store.get_byte(self.rec, self.pos as isize, 0) == 0 {
                "false"
            } else {
                "true"
            };
            return write!(f, "{}", val);
        } else if self.known_type == 5 {
            let text_nr = self.store.get_int(self.rec, self.pos as isize) as u32;
            let text_val = self.store.get_str(text_nr);
            return write!(f, "\"{}\"", text_val);
        }
        if let Parts::Enum(vals) = &self.types.types[self.known_type as usize].parts {
            let v = self.store.get_byte(self.rec, self.pos as isize, 0);
            let enum_val = if v >= vals.len() as i32 {
                "null"
            } else {
                &vals[v as usize]
            };
            write!(f, "{}", enum_val)
        } else if let Parts::Struct(s) = &self.types.types[self.known_type as usize].parts {
            self.write_struct(f, s, indent)
        } else if let Parts::Vector(tp) = &self.types.types[self.known_type as usize].parts {
            self.write_list(f, *tp, indent)
        } else if let Parts::Byte(from, nullable) =
            &self.types.types[self.known_type as usize].parts
        {
            let v = self.store.get_byte(self.rec, self.pos as isize, *from);
            if *nullable && v == 255 {
                f.write_str("null")
            } else {
                write!(f, "{}", v)
            }
        } else if let Parts::Short(from, nullable) =
            &self.types.types[self.known_type as usize].parts
        {
            let v = self.store.get_short(self.rec, self.pos as isize, *from);
            if *nullable && v == 65535 {
                f.write_str("null")
            } else {
                write!(f, "{}", v)
            }
        } else {
            panic!(
                "Not matching parts:{:?} type:{} name:{}",
                self.types.types[self.known_type as usize].parts,
                self.known_type,
                self.types.types[self.known_type as usize].name
            )
        }
    }

    fn write_struct(&self, f: &mut Formatter<'_>, s: &Struct, indent: u16) -> Result {
        let complex = self.pretty && self.types.types[self.known_type as usize].complex;
        // TODO reference to an object inside a field instead of the object itself, show the key
        f.write_str("{")?;
        if complex && indent > 0 {
            f.write_str(&self.new_line(indent + 1))?;
        } else if self.pretty {
            f.write_str(" ")?;
        }
        if !s.childs.is_empty() {
            let actual = self.store.get_short(self.rec, 4, 0) as u16;
            write!(f, "type:\"{}\",", self.types.types[actual as usize].name)?;
            if complex {
                f.write_str(&self.new_line(indent + 1))?
            } else if self.pretty {
                f.write_str(" ")?
            }
            let sub = ShowDb {
                types: self.types,
                store: self.store,
                rec: self.rec,
                pos: self.pos,
                known_type: actual,
                pretty: self.pretty,
            };
            if let Parts::Struct(actual_s) = &self.types.types[actual as usize].parts {
                sub.write_fields(f, actual_s, indent, complex)?;
            } else {
                panic!("Incorrect actual struct")
            }
        } else {
            self.write_fields(f, s, indent, complex)?;
        }
        if complex {
            f.write_str(&self.new_line(indent))?;
        }
        f.write_str("}")
    }

    fn write_fields(
        &self,
        f: &mut Formatter<'_>,
        s: &Struct,
        indent: u16,
        complex: bool,
    ) -> Result {
        let mut first = true;
        for fld in &s.fields {
            if self.types.is_null(
                self.store,
                self.rec,
                self.pos + fld.position as u32,
                fld.content,
            ) {
                continue;
            }
            if first {
                first = false;
            } else {
                f.write_str(",")?;
                if complex {
                    f.write_str(&self.new_line(indent + 1))?
                } else if self.pretty {
                    f.write_str(" ")?
                }
            }
            write!(f, "{}:", fld.name)?;
            if self.pretty {
                f.write_str(" ")?
            }
            let sub = ShowDb {
                types: self.types,
                store: self.store,
                rec: self.rec,
                pos: self.pos + fld.position as u32,
                known_type: fld.content,
                pretty: self.pretty,
            };
            sub.write(f, indent + 1)?
        }
        Ok(())
    }

    fn new_line(&self, indent: u16) -> String {
        let mut res = "\n".to_string();
        for _ in 0..indent {
            res += "  ";
        }
        res
    }

    fn write_list(&self, f: &mut Formatter<'_>, content: u16, indent: u16) -> Result {
        let contain = self.store.get_int(self.rec, self.pos as isize) as u32;
        let complex = self.pretty && self.types.types[content as usize].complex;
        f.write_str("[")?;
        if complex && indent > 0 {
            f.write_str(&self.new_line(indent + 1))?;
        } else if self.pretty {
            f.write_str(" ")?;
        }
        let mut first_elm = true;
        let size = self.types.types[content as usize].size;
        for index in 0..self.store.get_int(contain, 4) {
            if first_elm {
                first_elm = false;
            } else {
                f.write_str(",")?;
                if complex {
                    f.write_str(&self.new_line(indent + 1))?;
                } else if self.pretty {
                    f.write_str(" ")?
                }
            }
            let sub = ShowDb {
                types: self.types,
                store: self.store,
                rec: contain,
                pos: 8 + index as u32 * size as u32,
                known_type: content,
                pretty: self.pretty,
            };
            sub.write(f, indent + 1)?
        }
        if self.pretty {
            f.write_str(" ")?
        }
        f.write_str("]")
    }
}

impl<'a> core::fmt::Display for ShowDb<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.write(f, 0)
    }
}
