// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Database operations on stores
use crate::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
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

    pub fn enumerate(&mut self, name: &str) -> u16 {
        let num = self.types.len() as u16;
        self.types.push(Type {
            name: name.to_string(),
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

    #[allow(dead_code)]
    pub fn to_enum(&self, known_type: u16, value: &str) -> u16 {
        if let Parts::Enum(values) = &self.types[known_type as usize].parts {
            for (idx, val) in values.iter().enumerate() {
                if val == value {
                    return idx as u16;
                }
            }
        }
        u16::MAX
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
    pub stores: Vec<Store>,
    max: u16,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct DbRef {
    pub store_nr: u16,
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

    pub fn type_claim(&self, tp: u16) -> u32 {
        (self.types.types[tp as usize].size as u32 + 7) / 8
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

    pub fn null() -> DbRef {
        DbRef {
            store_nr: u16::MAX,
            rec: 0,
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

    pub fn parse(&mut self, text: &str, tp: u16, result: DbRef) -> bool {
        let mut pos = 0;
        self.parsing(text, &mut pos, tp, result)
    }

    fn parsing(&mut self, text: &str, pos: &mut usize, tp: u16, result: DbRef) -> bool {
        if match_null(text, pos) {
            self.set_default_value(tp, result);
        }
        match &self.types.types[tp as usize].parts {
            Parts::Base => {
                if !self.parse_simple(text, pos, tp, result) {
                    return false;
                }
            }
            Parts::Vector(content) => {
                match_empty(text, pos);
                if match_token(text, pos, b'[') {
                    match_empty(text, pos);
                    if match_token(text, pos, b']') {
                        return true;
                    }
                    loop {
                        let res = self.vector_append(
                            &Vector::DbRef(result),
                            1,
                            self.types.types[*content as usize].size as u32,
                        );
                        if !self.parsing(text, pos, *content, res) {
                            return false;
                        }
                        match_empty(text, pos);
                        if !match_token(text, pos, b',') {
                            break;
                        }
                        match_empty(text, pos);
                    }
                    match_empty(text, pos);
                    if !match_token(text, pos, b']') {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            Parts::Struct(object) => {
                if match_token(text, pos, b'{') {
                    match_empty(text, pos);
                    if match_token(text, pos, b'}') {
                        return true;
                    }
                    let fld = if result.rec == 0 { 0 } else { result.pos };
                    let rec = if result.rec == 0 {
                        let size = self.types.types[tp as usize].size;
                        self.mut_store(&result).claim((size as u32 + 7) / 8)
                    } else {
                        result.rec
                    };
                    let mut found_fields = HashSet::new();
                    loop {
                        let mut field_name = "".to_string();
                        if !match_identifier(text, pos, &mut field_name) {
                            return false;
                        }
                        match_empty(text, pos);
                        if !match_token(text, pos, b':') {
                            return false;
                        }
                        match_empty(text, pos);
                        for f in &object.fields {
                            if f.name == field_name {
                                let field = DbRef {
                                    store_nr: result.store_nr,
                                    rec,
                                    pos: fld + f.position as u32,
                                };
                                if !self.parsing(text, pos, f.content, field) {
                                    return false;
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
                        return false;
                    }
                    for f in &object.fields {
                        if !found_fields.contains(&f.name) {
                            let field = DbRef {
                                store_nr: result.store_nr,
                                rec,
                                pos: result.pos + f.position as u32,
                            };
                            self.set_default_value(f.content, field);
                        }
                    }
                } else {
                    return false;
                }
            }
            Parts::Enum(fields) => {
                let mut value = "".to_string();
                if !match_text(text, pos, &mut value) {
                    return false;
                }
                let mut val = 0;
                for f in fields {
                    if *f == value {
                        break;
                    }
                    val += 1;
                }
                self.mut_store(&result)
                    .set_int(result.rec, result.pos as isize, val);
            }
            Parts::Byte(from, _null) => {
                let mut value = 0;
                if !match_integer(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_byte(result.rec, result.pos as isize, *from, value);
            }
            Parts::Short(from, _null) => {
                let mut value = 0;
                if !match_integer(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_short(result.rec, result.pos as isize, *from, value);
            }
        }
        true
    }

    fn parse_simple(&mut self, text: &str, pos: &mut usize, tp: u16, result: DbRef) -> bool {
        match tp {
            0 => {
                let mut value = 0;
                if !match_integer(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_int(result.rec, result.pos as isize, value);
            }
            1 => {
                let mut value = 0;
                if !match_long(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_long(result.rec, result.pos as isize, value);
            }
            2 => {
                let mut value = 0.0;
                if !match_single(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_single(result.rec, result.pos as isize, value);
            }
            3 => {
                let mut value = 0.0;
                if !match_float(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result)
                    .set_float(result.rec, result.pos as isize, value);
            }
            4 => {
                let mut value = false;
                if !match_boolean(text, pos, &mut value) {
                    return false;
                }
                self.mut_store(&result).set_byte(
                    result.rec,
                    result.pos as isize,
                    0,
                    if value { 1 } else { 0 },
                );
            }
            5 => {
                let mut value = "".to_string();
                if !match_text(text, pos, &mut value) {
                    return false;
                }
                let text_pos = self.mut_store(&result).set_str(&value);
                self.mut_store(&result)
                    .set_int(result.rec, result.pos as isize, text_pos as i32);
            }
            _ => {
                return false;
            }
        }
        true
    }

    fn set_default_value(&mut self, tp: u16, rec: DbRef) {
        if tp < 6 {
            match tp {
                0 => {
                    self.mut_store(&rec)
                        .set_int(rec.rec, rec.pos as isize, i32::MIN);
                }
                1 => {
                    self.mut_store(&rec)
                        .set_long(rec.rec, rec.pos as isize, i64::MIN);
                }
                2 => {
                    self.mut_store(&rec)
                        .set_single(rec.rec, rec.pos as isize, f32::NAN);
                }
                3 => {
                    self.mut_store(&rec)
                        .set_float(rec.rec, rec.pos as isize, f64::NAN);
                }
                4 => {
                    self.mut_store(&rec)
                        .set_byte(rec.rec, rec.pos as isize, 0, 0);
                }
                5 => {
                    self.mut_store(&rec).set_int(rec.rec, rec.pos as isize, 0);
                }
                _ => {}
            }
            return;
        }
        let known_type = &self.types.types[tp as usize];
        match &known_type.parts {
            Parts::Enum(_) => {
                self.mut_store(&rec)
                    .set_byte(rec.rec, rec.pos as isize, 0, 255);
            }
            Parts::Byte(_, null) => {
                self.mut_store(&rec).set_byte(
                    rec.rec,
                    rec.pos as isize,
                    0,
                    if *null { 255 } else { 0 },
                );
            }
            Parts::Short(_, null) => {
                self.mut_store(&rec).set_short(
                    rec.rec,
                    rec.pos as isize,
                    0,
                    if *null { 65535 } else { 0 },
                );
            }
            _ => {
                panic!("not implemented yet!")
            }
        }
    }

    pub fn length_vector(&self, v: &Vector) -> u32 {
        if let Vector::DbRef(db) = v {
            let store = self.store(db);
            let v_rec = store.get_int(db.rec, db.pos as isize) as u32;
            if v_rec == 0 {
                0
            } else {
                store.get_int(v_rec, 4) as u32
            }
        } else if let Vector::DbSlice(_, l) = v {
            *l
        } else {
            0
        }
    }

    pub fn clear_vector(&mut self, v: &Vector) {
        if let Vector::DbRef(db) = v {
            let store = self.mut_store(db);
            let v_rec = store.get_int(db.rec, db.pos as isize) as u32;
            if v_rec != 0 {
                // Only set size of the vector to 0
                // TODO when the main path to a separate allocated objects: remove these
                // TODO lower string reference counts where needed
                store.set_int(v_rec, 4, 0);
            }
        }
    }

    pub fn slice_vector(&self, v: &Vector, size: u32, from: i32, till: i32) -> Vector {
        let l = self.length_vector(v);
        let f = if from < 0 { from + l as i32 } else { from };
        let t = if till == i32::MIN {
            from + 1
        } else if till < from {
            from
        } else {
            till
        };
        match v {
            Vector::DbRef(db) => Vector::DbSlice(
                DbRef {
                    store_nr: db.store_nr,
                    rec: db.rec,
                    pos: db.pos + size * f as u32,
                },
                (t - f) as u32,
            ),
            Vector::DbSlice(db, _) => Vector::DbSlice(
                DbRef {
                    store_nr: db.store_nr,
                    rec: db.rec,
                    pos: db.pos + size * f as u32,
                },
                (t - f) as u32,
            ),
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

    pub fn get_vector(&self, v: &Vector, size: u32, index: i32) -> DbRef {
        match self.slice_vector(v, size, index, index + 1) {
            Vector::DbSlice(db, _) => db,
            Vector::DbRef(db) => db,
        }
    }

    pub fn vector_append(&mut self, v: &Vector, add: u32, size: u32) -> DbRef {
        match v {
            Vector::DbRef(db) => {
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
            Vector::DbSlice(db, _) => {
                let store = self.mut_store(db);
                let r = store.claim(1);
                store.set_int(r, 4, 0);
                let res = Vector::DbRef(DbRef {
                    store_nr: db.store_nr,
                    rec: r,
                    pos: 0,
                });
                self.vector_add(&res, v, size);
                self.vector_append(&res, add, size)
            }
        }
    }

    // TODO copy child records & strings during copy (string reference counting on same)
    pub fn vector_add(&mut self, v: &Vector, other: &Vector, size: u32) {
        match v {
            Vector::DbRef(db) => {
                let o_length = self.length_vector(other);
                if o_length == 0 {
                    // Other vector has no data
                    return;
                }
                let o_db = match other {
                    Vector::DbRef(db) => db,
                    Vector::DbSlice(db, _) => db,
                };
                let o_rec = match other {
                    Vector::DbRef(db) => self.store(db).get_int(db.rec, db.pos as isize) as u32,
                    Vector::DbSlice(db, _) => db.rec,
                };
                let o_pos = match other {
                    Vector::DbRef(_) => 0,
                    Vector::DbSlice(db, _) => db.pos,
                };
                let new_db = self.vector_append(v, o_length, size);
                if db.store_nr == o_db.store_nr {
                    self.mut_store(db).copy_block(
                        o_rec,
                        o_pos as isize,
                        new_db.rec,
                        new_db.pos as isize,
                        o_length as isize * size as isize,
                    )
                } else {
                    let o_store: &Store;
                    let db_store: &mut Store;
                    unsafe {
                        o_store = (*(self as *const Stores)).store(o_db);
                        db_store = (*(self as *mut Stores)).mut_store(db);
                    }
                    o_store.copy_block_between(
                        o_rec,
                        o_pos as isize,
                        db_store,
                        new_db.rec,
                        new_db.pos as isize,
                        o_length as isize * size as isize,
                    )
                }
            }
            Vector::DbSlice(db, _) => {
                let store = self.mut_store(db);
                let r = store.claim(1);
                store.set_int(r, 4, 0);
                let res = Vector::DbRef(DbRef {
                    store_nr: db.store_nr,
                    rec: r,
                    pos: 0,
                });
                self.vector_add(&res, v, size);
                self.vector_add(&res, other, size)
            }
        }
    }

    // TODO change slice to its own vector on updating it
    pub fn insert_vector(&mut self, v: &Vector, size: u32, index: i32) -> DbRef {
        let len = self.length_vector(v);
        let real = if index < 0 { index + len as i32 } else { index };
        let db = match v {
            Vector::DbRef(db) => db,
            Vector::DbSlice(db, _) => db,
        };
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

    // TODO update slice to full vector on updating it
    // TODO child records of removed records, lower string reference counts
    pub fn remove_vector(&mut self, v: &Vector, size: u32, index: i32) -> bool {
        let db = match v {
            Vector::DbRef(db) => db,
            Vector::DbSlice(db, _) => db,
        };
        let len = self.length_vector(v);
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

    pub fn get_file(&mut self, file: DbRef) -> bool {
        if file.rec == 0 {
            return false;
        }
        let store = self.mut_store(&file);
        let filename = store.get_str(store.get_int(file.rec, file.pos as isize + 4) as u32);
        let path = std::path::Path::new(filename);
        fill_file(path, store, file)
    }

    pub fn get_dir(&mut self, file_path: String, result: DbRef) -> bool {
        let path = std::path::Path::new(&file_path);
        if let Ok(iter) = std::fs::read_dir(path) {
            let vector = DbRef {
                store_nr: result.store_nr,
                rec: result.rec,
                pos: result.pos + 16,
            };
            for entry in iter.flatten() {
                let v = Vector::DbRef(vector);
                let elm = self.vector_append(&v, 1, 17);
                let path = entry.path();
                if let Some(name) = path.to_str() {
                    let store = self.mut_store(&result);
                    let name_pos = store.set_str(name) as i32;
                    store.set_int(elm.rec, elm.pos as isize + 4, name_pos);
                    if !fill_file(&path, store, elm) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }

    pub fn get_png(&mut self, file_path: String, result: DbRef) -> bool {
        let store = self.mut_store(&result);
        if let Ok((img, width, height)) = crate::png_store::read(&file_path, store) {
            if let Some(name) = std::path::Path::new(&file_path).file_name() {
                let name_pos = store.set_str(name.to_str().unwrap());
                store.set_int(result.rec, result.pos as isize + 4, name_pos as i32);
                store.set_int(result.rec, result.pos as isize + 8, width as i32);
                store.set_int(result.rec, result.pos as isize + 12, height as i32);
                store.set_int(result.rec, result.pos as isize + 16, img as i32);
                true
            } else {
                false
            }
        } else {
            false
        }
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

fn match_boolean(text: &str, pos: &mut usize, value: &mut bool) -> bool {
    if text.len() >= *pos + 4 && &text[*pos..*pos + 4] == "true" {
        *pos += 4;
        *value = true;
        true
    } else if text.len() >= *pos + 5 && &text[*pos..*pos + 5] == "false" {
        *pos += 4;
        *value = false;
        true
    } else {
        false
    }
}

fn match_integer(text: &str, pos: &mut usize, value: &mut i32) -> bool {
    let mut c = *pos;
    let bytes = text.as_bytes();
    if c < bytes.len() && bytes[c] == b'-' {
        c += 1;
    }
    while c < bytes.len() && bytes[c] >= b'0' && bytes[c] <= b'9' {
        c += 1;
    }
    if c == *pos {
        false
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        true
    }
}

fn match_long(text: &str, pos: &mut usize, value: &mut i64) -> bool {
    let mut c = *pos;
    let bytes = text.as_bytes();
    if c < bytes.len() && bytes[c] == b'-' {
        c += 1;
    }
    while c < bytes.len() && bytes[c] >= b'0' && bytes[c] <= b'9' {
        c += 1;
    }
    if c == *pos {
        false
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        true
    }
}

fn match_single(text: &str, pos: &mut usize, value: &mut f32) -> bool {
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
    if c == *pos {
        false
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        true
    }
}

fn match_float(text: &str, pos: &mut usize, value: &mut f64) -> bool {
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
    if c == *pos {
        false
    } else {
        *value = text[*pos..c].parse().unwrap();
        *pos = c;
        true
    }
}

fn match_identifier(text: &str, pos: &mut usize, value: &mut String) -> bool {
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
        true
    } else {
        false
    }
}

fn match_text(text: &str, pos: &mut usize, value: &mut String) -> bool {
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
                    return false;
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
                    return false;
                }
            } else {
                let s = c;
                while c < bytes.len() && bytes[c] > 127 {
                    c += 1;
                }
                if c == bytes.len() || bytes[c] == close {
                    return false;
                }
                c += 1;
                *value += &text[s..c];
            }
        }
        if bytes[c] != close {
            return false;
        }
        *pos = c + 1;
    }
    true
}

fn fill_file(path: &std::path::Path, store: &mut Store, file: DbRef) -> bool {
    if let Ok(data) = path.metadata() {
        store.set_long(file.rec, file.pos as isize + 8, data.len() as i64);
        store.set_byte(
            file.rec,
            file.pos as isize + 16,
            0,
            if data.is_dir() { 1 } else { 0 },
        );
        true
    } else {
        false
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

#[derive(Debug, PartialEq, Clone)]
pub enum Vector {
    /// A part of a database vector, can only point to an immutable vector
    DbSlice(DbRef, u32),
    /// A mutable database vector, pointing to the field holding the actual position
    DbRef(DbRef),
}
