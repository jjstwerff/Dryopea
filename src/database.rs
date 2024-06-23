// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Database operations on stores
use crate::store::Store;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum Container {
    None,
    Vector,
    Key(u16, Vec<u16>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Field {
    name: String,
    container: Container,
    content: u16,
    position: u16,
    // TODO define default values for fields
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Type {
    name: String,
    parts: Parts,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Struct {
    fields: Vec<Field>,
    size: u16,
    parent: u16,
    childs: Vec<u16>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum Parts {
    Base,
    Struct(Box<Struct>),
    Enum(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KnownTypes {
    types: Vec<Type>,
}

#[allow(dead_code)]
impl KnownTypes {
    pub fn new() -> KnownTypes {
        let mut types = KnownTypes { types: Vec::new() };
        types.types.push(Type {
            name: "integer".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "long".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "single".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "float".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "boolean".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "text".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "vector".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "sorted".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "hash".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "index".to_string(),
            parts: Parts::Base,
        });
        types.types.push(Type {
            name: "radix".to_string(),
            parts: Parts::Base,
        });
        types
    }

    pub fn structure(&mut self, name: String, size: u16, parent: u16) -> u16 {
        let num = self.types.len() as u16;
        self.types.push(Type {
            name,
            parts: Parts::Struct(Box::new(Struct {
                fields: Vec::new(),
                size,
                parent,
                childs: Vec::new(),
            })),
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
        let parent = self.types[known_type as usize].name.clone();
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            if position >= s.size {
                panic!(
                    "Field {} outside structure {} size {}",
                    name, parent, s.size
                );
            }
            let num = s.fields.len() as u16;
            s.fields.push(Field {
                name,
                container: Container::None,
                content,
                position,
            });
            num
        } else {
            panic!("Adding field {} to a non structure type {}", name, parent);
        }
    }

    pub fn vector(&mut self, known_type: u16, field: u16) {
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            s.fields[field as usize].container = Container::Vector;
        } else {
            panic!(
                "Mutating a field on a non structure {}",
                self.types[known_type as usize].name
            );
        }
    }

    pub fn key(&mut self, known_type: u16, field: u16, container: u16, key: u16) {
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            if let Container::Key(_, _) = s.fields[field as usize].container {
            } else {
                s.fields[field as usize].container = Container::Key(container, Vec::new());
            }
            if let Container::Key(_, keys) = &mut s.fields[field as usize].container {
                keys.push(key);
            }
        }
    }

    pub fn enumerate(&mut self, name: String) -> u16 {
        let num = self.types.len() as u16;
        self.types.push(Type {
            name,
            parts: Parts::Enum(Vec::new()),
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

pub struct DataBase<'a> {
    types: &'a KnownTypes,
    store: &'a Store,
    rec: u32,
    pos: u32,
    known_type: u16,
    pretty: bool,
}

impl<'a> DataBase<'a> {
    #[allow(dead_code)]
    pub fn new(
        types: &'a KnownTypes,
        store: &'a Store,
        rec: u32,
        pos: u32,
        known_type: u16,
        pretty: bool,
    ) -> DataBase<'a> {
        DataBase {
            types,
            store,
            rec,
            pos,
            known_type,
            pretty,
        }
    }
}

impl<'a> core::fmt::Display for DataBase<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.known_type == 0 {
            return write!(f, "{}", self.store.get_int(self.rec, self.pos as isize));
        } else if self.known_type == 1 {
            return write!(f, "{}", self.store.get_long(self.rec, self.pos as isize));
        } else if self.known_type == 2 {
            return write!(f, "{}", self.store.get_single(self.rec, self.pos as isize));
        } else if self.known_type == 3 {
            return write!(f, "{}", self.store.get_float(self.rec, self.pos as isize));
        } else if self.known_type == 4 {
            return write!(
                f,
                "{}",
                if self.store.get_byte(self.rec, self.pos as isize, 0) == 0 {
                    "false"
                } else {
                    "true"
                }
            );
        } else if self.known_type == 5 {
            return write!(
                f,
                "\"{}\"",
                self.store
                    .get_str(self.store.get_int(self.rec, self.pos as isize) as u32)
            );
        }
        // TODO reference to an object inside a field instead of the object itself, show the key
        if let Parts::Struct(s) = &self.types.types[self.known_type as usize].parts {
            if !s.childs.is_empty() {
                let actual = self.store.get_short(self.rec, 4, 0) as u16;
                write!(f, "type:\"{}\",", self.types.types[actual as usize].name)?;
                if self.pretty {
                    f.write_str(" \n")?
                }
                let sub = DataBase {
                    types: self.types,
                    store: self.store,
                    rec: self.rec,
                    pos: self.pos,
                    known_type: actual,
                    pretty: self.pretty,
                };
                return sub.fmt(f);
            }
            f.write_str("{")?;
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
                    if self.pretty {
                        f.write_str(" ")?
                    }
                }
                write!(f, "{}:", fld.name)?;
                if self.pretty {
                    f.write_str(" ")?
                }
                if fld.container != Container::None {
                    let contain = self.store.get_int(self.rec, self.pos as isize) as u32;
                    // TODO not all structures are vectors, however sorted is and hash is somewhat
                    f.write_str("[")?;
                    let mut first_elm = true;
                    for index in 0..self.store.get_int(contain, 4) {
                        if first_elm {
                            f.write_str(",")?;
                            if self.pretty {
                                f.write_str(" ")?
                            }
                        } else {
                            first_elm = false;
                        }
                        let sub = DataBase {
                            types: self.types,
                            store: self.store,
                            rec: contain,
                            pos: 8 + index as u32 * s.size as u32,
                            known_type: fld.content,
                            pretty: self.pretty,
                        };
                        sub.fmt(f)?
                    }
                    f.write_str("]")?;
                } else {
                    let sub = DataBase {
                        types: self.types,
                        store: self.store,
                        rec: self.rec,
                        pos: self.pos + fld.position as u32,
                        known_type: fld.content,
                        pretty: self.pretty,
                    };
                    sub.fmt(f)?
                }
            }
            f.write_str("}")?;
        } else if let Parts::Enum(vals) = &self.types.types[self.rec as usize].parts {
            f.write_str(&vals[self.store.get_byte(self.rec, self.pos as isize, 0) as usize])?;
        }
        Ok(())
    }
}
