// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Database operations on stores
use crate::store::Store;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Container {
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
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KnownTypes {
    types: Vec<Type>,
}

impl KnownTypes {
    pub fn new() -> KnownTypes {
        let mut types = KnownTypes { types: Vec::new() };
        types.types.push(Type {
            name: "integer".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: false,
        });
        types.types.push(Type {
            name: "long".to_string(),
            parts: Parts::Base,
            size: 8,
            complex: false,
        });
        types.types.push(Type {
            name: "single".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: false,
        });
        types.types.push(Type {
            name: "float".to_string(),
            parts: Parts::Base,
            size: 8,
            complex: false,
        });
        types.types.push(Type {
            name: "boolean".to_string(),
            parts: Parts::Base,
            size: 1,
            complex: false,
        });
        types.types.push(Type {
            name: "text".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types.types.push(Type {
            name: "vector".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types.types.push(Type {
            name: "sorted".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types.types.push(Type {
            name: "hash".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types.types.push(Type {
            name: "index".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types.types.push(Type {
            name: "radix".to_string(),
            parts: Parts::Base,
            size: 4,
            complex: true,
        });
        types
    }

    pub fn structure(&mut self, name: String, size: u16, parent: u16) -> u16 {
        let num = self.types.len() as u16;
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
        let parent = self.types[known_type as usize].name.clone();
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            if position >= size {
                panic!(
                    "Field {}.{name} outside structure {parent} size {size}",
                    self.types[known_type as usize].name
                );
            }
            let num = s.fields.len() as u16;
            s.fields.push(Field {
                name,
                container: Container::None,
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
            panic!("Adding field {name} to a non structure type {parent}");
        }
    }

    pub fn vector(&mut self, known_type: u16, field: u16) {
        if let Parts::Struct(s) = &mut self.types[known_type as usize].parts {
            s.fields[field as usize].container = Container::Vector;
            self.types[known_type as usize].complex = true;
        } else {
            panic!(
                "Mutating a field on a non structure {}",
                self.types[known_type as usize].name
            );
        }
    }

    #[allow(dead_code)]
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
            if value > 0 || value as usize <= values.len() {
                return values[value as usize - 1].clone();
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
pub struct DataBase<'a> {
    types: &'a KnownTypes,
    store: &'a Store,
    rec: u32,
    pos: u32,
    container: Container,
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
        container: Container,
        known_type: u16,
        pretty: bool,
    ) -> DataBase<'a> {
        DataBase {
            types,
            store,
            rec,
            pos,
            container,
            known_type,
            pretty,
        }
    }

    fn write(&self, f: &mut Formatter<'_>, indent: u16) -> Result {
        if self.container != Container::None {
            return self.write_list(f, indent);
        }
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
            let enum_val = if v <= 0 {
                "null"
            } else {
                &vals[v as usize - 1]
            };
            write!(f, "{}", enum_val)
        } else if let Parts::Struct(s) = &self.types.types[self.known_type as usize].parts {
            self.write_struct(f, s, indent)
        } else if let Parts::Enum(vals) = &self.types.types[self.rec as usize].parts {
            f.write_str(&vals[self.store.get_byte(self.rec, self.pos as isize, 0) as usize])
        } else {
            panic!("Not matching parts");
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
            let sub = DataBase {
                types: self.types,
                store: self.store,
                rec: self.rec,
                pos: self.pos,
                container: self.container.clone(),
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
            if fld.container == Container::None
                && self.types.is_null(
                    self.store,
                    self.rec,
                    self.pos + fld.position as u32,
                    fld.content,
                )
            {
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
            let sub = DataBase {
                types: self.types,
                store: self.store,
                rec: self.rec,
                pos: self.pos + fld.position as u32,
                container: fld.container.clone(),
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

    fn write_list(&self, f: &mut Formatter<'_>, indent: u16) -> Result {
        let contain = self.store.get_int(self.rec, self.pos as isize) as u32;
        let complex = self.pretty && self.types.types[self.known_type as usize].complex;
        f.write_str("[")?;
        if complex && indent > 0 {
            f.write_str(&self.new_line(indent + 1))?;
        } else if self.pretty {
            f.write_str(" ")?;
        }
        let mut first_elm = true;
        let size = self.types.types[self.known_type as usize].size;
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
            let sub = DataBase {
                types: self.types,
                store: self.store,
                rec: contain,
                pos: 8 + index as u32 * size as u32,
                container: Container::None,
                known_type: self.known_type,
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

impl<'a> core::fmt::Display for DataBase<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.write(f, 0)
    }
}
