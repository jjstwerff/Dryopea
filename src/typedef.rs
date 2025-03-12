// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Calculate the positions of fields inside a record
#![allow(clippy::cast_possible_truncation)]

extern crate strum;

use crate::data::{Data, DefType, I32, Type, Value};
use crate::database::Stores;
use crate::diagnostics::Level;
use crate::lexer::Lexer;

/// Set the correct type and initial size in definitions.
/// This will not factor in the space for attributes for records
/// as we still need to analyze the actual use of records.
pub fn complete_definition(_lexer: &mut Lexer, data: &mut Data, d_nr: u32) {
    match data.def(d_nr).name.as_str() {
        "vector" => {
            data.set_returned(d_nr, Type::Vector(Box::new(Type::Unknown(0)), Vec::new()));
            data.definitions[d_nr as usize].known_type = 6;
        }
        "long" => {
            data.set_returned(d_nr, Type::Long);
            data.definitions[d_nr as usize].known_type = 1;
        }
        "integer" => {
            data.set_returned(d_nr, I32.clone());
            data.definitions[d_nr as usize].known_type = 0;
        }
        "float" => {
            data.set_returned(d_nr, Type::Float);
            data.definitions[d_nr as usize].known_type = 3;
        }
        "single" => {
            data.set_returned(d_nr, Type::Single);
            data.definitions[d_nr as usize].known_type = 2;
        }
        "text" => {
            data.set_returned(d_nr, Type::Text(false, Vec::new()));
            data.definitions[d_nr as usize].known_type = 5;
        }
        "boolean" => {
            data.set_returned(d_nr, Type::Boolean);
            data.definitions[d_nr as usize].known_type = 4;
        }
        "enumerate" => {
            data.set_returned(d_nr, Type::Enum(d_nr));
        }
        "function" => {
            data.set_returned(d_nr, Type::Routine(d_nr));
        }
        "radix" | "hash" | "reference" | "index" => {
            data.set_returned(d_nr, Type::Reference(d_nr, Vec::new()));
        }
        "keys_definition" => {
            data.set_returned(d_nr, Type::Keys);
            data.definitions[d_nr as usize].known_type = 7;
        }
        _ => {}
    }
}

fn copy_unknown_fields(data: &mut Data, d: u32) {
    for nr in 0..data.attributes(d) {
        if let Type::Unknown(was) = data.attr_type(d, nr) {
            data.set_attr_type(d, nr, data.def(was).returned.clone());
        }
    }
}

pub fn actual_types(data: &mut Data, database: &mut Stores, lexer: &mut Lexer, start_def: u32) {
    // Determine the actual type of structs regarding their use
    for d in start_def..data.definitions() {
        if data.def_type(d) == DefType::Struct {
            data.definitions[d as usize].returned = Type::Reference(d, Vec::new());
        }
    }
    for d in start_def..data.definitions() {
        match data.def_type(d) {
            DefType::Unknown => {
                lexer.pos_diagnostic(
                    Level::Error,
                    &data.def(d).position,
                    &format!("Error: Undefined type {}", data.def(d).name),
                );
            }
            DefType::Function => {
                copy_unknown_fields(data, d);
                if let Type::Unknown(was) = data.def(d).returned {
                    data.set_returned(d, data.def(was).returned.clone());
                }
            }
            DefType::Struct => {
                copy_unknown_fields(data, d);
            }
            DefType::Enum => {
                let e_nr = database.enumerate(&data.def(d).name.clone());
                for a in 0..data.attributes(d) {
                    database.value(e_nr, &data.attr_name(d, a));
                    data.set_attr_value(d, a, Value::Enum(a as u8 + 1, e_nr));
                }
                data.definitions[d as usize].known_type = e_nr;
            }
            _ => {}
        }
    }
}

pub fn fill_all(data: &mut Data, database: &mut Stores, start_def: u32) {
    for d_nr in start_def..data.definitions() {
        if data.def_type(d_nr) == DefType::Struct {
            fill_database(data, database, d_nr);
        }
    }
}

fn fill_database(data: &mut Data, database: &mut Stores, d_nr: u32) {
    if data.def(d_nr).name == "Unknown(0)" {
        return;
    }
    let s_type = database.structure(&data.def(d_nr).name);
    data.definitions[d_nr as usize].known_type = s_type;
    for a_nr in 0..data.attributes(d_nr) {
        if !data.attr_mutable(d_nr, a_nr) {
            continue;
        }
        let a_type = data.attr_type(d_nr, a_nr);
        let t_nr = data.type_elm(&a_type);
        let nullable = data.attr_nullable(d_nr, a_nr);
        if t_nr < u32::MAX {
            let tp = match a_type {
                Type::Vector(c_type, _) => {
                    let c_nr = data.type_elm(&c_type);
                    assert_ne!(
                        c_nr,
                        u32::MAX,
                        "Unknown vector {} content type on [{d_nr}]{}.{}",
                        c_type.name(data),
                        data.def(d_nr).name,
                        data.attr_name(d_nr, a_nr)
                    );
                    let c_tp = data.def(c_nr).known_type;
                    let tp = database.vector(c_tp);
                    data.check_vector(c_nr, tp, &data.def(d_nr).position.clone());
                    tp
                }
                Type::Integer(min, _) => {
                    let s = a_type.size(nullable);
                    if s == 1 {
                        database.byte(min, nullable)
                    } else if s == 2 {
                        database.short(min, nullable)
                    } else {
                        database.name("integer")
                    }
                }
                Type::Hash(content, key_fields, _) => {
                    database.hash(data.def(content).known_type, &key_fields)
                }
                Type::Index(content, key_fields, _) => {
                    database.index(data.def(content).known_type, &key_fields)
                }
                Type::Sorted(content, key_fields, _) => {
                    database.sorted(data.def(content).known_type, &key_fields)
                }
                Type::Spacial(content, key_fields, _) => {
                    database.spacial(data.def(content).known_type, &key_fields)
                }
                _ => data.def(t_nr).known_type,
            };
            database.field(s_type, &data.attr_name(d_nr, a_nr), tp);
        }
    }
}
