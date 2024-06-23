// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Calculate the positions of fields inside a record

extern crate strum;

use crate::data::{Data, DefType, Type};
use crate::database::KnownTypes;
use crate::diagnostics::Level;
use crate::lexer::Lexer;
use std::cmp::Ordering;
use std::collections::HashMap;

/// Set the correct type and initial size in definitions.
/// This will not factor in the space for attributes for records
/// as we still need to analyze the actual use of records.
pub fn complete_definition(_lexer: &mut Lexer, data: &mut Data, d_nr: u32) {
    match data.def_name(d_nr).as_str() {
        "vector" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Vector(Box::new(Type::Unknown(0))));
            data.set_known_type(d_nr, 6);
        }
        "long" => {
            data.def_set_size(d_nr, 8, 8);
            data.set_returned(d_nr, Type::Long);
            data.set_known_type(d_nr, 1);
        }
        "integer" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Integer);
            data.set_known_type(d_nr, 0);
        }
        "float" => {
            data.def_set_size(d_nr, 8, 8);
            data.set_returned(d_nr, Type::Float);
            data.set_known_type(d_nr, 3);
        }
        "single" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Single);
            data.set_known_type(d_nr, 2);
        }
        "text" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Text);
            data.set_known_type(d_nr, 5);
        }
        "boolean" => {
            data.def_set_size(d_nr, 1, 1);
            data.set_returned(d_nr, Type::Boolean);
            data.set_known_type(d_nr, 4);
        }
        "enumerate" => {
            data.def_set_size(d_nr, 1, 1);
            data.set_returned(d_nr, Type::Enum(d_nr));
        }
        "function" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Routine(d_nr));
        }
        "radix" | "hash" | "reference" | "index" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Reference(d_nr));
        }
        _ => {}
    }
}

pub fn actual_types(
    data: &mut Data,
    known_types: &mut KnownTypes,
    lexer: &mut Lexer,
    start_def: u32,
) {
    // Determine the actual type of structs regarding their use
    for d in start_def..data.definitions() {
        if data.def_type(d) == DefType::Struct {
            let mut inner = false;
            for id in start_def..data.definitions() {
                if data.def_type(d) == DefType::Struct {
                    for a in 0..data.attributes(id) {
                        if let Type::Vector(sub) = data.attr_type(id, a) {
                            if *sub == Type::Reference(d) || *sub == Type::Unknown(d) {
                                inner = true;
                            }
                        }
                    }
                }
            }
            if data.def_referenced(d) {
                inner = false;
            }
            data.set_returned(
                d,
                if inner {
                    Type::Inner(d)
                } else {
                    Type::Reference(d)
                },
            );
        }
    }
    for d in start_def..data.definitions() {
        match data.def_type(d) {
            DefType::Unknown => {
                lexer.pos_diagnostic(
                    Level::Error,
                    data.def_pos(d),
                    &format!("Error: Undefined type {}", data.def_name(d)),
                );
            }
            DefType::Function => {
                for a in 0..data.attributes(d) {
                    if let Type::Unknown(was) = data.attr_type(d, a) {
                        data.set_attr_type(d, a, data.returned(was))
                    }
                }
                if let Type::Unknown(was) = data.returned(d) {
                    data.set_returned(d, data.returned(was))
                }
            }
            DefType::Struct => {
                for nr in 0..data.attributes(d) {
                    if let Type::Unknown(was) = data.attr_type(d, nr) {
                        data.set_attr_type(d, nr, data.returned(was));
                    }
                }
                calculate_positions(data, known_types, d);
            }
            DefType::Enum => {
                let e_nr = known_types.enumerate(data.def_name(d));
                for a in 0..data.attributes(d) {
                    known_types.value(e_nr, data.attr_name(d, a));
                }
                data.set_known_type(d, e_nr);
            }
            _ => {}
        }
    }
}

fn calculate_positions(data: &mut Data, known_types: &mut KnownTypes, d_nr: u32) {
    // A gap on position. The only gaps allowed are due to their alignments
    let mut gaps = HashMap::new();
    // Keep space for the claimed record size and start on the first 8 byte alignment position after that
    let mut positions = HashMap::new();
    let mut pos = initial_pos(data, d_nr, &mut positions);
    // Check if we have a "reference" field on position 4 already, otherwise register a gap.
    if !data.def_inner(d_nr) && data.attr(d_nr, "reference") == u16::MAX {
        gaps.insert(4, 4);
    }
    let mut struct_align = 1;
    for al in [8, 4, 2, 1] {
        for nr in 0..data.attributes(d_nr) {
            let a_pos = data.attr_pos(d_nr, nr);
            if a_pos != u32::MAX {
                continue;
            }
            let tp = data.attr_type(d_nr, nr);
            let size;
            let align;
            if tp == Type::Link {
                size = 9;
                align = 4;
            } else {
                let sub = data.type_def_nr(&tp);
                size = data.def_size(sub);
                align = data.def_align(sub);
            }
            let sub_size = if let Type::Inner(_) = data.attr_type(d_nr, nr) {
                if data.attr(d_nr, "reference") == u16::MAX {
                    // this for example a parent
                    size - 8
                } else if align != 8 {
                    // only claim 4 less when we already are on a 4 byte or lower alignment
                    size - 4
                } else {
                    size
                }
            } else {
                size
            };
            if align == al {
                let mut first = 0;
                let mut first_size = 0;
                for (&gap_pos, &size) in &gaps {
                    if size >= sub_size {
                        first = gap_pos;
                        first_size = size;
                        break;
                    }
                }
                match first_size.cmp(&sub_size) {
                    Ordering::Equal => {
                        gaps.remove(&first);
                        positions.insert(nr, first);
                    }
                    Ordering::Greater => {
                        // claim the back side of the gap
                        let new_size = first_size - sub_size;
                        gaps.insert(first, new_size);
                        positions.insert(nr, first + new_size);
                    }
                    Ordering::Less => {
                        positions.insert(nr, pos);
                        pos += sub_size;
                        if struct_align == 1 {
                            struct_align = al;
                        }
                    }
                }
            }
        }
    }
    for (n, pos) in positions {
        data.set_attr_pos(d_nr, n, pos);
    }
    data.def_set_size(d_nr, pos, struct_align);
    let s_type = known_types.structure(data.def_name(d_nr), data.def_size(d_nr) as u16, u16::MAX);
    data.set_known_type(d_nr, s_type);
    for a_nr in 0..data.attributes(d_nr) {
        let a_type = data.attr_type(d_nr, a_nr);
        let t_nr = data.type_elm(&a_type);
        if t_nr < u32::MAX {
            let tp = data.def_known_type(t_nr);
            let f_nr = known_types.field(
                s_type,
                data.attr_name(d_nr, a_nr),
                tp,
                data.attr_pos(d_nr, a_nr) as u16,
            );
            if let Type::Vector(_) = a_type {
                known_types.vector(s_type, f_nr);
            }
        }
    }
}

fn initial_pos(data: &mut Data, d_nr: u32, positions: &mut HashMap<u16, u32>) -> u32 {
    let mut pos = if data.def_inner(d_nr) { 0 } else { 8 };
    // fill positions with immutable fields with known positions
    for nr in 0..data.attributes(d_nr) {
        let a_pos = data.attr_pos(d_nr, nr);
        if a_pos > 0 && a_pos < u32::MAX {
            // when there is a position of an immutable field keep this space free
            positions.insert(nr, a_pos);
            let sub = data.type_def_nr(&data.attr_type(d_nr, nr));
            let size = data.def_size(sub)
                - if let Type::Inner(_) | Type::Reference(_) = data.attr_type(d_nr, nr) {
                    8
                } else {
                    0
                };
            pos = std::cmp::max(pos, a_pos + size);
        }
    }
    pos
}
