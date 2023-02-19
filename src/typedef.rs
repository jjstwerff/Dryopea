// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Calculate the positions of fields inside a record

extern crate strum;

use crate::data::{Data, Type};
use crate::lexer::Lexer;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

/// Set the correct type and initial size in definitions.
/// This will not factor in the space for attributes for records
/// as we still need to analyze the actual use of records.
pub fn complete_definition(_lexer: &mut Lexer, data: &mut Data, d_nr: u32) {
    match data.def_name(d_nr).as_str() {
        "vector" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Vector(Box::new(Type::Unknown)));
        }
        "long" => {
            data.def_set_size(d_nr, 8, 8);
            data.set_returned(d_nr, Type::Long);
        }
        "integer" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Integer);
        }
        "float" => {
            data.def_set_size(d_nr, 8, 8);
            data.set_returned(d_nr, Type::Float);
        }
        "single" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Single);
        }
        "text" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Text);
        }
        "boolean" => {
            data.def_set_size(d_nr, 1, 1);
            data.set_returned(d_nr, Type::Boolean);
        }
        "enumerate" => {
            data.def_set_size(d_nr, 1, 1);
            data.set_returned(d_nr, Type::Enum(d_nr));
        }
        "function" => {
            data.def_set_size(d_nr, 4, 4);
            data.set_returned(d_nr, Type::Routine(d_nr));
        }
        _ => {
            if data.returned(d_nr) == Type::Unknown {
                data.def_set_size(d_nr, 4, 4);
                data.set_returned(d_nr, Type::Reference(d_nr));
            }
        }
    }
}

/// Possibly move References to records on attributes to Inner.
/// Calculate positions of attributes within definitions.
#[allow(dead_code)]
pub fn analyze(data: &mut Data) {
    for d_nr in 0..data.definitions() {
        finish(d_nr, data);
    }
}

fn finish(d_nr: u32, data: &mut Data) {
    if data.finished(d_nr) {
        return;
    }
    data.finish(d_nr);
    if let Type::Enum(_) = data.returned(d_nr) {
        finish_attributes(data, d_nr);
        calculate_positions(data, d_nr);
    }
    if let Type::Reference(_) = data.returned(d_nr) {
        finish_attributes(data, d_nr);
        if data.def_inline(d_nr) {
            data.set_returned(d_nr, Type::Inner(d_nr));
        }
        inner_records(data, d_nr);
        calculate_positions(data, d_nr);
    }
}

// This should not be run on routines.
/// Check if a Reference to a record could be moved from their own allocation to an Inner record.
fn inner_records(data: &mut Data, d_nr: u32) {
    for anr in 0..data.attributes(d_nr) {
        let atd = &data.attr_type(d_nr, anr);
        if let Type::Reference(sub) = atd {
            if data.def_inline(*sub) {
                data.set_returned(d_nr, Type::Inner(*sub));
            }
        } else if let Type::Vector(cont) = atd {
            if let Type::Reference(sub) = &(**cont) {
                if data.def_inline(*sub) {
                    data.set_returned(d_nr, Type::Vector(Box::new(Type::Inner(*sub))));
                }
            }
        }
    }
}

pub fn finish_attributes(data: &mut Data, d_nr: u32) {
    let mut todo = HashSet::new();
    for a in 0..data.attributes(d_nr) {
        let elm = data.type_elm(&data.attr_type(d_nr, a));
        if !data.finished(elm) {
            todo.insert(elm);
        }
    }
    for elm in todo {
        finish(elm, data);
    }
}

fn calculate_positions(data: &mut Data, d_nr: u32) {
    // A gap on position. The only gaps allowed are due to their alignments
    let mut gaps = HashMap::new();
    // Keep space for the claimed record size and start on the first 8 byte alignment position after that
    let mut pos = 8;
    // Check if we have an "reference" field on position 4 already, otherwise register a gap.
    if data.attr(d_nr, "reference") == u16::MAX {
        gaps.insert(4, 4);
    }
    let mut align = 1;
    let mut positions = HashMap::new();
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
    for al in [8, 4, 2, 1] {
        for nr in 0..data.attributes(d_nr) {
            let a_pos = data.attr_pos(d_nr, nr);
            if a_pos > 0 && a_pos < u32::MAX {
                continue;
            }
            let sub = data.type_def_nr(&data.attr_type(d_nr, nr));
            let size = data.def_size(sub);
            let sub_size = if let Type::Inner(_) = data.attr_type(d_nr, nr) {
                if data.attr(d_nr, "reference") == u16::MAX {
                    // this for example a parent
                    size - 8
                } else if data.def_align(sub) != 8 {
                    // only claim 4 less when we already are on a 4 byte or lower alignment
                    size - 4
                } else {
                    size
                }
            } else {
                size
            };
            if data.def_align(sub) == al {
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
                        if align == 1 {
                            align = al;
                        }
                    }
                }
            }
        }
    }
    for (n, pos) in positions {
        data.set_attr_pos(d_nr, n, pos);
    }
    data.def_set_size(d_nr, pos, align);
}
