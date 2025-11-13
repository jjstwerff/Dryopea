// Copyright (c) 2024-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(dead_code)]
//! Fast interpreter for binary code.
use crate::data::{Data, DefType};
use crate::state::State;
use crate::text;
use crate::variables::Function;
use std::io::{Error, Write};
// Bytecode generation

/// Create byte code.
pub fn byte_code(state: &mut State, data: &mut Data) {
    text::init(state);
    for d_nr in 0..data.definitions() {
        if !matches!(data.def(d_nr).def_type, DefType::Function) || data.def(d_nr).is_operator() {
            continue;
        }
        state.def_code(d_nr, data);
    }
}

/// Dump byte code result to the given writer.
/// # Errors
/// When the writer didn't accept the data.
pub fn show_code(writer: &mut dyn Write, state: &mut State, data: &mut Data) -> Result<(), Error> {
    for d_nr in 0..data.definitions() {
        if !matches!(data.def(d_nr).def_type, DefType::Function) || data.def(d_nr).is_operator() {
            continue;
        }
        let show = !data.def(d_nr).position.file.starts_with("default/");
        if show {
            write!(writer, "{} ", data.def(d_nr).header(data, d_nr))?;
            let mut vars = Function::copy(&data.def(d_nr).variables);
            data.show_code(writer, &mut vars, &data.def(d_nr).code, 0, false)?;
            writeln!(writer, "\n")?;
            write!(writer, "byte-code for {}:", data.def(d_nr).position.file)?;
            state.dump_code(writer, d_nr, data)?;
        }
    }
    Ok(())
}
