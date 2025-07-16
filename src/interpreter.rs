// Copyright (c) 2024 Jurjen Stellingwerff
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

/// Create byte code and dump the result to the given writer.
/// # Errors
/// When the writer didn't accept the data.
pub fn byte_code(writer: &mut dyn Write, state: &mut State, data: &mut Data) -> Result<(), Error> {
    text::init(state);
    for d_nr in 0..data.definitions() {
        let d = data.def(d_nr);
        if !matches!(d.def_type, DefType::Function) || d.is_operator() {
            continue;
        }
        let show = !data.def(d_nr).position.file.starts_with("default/");
        if show {
            write!(writer, "{} ", data.def(d_nr).header(data, d_nr))?;
            let mut vars = Function::copy(&data.def(d_nr).variables);
            data.show_code(writer, &mut vars, &data.def(d_nr).code, 0, false, d_nr)?;
            writeln!(writer, "\n")?;
            write!(writer, "byte-code for {}:", data.def(d_nr).position.file)?;
        }
        state.def_code(d_nr, data, show, writer);
    }
    Ok(())
}
