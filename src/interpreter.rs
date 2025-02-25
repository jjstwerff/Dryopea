// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(dead_code)]
//! Fast interpreter for binary code, including library and coroutines support.
use crate::data::{Data, DefType};
use crate::state::State;
use crate::text;
use std::io::{Error, Write};
// Bytecode generation

/// Create byte code and dump the result to the given writer.
/// # Errors
/// When the writer didn't accept the data.
pub fn byte_code(data: &mut Data, writer: &mut dyn Write, state: &mut State) -> Result<(), Error> {
    text::init(state);
    for d_nr in 0..data.definitions() {
        let d = data.def(d_nr);
        if !matches!(d.def_type, DefType::Function) || d.is_operator() {
            continue;
        }
        let show = !data.def(d_nr).position.file.starts_with("default/");
        if show {
            write!(writer, "fn {} ", data.def(d_nr).name)?;
            data.show_code(writer, d_nr, &data.def(d_nr).code, 0, false)?;
            writeln!(writer, "\n")?;
            write!(writer, "byte-code for {}:", data.def(d_nr).position.file)?;
        }
        state.def_code(d_nr, data);
        if show {
            state.dump_code(writer, d_nr, data)?;
        }
    }
    Ok(())
}
