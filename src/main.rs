// Copyright (c) 2022-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![warn(clippy::pedantic)]

#[macro_use]
pub mod diagnostics;
mod calc;
mod data;
mod database;
mod external;
mod fill;
mod hash;
mod interpreter;
mod keys;
mod lexer;
mod parser;
mod png_store;
mod scopes;
mod stack;
mod state;
mod store;
mod text;
mod tree;
mod typedef;
mod variables;
mod vector;

use crate::state::State;
use std::env;

fn main() -> std::io::Result<()> {
    let mut args = env::args_os();
    args.next();
    let mut p = parser::Parser::new();
    let dir = project_dir();
    p.parse_dir(&(dir + "default"), true)?;
    if let Some(file_name) = args.next() {
        p.parse(file_name.to_str().unwrap(), false);
    }
    for l in p.diagnostics.lines() {
        println!("{l}");
    }
    if !p.diagnostics.is_empty() {
        return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
    }
    scopes::check(&mut p.data);
    let mut state = State::new(p.database);
    interpreter::byte_code(&mut state, &mut p.data);
    state.execute("main", &p.data);
    Ok(())
    //state.execute_log(&mut w, "main", &p.data)
}

fn project_dir() -> String {
    let direct = if let Ok(prog) = env::current_exe() {
        prog.to_str().unwrap().to_string()
    } else {
        String::new()
    };
    let mut dir = if direct.ends_with("lavition") {
        &direct[0..direct.len() - 8]
    } else {
        &direct
    };
    if dir.ends_with("target/release/") {
        dir = &dir[..dir.len() - 15];
    }
    dir.to_string()
}
