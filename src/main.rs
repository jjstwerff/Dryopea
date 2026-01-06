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

fn main() {
    let mut args = env::args_os();
    args.next();
    let mut file_name = String::new();
    let mut dir = project_dir();
    while let Some(arg) = args.next() {
        let a = arg.to_str().unwrap();
        if a == "--version" {
            println!("lavition {}", env!("CARGO_PKG_VERSION"));
            return;
        } else if a == "--path" {
            dir = args.next().unwrap().to_str().unwrap().to_string();
        } else if a == "--help" || a == "-h" || a == "-?" {
            println!("usage: lavition [option] [file]");
            println!("Options:");
            println!("--version       : print version information");
            println!("-h, --help, -?  : print this help message");
            println!("--path [DIR]    : set a specific project directory");
            return;
        } else if a.starts_with('-') {
            println!("unknown option: {a}");
            println!("usage: lavition [option] [file]");
            println!("Try `lavition --help` for more information.");
            return;
        } else if file_name.is_empty() {
            file_name = a.to_string();
        } else {
            // TODO allow arguments to be passed to the program
            println!("Duplicate file name: {a}");
            return;
        }
    }
    let mut p = parser::Parser::new();
    p.parse_dir(&(dir + "default"), true, false).unwrap();
    if file_name.is_empty() {
        println!("lavition: no input file specified.");
        println!("usage: lavition [option] [file]");
        return;
    }
    p.parse(&file_name, false);
    if !p.diagnostics.is_empty() {
        for l in p.diagnostics.lines() {
            println!("{l}");
        }
        return;
    }
    scopes::check(&mut p.data);
    let mut state = State::new(p.database);
    interpreter::byte_code(&mut state, &mut p.data);
    state.execute("main", &p.data);
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
    if dir.ends_with("target/debug/") {
        dir = &dir[..dir.len() - 13];
    }
    dir.to_string()
}
