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
mod log_config;
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
    let mut project: Option<String> = None;
    let mut lib_dirs: Vec<String> = Vec::new();
    while let Some(arg) = args.next() {
        let a = arg.to_str().unwrap();
        if a == "--version" {
            println!("lavition {}", env!("CARGO_PKG_VERSION"));
            return;
        } else if a == "--path" {
            dir = args.next().unwrap().to_str().unwrap().to_string();
        } else if a == "--project" {
            project = Some(args.next().unwrap().to_str().unwrap().to_string());
        } else if a == "--lib" {
            lib_dirs.push(args.next().unwrap().to_str().unwrap().to_string());
        } else if a == "--help" || a == "-h" || a == "-?" {
            println!("usage: lavition [options] <file>");
            println!("Options:");
            println!("  --version          print version information");
            println!("  -h, --help, -?     print this help message");
            println!("  --path <dir>       directory containing the default/ library (default: binary location)");
            println!("  --project <dir>    run the script as if launched from <dir>; file I/O is");
            println!("                     sandboxed there and its lib/ sub-directory is searched");
            println!("                     for 'use' imports (useful when the script lives in /tmp)");
            println!("  --lib <dir>        add <dir> to the 'use' import search path; may be");
            println!("                     repeated for multiple directories");
            return;
        } else if a.starts_with('-') {
            println!("unknown option: {a}");
            println!("usage: lavition [options] <file>");
            println!("Try `lavition --help` for more information.");
            std::process::exit(1);
        } else if file_name.is_empty() {
            file_name = a.to_string();
        } else {
            // TODO allow arguments to be passed to the program
            println!("Duplicate file name: {a}");
            std::process::exit(1);
        }
    }
    if file_name.is_empty() {
        println!("lavition: no input file specified.");
        println!("usage: lavition [options] <file>");
        std::process::exit(1);
    }
    // Resolve the script path to absolute before potentially changing directory.
    let abs_file = std::path::Path::new(&file_name)
        .canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(&file_name));
    let abs_file = abs_file.to_str().unwrap().to_string();
    // --project: change working directory so file I/O is sandboxed to the project root.
    if let Some(ref proj) = project {
        if let Err(e) = env::set_current_dir(proj) {
            println!("Error: cannot change to project directory '{proj}': {e}");
            std::process::exit(1);
        }
        // Also expose the project's lib/ sub-directory for 'use' imports.
        lib_dirs.insert(0, format!("{proj}/lib"));
    }
    let mut p = parser::Parser::new();
    p.lib_dirs = lib_dirs;
    p.parse_dir(&(dir + "default"), true, false).unwrap();
    p.parse(&abs_file, false);
    if !p.diagnostics.is_empty() {
        for l in p.diagnostics.lines() {
            println!("{l}");
        }
        std::process::exit(1);
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
