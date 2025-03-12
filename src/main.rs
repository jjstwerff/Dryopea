// Copyright (c) 2022 Jurjen Stellingwerff
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
mod variables;

mod logger;
mod parser;
mod png_store;
mod stack;
mod state;
mod store;
mod text;
mod tree;
mod typedef;
mod vector;
use crate::state::State;
use clap::Parser;

#[derive(Parser)]
#[command(author = "Jurjen Stellingwerff <j.stellingwerff@gmail.com>",
    version = "0.1.0",
    about = "Parse gcp files",
    long_about = None)]
struct Cli {
    /// Directory to be parsed
    #[arg(value_name = "DIR", required = false, default_value = ".")]
    dir: std::path::PathBuf,
}

fn main() -> std::io::Result<()> {
    let mut w = std::fs::File::create("log.txt")?;
    let cli: Cli = Cli::parse();
    let mut p = parser::Parser::new();
    p.parse_dir("default", true)?;
    p.parse_dir(cli.dir.to_str().unwrap(), false)?;
    let mut state = State::new(p.database);
    interpreter::byte_code(&mut w, &mut state, &mut p.data)?;
    state.execute_log(&mut w, "test", &p.data)
}
