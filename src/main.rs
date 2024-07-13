// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#[macro_use]
pub mod diagnostics;
mod data;
mod database;
mod lexer;
mod parser;
mod png_store;
mod store;
mod typedef;
mod types;
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
    let cli: Cli = Cli::parse();
    let mut p = parser::Parser::new();
    p.parse_dir("default", true)?;
    p.parse_dir(cli.dir.to_str().unwrap(), false)
}
