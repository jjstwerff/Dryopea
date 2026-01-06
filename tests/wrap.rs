// Copyright (c) 2022-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

#[cfg(debug_assertions)]
use dryopea::data::Data;
use dryopea::interpreter::byte_code;
#[cfg(debug_assertions)]
use dryopea::interpreter::show_code;
use dryopea::parser::Parser;
use dryopea::scopes;
use dryopea::state::State;
#[cfg(debug_assertions)]
use std::fs::File;
#[cfg(debug_assertions)]
use std::io::{Error, Write};
use std::path::PathBuf;

#[test]
fn dir() -> std::io::Result<()> {
    let dir = "tests/suite";
    let mut files = Vec::new();
    for f in std::fs::read_dir(dir)? {
        files.push(f?.path());
    }
    files.sort();
    for entry in files {
        let own_file = entry
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("lav"));
        if !own_file {
            continue;
        }
        run_test(entry, false)?;
    }
    Ok(())
}

#[test]
fn last() -> std::io::Result<()> {
    run_test(PathBuf::from("tests/suite/16-parser.lav"), false)
}

fn run_test(entry: PathBuf, debug: bool) -> std::io::Result<()> {
    println!("run {entry:?}");
    let mut p = Parser::new();
    p.parse_dir("default", true, debug)?;
    #[cfg(debug_assertions)]
    let types = p.database.types.len();
    let path = entry.to_string_lossy().to_string();
    p.parse(&path, false);
    for l in p.diagnostics.lines() {
        println!("{l}");
    }
    if !p.diagnostics.is_empty() {
        return Err(Error::from(std::io::ErrorKind::InvalidData));
    }
    scopes::check(&mut p.data);
    let mut state = State::new(p.database);
    byte_code(&mut state, &mut p.data);
    #[cfg(debug_assertions)]
    let mut w = dump_results(entry, &mut p.data, types, &mut state)?;
    if debug {
        #[cfg(debug_assertions)]
        state.execute_log(&mut w, "main", &p.data)?;
        #[cfg(not(debug_assertions))]
        state.execute("main", &p.data);
    } else {
        state.execute("main", &p.data);
    }
    Ok(())
}

#[cfg(debug_assertions)]
fn dump_results(
    entry: PathBuf,
    data: &mut Data,
    types: usize,
    state: &mut State,
) -> Result<File, Error> {
    let filename = entry.file_name().unwrap_or_default().to_string_lossy();
    let mut w = File::create(format!("tests/code/{filename}.txt"))?;
    for tp in types..state.database.types.len() {
        writeln!(
            &mut w,
            "Type {tp}:{}",
            state.database.show_type(tp as u16, true)
        )?;
    }
    show_code(&mut w, state, data)?;
    Ok(w)
}
