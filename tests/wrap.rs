// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

use dryopea::interpreter::byte_code;
use dryopea::parser::Parser;
use dryopea::scopes;
use dryopea::state::State;
use std::io::Write;

#[test]
fn dir() -> std::io::Result<()> {
    let debug = false;
    let dir = "tests/suite";
    let mut files = Vec::new();
    for f in std::fs::read_dir(dir)? {
        files.push(f?.path());
    }
    files.sort();
    for entry in files {
        // if !entry.file_name().unwrap().to_str().unwrap().starts_with("15-") { continue; }
        println!("run {entry:?}");
        let own_file = entry
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("lav"));
        if !own_file {
            continue;
        }
        let mut p = Parser::new();
        p.parse_dir("default", true)?;
        let types = p.database.types.len();
        let path = entry.to_string_lossy().to_string();
        p.parse(&path, false);
        for l in p.diagnostics.lines() {
            println!("{l}");
        }
        if !p.diagnostics.is_empty() {
            return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
        }
        scopes::check(&mut p.data);
        let mut state = State::new(p.database);
        let filename = entry.file_name().unwrap_or_default().to_string_lossy();
        let mut w = std::fs::File::create(format!("tests/code/{filename}.txt"))?;
        for tp in types..state.database.types.len() {
            writeln!(
                &mut w,
                "Type {tp}:{}",
                state.database.show_type(tp as u16, true)
            )?;
        }
        byte_code(&mut w, &mut state, &mut p.data)?;
        if debug {
            state.execute_log(&mut w, "main", &p.data)?;
        } else {
            state.execute(p.data.def_nr("main"), &p.data);
        }
    }
    Ok(())
}
