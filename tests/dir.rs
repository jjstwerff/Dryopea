// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

use dryopea::interpreter::byte_code;
use dryopea::parser::Parser;
use dryopea::state::State;

#[test]
fn dir() -> std::io::Result<()> {
    let dir = "tests/suite";
    for f in std::fs::read_dir(dir)? {
        let entry = f?.path();
        let own_file = entry
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("gcp"));
        if !own_file {
            continue;
        }
        let mut p = Parser::new();
        p.parse_dir("default", true)?;
        let path = entry.to_string_lossy().to_string();
        p.parse(&path, false);
        for l in p.diagnostics.lines() {
            println!("{l}")
        }
        if !p.diagnostics.is_empty() {
            return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
        }
        let filename = entry.file_name().unwrap_or_default().to_string_lossy();
        let mut state = State::new(p.database);
        byte_code(
            &mut p.data,
            &mut std::fs::File::create(format!("tests/generated/{filename}.dump"))?,
            &mut state,
        )?;
    }
    Ok(())
}
