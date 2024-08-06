// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(dead_code)]

//! Testing framework
extern crate dryopea;
use std::io::Write;

/// Evaluate the given code.
/// When a result is given there should be a @test routine returning this result.
/// When a type is also given this result should be of that type.
/// Defining an error will not expect a result but will validate that this specific error is thrown.
/// Defining warnings will just validate the given warnings and not expect a change of flow.
#[macro_export]
macro_rules! code {
    ($code:expr) => {
        testing::testing_code($code, stdext::function_name!())
    };
}

/// Directly evaluate a given expression.
/// This is shorthand for a test routine returning this expression.
#[macro_export]
macro_rules! expr {
    ($code:expr) => {
        testing::testing_expr($code, stdext::function_name!())
    };
}

use dryopea::data::{Type, Value};
use dryopea::diagnostics::Level;
use dryopea::interpreter::State;
use dryopea::parser::Parser;
use std::collections::BTreeSet;
use std::collections::HashMap;

// The test data for one test.
// Many parts can remain empty for each given test.
pub struct Test {
    name: String,
    file: String,
    expr: String,
    code: String,
    warnings: Vec<String>,
    errors: Vec<String>,
    sizes: HashMap<String, u32>,
    result: Value,
    tp: Type,
}

impl Test {
    /// Expect the parsing of the test to end in this error.
    /// Can be given multiple times to expect more than one error.
    pub fn error(&mut self, text: &str) -> &mut Test {
        if self.result != Value::Null {
            panic!("Cannot combine result with errors");
        }
        self.errors.push(text.to_string());
        self
    }

    /// Expect this warning during parsing.
    /// This will not change if it should result in and error or a normal result.
    pub fn warning(&mut self, text: &str) -> &mut Test {
        self.warnings.push(text.to_string());
        self
    }

    /// Shorthand expressions for a test routine that returns a result.
    pub fn expr(&mut self, value: &str) -> &mut Test {
        self.expr = value.to_string();
        self
    }

    /// The expected result value. Cannot be combined with expected errors.
    pub fn result(&mut self, value: Value) -> &mut Test {
        if !self.errors.is_empty() {
            panic!("Cannot combine result with errors");
        }
        if matches!(value, Value::Boolean(_)) {
            self.tp = Type::Boolean;
        }
        self.result = value;
        self
    }

    /// In some cases the result type will different from its internal type.
    /// This is the case for Type::Boolean or Type::Enum types that return Value::Int(_) values.
    /// Also Value::None results can happen in combination with most other types.
    pub fn tp(&mut self, tp: Type) -> &mut Test {
        self.tp = tp;
        self
    }

    fn test(&self) -> String {
        if self.return_type() == "" {
            format!("fn test() {{\n{}\n}}", self.expr)
        } else {
            format!("fn test() -> {} {{\n{}\n}}", self.return_type(), self.expr)
        }
    }
}

impl Drop for Test {
    // The actual evaluation of the test happens when the Test object is dropped.
    // So there is no need for an 'activate' method call.
    fn drop(&mut self) {
        let mut p = Parser::new();
        p.parse_dir("default", true).unwrap();
        let start = p.data.definitions();
        let mut code = self.code.clone();
        if !self.expr.is_empty() {
            if !code.is_empty() {
                code += "\n";
            }
            code += &self.test();
        }
        p.parse_str(&code, &self.name);
        if !self.expr.is_empty() {
            p.data.def_used(p.data.def_nr("test"));
        }
        for (d, s) in &self.sizes {
            assert_eq!(p.data.def(p.data.def_nr(d)).size, *s, "Size of {}", *d);
        }
        self.generate_code(&p, start).unwrap();
        // Validate that we found the correct warnings and errors. Halt when differences are found.
        self.assert_diagnostics(&p);
        // Do not interpret anything when parsing did not succeed.
        if p.diagnostics.level() >= Level::Error {
            return;
        }
        if !self.tp.is_unknown() {
            assert_eq!(p.data.def(p.data.def_nr("test")).returned, self.tp);
        }
        let types = p.data.known_types.clone();
        let mut i = State::new(&mut p.data, &types);
        let test = p.data.def_nr("test");
        i.byte_code(test, &p.data);
        let mut w =
            std::fs::File::create(format!("tests/generated/{}_{}.code", self.file, self.name))
                .unwrap();
        i.dump_code(&mut w, test, &p.data);
        /*
        // Only write the interpreter log when a different result is found.
        if res != self.result {
            let w =
                std::fs::File::create(format!("tests/generated/{}_{}.log", self.file, self.name))
                    .unwrap();
            i.calculate("test", Some(w)).unwrap();
        }
        assert_eq!(self.result, res);
        */
    }
}

impl Test {
    fn generate_code(&self, p: &Parser, start: u32) -> std::io::Result<()> {
        let w = &mut std::fs::File::create("tests/generated/default.rs")?;
        p.data.output(w, 0, start)?;
        // Write code output when the result is tested, not only for errors or warnings.
        if self.result != Value::Null || !self.tp.is_unknown() {
            let w = &mut std::fs::File::create(format!(
                "tests/generated/{}_{}.rs",
                self.file, self.name
            ))?;
            p.data.output(w, start, p.data.definitions())?;
            writeln!(w, "#[test]\nfn code_{}() {{", self.name)?;
            writeln!(w, "    let mut types = KnownTypes::new();")?;
            writeln!(w, "    init(&mut types);")?;
            writeln!(w, "    let mut stores = Stores::new(&types);")?;
            write!(w, "    assert_eq!(")?;
            p.data.output_code(w, &self.result, 0)?;
            writeln!(w, ", test(&mut stores));\n}}")?;
        }
        Ok(())
    }
    fn assert_diagnostics(&self, p: &Parser) {
        let mut expected = BTreeSet::new();
        for w in &self.warnings {
            expected.insert(w.clone());
        }
        for w in &self.errors {
            expected.insert(w.clone());
        }
        let mut found = "".to_string();
        for l in p.diagnostics.lines() {
            if expected.contains(l) {
                expected.remove(l);
            } else {
                found += l;
                found += "\n";
            }
        }
        let mut was = "".to_string();
        for e in expected {
            was += &e;
            was += "\n";
        }
        if !found.is_empty() || !was.is_empty() {
            if !self.code.is_empty() {
                println!("{}", self.code);
            }
            if !self.expr.is_empty() {
                println!("{}", self.test());
            }
            panic!("Found {found} Expected {was}");
        }
    }

    // Try to decipher the correct return type from value() and tp() data.
    fn return_type(&self) -> &str {
        let tp = if self.tp.is_unknown() {
            if let Value::Int(_) = self.result {
                Type::Integer
            } else if let Value::Long(_) = self.result {
                Type::Long
            } else if let Value::Text(_) = self.result {
                Type::Text
            } else if let Value::Float(_) = self.result {
                Type::Float
            } else if let Value::Null = self.result {
                return "";
            } else {
                Type::Unknown(0)
            }
        } else {
            self.tp.clone()
        };
        if let Type::Integer = tp {
            "integer"
        } else if let Type::Text = tp {
            "text"
        } else if let Type::Long = tp {
            "long"
        } else if let Type::Boolean = tp {
            "boolean"
        } else if let Type::Float = tp {
            "float"
        } else {
            panic!("Unknown type {tp:?}");
        }
    }
}

fn short(name: &str) -> String {
    let s: Vec<&str> = name.split("::").collect();
    s[s.len() - 1].to_string()
}

fn front(name: &str) -> String {
    let s: Vec<&str> = name.split("::").collect();
    s[s.len() - 2].to_string()
}

pub fn testing_code(code: &str, test: &str) -> Test {
    Test {
        name: short(test),
        file: front(test),
        expr: "".to_string(),
        code: code.to_string(),
        warnings: vec![],
        errors: vec![],
        result: Value::Null,
        tp: Type::Unknown(0),
        sizes: HashMap::new(),
    }
}

pub fn testing_expr(expr: &str, test: &str) -> Test {
    Test {
        name: short(test),
        file: front(test),
        expr: expr.to_string(),
        code: "".to_string(),
        warnings: vec![],
        errors: vec![],
        result: Value::Null,
        tp: Type::Unknown(0),
        sizes: HashMap::new(),
    }
}

#[test]
fn dir() -> std::io::Result<()> {
    let dir = "tests/suite";
    for f in std::fs::read_dir(dir)? {
        let filename = f.unwrap().file_name().to_string_lossy().to_string();
        if !filename.ends_with(".gcp") {
            continue;
        }
        let mut p = Parser::new();
        p.parse_dir("default", true)?;
        p.parse(&format!("{dir}/{filename}"), false);
        for l in p.diagnostics.lines() {
            println!("{l}")
        }
        if !p.diagnostics.is_empty() {
            return Err(std::io::Error::from(std::io::ErrorKind::InvalidData));
        }
        //let file = &filename[..filename.len() - 4];
        //p.data.output_webassembly(&file)?;
        //TODO validate via parser
        //TODO write a lot more tests via this path, probably rewrite all current tests
        //let i = Inter::new(&p.data); i.calculate("main", None).unwrap();
    }
    Ok(())
}
