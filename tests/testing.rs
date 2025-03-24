// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(dead_code)]

//! Testing framework
use dryopea::create;
extern crate dryopea;
use dryopea::generation::Output;
use dryopea::interpreter::byte_code;
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
use dryopea::parser::Parser;
use dryopea::state::State;
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
    fatal: Vec<String>,
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

    pub fn fatal(&mut self, text: &str) -> &mut Test {
        if self.result != Value::Null {
            panic!("Cannot combine result with fatal");
        }
        self.fatal.push(text.to_string());
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
        let mut res = match &self.result {
            Value::Long(v) => v.to_string() + "l",
            Value::Int(v) => v.to_string(),
            Value::Enum(v, _) => v.to_string(),
            Value::Boolean(v) if *v => "true".to_string(),
            Value::Boolean(_) => "false".to_string(),
            Value::Text(v) => replace_tokens(v),
            Value::Float(v) => v.to_string(),
            Value::Single(v) => v.to_string(),
            Value::Null => return format!("pub fn test() {{\n    {};\n}}", self.expr),
            _ => panic!("test {:?}", self.result),
        };
        let mut message = res.clone();
        if matches!(self.result, Value::Text(_)) {
            message = "\\\"".to_string() + &res + "\\\"";
            res = "\"".to_string() + &res + "\"";
        }
        format!(
            "pub fn test() {{\n    test_value = {{{}}};\n    assert(\n        test_value == {},\n        \"Test failed {{test_value}} != {}\"\n    );\n}}",
            self.expr, res, message
        )
    }
}

fn replace_tokens(res: &str) -> String {
    res.replace("{", "{{")
        .replace("}", "}}")
        .replace("\n", "\\n")
        .replace("\"", "\\\"")
}

impl Drop for Test {
    // The actual evaluation of the test happens when the Test object is dropped.
    // So there is no need for an 'activate' method call.
    fn drop(&mut self) {
        let mut p = Parser::new();
        p.parse_dir("default", true).unwrap();
        let types = p.database.types.len();
        let start = p.data.definitions();
        let mut code = self.code.clone();
        if !self.expr.is_empty() {
            if !code.is_empty() {
                code += "\n\n";
            }
            code += &self.test();
        }
        let mut w =
            std::fs::File::create(format!("tests/code/{}_{}.txt", self.file, self.name)).unwrap();
        writeln!(w, "{}", code).unwrap();
        p.parse_str(&code, &self.name, false);
        let to = p.database.types.len();
        for tp in types..to {
            writeln!(w, "Type {tp}:{}", p.database.show_type(tp as u16, true)).unwrap();
        }
        for (d, s) in &self.sizes {
            let size = p.database.size(p.data.def(p.data.def_nr(d)).known_type);
            assert_eq!(u32::from(size), *s, "Size of {}", *d);
        }
        self.generate_code(&p, start).unwrap();
        // Validate that we found the correct warnings and errors. Halt when differences are found.
        self.assert_diagnostics(&p);
        // Do not interpret anything when parsing did not succeed.
        if p.diagnostics.level() >= Level::Error {
            return;
        }
        create::generate_code(&p.data).unwrap();
        create::generate_lib(&p.data).unwrap();
        let mut state = State::new(p.database);
        byte_code(&mut w, &mut state, &mut p.data).unwrap();
        state.execute_log(&mut w, "test", &p.data).unwrap();
    }
}

impl Test {
    fn generate_code(&self, p: &Parser, start: u32) -> std::io::Result<()> {
        let w = &mut std::fs::File::create("tests/generated/default.rs")?;
        let o = Output {
            data: &p.data,
            stores: &p.database,
        };
        o.output(w, 0, start)?;
        // Write code output when the result is tested, not only for errors or warnings.
        if self.result != Value::Null || !self.tp.is_unknown() {
            let w = &mut std::fs::File::create(format!(
                "tests/generated/{}_{}.rs",
                self.file, self.name
            ))?;
            let def_nr = p.data.definitions();
            o.output(w, start, def_nr)?;
            writeln!(w, "#[test]\nfn code_{}() {{", self.name)?;
            writeln!(w, "    let mut types = KnownTypes::new();")?;
            writeln!(w, "    init(&mut types);")?;
            writeln!(w, "    let mut stores = Stores::new(&types);")?;
            write!(w, "    assert_eq!(")?;
            o.output_code(w, &self.result, def_nr, 0)?;
            writeln!(w, ", test(&mut stores));\n}}")?;
        }
        Ok(())
    }
    fn assert_diagnostics(&self, p: &Parser) {
        let mut expected = BTreeSet::new();
        for w in &self.warnings {
            expected.insert(format!("Warning: {w}"));
        }
        for w in &self.errors {
            expected.insert(format!("Error: {w}"));
        }
        for w in &self.fatal {
            expected.insert(format!("Fatal: {w}"));
        }
        let mut found = "".to_string();
        for l in p.diagnostics.lines() {
            if expected.contains(l) {
                expected.remove(l);
            } else {
                if !found.is_empty() {
                    found += "|";
                }
                found += l;
            }
        }
        let mut was = "".to_string();
        for e in expected {
            if !was.is_empty() {
                was += "|";
            }
            was += &e;
        }
        if !found.is_empty() || !was.is_empty() {
            if !self.code.is_empty() {
                println!("{}", self.code);
            }
            if !self.expr.is_empty() {
                println!("{}", self.test());
            }
            panic!("Found '{found}' Expected '{was}'");
        }
    }

    // Try to decipher the correct return type from value() and tp() data.
    fn return_type(&self) -> &str {
        let tp = if self.tp.is_unknown() {
            if let Value::Int(_) = self.result {
                Type::Integer(i32::MIN, i32::MAX as u32)
            } else if let Value::Long(_) = self.result {
                Type::Long
            } else if let Value::Text(_) = self.result {
                Type::Text(Vec::new())
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
        if let Type::Integer(_, _) = tp {
            "integer"
        } else if let Type::Text(_) = tp {
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
        fatal: vec![],
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
        fatal: vec![],
        result: Value::Null,
        tp: Type::Unknown(0),
        sizes: HashMap::new(),
    }
}
