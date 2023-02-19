// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(dead_code)]

//! Testing framework
extern crate dryopea;

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
/// This is short hand for a test routine returning this expression.
#[macro_export]
macro_rules! expr {
    ($code:expr) => {
        testing::testing_expr($code, stdext::function_name!())
    };
}

use dryopea::data::{Type, Value};
use dryopea::diagnostics::Level;
use dryopea::inter::Inter;
use dryopea::parser::Parser;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::Write;

// The test data for one test.
// Many parts can remain empty for each given test.
pub struct Test {
    name: String,
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

    /// Expect a definition with the given size to be created.
    pub fn def(&mut self, name: &str, size: u32) -> &mut Test {
        self.sizes.insert(name.to_string(), size);
        self
    }

    /// Short hand expressions for a test routine that returns a result.
    pub fn expr(&mut self, value: &str) -> &mut Test {
        self.expr = value.to_string();
        self
    }

    /// The expected result value. Cannot be combined with expected errors.
    pub fn result(&mut self, value: Value) -> &mut Test {
        if !self.errors.is_empty() {
            panic!("Cannot combine result with errors");
        }
        self.result = value;
        self
    }

    /// In some cases the result type will different from it's internal type.
    /// This is the case for Type::Boolean or Type::Enum types that return Value::Int(_) values.
    /// Also Value::None results can happen in combination with most other types.
    pub fn tp(&mut self, tp: Type) -> &mut Test {
        self.tp = tp;
        self
    }
}

impl Drop for Test {
    // The actual evaluation of the test happens when the Test object is dropped.
    // So there is no need for an 'activate' method call.
    fn drop(&mut self) {
        let mut p = Parser::new();
        p.parse_dir("default", true);
        let start = p.data.definitions();
        if !self.code.is_empty() {
            p.parse_str(&self.code, &self.name);
        }
        if !self.expr.is_empty() {
            if self.return_type() == "" {
                p.parse_str(&format!("fn test() {{\n{}\n}}", self.expr), &self.name);
            } else {
                p.parse_str(
                    &format!("fn test() -> {} {{\n{}\n}}", self.return_type(), self.expr),
                    &self.name,
                );
            }
            p.data.def_used(p.data.def_nr("test"));
        }
        for (d, s) in &self.sizes {
            assert_eq!(p.data.def_size(p.data.def_nr(d)), *s, "Size of {}", *d);
        }
        if !p.diagnostics.is_empty() {
            if let Ok(mut f) = std::fs::File::create(format!("tests/generated/{}.txt", self.name)) {
                writeln!(f, "{}", p.diagnostics).unwrap();
            }
        }
        self.assert_diagnostics(&p);
        if p.diagnostics.level() >= Level::Error {
            return;
        }
        let w = &mut std::fs::File::create("tests/generated/default.rs").unwrap();
        for d in 0..start {
            p.data.output_def(w, d);
        }
        let w = &mut std::fs::File::create(format!("tests/generated/{}.rs", self.name)).unwrap();
        for d in start..p.data.definitions() {
            p.data.output_def(w, d);
        }
        if self.tp != Type::Unknown {
            assert_eq!(p.data.returned(p.data.def_nr("test")), self.tp);
        }
        let i = Inter::new(&p.data);
        let res = i.calculate("test").unwrap();
        assert_eq!(self.result, res);
    }
}

impl Test {
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
                found += &l;
                found += "\n";
            }
        }
        let mut was = "".to_string();
        for e in expected {
            was += &e;
            was += "\n";
        }
        if !found.is_empty() || !was.is_empty() {
            panic!("Found {found} Expected {was}");
        }
    }

    // Try to decipher the correct return type from value() and tp() data.
    fn return_type(&self) -> &str {
        let tp = if self.tp == Type::Unknown {
            if let Value::Int(_) = self.result {
                Type::Integer
            } else if let Value::Text(_) = self.result {
                Type::Text
            } else if let Value::Float(_) = self.result {
                Type::Float
            } else if let Value::Null = self.result {
                return "";
            } else {
                Type::Unknown
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

pub fn testing_code(code: &str, test: &str) -> Test {
    Test {
        name: short(test),
        expr: "".to_string(),
        code: code.to_string(),
        warnings: vec![],
        errors: vec![],
        result: Value::Null,
        tp: Type::Unknown,
        sizes: HashMap::new(),
    }
}

pub fn testing_expr(expr: &str, test: &str) -> Test {
    Test {
        name: short(test),
        expr: expr.to_string(),
        code: "".to_string(),
        warnings: vec![],
        errors: vec![],
        result: Value::Null,
        tp: Type::Unknown,
        sizes: HashMap::new(),
    }
}
