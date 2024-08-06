// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Keep track of the defined local variables and parameters during
//! parsing

use crate::data::{Data, Type, Value};
use crate::diagnostics::*;
use crate::lexer::{Lexer, Position};
use std::collections::HashMap;

/// Per defined variable
#[derive(Debug, Clone)]
pub struct Variable {
    var_type: Type,
    /// Definition position of this variable
    position: Position,
    /// The number of times the content of this variable was read.
    uses: u32,
}

pub struct Types<'a> {
    /// Per stack position the claimed variables.
    var_nrs: HashMap<u32, Variable>,
    /// Local variable positions in the current routine
    variables: Vec<HashMap<String, u32>>,
    /// Possible routines per operator, filled during parsing of 01_code.md
    possible: HashMap<&'a str, Vec<u32>>,
}

static EMPTY: [u32; 0] = [];

lazy_static::lazy_static! {
    static ref OP_NAMES: HashMap<&'static str, &'static str> = [
        ("OpAbs", "Abs"), ("OpMath", "Math"), ("OpDatabase", "Database"),
        ("OpRange", ".."), ("OpOr", "||"), ("OpAnd", "&&"),
        ("OpEq", "=="), ("OpNe", "!="), ("OpLt", "<"), ("OpLe", "<="), ("OpGt", ">"), ("OpGe", ">="),
        ("OpLand", "&"), ("OpLor", "|"), ("OpEor", "^"), ("OpLeft", "<<"), ("OpRight", ">>"),
        ("OpMin", "-"), ("OpAdd", "+"), ("OpMul", "*"), ("OpDiv", "/"), ("OpRem", "%"), ("OpNot", "!"), ("OpAppend", "+="),
        ("OpConv", "Conv"), ("OpCast", "Cast"),
        ("OpGet", "Get"), ("OpSet", "Set"), ("OpInsert", "Insert"), ("OpRemove", "Remove"), ("OpClear", "Clear"),
        ("OpLength", "Len"), ("OpFormat", "Format"), ("OpFinish", "Finish"), ("OpAlign", "Align"), ("OpAssert", "Assert"), ("OpPrint", "Print")
    ].iter().copied().collect();
}

/// Return the size of the first two capitalized words.
/// Or the first three when the second is only a single uppercase character.
/// Return 0 when those were not found.
fn name_start(name: &str) -> usize {
    let mut chars = name.chars();
    let mut nr = 0;
    let mut cap = 0;
    loop {
        if let Some(ch) = chars.next() {
            if ch.is_uppercase() {
                if (cap > 1 && nr > 3) || cap > 2 {
                    break;
                }
                cap += 1;
            }
        } else {
            if cap < 2 {
                nr = 0
            }
            break;
        }
        nr += 1
    }
    nr
}

impl<'a> Default for Types<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Types<'a> {
    pub fn new() -> Types<'a> {
        Types {
            variables: Vec::new(),
            var_nrs: HashMap::new(),
            possible: HashMap::new(),
        }
    }

    pub fn push(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.variables.pop();
    }

    /// Search a variable, return its position and type.
    /// Increase the uses of it with one.
    pub fn var_name(&mut self, name: &str) -> Option<(u32, Type)> {
        for vars in self.variables.iter().rev() {
            if let Some(nr) = vars.get(name) {
                self.var_nrs.get_mut(nr).unwrap().uses += 1;
                return Some((*nr, self.var_nrs[nr].var_type.clone()));
            }
        }
        None
    }

    /// Decrease the uses of a variable on a stack position.
    /// This was already increased by the var_name call before, but it was not actually used.
    pub fn assign(&mut self, nr: u32) {
        self.var_nrs.get_mut(&nr).unwrap().uses -= 1;
    }

    pub fn used(&self, name: &str) -> bool {
        for vars in self.variables.iter().rev() {
            if let Some(nr) = vars.get(name) {
                return self.var_nrs[nr].uses > 0;
            }
        }
        false
    }

    /// Get the next free variable number.
    pub fn variable_number(&self) -> u32 {
        let mut nr: u32 = 0;
        for v in &self.variables {
            nr += v.len() as u32;
        }
        nr
    }

    /// A new local variable gets its type via assignment instead of explicit.
    pub fn change_var_type(
        &mut self,
        lexer: &mut Lexer,
        data: &mut Data,
        val: &Value,
        tp: &Type,
    ) -> bool {
        // do not expect a field because that should already be a correct value
        if tp.is_unknown() {
            return false;
        }
        if let Value::Var(vnr) = val {
            if let Type::Vector(inner) = &self.var_nrs.get_mut(vnr).unwrap().var_type {
                if let Type::Unknown(_) = **inner {
                    self.var_nrs.get_mut(vnr).unwrap().var_type = tp.clone();
                    data.vector_def(lexer, tp);
                    true
                } else {
                    false
                }
            } else if self.var_nrs.get_mut(vnr).unwrap().var_type.is_unknown() {
                self.var_nrs.get_mut(vnr).unwrap().var_type = tp.clone();
                true
            } else if &self.var_nrs.get_mut(vnr).unwrap().var_type != tp {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Cannot change type of variable {}",
                    self.name(*vnr)
                );
                false
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Create a local variable in the current scope
    pub fn create_var(&mut self, name: String, var_type: Type, position: &Position) -> u32 {
        let vnr = self.variable_number();
        let len = self.variables.len();
        let last = if len > 0 { len - 1 } else { 0 };
        if let Some(var) = self.variables.get_mut(last) {
            self.var_nrs.insert(
                vnr,
                Variable {
                    var_type,
                    position: position.clone(),
                    uses: 1,
                },
            );
            var.insert(name, vnr);
        }
        vnr
    }

    pub fn name(&self, v_nr: u32) -> String {
        for vars in &self.variables {
            for (nm, nr) in vars {
                if *nr == v_nr {
                    return nm.clone();
                }
            }
        }
        "".to_string()
    }

    pub fn next_var(&self) -> u32 {
        self.variables.len() as u32
    }

    /// Add this definition to the possible operators structure when it matches an operator type.
    pub fn add_possible(&mut self, data: &Data, name: String) -> bool {
        let d_nr = data.definitions() - 1;
        let start = &name[0..name_start(&name)];
        if start.is_empty() || !OP_NAMES.contains_key(start) {
            return false;
        }
        self.possible.entry(OP_NAMES[start]).or_default().push(d_nr);
        true
    }

    pub fn get_possible(&self, op: &str) -> std::slice::Iter<'_, u32> {
        if let Some(it) = self.possible.get(op) {
            it.iter()
        } else {
            EMPTY.iter()
        }
    }

    /// Create variables from the definition parameters
    pub fn parameters(&mut self, data: &Data, d_nr: u32) -> Type {
        let mut vars = HashMap::new();
        for a_nr in 0..data.attributes(d_nr) {
            let var = Variable {
                var_type: data.attr_type(d_nr, a_nr),
                position: data.def(d_nr).position.clone(),
                uses: 0,
            };
            self.var_nrs.insert(a_nr as u32, var.clone());
            vars.insert(data.attr_name(d_nr, a_nr), a_nr as u32);
        }
        self.variables.push(vars);
        data.def(d_nr).returned.clone()
    }

    /// At the end of routine code, clear the currently known variables.
    pub fn clear(&mut self) {
        self.variables.clear();
        self.var_nrs.clear();
    }

    pub fn test_used(&self, parameters: u16, diagnostics: &mut Diagnostics) {
        for blocks in &self.variables {
            for (name, nr) in blocks {
                let var = &self.var_nrs[nr];
                if var.uses == 0 && !name.starts_with('_') {
                    // TODO: add the location of the definition of this variable.
                    diagnostics.add(
                        Level::Warning,
                        &format!(
                            "{} {} is never read in {} line {}:{}",
                            if *nr < parameters as u32 {
                                "Parameter"
                            } else {
                                "Variable"
                            },
                            name,
                            var.position.file,
                            var.position.line,
                            var.position.pos
                        ),
                    );
                }
            }
        }
    }

    pub fn validate(&self, lexer: &mut Lexer) -> bool {
        for (&name, &code) in OP_NAMES.iter() {
            if !self.possible.contains_key(code) {
                diagnostic!(lexer, Level::Error, "Undefined operator {}", name);
                return false;
            }
        }
        true
    }
}
