// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_possible_truncation)]
use crate::data::{Context, Type, Value};
use crate::diagnostics::{Level, diagnostic_format};
use crate::keys::DbRef;
use crate::lexer::Lexer;
/**
This administrates variables and scopes for a specific function.
- The first scope:0 is for function arguments.
- Variables might exist in multiple scopes but not with different types.
- We allow for variables to move to a higher scope.
*/
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};

// Scope 0 is the function with all variables are function arguments.
// Scope 1 is the main scope of this function with variables.
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Scope {
    parent: u16,
    from: (u32, u32),
    till: (u32, u32),
}

// This is created for every variable instance, even if those are of the same name.
#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    type_def: Type,
    scope: u16,
    source: (u32, u32),
    stack: u16,
    uses: u16,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub current_scope: u16,
    next_scope: u16,
    scopes: Vec<Scope>,
    variables: Vec<Variable>,
    current_work: u16,
    work: Vec<u16>,
    // Only the last known instance of this variable in the function, so for instances
    // we need to remember the variable number within the variables vector.
    names: HashMap<String, u16>,
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for v in &self.variables {
            f.write_fmt(format_args!("{v:?}\n"))?;
        }
        Ok(())
    }
}

impl Function {
    pub fn new() -> Self {
        Function {
            current_scope: 0,
            next_scope: 0,
            scopes: vec![Scope {
                parent: u16::MAX,
                from: (0, 0),
                till: (u32::MAX, u32::MAX),
            }],
            current_work: 0,
            variables: Vec::new(),
            work: Vec::new(),
            names: HashMap::new(),
        }
    }

    pub fn append(&mut self, other: &mut Function) {
        self.current_scope = 0;
        self.next_scope = 0;
        self.scopes.clear();
        self.scopes.append(&mut other.scopes);
        self.current_work = other.current_work;
        self.variables.clear();
        self.variables.append(&mut other.variables);
        self.work.clear();
        self.work.append(&mut other.work);
        self.names.clear();
        self.names.clone_from(&other.names);
        other.names.clear();
    }

    /** We have to allow for multiple passes through the scopes.
     */
    pub fn start_scope(&mut self, lexer: &Lexer) {
        let parent = self.current_scope;
        self.next_scope += 1;
        self.current_scope = self.next_scope;
        self.scopes.push(Scope {
            parent,
            from: lexer.at(),
            till: (u32::MAX, u32::MAX),
        });
    }

    pub fn finish_scope(&mut self, lexer: &Lexer) {
        self.scopes[self.current_scope as usize].till = lexer.at();
        self.current_scope = self.scopes[self.current_scope as usize].parent;
    }

    pub fn name(&self, var_nr: u16) -> &str {
        &self.variables[var_nr as usize].name
    }

    pub fn tp(&self, var_nr: u16) -> &Type {
        &self.variables[var_nr as usize].type_def
    }

    pub fn uses(&self, var_nr: u16) -> u16 {
        self.variables[var_nr as usize].uses
    }

    pub fn stack(&self, var_nr: u16) -> u16 {
        self.variables[var_nr as usize].stack
    }

    pub fn in_use(&mut self, var_nr: u16, plus: bool) {
        if plus {
            self.variables[var_nr as usize].uses += 1;
        } else {
            self.variables[var_nr as usize].uses -= 1;
        }
    }

    pub fn exists(&self, var_nr: u16) -> bool {
        var_nr < self.variables.len() as u16
    }

    pub fn name_exists(&self, name: &str) -> bool {
        if let Some(nr) = self.names.get(name) {
            self.is_active(self.variables[*nr as usize].scope)
        } else {
            false
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        if let Some(nr) = self.names.get(name) {
            if self.is_active(self.variables[*nr as usize].scope) {
                return Some(&self.variables[*nr as usize]);
            }
        }
        None
    }

    pub fn var(&self, name: &str) -> u16 {
        if let Some(nr) = self.names.get(name) {
            *nr
        } else {
            u16::MAX
        }
    }

    pub fn next_var(&self) -> u16 {
        self.variables.len() as u16
    }

    pub fn add_variable(&mut self, name: &str, type_def: &Type, lexer: &mut Lexer) -> u16 {
        if let Some(cur) = self.names.get(name) {
            if !type_def.is_unknown() && self.variables[*cur as usize].type_def != *type_def {
                diagnostic!(lexer, Level::Error, "Variable '{name}' cannot change type");
                return u16::MAX;
            }
            let cur_scope = self.variables[*cur as usize].scope;
            if self.is_active(cur_scope) {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Redefined {} {}",
                    if cur_scope == 0 {
                        "argument"
                    } else {
                        "variable"
                    },
                    name
                );
                return u16::MAX;
            }
        }
        let v = self.variables.len() as u16;
        self.names.insert(name.to_string(), v);
        self.variables.push(Variable {
            name: name.to_string(),
            type_def: type_def.clone(),
            scope: self.current_scope,
            source: lexer.at(),
            stack: u16::MAX,
            uses: 0,
        });
        v
    }

    pub fn change_var_type(&mut self, var_nr: u16, type_def: &Type, lexer: &mut Lexer) -> bool {
        let var_tp = &self.variables[var_nr as usize].type_def;
        if type_def.is_unknown() || var_tp == type_def {
            return self.variables[var_nr as usize].source == lexer.at();
        }
        if let (Type::Vector(tp), Type::Vector(to)) = (var_tp, type_def) {
            if to.is_unknown() {
                return self.variables[var_nr as usize].source == lexer.at();
            }
            if !tp.is_unknown() {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Variable '{}' cannot change type",
                    self.variables[var_nr as usize].name
                );
            }
        } else if !var_tp.is_unknown() {
            diagnostic!(lexer, Level::Error, "Variable cannot change type");
        }
        self.variables[var_nr as usize].type_def = type_def.clone();
        self.variables[var_nr as usize].source == lexer.at()
    }

    pub fn is_active(&self, scope: u16) -> bool {
        let mut s = self.current_scope;
        while s != u16::MAX {
            if s == scope {
                return true;
            }
            s = self.scopes[s as usize].parent;
        }
        false
    }

    pub fn is_argument(&self, var_nr: u16) -> bool {
        self.variables[var_nr as usize].scope == 0
    }

    pub fn test_used(&self, lexer: &mut Lexer) {
        for var in &self.variables {
            if var.name.starts_with('_') {
                continue;
            }
            if var.uses == 0 {
                lexer.to(&var.source);
                diagnostic!(
                    lexer,
                    Level::Warning,
                    "{} {} is never read",
                    if var.scope == 0 {
                        "Parameter"
                    } else {
                        "Variable"
                    },
                    var.name,
                );
            }
        }
    }

    pub fn work(&mut self, lexer: &mut Lexer) -> u16 {
        if self.current_work < self.work.len() as u16 {
            let w = self.current_work;
            self.current_work += 1;
            self.work[w as usize]
        } else {
            let w = self.variables.len() as u16;
            let v = self.add_variable(&format!("__work_{w}"), &Type::Text(false), lexer);
            self.work.push(v);
            w
        }
    }

    pub fn validate(&self, code: &Value, scope: u16) {
        match code {
            Value::Block(ls) | Value::Loop(ls) => {
                for l in ls {
                    self.validate(l, scope + 1);
                }
            }
            Value::Let(v, _) => {
                assert_eq!(
                    self.variables[*v as usize].scope, scope,
                    "Incorrect scope of {}",
                    self.variables[*v as usize].name
                );
            }
            Value::Var(v) | Value::Set(v, _) => {
                let var_scope = self.variables[*v as usize].scope;
                let mut s = scope;
                while s != u16::MAX {
                    if var_scope == s {
                        break;
                    }
                    s = self.scopes[s as usize].parent;
                }
                if s == u16::MAX {
                    println!("variables:{self}");
                    println!("code:{code:?}");
                }
                assert_ne!(
                    s,
                    u16::MAX,
                    "Variable {}[{}] {} out of scope",
                    self.variables[*v as usize].name,
                    self.variables[*v as usize].scope,
                    if matches!(code, Value::Var(_)) {
                        "used"
                    } else {
                        "set"
                    }
                );
            }
            _ => {}
        }
    }

    /// Move the scope of a given variable to the given scope.
    pub fn move_scope(&self, var_nr: u16, to_scope: u16, code: &mut Value) {
        self.validate(code, 0);
        // Problem when this is not a parent scope.
        let mut s = self.variables[var_nr as usize].scope;
        let mut found = false;
        while s != u16::MAX {
            if s == to_scope {
                found = true;
            }
            s = self.scopes[s as usize].parent;
        }
        assert!(
            found,
            "move_scope is not to a parent of the current variable scope"
        );
        // Merge when this variable is also defined in a child of this scope.
        panic!("Not implemented yet!");
        // self.validate(code, 0);
    }

    /// Calculate stack positions of the known variables.
    pub fn positions(&mut self) {
        let mut stack = 0;
        for (scope_nr, vars) in self.gather_scopes() {
            for v_nr in vars {
                self.variables[v_nr as usize].stack = stack;
                stack += size(
                    &self.variables[v_nr as usize].type_def,
                    if scope_nr == 0 {
                        &Context::Argument
                    } else {
                        &Context::Variable
                    },
                );
            }
        }
    }

    /// Return the variables that directly reside inside known scopes
    pub fn gather_scopes(&self) -> BTreeMap<u16, Vec<u16>> {
        let mut res = BTreeMap::new();
        for (v_nr, v) in self.variables.iter().enumerate() {
            res.entry(v.scope)
                .or_insert_with(Vec::new)
                .push(v_nr as u16);
        }
        res
    }
}

pub fn size(tp: &Type, context: &Context) -> u16 {
    match tp {
        Type::Integer(min, max)
            if context == &Context::Constant && i64::from(*max) - i64::from(*min) <= 256 =>
        {
            1
        }
        Type::Integer(min, max)
            if context == &Context::Constant && i64::from(*max) - i64::from(*min) <= 65536 =>
        {
            2
        }
        Type::Boolean | Type::Enum(_) => 1,
        Type::Integer(_, _) | Type::Single | Type::Function(_, _) => 4,
        Type::Long | Type::Float => 8,
        Type::Text(_) if context == &Context::Variable => size_of::<String>() as u16,
        Type::Text(_) => size_of::<&str>() as u16,
        Type::RefVar(_) | Type::Reference(_) | Type::Vector(_) => size_of::<DbRef>() as u16,
        _ => 0,
    }
}
