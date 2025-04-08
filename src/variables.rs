// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_possible_truncation)]
#![allow(dead_code)]
use crate::data;
use crate::data::{Context, Data, Type, Value};
use crate::diagnostics::{Level, diagnostic_format};
use crate::keys::DbRef;
use crate::lexer::Lexer;
/**
This administrates variables and scopes for a specific function.
- The first scope:0 is for function arguments.
- Variables might exist in multiple scopes but not with different types.
- We allow for variables to move to a higher scope.
*/
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};

// Scope 0 is the function with all variables are function arguments.
// Scope 1 is the main scope of this function with variables.
#[derive(Debug, Clone)]
struct Scope {
    parent: u16,
    context: String,
    result: Type,
    from: (u32, u32),
    till: (u32, u32),
}

// This is created for every variable instance, even if those are of the same name.
#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    pub type_def: Type,
    scope: u16,
    source: (u32, u32),
    stack: u16,
    uses: u16,
}

#[derive(Debug, Clone)]
pub struct Function {
    current_scope: u16,
    last_scope: u16,
    unique: u16,
    scopes: Vec<Scope>,
    variables: Vec<Variable>,
    current_work: u16,
    work: BTreeSet<u16>,
    // Only the last known instance of this variable in the function, so for instances
    // we need to remember the variable number within the variables vector.
    names: HashMap<String, Vec<u16>>,
    pub logging: bool,
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
            current_scope: u16::MAX,
            last_scope: u16::MAX,
            unique: 0,
            scopes: Vec::new(),
            current_work: 0,
            variables: Vec::new(),
            work: BTreeSet::new(),
            names: HashMap::new(),
            logging: false,
        }
    }

    pub fn append(&mut self, other: &mut Function) {
        self.current_scope = u16::MAX;
        self.last_scope = u16::MAX;
        self.logging = other.logging;
        self.unique = 0;
        other.unique = 0;
        self.scopes.clear();
        self.scopes.append(&mut other.scopes);
        self.variables.clear();
        self.variables.append(&mut other.variables);
        for v in &mut self.variables {
            v.uses = 0;
        }
        self.current_work = 0;
        self.work.clear();
        self.names.clear();
        self.names.clone_from(&other.names);
        other.names.clear();
    }

    pub fn copy(other: &Function) -> Self {
        let mut r = Function {
            current_scope: u16::MAX,
            last_scope: u16::MAX,
            unique: 0,
            scopes: other.scopes.clone(),
            variables: other.variables.clone(),
            current_work: 0,
            work: BTreeSet::new(),
            names: other.names.clone(),
            logging: other.logging,
        };
        r.start_scope((0, 0), "show arguments");
        r
    }

    pub fn scope(&self) -> u16 {
        self.current_scope
    }

    pub fn variables(&self, from_scopes: u16) -> Vec<u16> {
        let mut res = Vec::new();
        let mut scopes = HashSet::new();
        let mut sc = self.current_scope;
        for _ in 0..=from_scopes {
            scopes.insert(sc);
            sc = self.scopes[sc as usize].parent;
            if sc == u16::MAX {
                break;
            }
        }
        for (v_nr, v) in self.variables.iter().enumerate() {
            if scopes.contains(&v.scope) {
                res.push(v_nr as u16);
            }
        }
        res
    }

    pub fn result(&self) -> &Type {
        &self.scopes[self.current_scope as usize].result
    }

    pub fn context(&self) -> &str {
        &self.scopes[self.current_scope as usize].context
    }

    /** We have to allow for multiple passes through the scopes.
    # Panics
    When a later pass through the code creates a new scope.
     */
    #[allow(clippy::large_types_passed_by_value)]
    pub fn start_scope(&mut self, at: (u32, u32), context: &str) -> u16 {
        let parent = self.current_scope;
        if self.last_scope == u16::MAX {
            self.current_scope = 0;
        } else {
            self.current_scope = self.last_scope + 1;
        }
        if self.logging {
            println!("start scope {} context {context}", self.current_scope);
        }
        assert!(
            self.current_scope as usize <= self.scopes.len(),
            "Broken scope {}",
            self.current_scope
        );
        self.last_scope = self.current_scope;
        if self.current_scope as usize == self.scopes.len() {
            assert_ne!(
                at,
                (0, 0),
                "Cannot find scope {} >= {}",
                self.current_scope,
                self.scopes.len()
            );
            self.scopes.push(Scope {
                parent,
                from: at,
                result: Type::Void,
                till: (u32::MAX, u32::MAX),
                context: context.to_string(),
            });
        } else if at != (0, 0) {
            assert_eq!(
                self.scopes[self.current_scope as usize].context, context,
                "Different contexts on scope {} at {}:{}",
                self.current_scope, at.0, at.1
            );
        }
        self.current_scope
    }

    #[allow(clippy::large_types_passed_by_value)]
    pub fn finish_scope(&mut self, scope: u16, result: &Type, at: (u32, u32)) {
        assert_eq!(
            self.current_scope, scope,
            "Incorrect scope finish {:?} vs {:?}",
            self.scopes[self.current_scope as usize], self.scopes[scope as usize]
        );
        if self.logging {
            println!(
                "finish {} context {}",
                self.current_scope, self.scopes[self.current_scope as usize].context
            );
        }
        assert_ne!(
            self.current_scope,
            u16::MAX,
            "No active scope at {}:{}",
            at.0,
            at.1
        );
        self.scopes[self.current_scope as usize].till = at;
        if !matches!(result, Type::Unknown(_)) {
            self.scopes[self.current_scope as usize].result = result.clone();
        }
        if self.current_scope > self.last_scope {
            self.last_scope = self.current_scope;
        }
        self.current_scope = self.scopes[self.current_scope as usize].parent;
    }

    pub fn reset(&mut self) {
        self.current_scope = u16::MAX;
        self.last_scope = u16::MAX;
    }

    pub fn name(&self, var_nr: u16) -> &str {
        if var_nr == u16::MAX {
            return "??";
        }
        &self.variables[var_nr as usize].name
    }

    pub fn size(&self, var_nr: u16, context: &Context) -> u16 {
        size(&self.variables[var_nr as usize].type_def, context)
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
            for n in nr {
                if self.is_active(self.variables[*n as usize].scope) {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        if let Some(nr) = self.names.get(name) {
            for n in nr {
                if self.is_active(self.variables[*n as usize].scope) {
                    return Some(&self.variables[*n as usize]);
                }
            }
        }
        None
    }

    pub fn arguments(&self) -> Vec<u16> {
        let mut arg = Vec::new();
        for (v_nr, v) in self.variables.iter().enumerate() {
            if v.scope == 0 {
                arg.push(v_nr as u16);
            }
        }
        arg
    }

    pub fn var(&self, name: &str) -> u16 {
        if let Some(nr) = self.names.get(name) {
            for n in nr {
                if self.variables[*n as usize].scope < 2
                    || self.is_active(self.variables[*n as usize].scope)
                {
                    return *n;
                }
            }
        }
        u16::MAX
    }

    pub fn next_var(&self) -> u16 {
        self.variables.len() as u16
    }

    pub fn unique(&mut self, name: &str, type_def: &Type, lexer: &mut Lexer) -> u16 {
        self.unique += 1;
        self.add_variable(&format!("_{name}_{}", self.unique), type_def, lexer)
    }

    pub fn add_variable(&mut self, name: &str, type_def: &Type, lexer: &mut Lexer) -> u16 {
        // Due to 2 passes through the code, we will add the same variable a second time.
        if let Some(nr) = self.names.get(name) {
            for n in nr {
                if self.is_active(self.variables[*n as usize].scope) {
                    if self.variables[*n as usize].type_def.is_unknown() {
                        self.variables[*n as usize].type_def = type_def.clone();
                    }
                    return *n;
                }
            }
            return self.new_var(name, type_def, lexer);
        }
        assert_ne!(
            self.current_scope,
            u16::MAX,
            "Start a scope before adding variable '{name}'"
        );
        self.new_var(name, type_def, lexer)
    }

    fn new_var(&mut self, name: &str, type_def: &Type, lexer: &mut Lexer) -> u16 {
        if self.logging {
            println!("new_var {name} tp {type_def} scope {}", self.current_scope);
        }
        let v = self.variables.len() as u16;
        if self.names.contains_key(name) {
            self.names.get_mut(name).unwrap().push(v);
        } else {
            self.names.insert(name.to_string(), vec![v]);
        }
        self.variables.push(Variable {
            name: name.to_string(),
            type_def: type_def.clone(),
            scope: self.current_scope,
            source: lexer.at(),
            stack: u16::MAX,
            uses: 1,
        });
        v
    }

    pub fn change_var_type(
        &mut self,
        var_nr: u16,
        type_def: &Type,
        data: &Data,
        lexer: &mut Lexer,
    ) -> bool {
        let var_tp = &self.variables[var_nr as usize].type_def;
        if type_def.is_unknown() || var_tp.is_equal(type_def) {
            return self.is_new(var_nr);
        }
        if let (Type::Vector(tp, _), Type::Vector(to, _)) = (var_tp, type_def) {
            if to.is_unknown() {
                return self.is_new(var_nr);
            }
            if !tp.is_unknown() {
                diagnostic!(
                    lexer,
                    Level::Error,
                    "Variable '{}' cannot change type from {} to {}",
                    self.variables[var_nr as usize].name,
                    self.variables[var_nr as usize].type_def.name(data),
                    type_def.name(data)
                );
            }
        } else if !var_tp.is_unknown() {
            if let Type::RefVar(in_tp) = var_tp {
                if in_tp.is_equal(type_def) {
                    return self.is_new(var_nr);
                }
            }
            diagnostic!(
                lexer,
                Level::Error,
                "Variable '{}' cannot change type from {} to {}",
                self.name(var_nr),
                self.variables[var_nr as usize].type_def.name(data),
                type_def.name(data)
            );
        }
        self.variables[var_nr as usize].type_def = type_def.clone();
        self.is_new(var_nr)
    }

    fn is_new(&self, var_nr: u16) -> bool {
        self.variables[var_nr as usize].scope > 0 && self.variables[var_nr as usize].uses == 0
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

    pub fn test_used(&self, lexer: &mut Lexer, data: &Data) {
        for var in &self.variables {
            if var.name.starts_with('_') || var.name.contains('#') {
                continue;
            }
            if var.uses == 0 && data.def_nr(&var.name) == u32::MAX {
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
        let n = format!("__work_{}", self.current_work + 1);
        self.current_work += 1;
        let v = if let Some(nr) = self.names.get(&n) {
            *nr.first().unwrap()
        } else {
            let v = self.add_variable(&n, &Type::Text(Vec::new()), lexer);
            // work variables always live in the main scope.
            if self.variables[v as usize].scope > 1 {
                self.variables[v as usize].scope = 1;
            }
            v
        };
        self.work.insert(v);
        v
    }

    pub fn work_vars(&self) -> Vec<u16> {
        let mut res = Vec::new();
        for v in &self.work {
            res.push(*v);
        }
        res
    }

    pub fn validate(
        &mut self,
        code: &Value,
        name: &str,
        file: &str,
        data: &Data,
        started: &mut HashSet<u16>,
    ) -> Type {
        match code {
            Value::Block(ls) | Value::Loop(ls) => {
                assert!(
                    self.last_scope as usize + 1 < self.scopes.len(),
                    "Ran out of scopes fn {name} at {file}"
                );
                let bl = self.start_scope((0, 0), "validate block");
                let mut tp = Type::Void;
                for l in ls {
                    tp = self.validate(l, name, file, data, started);
                }
                let mut result = self.result();
                if let (Type::Reference(tp_elm, _), Type::Reference(_, _)) = (&tp, result) {
                    if *tp_elm == data.def_nr("reference") {
                        result = &Type::Null;
                    }
                }
                if result != &Type::Null {
                    // specifically ignore else if block
                    assert!(
                        result.is_equal(&tp),
                        "Different types on block {} vs {} scope '{}' at {file}:{}:{}",
                        tp.show(data, self),
                        result.show(data, self),
                        self.scopes[self.current_scope as usize].context,
                        self.scopes[self.current_scope as usize].till.0,
                        self.scopes[self.current_scope as usize].till.1,
                    );
                }
                self.finish_scope(bl, &Type::Unknown(0), (0, 0));
                tp
            }
            Value::Drop(cd) | Value::Return(cd) => {
                self.validate(cd, name, file, data, started);
                Type::Void
            }
            Value::Iter(_, _) => {
                panic!("Should have been rewritten at {file}");
            }
            Value::Call(nr, vl) => {
                for v in vl {
                    self.validate(v, name, file, data, started);
                }
                data.def(*nr).returned.clone()
            }
            Value::If(cd, tcd, fcd) => {
                self.validate(cd, name, file, data, started);
                let mut res = Type::Void;
                if **tcd != Value::Null {
                    res = self.validate(tcd, name, file, data, started);
                }
                if **fcd != Value::Null {
                    self.validate(fcd, name, file, data, started);
                }
                res
            }
            Value::Set(v, cd) => {
                if started.contains(v) {
                    assert_ne!(
                        self.current_scope,
                        u16::MAX,
                        "Variable {}[{}] used out of scope {} at {file}:{}:{}",
                        self.variables[*v as usize].name,
                        self.variables[*v as usize].scope,
                        self.current_scope,
                        self.variables[*v as usize].source.0,
                        self.variables[*v as usize].source.1
                    );
                } else {
                    started.insert(*v);
                    assert_eq!(
                        self.variables[*v as usize].scope,
                        self.current_scope,
                        "Incorrect scope of {} on {name} at {file}:{}:{}",
                        self.variables[*v as usize].name,
                        self.variables[*v as usize].source.0,
                        self.variables[*v as usize].source.1
                    );
                }
                self.validate(cd, name, file, data, started);
                Type::Void
            }
            Value::Var(v) => self.validate_var(name, file, started, *v),
            Value::Int(_) => data::I32.clone(),
            Value::Long(_) => Type::Long,
            Value::Text(_) => Type::Text(Vec::new()),
            Value::Boolean(_) => Type::Boolean,
            Value::Float(_) => Type::Float,
            Value::Single(_) => Type::Single,
            Value::Enum(_, _) => Type::Enum(0), // TODO find way to determine the definition
            _ => Type::Void,                    // break, continue, null
        }
    }

    fn validate_var(&mut self, name: &str, file: &str, started: &mut HashSet<u16>, v: u16) -> Type {
        assert!(
            started.contains(&v) || self.variables[v as usize].scope == 0,
            "Variable {} not yet started at {name} scope {} at {file}:{}:{}",
            self.variables[v as usize].name,
            self.variables[v as usize].scope,
            self.variables[v as usize].source.0,
            self.variables[v as usize].source.1
        );
        let var_scope = self.variables[v as usize].scope;
        let mut s = self.current_scope;
        while s != u16::MAX {
            if var_scope == s {
                break;
            }
            s = self.scopes[s as usize].parent;
        }
        if s == u16::MAX {
            println!("variables:{self}");
        }
        assert_ne!(
            s,
            u16::MAX,
            "Variable {}[{}] used out of scope {} at {file}:{}:{}",
            self.variables[v as usize].name,
            self.variables[v as usize].scope,
            self.current_scope,
            self.variables[v as usize].source.0,
            self.variables[v as usize].source.1
        );
        self.variables[v as usize].type_def.clone()
    }

    pub fn claim(&mut self, var: u16, pos: u16, context: &Context) -> u16 {
        assert_eq!(
            self.variables[var as usize].stack,
            u16::MAX,
            "Claiming a claimed variable {}",
            self.name(var)
        );
        let size = size(&self.variables[var as usize].type_def, context);
        self.variables[var as usize].stack = pos;
        pos + size
    }

    pub fn free(&mut self, pos: u16) {
        for v in &mut self.variables {
            if v.stack != u16::MAX && v.stack >= pos {
                v.stack = u16::MAX;
            }
        }
    }

    /// Move the scope of a given variable to the given scope.
    pub fn move_scope(&mut self, var_nr: u16, to_scope: u16) {
        if self.variables[var_nr as usize].scope < to_scope {
            return;
        }
        // Problem when this is not a parent scope.
        assert!(to_scope == 0 || to_scope == 1, "Incorrect scope");
        self.variables[var_nr as usize].scope = to_scope;
        if to_scope == 1 {
            self.work.insert(var_nr);
        } else {
            self.work.remove(&var_nr);
        }
    }

    pub fn set_type(&mut self, var_nr: u16, tp: Type) {
        self.variables[var_nr as usize].type_def = tp;
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
        Type::RefVar(_)
        | Type::Reference(_, _)
        | Type::Vector(_, _)
        | Type::Hash(_, _, _)
        | Type::Sorted(_, _, _)
        | Type::Spacial(_, _, _) => size_of::<DbRef>() as u16,
        _ => 0,
    }
}
