// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_possible_truncation)]
#![allow(dead_code)]
#![allow(clippy::large_types_passed_by_value)]
use crate::data;
use crate::data::{Context, Data, Type, Value};
use crate::diagnostics::{Level, diagnostic_format};
use crate::keys::DbRef;
use crate::lexer::Lexer;
/**
This administrates variables and scopes for a specific function.
- The first scope (0) is for function arguments.
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
    parts: Vec<u16>,
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
    steps: Vec<u8>,
    unique: u16,
    scopes: Vec<Scope>,
    stack: Vec<u16>,
    variables: Vec<Variable>,
    work_text: u16,
    work_ref: u16,
    // Work variables for texts
    work_texts: BTreeSet<u16>,
    // Work variables for stores
    work_refs: BTreeSet<u16>,
    // Only the last known instance of this variable in the function, so for instances
    // we need to remember the variable number within the variable vector.
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
            stack: Vec::new(),
            steps: Vec::new(),
            unique: 0,
            scopes: Vec::new(),
            work_text: 0,
            work_ref: 0,
            variables: Vec::new(),
            work_texts: BTreeSet::new(),
            work_refs: BTreeSet::new(),
            names: HashMap::new(),
            logging: false,
        }
    }

    pub fn dump(&self) {
        for (s_nr, s) in self.scopes.iter().enumerate() {
            println!("{s_nr}:{s:?}");
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
        self.work_text = 0;
        self.work_ref = 0;
        self.work_texts.clear();
        self.work_refs.clear();
        self.names.clear();
        self.names.clone_from(&other.names);
        other.names.clear();
    }

    pub fn copy(other: &Function) -> Self {
        let mut r = Function {
            current_scope: u16::MAX,
            last_scope: u16::MAX,
            steps: Vec::new(),
            stack: Vec::new(),
            unique: 0,
            scopes: other.scopes.clone(),
            variables: other.variables.clone(),
            work_text: 0,
            work_ref: 0,
            work_texts: BTreeSet::new(),
            work_refs: BTreeSet::new(),
            names: other.names.clone(),
            logging: other.logging,
        };
        r.start_next();
        r
    }

    pub fn scope(&self) -> u16 {
        self.current_scope
    }

    pub fn variables(&self, to_scope: u16) -> Vec<u16> {
        let mut res = Vec::new();
        let mut scopes = HashSet::new();
        let mut sc = self.current_scope;
        loop {
            if sc == 0 {
                // never return function arguments
                break;
            }
            scopes.insert(sc);
            if sc == to_scope {
                break;
            }
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

    // last_scope is the current scope we iterate (as parent)
    pub fn start_next(&mut self) -> u16 {
        if self.stack.is_empty() {
            self.current_scope = 0;
            self.last_scope = 0;
            self.stack.push(self.current_scope);
            if self.logging {
                println!("start_next stack:{:?}", self.stack);
            }
            self.current_scope
        } else {
            let parent = *self.stack.last().unwrap();
            let mut last = u16::MAX;
            for s in &self.scopes[parent as usize].parts {
                if last == self.last_scope {
                    self.last_scope = self.current_scope;
                    self.current_scope = *s;
                    self.stack.push(*s);
                    if self.logging {
                        println!(
                            "start_next parent:{parent} s:{s} stack:{:?} parts:{:?}",
                            self.stack, self.scopes[parent as usize].parts
                        );
                    }
                    return *s;
                }
                last = *s;
            }
            if let Some(first) = self.scopes[parent as usize].parts.first() {
                self.last_scope = self.current_scope;
                self.current_scope = *first;
                self.stack.push(*first);
                if self.logging {
                    println!(
                        "start_next parent:{parent} current:{first} stack:{:?} parts:{:?}",
                        self.stack, self.scopes[parent as usize].parts
                    );
                }
                *first
            } else {
                panic!("Iterating empty scope {parent}");
            }
        }
    }

    pub fn finish_next(&mut self, from: u16, function: &str) {
        if self.logging {
            println!(
                "finish_next from:{from} current:{} stack:{:?}",
                self.current_scope, self.stack
            );
        }
        assert_eq!(from, self.current_scope, "Problem of scopes in {function}");
        assert_eq!(
            from,
            self.stack.pop().unwrap(),
            "Problem of scopes in {function}"
        );
        if !self.scopes[self.current_scope as usize].parts.is_empty() {
            assert_eq!(
                self.last_scope,
                *self.scopes[self.current_scope as usize]
                    .parts
                    .last()
                    .unwrap(),
                "Finishing non empty scope in {function}"
            );
        }
        if let Some(parent) = self.stack.last() {
            if let Some(last) = self.scopes[*parent as usize].parts.last() {
                if *last == from {
                    self.last_scope = self.current_scope;
                    self.current_scope = *parent;
                } else {
                    self.last_scope = self.current_scope;
                    self.current_scope = self.scopes[self.current_scope as usize].parent;
                }
            } else {
                self.last_scope = self.current_scope;
                self.current_scope = self.scopes[self.current_scope as usize].parent;
            }
        }
    }

    /** We have to allow for multiple passes through the scopes.
    # Panics
    When the later pass through the code creates a new scope.
     */
    pub fn start_scope(&mut self, at: (u32, u32), context: &str) -> u16 {
        let parent = self.current_scope;
        if self.last_scope == u16::MAX {
            self.current_scope = 0;
        } else {
            self.current_scope = self.last_scope + 1;
        }
        if self.logging {
            println!(
                "start scope {} context {context} at {}:{}",
                self.current_scope, at.0, at.1
            );
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
                parts: Vec::new(),
            });
            assert_ne!(parent, self.current_scope, "Incorrect scopes");
            if parent != u16::MAX {
                self.scopes[parent as usize].parts.push(self.current_scope);
            }
        } else if at != (0, 0) {
            assert_eq!(
                self.scopes[self.current_scope as usize].context, context,
                "Different contexts on scope {} at {}:{}",
                self.current_scope, at.0, at.1
            );
        }
        self.current_scope
    }

    pub fn finish_scope(&mut self, scope: u16, result: &Type, at: (u32, u32)) {
        assert_eq!(
            self.current_scope, scope,
            "Incorrect scope finish {:?} vs {:?}",
            self.scopes[self.current_scope as usize], self.scopes[scope as usize]
        );
        if self.logging {
            println!(
                "finish {} context {} at {}:{}",
                self.current_scope, self.scopes[self.current_scope as usize].context, at.0, at.1
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

    pub fn cur(&self) -> String {
        format!(
            "Scope {}:{:?}",
            self.current_scope, self.scopes[self.current_scope as usize]
        )
    }

    pub fn last_scope(&self) -> u16 {
        self.last_scope
    }

    pub fn move_scope(&mut self, scope: u16, into: u16) {
        assert!(
            scope < self.scopes.len() as u16,
            "Scope {scope} out of range"
        );
        assert!(into < self.scopes.len() as u16, "Scope {into} out of range");
        let parent = self.scopes[scope as usize].parent;
        self.scopes[parent as usize].parts.retain(|s| *s != scope);
        self.scopes[into as usize].parts.push(scope);
    }

    pub fn reset(&mut self) {
        self.current_scope = u16::MAX;
        self.last_scope = u16::MAX;
        self.stack.clear();
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

    pub fn work_text(&mut self, lexer: &mut Lexer) -> u16 {
        let n = format!("__work_{}", self.work_text + 1);
        self.work_text += 1;
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
        self.work_texts.insert(v);
        v
    }

    pub fn work_refs(&mut self, tp: &Type, lexer: &mut Lexer) -> u16 {
        let n = format!("__ref_{}", self.work_ref + 1);
        self.work_ref += 1;
        let v = if let Some(nr) = self.names.get(&n) {
            let vr = *nr.first().unwrap();
            self.variables[vr as usize].type_def = tp.clone();
            vr
        } else {
            let v = self.add_variable(&n, tp, lexer);
            // work variables always live in the main scope.
            if self.variables[v as usize].scope > 1 {
                self.variables[v as usize].scope = 1;
            }
            v
        };
        self.work_refs.insert(v);
        v
    }

    pub fn work_texts(&self) -> Vec<u16> {
        let mut res = Vec::new();
        for v in &self.work_texts {
            res.push(*v);
        }
        res
    }

    pub fn work_references(&self) -> Vec<u16> {
        let mut res = Vec::new();
        for v in &self.work_refs {
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
                let bl = self.start_next();
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
                    // ignore else if block
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
                self.finish_next(bl, name);
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
                        "Incorrect variable scope of {} on {name} at {file}:{}:{}",
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

    pub fn free(&mut self) {
        let mut intern = HashSet::new();
        self.intern(&mut intern, self.current_scope);
        for v in &mut self.variables {
            if intern.contains(&v.scope) {
                v.stack = u16::MAX;
            }
        }
    }

    fn intern(&self, result: &mut HashSet<u16>, scope: u16) {
        result.insert(scope);
        for s in self.scopes[scope as usize].parts.clone() {
            self.intern(result, s);
        }
    }

    /// Move the scope of a given variable to the given scope.
    pub fn move_text_scope(&mut self, var_nr: u16, to_scope: u16) {
        if self.variables[var_nr as usize].scope < to_scope {
            return;
        }
        // Problem when this is not a parent scope.
        assert!(to_scope == 0 || to_scope == 1, "Incorrect scope");
        self.variables[var_nr as usize].scope = to_scope;
        if to_scope == 1 {
            self.work_texts.insert(var_nr);
        } else {
            self.work_texts.remove(&var_nr);
        }
    }

    pub fn move_ref_scope(&mut self, var_nr: u16, to_scope: u16) {
        if self.variables[var_nr as usize].scope < to_scope {
            return;
        }
        // Problem when this is not a parent scope.
        assert!(to_scope == 0 || to_scope == 1, "Incorrect scope");
        self.variables[var_nr as usize].scope = to_scope;
        if to_scope == 1 {
            self.work_refs.insert(var_nr);
        } else {
            self.work_refs.remove(&var_nr);
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
