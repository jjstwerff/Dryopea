// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_possible_truncation)]
#![allow(dead_code)]
#![allow(clippy::large_types_passed_by_value)]
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
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};

// Iterator details on each for loop inside the current function
#[derive(Debug, Clone)]
struct Iterator {
    inside: u16,       // iterator number or MAX when top level loop
    variable: u16,     // variable number
    on: u8,            // structure type and direction
    db_tp: u16,        // database type of this structure
    value: Box<Value>, // code to gain the structure or Value::Null for a range
    counter: u16,      // variable number or MAX when it is not used
}

// This is created for every variable instance, even if those are of the same name.
#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    type_def: Type,
    source: (u32, u32),
    scope: u16,
    stack_pos: u16,
    uses: u16,
    argument: bool,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub file: String,
    steps: Vec<u8>,
    unique: u16,
    current_loop: u16,
    loops: Vec<Iterator>,
    variables: Vec<Variable>,
    work_text: u16,
    work_ref: u16,
    // Work variables for texts
    work_texts: BTreeSet<u16>,
    // Work variables for stores
    work_refs: BTreeSet<u16>,
    // The names store only the last known instance of this variable in the function.
    names: HashMap<String, u16>,
    pub done: bool,
    pub logging: bool,
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
    pub fn new(name: &str, file: &str) -> Self {
        Function {
            name: name.to_string(),
            file: file.to_string(),
            steps: Vec::new(),
            unique: 0,
            current_loop: u16::MAX,
            loops: Vec::new(),
            work_text: 0,
            work_ref: 0,
            variables: Vec::new(),
            work_texts: BTreeSet::new(),
            work_refs: BTreeSet::new(),
            names: HashMap::new(),
            logging: false,
            done: false,
        }
    }

    pub fn append(&mut self, other: &mut Function) {
        self.current_loop = u16::MAX;
        self.logging = other.logging;
        self.unique = 0;
        other.unique = 0;
        self.loops.clear();
        self.loops.append(&mut other.loops);
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
        Function {
            name: other.name.clone(),
            file: other.file.clone(),
            current_loop: u16::MAX,
            steps: Vec::new(),
            unique: 0,
            loops: other.loops.clone(),
            variables: other.variables.clone(),
            work_text: 0,
            work_ref: 0,
            work_texts: BTreeSet::new(),
            work_refs: BTreeSet::new(),
            names: other.names.clone(),
            logging: other.logging,
            done: other.done,
        }
    }

    pub fn start_loop(&mut self) -> u16 {
        self.loops.push(Iterator {
            inside: self.current_loop,
            variable: u16::MAX,
            on: 0,
            db_tp: u16::MAX,
            value: Box::new(Value::Null),
            counter: u16::MAX,
        });
        self.current_loop = self.loops.len() as u16 - 1;
        self.current_loop
    }

    pub fn loop_var(&mut self, variable: u16) {
        self.loops[self.current_loop as usize].variable = variable;
    }

    pub fn set_loop(&mut self, on: u8, db_tp: u16, value: &Value) {
        let l = &mut self.loops[self.current_loop as usize];
        l.on = on;
        l.db_tp = db_tp;
        l.value = Box::new(value.clone());
    }

    /**
    Stop the current loop.
    # Panics
    When this loop is not started.
    */
    pub fn finish_loop(&mut self, loop_nr: u16) {
        assert_eq!(self.current_loop, loop_nr, "Incorrect loop finish");
        self.current_loop = self.loops[self.current_loop as usize].inside;
    }

    pub fn loop_count(&mut self, count_var: u16) {
        self.loops[self.current_loop as usize].counter = count_var;
    }

    pub fn loop_counter(&mut self) -> u16 {
        self.loops[self.current_loop as usize].counter
    }

    pub fn loop_nr(&self, variable: &str) -> u16 {
        let mut c = self.current_loop;
        let mut nr = 0;
        while c != u16::MAX
            && self.variables[self.loops[c as usize].variable as usize].name != variable
        {
            c = self.loops[c as usize].inside;
            nr += 1;
        }
        nr
    }

    pub fn loop_on(&self, var_nr: u16) -> u8 {
        let mut c = self.current_loop;
        while c != u16::MAX {
            if self.loops[c as usize].variable == var_nr {
                return self.loops[c as usize].on;
            }
            c = self.loops[c as usize].inside;
        }
        0
    }

    pub fn loop_value(&self, var_nr: u16) -> &Value {
        let mut c = self.current_loop;
        while c != u16::MAX {
            if self.loops[c as usize].variable == var_nr {
                return &self.loops[c as usize].value;
            }
            c = self.loops[c as usize].inside;
        }
        &Value::Null
    }

    pub fn loop_db_tp(&self, var_nr: u16) -> u16 {
        let mut c = self.current_loop;
        while c != u16::MAX {
            if self.loops[c as usize].variable == var_nr {
                return self.loops[c as usize].db_tp;
            }
            c = self.loops[c as usize].inside;
        }
        u16::MAX
    }

    pub fn counter(&mut self) -> u16 {
        self.loops[self.current_loop as usize].counter
    }

    pub fn needs_counter(&mut self, counter: u16) {
        self.loops[self.current_loop as usize].counter = counter;
    }

    pub fn name(&self, var_nr: u16) -> &str {
        if var_nr as usize >= self.variables.len() {
            return "??";
        }
        &self.variables[var_nr as usize].name
    }

    pub fn set_scope(&mut self, var_nr: u16, scope: u16) {
        assert!((var_nr as usize) < self.variables.len(), "Unknown variable");
        assert_eq!(
            self.variables[var_nr as usize].scope,
            u16::MAX,
            "Variable has a scope"
        );
        self.variables[var_nr as usize].scope = scope;
        self.done = true;
    }

    pub fn scope(&self, var_nr: u16) -> u16 {
        if var_nr as usize >= self.variables.len() {
            return u16::MAX;
        }
        self.variables[var_nr as usize].scope
    }

    pub fn on_scope(&self, scopes: &HashSet<u16>) -> Vec<u16> {
        let mut res = Vec::new();
        for (v_nr, v) in self.variables.iter().enumerate() {
            if scopes.contains(&v.scope) {
                res.push(v_nr as u16);
            }
        }
        res
    }

    pub fn size(&self, var_nr: u16, context: &Context) -> u16 {
        size(&self.variables[var_nr as usize].type_def, context)
    }

    pub fn tp(&self, var_nr: u16) -> &Type {
        &self.variables[var_nr as usize].type_def
    }

    pub fn is_independent(&self, var_nr: u16) -> bool {
        let d = self.variables[var_nr as usize].type_def.depend();
        d.is_empty() || (d.len() == 1 && d[0] == var_nr)
    }

    pub fn depend(&mut self, var_nr: u16, on: u16) {
        if on != u16::MAX {
            self.variables[var_nr as usize].type_def =
                self.variables[var_nr as usize].type_def.depending(on);
        }
    }

    pub fn uses(&self, var_nr: u16) -> u16 {
        self.variables[var_nr as usize].uses
    }

    pub fn stack(&self, var_nr: u16) -> u16 {
        self.variables[var_nr as usize].stack_pos
    }

    pub fn set_stack(&mut self, var_nr: u16, pos: u16) {
        self.variables[var_nr as usize].stack_pos = pos;
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
        self.names.contains_key(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        if let Some(nr) = self.names.get(name) {
            return Some(&self.variables[*nr as usize]);
        }
        None
    }

    pub fn arguments(&self) -> Vec<u16> {
        let mut arg = Vec::new();
        for (v_nr, v) in self.variables.iter().enumerate() {
            if v.argument {
                arg.push(v_nr as u16);
            }
        }
        arg
    }

    pub fn var(&self, name: &str) -> u16 {
        if let Some(nr) = self.names.get(name) {
            return *nr;
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
            if self.variables[*nr as usize].type_def.is_unknown() {
                self.variables[*nr as usize].type_def = type_def.clone();
            }
            return *nr;
        }
        self.new_var(name, type_def, lexer)
    }

    /// Create an exact copy of a variable, used to duplicate them when reused in later scopes.
    pub fn copy_variable(&mut self, var: u16) -> u16 {
        let v = self.variables.len() as u16;
        self.variables.push(Variable {
            name: self.variables[var as usize].name.clone(),
            type_def: self.variables[var as usize].type_def.clone(),
            source: self.variables[var as usize].source,
            scope: u16::MAX,
            stack_pos: u16::MAX,
            uses: 1,
            argument: false,
        });
        v
    }

    fn new_var(&mut self, name: &str, type_def: &Type, lexer: &mut Lexer) -> u16 {
        let v = self.variables.len() as u16;
        if !self.names.contains_key(name) {
            self.names.insert(name.to_string(), v);
        }
        self.variables.push(Variable {
            name: name.to_string(),
            type_def: type_def.clone(),
            source: lexer.at(),
            scope: u16::MAX,
            stack_pos: u16::MAX,
            uses: 1,
            argument: false,
        });
        v
    }

    pub fn add_unique(&mut self, prefix: &str, type_def: &Type, scope: u16) -> u16 {
        let v = self.variables.len() as u16;
        self.variables.push(Variable {
            name: format!("_{prefix}_{v}"),
            type_def: type_def.clone(),
            source: (0, 0),
            scope,
            stack_pos: u16::MAX,
            uses: 1,
            argument: false,
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
            for on in type_def.depend() {
                self.depend(var_nr, on);
            }
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
            if let Type::RefVar(in_tp) = var_tp
                && in_tp.is_equal(type_def)
            {
                return self.is_new(var_nr);
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
        true
    }

    fn is_new(&self, var_nr: u16) -> bool {
        self.variables[var_nr as usize].uses == 0
    }

    pub fn become_argument(&mut self, var_nr: u16) {
        self.variables[var_nr as usize].argument = true;
    }

    pub fn is_argument(&self, var_nr: u16) -> bool {
        self.variables[var_nr as usize].argument
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
                    if var.argument {
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
            *nr
        } else {
            self.add_variable(&n, &Type::Text(Vec::new()), lexer)
        };
        self.work_texts.insert(v);
        v
    }

    pub fn work_refs(&mut self, tp: &Type, lexer: &mut Lexer) -> u16 {
        let n = format!("__ref_{}", self.work_ref + 1);
        self.work_ref += 1;
        let v = if let Some(nr) = self.names.get(&n) {
            *nr
        } else {
            self.add_variable(&n, tp, lexer)
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

    pub fn claim(&mut self, var: u16, pos: u16, context: &Context) -> u16 {
        self.variables[var as usize].stack_pos = pos;
        pos + size(&self.variables[var as usize].type_def, context)
    }

    pub fn set_type(&mut self, var_nr: u16, tp: Type) {
        self.variables[var_nr as usize].type_def = tp;
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
        Type::Integer(_, _) | Type::Single | Type::Function(_, _) | Type::Character => 4,
        Type::Long | Type::Float => 8,
        Type::Text(_) if context == &Context::Variable => size_of::<String>() as u16,
        Type::Text(_) => size_of::<&str>() as u16,
        Type::RefVar(_)
        | Type::Reference(_, _)
        | Type::Vector(_, _)
        | Type::Index(_, _, _)
        | Type::Hash(_, _, _)
        | Type::Sorted(_, _, _)
        | Type::Spacial(_, _, _) => size_of::<DbRef>() as u16,
        _ => 0,
    }
}
