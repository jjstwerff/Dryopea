// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Keep track of the defined local variables and parameters during
//! parsing
#![allow(clippy::cast_possible_truncation)]

use crate::data::{Type, Variable};
use std::collections::HashMap;

pub struct Types {
    /// Local variable positions in the current routine
    var_names: HashMap<String, u32>,
    /// The position of variables of this scope (points to after the lower scope)
    scopes: Vec<u32>,
    /// The current scope inside the code.
    scope: u8,
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

impl Types {
    pub fn new() -> Types {
        Types {
            var_names: HashMap::new(),
            scopes: Vec::new(),
            scope: 0,
        }
    }

    pub fn exists(&self, name: &str) -> bool {
        self.var_names.contains_key(name)
    }

    pub fn scope_push(&mut self, vars: u32) {
        self.scopes.push(vars);
        self.scope += 1;
    }

    /// Remove the variable names that are linked to the given scope.
    /// This will not remove the underlying variables.
    pub fn scope_pop(&mut self, vars: &[Variable]) {
        assert!(!self.scopes.is_empty(), "No scope to pop");
        let from = *self.scopes.last().unwrap();
        let till = vars.len() as u32;
        self.scopes.pop();
        self.scope -= 1;
        for v in from..till {
            self.var_names.remove(&vars[v as usize].name);
        }
    }

    pub fn var_nr(&self, name: &str) -> u32 {
        self.var_names[name]
    }

    /// Create a local variable in the current scope
    pub fn var_name(&mut self, name: &str, vnr: u32) {
        if !self.var_names.contains_key(name) {
            self.var_names.insert(name.to_string(), vnr);
        }
    }

    /// At the end of routine code, clear the currently known variables.
    pub fn clear(&mut self) {
        self.var_names.clear();
    }
}

impl Variable {
    #[must_use]
    pub fn used(&self) -> bool {
        self.uses > 0
    }

    #[must_use]
    pub fn get_type(&self) -> Type {
        self.var_type.clone()
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}
