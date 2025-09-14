// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashSet;
use crate::data::Type;
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

pub struct Scopes {
    current_scope: u16,
    last_scope: u16,
    scopes: Vec<Scope>,
    stack: Vec<u16>,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            current_scope: u16::MAX,
            last_scope: u16::MAX,
            scopes: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn dump(&self) {
        for (s_nr, s) in self.scopes.iter().enumerate() {
            println!("{s_nr}:{s:?}");
        }
    }

    pub fn scope(&self) -> u16 {
        self.current_scope
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
            return 0;
        }
        let parent = *self.stack.last().unwrap();
        let mut last = u16::MAX;
        for s in &self.scopes[parent as usize].parts {
            if last == self.last_scope {
                self.last_scope = self.current_scope;
                debug_assert_eq!(
                    self.scopes[*s as usize].parent, self.current_scope,
                    "Incorrect parent"
                );
                self.current_scope = *s;
                self.stack.push(*s);
                return *s;
            }
            last = *s;
        }
        if let Some(first) = self.scopes[parent as usize].parts.first() {
            self.last_scope = self.current_scope;
            debug_assert_eq!(
                self.scopes[*first as usize].parent, self.current_scope,
                "Incorrect parent"
            );
            self.current_scope = *first;
            self.stack.push(*first);
            *first
        } else {
            panic!(
                "Iterating empty scope {parent}:{}",
                self.scopes[parent as usize].context
            );
        }
    }

    pub fn finish_next(&mut self, from: u16, function: &str) {
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


    pub fn last_scope(&self) -> u16 {
        self.last_scope
    }

    pub fn reset(&mut self) {
        self.current_scope = u16::MAX;
        self.last_scope = u16::MAX;
        self.stack.clear();
    }

    fn is_active(&self, scope: u16) -> bool {
        let mut s = self.current_scope;
        while s != u16::MAX {
            if s == scope {
                return true;
            }
            s = self.scopes[s as usize].parent;
        }
        false
    }

    fn intern(&self, result: &mut HashSet<u16>, scope: u16) {
        result.insert(scope);
        for s in self.scopes[scope as usize].parts.clone() {
            self.intern(result, s);
        }
    }
}
