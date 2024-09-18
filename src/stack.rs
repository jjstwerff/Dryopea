// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_truncation)]
use crate::data::{Context, Data, Definition, Type, Value};
use crate::keys::DbRef;
use crate::state::State;

/**
    Calculate the space needed for data.
    This can depend on the context (on stack for an Argument or in code for a Constant)
*/
#[allow(clippy::needless_pass_by_value)]
pub fn size(tp: &Type, context: Context) -> u16 {
    match tp {
        Type::Integer(min, max)
            if context == Context::Constant && i64::from(*max) - i64::from(*min) <= 256 =>
        {
            1
        }
        Type::Integer(min, max)
            if context == Context::Constant && i64::from(*max) - i64::from(*min) <= 65536 =>
        {
            2
        }
        Type::Boolean | Type::Enum(_) => 1,
        Type::Integer(_, _) | Type::Single | Type::Function(_, _) => 4,
        Type::Long | Type::Float => 8,
        Type::Text if context == Context::Variable => size_of::<String>() as u16,
        Type::Text => size_of::<&str>() as u16,
        Type::Reference(_) | Type::Vector(_) => size_of::<DbRef>() as u16,
        _ => 0,
    }
}

/// Stack information on variable positions and scopes to generate byte-code.
#[allow(dead_code)]
pub struct Stack<'a> {
    /// Stack position of the various used variables and parameters.
    variables: Vec<u16>,
    /// Current stack position related to the current variables & expression.
    pub position: u16,
    pub data: &'a Data,
    def_nr: u32,
    pub scope: u8,
    pub logging: bool,
    /// Current loops (start-position, stack-position, break-positions)
    loops: Vec<(u32, u16, Vec<u32>)>,
}

impl<'a> Stack<'a> {
    pub fn new(data: &'a Data, def_nr: u32, logging: bool) -> Stack<'a> {
        Stack {
            variables: Vec::new(),
            position: 0,
            data,
            def_nr,
            scope: 0,
            logging,
            loops: Vec::new(),
        }
    }

    /** Return the amount of space on stack is needed as calculated from code */
    pub fn size_code(&self, val: &Value) -> u16 {
        match val {
            Value::Int(_) | Value::Single(_) => 4,
            Value::Long(_) | Value::Float(_) => 8,
            Value::Boolean(_) | Value::Enum(_, _) => 1,
            Value::Block(lp) => {
                if lp.is_empty() {
                    0
                } else {
                    self.size_code(lp.last().unwrap())
                }
            }
            Value::Call(d_nr, _) => size(&self.data.def(*d_nr).returned, Context::Argument),
            Value::If(_, true_val, _) => self.size_code(true_val),
            Value::Text(_) => size_of::<&str>() as u16,
            Value::Var(v) => size(
                &self.data.def(self.def_nr).variables[*v as usize].var_type,
                Context::Argument,
            ),
            _ => 0,
        }
    }

    pub fn start(&mut self, var: u32) {
        assert!(
            var < self.data.def(self.def_nr).variables.len() as u32,
            "Variable {var} doesn't exist in {}",
            self.data.def(self.def_nr).name
        );
        while self.variables.len() <= var as usize {
            self.variables.push(0);
        }
        assert_eq!(
            self.variables[var as usize],
            0,
            "Variable {} was not freed properly in {}",
            self.data.def(self.def_nr).variables[var as usize].name,
            self.data.def(self.def_nr).name
        );
        self.variables[var as usize] = self.position;
    }

    pub fn claim(&mut self, tp: &Type, context: Context) {
        self.position += size(tp, context);
    }

    pub fn operator(&mut self, d_nr: u32) {
        let d = self.data.def(d_nr);
        let mut parameters = 0;
        for p in &d.attributes {
            if p.mutable {
                parameters += size(&p.typedef, Context::Argument);
            }
        }
        let ret = size(&d.returned, Context::Argument);
        assert!(
            self.position >= parameters,
            "Incorrect stack {} versus {parameters} in {} operator {d_nr}:{}",
            self.position,
            self.data.def(self.def_nr).name,
            self.data.def(d_nr).name
        );
        self.position -= parameters;
        self.position += ret;
    }

    pub fn var_type(&self, var_nr: u32) -> &Type {
        assert!(
            var_nr < self.data.def(self.def_nr).variables.len() as u32,
            "Variable {var_nr} doesn't exist in {}",
            self.data.def(self.def_nr).name
        );
        &self.data.def(self.def_nr).variables[var_nr as usize].var_type
    }

    pub fn free(&mut self, to: u32) {
        for p in 0..self.variables.len() {
            if u32::from(self.variables[p]) >= to {
                self.variables[p] = 0;
            }
        }
    }

    pub fn position(&self, var: u32) -> u16 {
        assert!(
            var < self.variables.len() as u32,
            "Variable {} doesn't exist in {}",
            self.data.def(self.def_nr).variables[var as usize].name,
            self.data.def(self.def_nr).name
        );
        self.variables[var as usize]
    }

    pub fn add_op(&mut self, name: &str, state: &mut State) {
        let op_nr = self.data.def_nr(name);
        assert_ne!(op_nr, u32::MAX, "Unknown operator {name}");
        state.remember_stack(self.position);
        state.code_add(self.data.def(op_nr).op_code);
        self.operator(op_nr);
    }

    pub fn def(&self) -> &Definition {
        self.data.def(self.def_nr)
    }

    pub fn add_loop(&mut self, code_pos: u32) {
        self.loops.push((code_pos, self.position, Vec::new()));
    }

    pub fn end_loop(&mut self, state: &mut State) {
        let breaks = &self.loops.pop().unwrap().2;
        for b in breaks {
            state.code_put(*b, (i64::from(state.code_pos) - i64::from(*b) - 2) as i16);
        }
    }

    pub fn add_break(&mut self, code_pos: u32, loop_nr: u16) {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].2.push(code_pos);
    }

    pub fn get_loop(&self, loop_nr: u16) -> u32 {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].0
    }

    pub fn loop_position(&self, loop_nr: u16) -> u16 {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].1
    }
}
