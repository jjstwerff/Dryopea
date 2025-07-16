// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_truncation)]

use crate::data::{Context, Data, Type, Value};
use crate::database::Stores;
use crate::state::State;
use crate::variables;
use crate::variables::Function;
use std::collections::BTreeMap;

pub struct Loop {
    scope: u16,
    start_pos: u32,
    stack_pos: u16,
    break_pos: Vec<u32>,
}

/// Stack information on variable positions and scopes to generate byte-code.
#[allow(dead_code)]
pub struct Stack<'a> {
    pub position: u16,
    pub data: &'a Data,
    pub function: Function,
    pub def_nr: u32,
    pub logging: bool,
    loops: Vec<Loop>,
    /// All variables in their parent scope
    scopes: BTreeMap<u16, Vec<u16>>,
}

impl<'a> Stack<'a> {
    pub fn new(function: Function, data: &'a Data, def_nr: u32, logging: bool) -> Stack<'a> {
        Stack {
            position: 0,
            data,
            def_nr,
            logging,
            loops: Vec::new(),
            scopes: function.gather_scopes(),
            function,
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
            Value::Call(d_nr, _) => {
                variables::size(&self.data.def(*d_nr).returned, &Context::Argument)
            }
            Value::If(_, true_val, _) => self.size_code(true_val),
            Value::Text(_) => size_of::<&str>() as u16,
            Value::Var(v) => variables::size(self.function.tp(*v), &Context::Argument),
            _ => 0,
        }
    }

    pub fn type_code(&self, val: &Value, stores: &Stores) -> u16 {
        match val {
            Value::Int(_) => stores.name("integer"),
            Value::Long(_) => stores.name("long"),
            Value::Single(_) => stores.name("single"),
            Value::Float(_) => stores.name("float"),
            Value::Boolean(_) => stores.name("boolean"),
            Value::Text(_) => stores.name("text"),
            Value::Enum(_, tp) => *tp,
            Value::Block(lp) => {
                if lp.is_empty() {
                    u16::MAX
                } else {
                    self.type_code(lp.last().unwrap(), stores)
                }
            }
            Value::Call(d_nr, _) => {
                let return_type = &self.data.def(*d_nr).returned;
                if return_type == &Type::Void {
                    u16::MAX
                } else {
                    let ret_nr = self.data.type_def_nr(return_type);
                    self.data.def(ret_nr).known_type
                }
            }
            Value::If(_, true_val, _) => self.type_code(true_val, stores),
            Value::Var(v) => {
                self.data
                    .def(self.data.type_def_nr(self.function.tp(*v)))
                    .known_type
            }
            _ => u16::MAX,
        }
    }

    pub fn operator(&mut self, d_nr: u32) {
        let d = self.data.def(d_nr);
        let mut parameters = 0;
        for p in &d.attributes {
            if p.mutable {
                parameters += variables::size(&p.typedef, &Context::Argument);
            }
        }
        let ret = variables::size(&d.returned, &Context::Argument);
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

    pub fn add_op(&mut self, name: &str, state: &mut State) {
        let op_nr = self.data.def_nr(name);
        assert_ne!(op_nr, u32::MAX, "Unknown operator {name}");
        state.remember_stack(self.position);
        state.code_add(self.data.def(op_nr).op_code as u8);
        self.operator(op_nr);
    }

    pub fn add_loop(&mut self, scope: u16, code_pos: u32) {
        self.loops.push(Loop {
            scope,
            start_pos: code_pos,
            stack_pos: self.position,
            break_pos: Vec::new(),
        });
    }

    pub fn end_loop(&mut self, state: &mut State) {
        let breaks = &self.loops.pop().unwrap().break_pos;
        for b in breaks {
            state.code_put(*b, (i64::from(state.code_pos) - i64::from(*b) - 2) as i16);
        }
    }

    pub fn add_break(&mut self, code_pos: u32, loop_nr: u16) {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].break_pos.push(code_pos);
    }

    pub fn get_loop(&self, loop_nr: u16) -> u32 {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].start_pos
    }

    pub fn loop_position(&self, loop_nr: u16) -> u16 {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].stack_pos
    }

    pub fn loop_scope(&self, loop_nr: u16) -> u16 {
        let l = self.loops.len() - 1;
        self.loops[l - loop_nr as usize].scope
    }
}
