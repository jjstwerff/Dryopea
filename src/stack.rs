// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_truncation)]

use crate::data::{Context, Data};
use crate::state::State;
use crate::variables;
use crate::variables::Function;
use std::collections::BTreeMap;

/// Stack information on variable positions and scopes to generate byte-code.
#[allow(dead_code)]
pub struct Stack<'a> {
    pub position: u16,
    pub data: &'a Data,
    pub function: Function,
    pub def_nr: u32,
    pub logging: bool,
    /// Current loops (start-position, stack-position, break-positions)
    loops: Vec<(u32, u16, Vec<u32>)>,
    /// All variables in their parent scope
    scopes: BTreeMap<u16, Vec<u16>>,
}

impl<'a> Stack<'a> {
    pub fn new(function: Function, data: &'a mut Data, def_nr: u32, logging: bool) -> Stack<'a> {
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
        state.code_add(self.data.def(op_nr).op_code);
        self.operator(op_nr);
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
