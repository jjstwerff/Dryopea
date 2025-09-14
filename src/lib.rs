// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![warn(clippy::pedantic)]

#[macro_use]
pub mod diagnostics;
mod calc;
pub mod data;
pub mod database;
pub mod hash;
pub mod keys;
mod lexer;
pub mod scopes;
pub mod text;
mod variables;
pub mod vector;

pub mod external;
pub mod generation;
pub mod parser;
mod png_store;
mod radix_tree;
mod store;
pub mod tree;
mod typedef;

pub mod create;
pub mod fill;
pub mod state;

pub mod interpreter;
mod stack;
