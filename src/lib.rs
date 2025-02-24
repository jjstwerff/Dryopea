// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![warn(clippy::pedantic)]

#[macro_use]
pub mod diagnostics;
pub mod data;
mod lexer;
mod types;

mod calc;
pub mod database;

pub mod external;
pub mod generation;
pub mod hash;
pub mod keys;
mod logger;
pub mod parser;
mod png_store;
mod radix_tree;
mod rb_tree;
mod store;
mod typedef;

pub mod create;
pub mod fill;
pub mod state;

pub mod interpreter;
mod stack;
