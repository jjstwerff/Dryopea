// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![warn(clippy::pedantic)]

#[macro_use]
pub mod diagnostics;
pub mod data;
mod lexer;
pub mod text;
mod types;

mod calc;
pub mod database;
pub mod hash;
pub mod keys;
pub mod vector;

pub mod external;
pub mod generation;
mod logger;
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
