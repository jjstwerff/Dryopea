// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#[macro_use]
pub mod diagnostics;
pub mod data;
mod lexer;

mod database;
mod external;
mod hash;
pub mod inter;
mod logger;
pub mod parser;
mod radix_tree;
mod rb_tree;
mod store;
mod typedef;
mod types;
