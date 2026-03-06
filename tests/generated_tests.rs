// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Compiled tests generated from lav source code.
//! Each module corresponds to one file in tests/generated/.
//! Run the regular test suite first (cargo test) to (re)generate those files,
//! then run this binary to verify the generated Rust code produces the same results.

include!(concat!(env!("OUT_DIR"), "/generated_modules.rs"));
