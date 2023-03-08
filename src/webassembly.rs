// Copyright (c) 2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

use dryopea::wasm;

fn main() {
    let mut p = wasm::parse_wasm("webassembly/pkg/scriptlib_bg.wasm");
    p.fix_export("webassembly/pkg/scriptlib_exp.wasm");
    //wasm::parse_wasm("webassembly/pkg/scriptlib_exp.wasm");
}
