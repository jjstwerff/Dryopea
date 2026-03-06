// Copyright (c) 2024-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::data::{Context, Data, DefType, Definition, Type, Value};
use crate::database::Stores;
use std::collections::HashSet;
use std::io::Write;

pub struct Output<'a> {
    pub data: &'a Data,
    pub stores: &'a Stores,
}

/// Sanitize a lav variable name to a valid Rust identifier.
fn sanitize(name: &str) -> String {
    name.replace('#', "__")
}

impl Output<'_> {
    /**
    Try to output the webassembly needed for this code.
    # Errors
    When not possible to generate this correctly.
    */
    pub fn output_webassembly(&self, dir: &str) -> std::io::Result<()> {
        let program = "code/".to_string() + dir;
        let source = "code/".to_string() + dir + "/src";
        std::fs::create_dir_all(&source)?;
        std::fs::copy(
            "webassembly/src/external.rs",
            source.clone() + "/external.rs",
        )?;
        std::fs::copy("webassembly/src/store.rs", source.clone() + "/store.rs")?;
        let cw = &mut std::fs::File::create(program + "/Cargo.toml")?;
        cw.write_all(
            "[package]
name = \"scriptlib\"
version = \"0.1.0\"
"
            .as_bytes(),
        )?;
        let w = &mut std::fs::File::create(source + "/main.rs")?;
        w.write_all(
            "#![allow(unused_parens)]
mod external;
mod store;

use external::*;
"
            .as_bytes(),
        )?;
        self.output(w, 0, self.data.definitions())
    }

    /**
    Output rust code for a definition.
    # Errors
    When this resulted in a filesystem error.
    */
    pub fn output(&self, w: &mut dyn Write, from: u32, till: u32) -> std::io::Result<()> {
        writeln!(
            w,
            "\
#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_to_owned)]
#![allow(clippy::double_parens)]

extern crate dryopea;"
        )?;
        writeln!(w, "use dryopea::database::Stores;")?;
        writeln!(w, "use dryopea::keys::{{DbRef, Str, Key, Content}};")?;
        writeln!(w, "use dryopea::external;")?;
        writeln!(w, "use dryopea::external::*;")?;
        writeln!(w, "use dryopea::vector;\n")?;
        writeln!(w, "fn init(db: &mut Stores) {{")?;
        // Collect types to emit, sorted by known_type to match compile-time type ordering.
        // The compile-time DB creates vector types during parsing (via new_record), enums via
        // actual_types, and structs via fill_all — in that order. Sorting by known_type ensures
        // the runtime init recreates the same ordering so type IDs match.
        let mut type_defs: Vec<(u16, u32)> = Vec::new();
        for dnr in from..till {
            let def = self.data.def(dnr);
            let kt = def.known_type;
            let is_enum_value_with_attrs =
                def.def_type == DefType::EnumValue && !def.attributes.is_empty();
            if kt != u16::MAX
                && (matches!(def.def_type, DefType::Struct)
                    || def.def_type == DefType::Enum
                    || def.def_type == DefType::Vector
                    || is_enum_value_with_attrs)
            {
                type_defs.push((kt, dnr));
            }
        }
        type_defs.sort_by_key(|(kt, _)| *kt);
        for (_, dnr) in &type_defs {
            let dnr = *dnr;
            let def = self.data.def(dnr);
            if matches!(def.def_type, DefType::Struct) {
                self.output_struct(w, dnr, 0)?;
            } else if def.def_type == DefType::EnumValue && !def.attributes.is_empty() {
                // Determine the 1-based position in the parent enum's attributes
                let parent_nr = def.parent;
                let parent = self.data.def(parent_nr);
                let enum_value = parent
                    .attributes
                    .iter()
                    .enumerate()
                    .find(|(_, a)| a.name == def.name)
                    .map_or(0, |(i, _)| i as i32 + 1);
                self.output_struct(w, dnr, enum_value)?;
            } else if def.def_type == DefType::Enum {
                output_enum(w, def)?;
            } else if def.def_type == DefType::Vector {
                writeln!(
                    w,
                    "    db.vector({});",
                    self.data.def(def.parent).known_type
                )?;
            }
        }
        writeln!(w, "    db.finish();\n}}\n")?;
        for dnr in from..till {
            if matches!(self.data.def(dnr).def_type, DefType::Function) {
                self.output_function(w, dnr)?;
            }
        }
        Ok(())
    }

    fn output_struct(&self, w: &mut dyn Write, d_nr: u32, enum_value: i32) -> std::io::Result<()> {
        let def = self.data.def(d_nr);
        writeln!(
            w,
            "    let s = db.structure(\"{}\", {}); // {}",
            def.name, enum_value, def.known_type
        )?;
        // For EnumValue types, the compile-time DB may have an implicit "enum" discriminator
        // field at position 0 (added when a "byte" type already existed from another struct).
        // If the compile-time type has "enum" at position 0, we must emit it here so that
        // field indices match (content field is at index 1, not 0).
        if enum_value > 0
            && def.known_type != u16::MAX
            && self.stores.position(def.known_type, "enum") == 0
        {
            writeln!(w, "    let byte_enum = db.byte(0, false);")?;
            writeln!(w, "    db.field(s, \"enum\", byte_enum);")?;
        }
        for a in &def.attributes {
            let nm = a.name.clone();
            let td_nr = self.data.type_def_nr(&a.typedef);
            let tp = self.data.def(td_nr).known_type;
            assert_ne!(d_nr, u32::MAX, "Unknown def_nr for {:?}", a.typedef);
            let mut done = false;
            if let Type::Vector(c, _) = &a.typedef {
                let c_def = self.data.type_def_nr(c);
                if c_def != u32::MAX {
                    let content = self.data.def(c_def).known_type;
                    let vec_var = format!("vec_{}", sanitize(&nm));
                    writeln!(w, "    let {vec_var} = db.vector({content});")?;
                    writeln!(w, "    db.field(s, \"{nm}\", {vec_var});")?;
                }
                done = true;
            } else if let Type::Integer(min, _) = a.typedef {
                let s = a.typedef.size(a.nullable);
                if s == 1 {
                    let byte_var = format!("byte_{}", sanitize(&nm));
                    writeln!(w, "    let {byte_var} = db.byte({min}, {});", a.nullable)?;
                    writeln!(w, "    db.field(s, \"{nm}\", {byte_var});")?;
                    done = true;
                } else if s == 2 {
                    let short_var = format!("short_{}", sanitize(&nm));
                    writeln!(w, "    let {short_var} = db.short({min}, {});", a.nullable)?;
                    writeln!(w, "    db.field(s, \"{nm}\", {short_var});")?;
                    done = true;
                } else {
                    writeln!(w, "    db.field(s, \"{nm}\", 0);")?;
                    done = true;
                }
            }
            if !done {
                if let Type::Sorted(c_nr, keys, _) = &a.typedef {
                    let c_tp = self.data.def(*c_nr).known_type;
                    let sv = format!("sorted_{}", sanitize(&nm));
                    let keys_str = keys
                        .iter()
                        .map(|(k, asc)| format!("(\"{k}\".to_string(), {asc})"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    writeln!(w, "    let {sv} = db.sorted({c_tp}, &[{keys_str}]);")?;
                    writeln!(w, "    db.field(s, \"{nm}\", {sv});")?;
                    done = true;
                } else if let Type::Hash(c_nr, keys, _) = &a.typedef {
                    let c_tp = self.data.def(*c_nr).known_type;
                    let hv = format!("hash_{}", sanitize(&nm));
                    let keys_str = keys
                        .iter()
                        .map(|k| format!("\"{k}\".to_string()"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    writeln!(w, "    let {hv} = db.hash({c_tp}, &[{keys_str}]);")?;
                    writeln!(w, "    db.field(s, \"{nm}\", {hv});")?;
                    done = true;
                } else if let Type::Index(c_nr, keys, _) = &a.typedef {
                    let c_tp = self.data.def(*c_nr).known_type;
                    let iv = format!("index_{}", sanitize(&nm));
                    let keys_str = keys
                        .iter()
                        .map(|(k, asc)| format!("(\"{k}\".to_string(), {asc})"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    writeln!(w, "    let {iv} = db.index({c_tp}, &[{keys_str}]);")?;
                    writeln!(w, "    db.field(s, \"{nm}\", {iv});")?;
                    done = true;
                } else if tp != u16::MAX {
                    writeln!(w, "    db.field(s, \"{nm}\", {tp});")?;
                    done = true;
                }
            }
            let _ = done;
        }
        Ok(())
    }

    fn output_function(&self, w: &mut dyn Write, def_nr: u32) -> std::io::Result<()> {
        let def = self.data.def(def_nr);
        // Skip Op functions that are only used via inline #rust templates (no callable body needed).
        if def.name.starts_with("Op") && def.code == Value::Null && !def.rust.is_empty() {
            return Ok(());
        }
        write!(w, "fn {}(stores: &mut Stores", def.name)?;
        for a in &def.attributes {
            write!(
                w,
                ", mut var_{}: {}",
                sanitize(&a.name),
                self.data.rust_type(&a.typedef, &Context::Argument)
            )?;
        }
        write!(w, ") ")?;
        if def.returned != Type::Void {
            write!(
                w,
                "-> {} ",
                self.data.rust_type(&def.returned, &Context::Result)
            )?;
        }
        // Detect functions with linked-text returns: these have RefVar parameters alongside
        // a text return type. The generated Rust types are incompatible with the body code.
        let has_refvar_param = def
            .attributes
            .iter()
            .any(|a| matches!(a.typedef, Type::RefVar(_)));
        let use_todo = has_refvar_param && def.code != Value::Null;
        let mut declared = HashSet::new();
        // Mark argument variables as already declared so Set won't re-declare them.
        for arg_nr in def.variables.arguments() {
            declared.insert(arg_nr);
        }
        if use_todo {
            writeln!(w, "{{")?;
            writeln!(w, "  todo!()")?;
            writeln!(w, "\n}}")?;
        } else if let Value::Block(_) = def.code {
            self.output_code_inner(w, &def.code, def_nr, 0, &mut declared)?;
        } else {
            writeln!(w, "{{")?;
            if def.code != Value::Null {
                self.output_code_inner(w, &def.code, def_nr, 0, &mut declared)?;
            } else if def.returned != Type::Void {
                // Functions with no body that return a non-void type need a placeholder.
                write!(w, "  todo!()")?;
            }
            writeln!(w, "\n}}")?;
        }
        writeln!(w, "\n")
    }

    /**
    Output rust code for a specific value.
    # Errors
    On encountered filesystem problems.
    # Panics
    When internal output code calls would return non utf-8 data.
    */
    pub fn output_code(
        &self,
        w: &mut dyn Write,
        code: &Value,
        def_nr: u32,
        indent: u32,
    ) -> std::io::Result<()> {
        let mut declared = HashSet::new();
        self.output_code_inner(w, code, def_nr, indent, &mut declared)
    }

    /// Returns true if the value contains any call to a user-defined function
    /// (non-rust-template) that takes `stores: &mut Stores`. Such sub-expressions
    /// need to be pre-evaluated into let bindings to avoid double-borrow of stores.
    fn needs_pre_eval(&self, v: &Value) -> bool {
        match v {
            Value::Call(d_nr, vals) => {
                let def = self.data.def(*d_nr);
                if def.rust.is_empty() {
                    // User-defined function: itself needs stores, so pre-eval needed
                    true
                } else {
                    // Template: check if any argument needs pre-eval
                    vals.iter().any(|a| self.needs_pre_eval(a))
                }
            }
            Value::Block(_) => true, // blocks can use stores internally
            Value::If(test, t, f) => {
                self.needs_pre_eval(test) || self.needs_pre_eval(t) || self.needs_pre_eval(f)
            }
            Value::Drop(v) => self.needs_pre_eval(v),
            _ => false,
        }
    }

    /// Generate code for `v` into a String buffer (without writing pre-evals to w).
    fn generate_expr_buf(
        &self,
        v: &Value,
        def_nr: u32,
        indent: u32,
        declared: &mut HashSet<u16>,
    ) -> std::io::Result<String> {
        let mut buf = std::io::BufWriter::new(Vec::new());
        self.output_code_inner(&mut buf, v, def_nr, indent, declared)?;
        Ok(String::from_utf8(buf.into_inner()?).unwrap())
    }

    /// Emit `v` as a block statement, pre-evaluating complex sub-expressions
    /// (user-defined function calls with block arguments, or multiple user-fn
    /// calls in the same expression) to avoid double-mutable-borrow of stores.
    fn output_operator(
        &self,
        w: &mut dyn Write,
        v: &Value,
        def_nr: u32,
        indent: u32,
        declared: &mut HashSet<u16>,
        pre_counter: &mut usize,
    ) -> std::io::Result<()> {
        // Collect pre-evals for user-fn calls that appear as sub-expressions
        // inside template expansions or as arguments needing stores sequencing.
        let pre_evals = self.collect_pre_evals(v, def_nr, indent, declared, pre_counter)?;
        // Write pre-eval let bindings before the main operator.
        for (name, code) in &pre_evals {
            for _i in 0..=indent {
                write!(w, "  ")?;
            }
            writeln!(w, "let {name} = {code};")?;
        }
        // Generate the main operator with substitution map applied.
        if pre_evals.is_empty() {
            self.output_code_inner(w, v, def_nr, indent, declared)
        } else {
            self.output_code_with_subst(w, v, def_nr, indent, declared, &pre_evals)
        }
    }

    /// Recursively collect pre-evaluations needed for `v`.
    /// Returns a list of `(var_name, expr_code)` pairs.
    fn collect_pre_evals(
        &self,
        v: &Value,
        def_nr: u32,
        indent: u32,
        declared: &mut HashSet<u16>,
        counter: &mut usize,
    ) -> std::io::Result<Vec<(String, String)>> {
        let mut result = Vec::new();
        self.collect_pre_evals_inner(v, def_nr, indent, declared, counter, &mut result)?;
        Ok(result)
    }

    fn collect_pre_evals_inner(
        &self,
        v: &Value,
        def_nr: u32,
        indent: u32,
        declared: &HashSet<u16>,
        counter: &mut usize,
        result: &mut Vec<(String, String)>,
    ) -> std::io::Result<()> {
        match v {
            Value::Call(d_nr, vals) => {
                let def_fn = self.data.def(*d_nr);
                if def_fn.rust.is_empty() {
                    // User-defined function: pre-eval any Block or nested user-fn arguments
                    // (both cause double-borrow of stores if left inline).
                    for arg in vals {
                        let needs_pre = matches!(arg, Value::Block(_)) || self.needs_pre_eval(arg);
                        if needs_pre {
                            let name = format!("_pre{}", *counter);
                            *counter += 1;
                            // Collect inner pre-evals first so the pre-eval code itself
                            // is free of double borrows.
                            let start_idx = result.len();
                            self.collect_pre_evals_inner(
                                arg, def_nr, indent, declared, counter, result,
                            )?;
                            let inner_pre_evals = result[start_idx..].to_vec();
                            let mut decl_clone = declared.clone();
                            let raw_code =
                                self.generate_expr_buf(arg, def_nr, indent, &mut decl_clone)?;
                            let code = if inner_pre_evals.is_empty() {
                                raw_code
                            } else {
                                let mut c = raw_code;
                                for (pre_name, pre_code) in &inner_pre_evals {
                                    c = c.replace(pre_code.as_str(), pre_name.as_str());
                                }
                                c
                            };
                            result.push((name, code));
                        } else {
                            // Recurse into non-stores arguments
                            self.collect_pre_evals_inner(
                                arg, def_nr, indent, declared, counter, result,
                            )?;
                        }
                    }
                } else {
                    // Template function: pre-eval Block args (they may use stores) and,
                    // when multiple user-fn args exist, pre-eval those too to avoid
                    // double-borrow of stores.
                    let block_count = vals.iter().filter(|a| matches!(a, Value::Block(_))).count();
                    let user_fn_count = vals.iter().filter(|a| self.needs_pre_eval(a)).count();
                    // Pre-eval when template uses stores itself (any user-fn arg causes conflict)
                    // or when multiple user-fn args exist (they'd conflict with each other).
                    let template_uses_stores = def_fn.rust.contains("stores");
                    let needs_pre_eval_args = block_count > 0
                        || user_fn_count > 1
                        || (template_uses_stores && user_fn_count > 0);
                    if needs_pre_eval_args {
                        for arg in vals {
                            let is_block = matches!(arg, Value::Block(_));
                            let is_multi_user_fn = user_fn_count > 1 && self.needs_pre_eval(arg);
                            let is_stores_conflict =
                                template_uses_stores && self.needs_pre_eval(arg);
                            if is_block || is_multi_user_fn || is_stores_conflict {
                                let name = format!("_pre{}", *counter);
                                *counter += 1;
                                let mut decl_clone = declared.clone();
                                // First collect inner pre-evals for this arg so we can
                                // substitute them in the generated code.
                                let start_idx = result.len();
                                self.collect_pre_evals_inner(
                                    arg, def_nr, indent, declared, counter, result,
                                )?;
                                let inner_pre_evals = result[start_idx..].to_vec();
                                let raw_code =
                                    self.generate_expr_buf(arg, def_nr, indent, &mut decl_clone)?;
                                let code = if inner_pre_evals.is_empty() {
                                    raw_code
                                } else {
                                    let mut c = raw_code;
                                    for (pre_name, pre_code) in &inner_pre_evals {
                                        c = c.replace(pre_code.as_str(), pre_name.as_str());
                                    }
                                    c
                                };
                                result.push((name, code));
                            } else {
                                self.collect_pre_evals_inner(
                                    arg, def_nr, indent, declared, counter, result,
                                )?;
                            }
                        }
                    } else {
                        // No blocks, single or no user-fn call: recurse into args.
                        for arg in vals {
                            self.collect_pre_evals_inner(
                                arg, def_nr, indent, declared, counter, result,
                            )?;
                        }
                    }
                }
            }
            _ => {} // Other values don't need pre-eval
        }
        Ok(())
    }

    /// Generate code for `v`, substituting pre-eval variable names where needed.
    /// The substitution map maps expression code → variable name.
    fn output_code_with_subst(
        &self,
        w: &mut dyn Write,
        v: &Value,
        def_nr: u32,
        indent: u32,
        declared: &mut HashSet<u16>,
        pre_evals: &[(String, String)],
    ) -> std::io::Result<()> {
        // Check if this expression was pre-evaluated
        let mut buf_check = std::io::BufWriter::new(Vec::new());
        self.output_code_inner(&mut buf_check, v, def_nr, indent, declared)?;
        let code = String::from_utf8(buf_check.into_inner()?).unwrap();
        // Check if this whole value's code matches any pre-eval
        for (name, pre_code) in pre_evals {
            if code == *pre_code {
                write!(w, "{name}")?;
                return Ok(());
            }
        }
        // Not matched: try to do substitution within sub-expressions
        // For now, fall back to writing the code with simple text substitution
        let mut result = code;
        for (name, pre_code) in pre_evals {
            result = result.replace(pre_code, name);
        }
        write!(w, "{result}")?;
        Ok(())
    }

    /// Returns true if the value produces a void (`()`) Rust expression.
    fn is_void_value(&self, v: &Value) -> bool {
        match v {
            Value::Null => true,
            Value::Drop(_) => true,
            Value::Set(_, _) => true,
            Value::If(_, _, false_v) => matches!(**false_v, Value::Null),
            Value::Call(d_nr, _) => {
                let def = self.data.def(*d_nr);
                matches!(def.returned, Type::Void)
            }
            Value::Block(bl) => matches!(bl.result, Type::Void),
            _ => false,
        }
    }

    fn output_code_inner(
        &self,
        w: &mut dyn Write,
        code: &Value,
        def_nr: u32,
        indent: u32,
        declared: &mut HashSet<u16>,
    ) -> std::io::Result<()> {
        match code {
            Value::Text(txt) => {
                // Use debug format to produce a properly-escaped Rust string literal.
                write!(w, "{:?}.to_string()", txt)?;
            }
            Value::Long(v) => {
                write!(w, "{v}_i64")?;
            }
            Value::Int(v) => {
                write!(w, "{v}_i32")?;
            }
            Value::Enum(v, _) => {
                write!(w, "{v}_u8")?;
            }
            Value::Boolean(v) => {
                write!(w, "{v}")?;
            }
            Value::Float(v) => {
                write!(w, "{v}_f64")?;
            }
            Value::Single(v) => {
                write!(w, "{v}_f32")?;
            }
            Value::Null => {
                write!(w, "()")?;
            }
            Value::Line(_) => {
                // Line markers are debug annotations; skip in Rust output.
            }
            Value::Break(_) => {
                write!(w, "break")?;
            }
            Value::Continue(_) => {
                write!(w, "continue")?;
            }
            Value::Drop(v) => {
                self.output_code_inner(w, v, def_nr, indent, declared)?;
            }
            Value::Insert(ops) => {
                for (vnr, v) in ops.iter().enumerate() {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code_inner(w, v, def_nr, indent + 1, declared)?;
                    if vnr < ops.len() - 1 {
                        writeln!(w, ";")?;
                    } else {
                        writeln!(w)?;
                    }
                }
            }
            Value::Block(bl) => {
                writeln!(
                    w,
                    "{{ //{}_{}: {}",
                    bl.name,
                    bl.scope,
                    bl.result.show(self.data, &self.data.def(def_nr).variables)
                )?;
                let is_void_block = matches!(bl.result, Type::Void);
                // When the block expects a non-void result but trailing operator(s) are
                // void (drops, if-without-else, etc.), find the last non-void operator
                // and capture its value before the trailing void ops run.
                let last_op_idx = bl.operators.len().saturating_sub(1);
                let return_idx = if is_void_block || bl.operators.is_empty() {
                    None
                } else {
                    bl.operators.iter().rposition(|v| !self.is_void_value(v))
                };
                // When the return value is NOT the last operator, we need a temp binding.
                let has_trailing_void = return_idx.map(|i| i < last_op_idx).unwrap_or(false);
                let needs_todo = !is_void_block && return_idx.is_none();
                let mut pre_counter = 0usize;
                for (vnr, v) in bl.operators.iter().enumerate() {
                    // Collect pre-evaluations needed for this operator (to avoid double
                    // mutable borrow of stores when user-defined functions are nested).
                    let pre_evals =
                        self.collect_pre_evals(v, def_nr, indent + 1, declared, &mut pre_counter)?;
                    for (name, code) in &pre_evals {
                        for _i in 0..=indent {
                            write!(w, "  ")?;
                        }
                        writeln!(w, "let {name} = {code};")?;
                    }
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    // Text-returning blocks may get Str from db reads; convert to String.
                    let is_text_block = matches!(bl.result, Type::Text(_));
                    let need_str_conv =
                        is_text_block && !matches!(v, Value::Var(_) | Value::Text(_));
                    if has_trailing_void && return_idx == Some(vnr) {
                        // Capture return value into a temp so trailing void ops can follow.
                        write!(w, "let _ret = ")?;
                        if pre_evals.is_empty() {
                            self.output_code_inner(w, v, def_nr, indent + 1, declared)?;
                        } else {
                            self.output_code_with_subst(
                                w,
                                v,
                                def_nr,
                                indent + 1,
                                declared,
                                &pre_evals,
                            )?;
                        }
                        if need_str_conv {
                            write!(w, ".to_string()")?;
                        }
                        writeln!(w, ";")?;
                    } else {
                        if pre_evals.is_empty() {
                            self.output_code_inner(w, v, def_nr, indent + 1, declared)?;
                        } else {
                            self.output_code_with_subst(
                                w,
                                v,
                                def_nr,
                                indent + 1,
                                declared,
                                &pre_evals,
                            )?;
                        }
                        let is_return_expr =
                            !is_void_block && !has_trailing_void && return_idx == Some(vnr);
                        if is_return_expr {
                            if need_str_conv {
                                write!(w, ".to_string()")?;
                            }
                            writeln!(w)?;
                        } else {
                            writeln!(w, ";")?;
                        }
                    }
                }
                if has_trailing_void {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    writeln!(w, "_ret")?;
                } else if needs_todo {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    writeln!(w, "todo!()")?;
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(
                    w,
                    "}} /*{}_{}: {}*/",
                    bl.name,
                    bl.scope,
                    bl.result.show(self.data, &self.data.def(def_nr).variables)
                )?;
            }
            Value::Loop(lp) => {
                writeln!(w, "loop {{ //{}_{}", lp.name, lp.scope)?;
                for v in &lp.operators {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code_inner(w, v, def_nr, indent + 1, declared)?;
                    writeln!(w, ";")?;
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(w, "}} /*{}_{}*/", lp.name, lp.scope)?;
            }
            Value::Set(var, to) => {
                let variables = &self.data.def(def_nr).variables;
                let name = sanitize(variables.name(*var));
                // For text/reference block assignments, pre-declare the variable so that
                // any drop(@var) inside the block (e.g., on break) can reference it.
                if !declared.contains(var) && matches!(**to, Value::Block(_)) {
                    let var_tp = variables.tp(*var);
                    if matches!(var_tp, Type::Text(_)) {
                        declared.insert(*var);
                        writeln!(w, "let mut var_{name} = String::new();")?;
                        for _i in 0..=indent {
                            write!(w, "  ")?;
                        }
                        write!(w, "var_{name} = ")?;
                        self.output_code_inner(w, to, def_nr, indent, declared)?;
                        return Ok(());
                    }
                }
                if declared.contains(var) {
                    write!(w, "var_{name} = ")?;
                } else {
                    declared.insert(*var);
                    let var_tp = variables.tp(*var);
                    let tp_str = self.data.rust_type(var_tp, &Context::Variable);
                    if tp_str != "??" {
                        write!(w, "let mut var_{name}: {tp_str} = ")?;
                    } else {
                        write!(w, "let mut var_{name} = ")?;
                    }
                }
                if matches!(**to, Value::Null)
                    && self.data.rust_type(variables.tp(*var), &Context::Variable) == "DbRef"
                {
                    write!(w, "stores.null()")?;
                } else {
                    let needs_to_string = matches!(variables.tp(*var), Type::Text(_))
                        && !matches!(**to, Value::Var(_) | Value::Text(_));
                    self.output_code_inner(w, to, def_nr, indent, declared)?;
                    if needs_to_string {
                        write!(w, ".to_string()")?;
                    }
                }
            }
            Value::Var(var) => {
                write!(
                    w,
                    "var_{}",
                    sanitize(self.data.def(def_nr).variables.name(*var))
                )?;
            }
            Value::If(test, true_v, false_v) => {
                self.output_if(w, def_nr, test, true_v, false_v, indent, declared)?;
            }
            Value::Call(d_nr, vals) => {
                let def_fn = self.data.def(*d_nr);
                if def_fn.rust.is_empty() {
                    write!(w, "{}(stores", self.data.def(*d_nr).name)?;
                    for (a_nr, v) in vals.iter().enumerate() {
                        write!(w, ", ")?;
                        // For character-typed parameters, Value::Int means a character code point.
                        if let Some(a) = def_fn.attributes.get(a_nr) {
                            if matches!(a.typedef, Type::Character) {
                                if let Value::Int(n) = v {
                                    write!(w, "char::from_u32({}_u32).unwrap_or('\\0')", n)?;
                                    continue;
                                }
                            }
                            // Text args to lav-defined functions: params are String,
                            // so Str values need .to_string() and local vars need .clone()
                            // to avoid borrow-vs-move conflicts.
                            if matches!(a.typedef, Type::Text(_)) {
                                self.output_code_inner(w, v, def_nr, indent, declared)?;
                                if matches!(v, Value::Var(_)) {
                                    write!(w, ".clone()")?;
                                } else {
                                    write!(w, ".to_string()")?;
                                }
                                continue;
                            }
                        }
                        self.output_code_inner(w, v, def_nr, indent, declared)?;
                    }
                    write!(w, ")")?;
                } else {
                    // Special case: OpGetRecord-like functions with `todo!()` template but extra
                    // key arguments not listed in the lav signature. Identified by having a
                    // reference return type (DbRef) so we don't mishandle other todo!() ops.
                    let returns_ref = matches!(
                        def_fn.returned,
                        Type::Reference(_, _)
                            | Type::RefVar(_)
                            | Type::Vector(_, _)
                            | Type::Hash(_, _, _)
                            | Type::Sorted(_, _, _)
                            | Type::Index(_, _, _)
                    );
                    if def_fn.rust == "todo!()"
                        && vals.len() > def_fn.attributes.len()
                        && returns_ref
                    {
                        let n_attrs = def_fn.attributes.len();
                        let key_vals = &vals[n_attrs..];
                        let mut data_buf = std::io::BufWriter::new(Vec::new());
                        self.output_code_inner(&mut data_buf, &vals[0], def_nr, indent, declared)?;
                        let data_code = String::from_utf8(data_buf.into_inner()?).unwrap();
                        let mut db_tp_buf = std::io::BufWriter::new(Vec::new());
                        self.output_code_inner(&mut db_tp_buf, &vals[1], def_nr, indent, declared)?;
                        let db_tp_code = String::from_utf8(db_tp_buf.into_inner()?).unwrap();
                        if key_vals.len() == 1 {
                            let key_v = &key_vals[0];
                            let is_int_key = matches!(key_v, Value::Int(_) | Value::Enum(_, _))
                                || matches!(key_v, Value::Call(d, _)
                                    if matches!(self.data.def(*d).returned,
                                        Type::Integer(_, _) | Type::Long));
                            let mut key_buf = std::io::BufWriter::new(Vec::new());
                            self.output_code_inner(&mut key_buf, key_v, def_nr, indent, declared)?;
                            let key_code = String::from_utf8(key_buf.into_inner()?).unwrap();
                            if is_int_key {
                                write!(
                                    w,
                                    "stores.find_int(&({data_code}), ({db_tp_code}) as u16, ({key_code}) as i64)"
                                )?;
                            } else {
                                write!(
                                    w,
                                    "stores.find_str(&({data_code}), ({db_tp_code}) as u16, ({key_code}).as_str())"
                                )?;
                            }
                        } else {
                            // Multi-key lookup: build a Content slice dynamically.
                            write!(w, "stores.find(&({data_code}), ({db_tp_code}) as u16, &[")?;
                            for (ki, key_v) in key_vals.iter().enumerate() {
                                if ki > 0 {
                                    write!(w, ", ")?;
                                }
                                let is_int_key = matches!(key_v, Value::Int(_) | Value::Enum(_, _))
                                    || matches!(key_v, Value::Call(d, _)
                                        if matches!(self.data.def(*d).returned,
                                            Type::Integer(_, _) | Type::Long));
                                let mut key_buf = std::io::BufWriter::new(Vec::new());
                                self.output_code_inner(
                                    &mut key_buf,
                                    key_v,
                                    def_nr,
                                    indent,
                                    declared,
                                )?;
                                let key_code = String::from_utf8(key_buf.into_inner()?).unwrap();
                                if is_int_key {
                                    write!(w, "Content::Long(({key_code}) as i64)")?;
                                } else {
                                    write!(w, "Content::Str(Str::new(({key_code}).as_str()))")?;
                                }
                            }
                            write!(w, "])")?;
                        }
                        return Ok(());
                    }
                    let mut res = def_fn.rust.clone();
                    for (a_nr, a) in def_fn.attributes.iter().enumerate() {
                        let name = "@".to_string() + &a.name;
                        let mut val_code = std::io::BufWriter::new(Vec::new());
                        if a_nr < vals.len() {
                            // For enum-typed parameters, Value::Null means the null enum byte (255).
                            let is_enum_param = matches!(a.typedef, Type::Enum(_, _, _));
                            if is_enum_param && matches!(vals[a_nr], Value::Null) {
                                let with = "255u8".to_string();
                                res = res.replace(&name, &format!("({with})"));
                                continue;
                            }
                            // For character-typed parameters, Value::Int means a character code point.
                            let is_char_param = matches!(a.typedef, Type::Character);
                            if is_char_param {
                                if let Value::Int(n) = vals[a_nr] {
                                    let with = format!("char::from_u32({n}_u32).unwrap_or('\\0')");
                                    res = res.replace(&name, &format!("({with})"));
                                    continue;
                                }
                            }
                            self.output_code_inner(
                                &mut val_code,
                                &vals[a_nr],
                                def_nr,
                                indent,
                                declared,
                            )?;
                            let mut with = String::from_utf8(val_code.into_inner()?).unwrap();
                            // Integer parameter receiving a char value needs explicit cast.
                            if matches!(a.typedef, Type::Integer(_, _)) {
                                let val_is_char = match &vals[a_nr] {
                                    Value::Var(n) => matches!(
                                        self.data.def(def_nr).variables.tp(*n),
                                        Type::Character
                                    ),
                                    Value::Call(d, _) => {
                                        matches!(self.data.def(*d).returned, Type::Character)
                                    }
                                    _ => false,
                                };
                                if val_is_char {
                                    with += " as u32 as i32";
                                }
                            }
                            res = res.replace(&name, &format!("({with})"));
                        } else {
                            println!(
                                "Problem def_fn {def_fn} attributes {:?} vals {vals:?}",
                                def_fn.attributes
                            );
                            break;
                        }
                    }
                    write!(w, "{res}")?;
                }
            }
            Value::Return(val) => {
                write!(w, "return ")?;
                self.output_code_inner(w, val, def_nr, indent, declared)?;
            }
            _ => write!(w, "{code:?}")?,
        }
        Ok(())
    }

    fn output_if(
        &self,
        w: &mut dyn Write,
        def_nr: u32,
        test: &Value,
        true_v: &Value,
        false_v: &Value,
        indent: u32,
        declared: &mut HashSet<u16>,
    ) -> std::io::Result<()> {
        write!(w, "if ")?;
        let b_true = matches!(*true_v, Value::Block(_));
        let b_false = matches!(*false_v, Value::Block(_));
        self.output_code_inner(w, test, def_nr, indent, declared)?;
        if b_true {
            write!(w, " ")?;
        } else {
            write!(w, " {{")?;
        }
        self.output_code_inner(w, true_v, def_nr, indent + u32::from(!b_true), declared)?;
        if let Value::Block(_) = *true_v {
            write!(w, " else ")?;
        } else {
            write!(w, "}} else ")?;
        }
        if !b_false {
            write!(w, "{{")?;
        }
        self.output_code_inner(w, false_v, def_nr, indent + u32::from(!b_false), declared)?;
        if !b_false {
            write!(w, "}}")?;
        }
        Ok(())
    }
}

fn output_enum(w: &mut dyn Write, def: &Definition) -> std::io::Result<()> {
    writeln!(w, "    let e = db.enumerate(\"{}\");", def.name)?;
    for a in &def.attributes {
        writeln!(w, "    db.value(e, \"{}\", u16::MAX);", a.name)?;
    }
    Ok(())
}
