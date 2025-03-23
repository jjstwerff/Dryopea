use crate::data::{Context, Data, Type};
use std::fs::File;
use std::io::Write;

fn operator_name(operator: &str) -> String {
    let mut result = String::new();
    for (i, c) in operator.chars().enumerate() {
        if i < 2 {
            continue;
        }
        if c.is_uppercase() {
            if i > 2 {
                result += "_";
            }
            result += c.to_lowercase().to_string().as_str();
        } else {
            result.push(c);
        };
    }
    result
}

/**
    Write a library file with the known library functions.
    # Errors
    When the file cannot be written correctly.
*/
pub fn generate_lib(data: &Data) -> std::io::Result<()> {
    let mut into = File::create("tests/generated/text.rs")?;
    writeln!(
        into,
        "#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
use crate::database::Stores;
use crate::keys::{{DbRef, Str}};
use crate::state;
use crate::state::{{Call, State}};

pub const FUNCTIONS: &[(&str, Call)] = &["
    )?;
    for d_nr in 0..data.definitions() {
        let d = data.def(d_nr);
        let n = &d.name;
        if !d.is_operator() && !d.rust.is_empty() {
            writeln!(into, "    (\"{n}\", {n}),")?;
        }
    }
    writeln!(
        into,
        "];

pub fn init(state: &mut State) {{
    for (name, implement) in FUNCTIONS {{
        state.static_fn(name, *implement);
    }}
}}"
    )?;
    for d_nr in 0..data.definitions() {
        let d = data.def(d_nr);
        let n = &d.name;
        if d.is_operator() || d.rust.is_empty() {
            continue;
        }
        writeln!(into, "\nfn {n}(stores: &mut Stores, stack: &mut DbRef) {{")?;
        for a in data.def(d_nr).attributes.iter().rev() {
            let tp = data.rust_type(&a.typedef, &Context::Argument);
            writeln!(into, "    let v_{} = *stores.get::<{tp}>(stack);", a.name)?;
        }
        let mut res = data.def(d_nr).rust.clone();
        replace_attributes(data, d_nr, &mut res);
        if d.returned == Type::Void {
            writeln!(into, "    {res}")?;
        } else {
            writeln!(into, "    let new_value = {{ {res} }};")?;
            writeln!(into, "    stores.put(stack, new_value);")?;
        }
        writeln!(into, "}}")?;
    }
    Ok(())
}

fn replace_attributes(data: &Data, d_nr: u32, res: &mut String) {
    for a_nr in 0..data.attributes(d_nr) {
        let name = "@".to_string() + &data.attr_name(d_nr, a_nr);
        let mut repl = "v_".to_string();
        repl += &data.attr_name(d_nr, a_nr);
        if matches!(data.attr_type(d_nr, a_nr), Type::Text(_, _)) {
            repl += ".str()";
        }
        *res = res.replace(&name, &repl);
    }
}

/// Create the content of the fill.rs file from the default library definitions.
/// # Errors
/// When the resulting file cannot be correctly written.
pub fn generate_code(data: &Data) -> std::io::Result<()> {
    let mut into = File::create("tests/generated/fill.rs")?;
    writeln!(
        into,
        "#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
use crate::database::Stores;
use crate::external;
use crate::keys::{{DbRef, Str}};
use crate::state::State;
use crate::vector;

pub const OPERATORS: &[fn(&mut State)] = &["
    )?;
    for d_nr in 0..data.definitions() {
        let n = &data.def(d_nr).name;
        if data.def(d_nr).is_operator() {
            writeln!(into, "    {},", operator_name(n))?;
        }
    }
    writeln!(into, "];")?;
    for d_nr in 0..data.definitions() {
        let n = &data.def(d_nr).name;
        if !data.def(d_nr).is_operator() {
            continue;
        }
        let name = operator_name(n);
        writeln!(into, "\nfn {name}(s: &mut State) {{")?;
        let mut res = data.def(d_nr).rust.clone();
        for a in &data.def(d_nr).attributes {
            if a.name.starts_with('_') || res.is_empty() {
                continue;
            }
            let tp = data.rust_type(&a.typedef, &Context::Argument);
            if !a.mutable {
                writeln!(into, "    let v_{} = *s.code::<{tp}>();", a.name)?;
            }
        }
        for a in data.def(d_nr).attributes.iter().rev() {
            if a.name.starts_with('_') || res.is_empty() {
                continue;
            }
            let tp = data.rust_type(&a.typedef, &Context::Argument);
            if a.mutable {
                if matches!(a.typedef, Type::Text(_, _)) {
                    writeln!(into, "    let v_{} = s.string();", a.name)?;
                } else {
                    writeln!(into, "    let v_{} = *s.get_stack::<{tp}>();", a.name)?;
                }
            }
        }
        replace_attributes(data, d_nr, &mut res);
        res = res.replace("stores.", "s.database.");
        let returned = &data.def(d_nr).returned;
        if res.is_empty() {
            writeln!(into, "    s.{name}();")?;
        } else if *returned == Type::Void
            || (matches!(*returned, Type::Text(_, _)) && data.def(d_nr).name.starts_with("OpConst"))
        {
            writeln!(into, "    {res}")?;
        } else {
            writeln!(into, "    let new_value = {res};")?;
            writeln!(into, "    s.put_stack(new_value);")?;
        }
        writeln!(into, "}}")?;
    }
    Ok(())
}
