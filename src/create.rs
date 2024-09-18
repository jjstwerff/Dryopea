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
#![allow(clippy::too_many_lines)]
use crate::database::{{DbRef, Stores, Str}};
use crate::external;
use crate::state::State;\n
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
            let tp = Data::rust_type(&a.typedef, Context::Argument);
            if !a.mutable {
                writeln!(into, "    let {} = *s.code::<{tp}>();", a.name)?;
            }
        }
        for a in data.def(d_nr).attributes.iter().rev() {
            if a.name.starts_with('_') || res.is_empty() {
                continue;
            }
            let tp = Data::rust_type(&a.typedef, Context::Argument);
            if a.reference {
                writeln!(into, "    let {} = *s.get_stack::<u32>();", a.name)?;
            } else if a.mutable {
                if a.typedef == Type::Text {
                    writeln!(into, "    let {} = s.string();", a.name)?;
                } else {
                    writeln!(into, "    let {} = *s.get_stack::<{tp}>();", a.name)?;
                }
            }
        }
        for a_nr in 0..data.attributes(d_nr) {
            let name = "@".to_string() + &data.attr_name(d_nr, a_nr);
            let mut repl = data.attr_name(d_nr, a_nr);
            if data.attr_type(d_nr, a_nr) == Type::Text {
                repl += ".str()";
            }
            res = res.replace(&name, &repl);
        }
        res = res.replace("stores.", "s.database.");
        let returned = &data.def(d_nr).returned;
        if res.is_empty() {
            writeln!(into, "    s.{name}();")?;
        } else if *returned == Type::Void
            || (*returned == Type::Text && data.def(d_nr).name.starts_with("OpConst"))
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
