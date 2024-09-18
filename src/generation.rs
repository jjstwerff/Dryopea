use crate::data::{Context, Data, DefType, Definition, Type, Value};
use crate::database::Stores;
use std::io::Write;

pub struct Output<'a> {
    pub data: &'a Data,
    pub stores: &'a Stores,
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
        writeln!(w, "use dryopea::database::{{Stores, KnownTypes, DbRef}};")?;
        writeln!(w, "use dryopea::external::*;\n")?;
        writeln!(w, "fn init(db: &mut KnownTypes) {{")?;
        for dnr in from..till {
            let def = self.data.def(dnr);
            if def.def_type == DefType::Struct {
                self.output_struct(w, dnr)?;
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
        writeln!(w, "}}\n")?;
        for dnr in from..till {
            if matches!(self.data.def(dnr).def_type, DefType::Function) {
                self.output_function(w, dnr)?;
            }
        }
        Ok(())
    }

    #[allow(clippy::cast_possible_truncation)]
    fn output_struct(&self, w: &mut dyn Write, d_nr: u32) -> std::io::Result<()> {
        let def = self.data.def(d_nr);
        let p = if def.parent == u32::MAX {
            u32::MAX
        } else {
            def.parent
        };
        let size = self.stores.size(def.known_type);
        writeln!(
            w,
            "    let s = db.structure(\"{}\".to_string(), {size}, {p}); // {}",
            def.name, def.known_type
        )?;
        for (f_nr, a) in def.attributes.iter().enumerate() {
            if !a.mutable {
                continue;
            }
            let nm = a.name.clone();
            let td_nr = self.data.type_def_nr(&a.typedef);
            let tp = self.data.def(td_nr).known_type;
            let pos = self.stores.position(def.known_type, f_nr as u16);
            assert_ne!(d_nr, u32::MAX, "Unknown def_nr for {:?}", a.typedef);
            let mut done = false;
            if let Type::Vector(c) = &a.typedef {
                let c_def = self.data.type_def_nr(c);
                if c_def != u32::MAX {
                    let content = self.data.def(c_def).known_type;
                    writeln!(
                        w,
                        "    db.field(s, \"{nm}\".to_string(), db.vector({content}), {pos});",
                    )?;
                }
                done = true;
            } else if let Type::Integer(min, _) = a.typedef {
                let s = a.typedef.size(a.nullable);
                if s == 1 {
                    writeln!(
                        w,
                        "    db.field(s, \"{nm}\".to_string(), db.byte({min}, {}), {pos});",
                        a.nullable
                    )?;
                    done = true;
                } else if s == 2 {
                    writeln!(
                        w,
                        "    db.field(s, \"{nm}\".to_string(), db.short({min}, {}), {pos});",
                        a.nullable
                    )?;
                    done = true;
                } else {
                    writeln!(w, "    db.field(s, \"{nm}\".to_string(), db.int(), {pos});")?;
                    done = true;
                }
            }
            if !done && tp != u16::MAX {
                writeln!(w, "    db.field(s, \"{nm}\".to_string(), {tp}, {pos});")?;
            }
        }
        Ok(())
    }

    fn output_function(&self, w: &mut dyn Write, def_nr: u32) -> std::io::Result<()> {
        let def = self.data.def(def_nr);
        if def.position.file == "default/01_code.gcp"
            && def.name.starts_with("Op")
            && def.code == Value::Null
        {
            return Ok(());
        }
        write!(w, "fn {}(stores: &mut Stores", def.name)?;
        for (anr, a) in def.attributes.iter().enumerate() {
            write!(
                w,
                ", var_{anr}: {}",
                Data::rust_type(
                    &a.typedef,
                    if a.reference {
                        Context::Reference
                    } else {
                        Context::Argument
                    }
                )
            )?;
        }
        write!(w, ") ")?;
        if def.returned != Type::Void {
            write!(w, "-> {} ", Data::rust_type(&def.returned, Context::Result))?;
        }
        if let Value::Block(_) = def.code {
            self.output_code(w, &def.code, def_nr, 0)?;
        } else {
            writeln!(w, "{{")?;
            if def.code != Value::Null {
                self.output_code(w, &def.code, def_nr, 0)?;
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
        match code {
            Value::Text(txt) => {
                write!(w, "\"{txt}\".to_string()")?;
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
            Value::Block(vals) => {
                writeln!(w, "{{")?;
                for (vnr, v) in vals.iter().enumerate() {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code(w, v, def_nr, indent + 1)?;
                    if vnr < vals.len() - 1 {
                        writeln!(w, ";")?;
                    } else {
                        writeln!(w)?;
                    }
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(w, "}}")?;
            }
            Value::Loop(vals) => {
                writeln!(w, "loop {{")?;
                for v in vals {
                    for _i in 0..=indent {
                        write!(w, "  ")?;
                    }
                    self.output_code(w, v, def_nr, indent + 1)?;
                    writeln!(w, ";")?;
                }
                for _i in 0..indent {
                    write!(w, "  ")?;
                }
                write!(w, "}}")?;
            }
            Value::Set(var, to) => {
                write!(
                    w,
                    "var_{} = ",
                    self.data.def(def_nr).variables[*var as usize].name
                )?;
                self.output_code(w, to, def_nr, indent)?;
            }
            Value::Let(var, to) => {
                write!(w, "let mut var_{var} = ")?;
                self.output_code(w, to, def_nr, indent)?;
            }
            Value::Var(var) => {
                write!(
                    w,
                    "var_{}",
                    self.data.def(def_nr).variables[*var as usize].name
                )?;
            }
            Value::If(test, true_v, false_v) => {
                self.output_if(w, def_nr, test, true_v, false_v, indent)?;
            }
            Value::Call(d_nr, vals) => {
                let def_fn = self.data.def(*d_nr);
                if def_fn.rust.is_empty() {
                    write!(w, "{}(stores", self.data.def(*d_nr).name)?;
                    for v in vals {
                        write!(w, ", ")?;
                        self.output_code(w, v, def_nr, indent)?;
                    }
                    write!(w, ")")?;
                } else {
                    let mut res = def_fn.rust.clone();
                    for (a_nr, a) in def_fn.attributes.iter().enumerate() {
                        let name = "@".to_string() + &a.name;
                        let mut val_code = std::io::BufWriter::new(Vec::new());
                        if a_nr < vals.len() {
                            self.output_code(&mut val_code, &vals[a_nr], def_nr, indent)?;
                            let with = String::from_utf8(val_code.into_inner()?).unwrap();
                            // TODO if with == "Null" { println!("Here! {} {code:?}", self.def_name(*d_nr));}
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
                self.output_code(w, val, def_nr, indent)?;
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
    ) -> std::io::Result<()> {
        write!(w, "if ")?;
        let b_true = matches!(*true_v, Value::Block(_));
        let b_false = matches!(*false_v, Value::Block(_));
        self.output_code(w, test, def_nr, indent)?;
        if b_true {
            write!(w, " ")?;
        } else {
            write!(w, " {{")?;
        };
        self.output_code(w, true_v, def_nr, indent + u32::from(!b_true))?;
        if let Value::Block(_) = *true_v {
            write!(w, " else ")?;
        } else {
            write!(w, "}} else ")?;
        }
        if !b_false {
            write!(w, "{{")?;
        }
        self.output_code(w, false_v, def_nr, indent + u32::from(!b_false))?;
        if !b_false {
            write!(w, "}}")?;
        }
        Ok(())
    }
}

fn output_enum(w: &mut dyn Write, def: &Definition) -> std::io::Result<()> {
    writeln!(w, "    let e = db.enumerate(\"{}\".to_string());", def.name)?;
    for a in &def.attributes {
        writeln!(w, "    db.value(e, \"{}\".to_string());", a.name)?;
    }
    Ok(())
}
