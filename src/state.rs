#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(dead_code)]

use crate::data::{Attribute, Block, Context, Data, I32, Type, Value};
use crate::database::{Parts, ShowDb, Stores};
use crate::fill::OPERATORS;
use crate::keys;
use crate::keys::{Content, DbRef, Key, Str};
use crate::stack::Stack;
use crate::text::FUNCTIONS;
use crate::tree;
use crate::variables::size;
use crate::vector;
use crate::{external, hash};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs::File;
use std::io::{Error, Read, Write};
use std::str::FromStr;

pub type Call = fn(&mut Stores, &mut DbRef);

/// Internal State of the interpreter to run bytecode.
pub struct State {
    bytecode: Vec<u8>,
    text_code: Vec<u8>,
    stack_cur: DbRef,
    pub stack_pos: u32,
    pub code_pos: u32,
    def_pos: u32,
    pub database: Stores,
    // Stack size of the arguments
    pub arguments: u16,
    // Local function stack positions of individual byte-code statements.
    pub stack: HashMap<u32, u16>,
    // Variables from byte code, used to also gain stack position
    pub vars: HashMap<u32, u16>,
    // Calls of function definitions from byte code.
    pub calls: HashMap<u32, Vec<u32>>,
    // Information for enumerate-types and database (record, vectors and fields) types.
    pub types: HashMap<u32, u16>,
    pub library: Vec<Call>,
    pub library_names: HashMap<String, u16>,
    text_positions: BTreeSet<u32>,
}

fn new_ref(data: &DbRef, pos: u32, arg: u16) -> DbRef {
    DbRef {
        store_nr: data.store_nr,
        rec: pos,
        pos: u32::from(arg),
    }
}

impl State {
    /**
    Create a new interpreter state
    # Panics
    When the statically defined alignment is not correct.
    */
    #[must_use]
    pub fn new(mut db: Stores) -> State {
        State {
            bytecode: Vec::new(),
            text_code: Vec::new(),
            stack_cur: db.database(1000),
            stack_pos: 4,
            code_pos: 0,
            def_pos: 0,
            database: db,
            arguments: 0,
            stack: HashMap::new(),
            vars: HashMap::new(),
            calls: HashMap::new(),
            types: HashMap::new(),
            library: Vec::new(),
            library_names: HashMap::new(),
            text_positions: BTreeSet::new(),
        }
    }

    pub fn static_fn(&mut self, name: &str, call: Call) {
        let nr = self.library.len() as u16;
        self.library_names.insert(name.to_string(), nr);
        self.library.push(call);
    }

    pub fn conv_text_from_null(&mut self) {
        self.put_stack(Str::new(""));
    }

    pub fn string_from_code(&mut self) {
        let size = *self.code::<u8>();
        let p = self.code_pos as usize;
        unsafe {
            let m = self
                .database
                .store_mut(&self.stack_cur)
                .addr_mut::<&str>(self.stack_cur.rec, self.stack_cur.pos + self.stack_pos);
            *m = std::str::from_utf8_unchecked(&self.bytecode[p..p + size as usize]);
            self.stack_pos += size_of::<&str>() as u32;
        }
        self.code_pos += u32::from(size);
    }

    pub fn string_from_texts(&mut self, start: i32, size: i32) {
        let p = start as usize;
        let s = unsafe {
            std::str::from_utf8_unchecked(&self.text_code[p..p + size as usize]).to_string()
        };
        self.put_stack(s);
    }

    #[must_use]
    pub fn string(&mut self) -> Str {
        self.stack_pos -= size_ptr();
        *self
            .database
            .store(&self.stack_cur)
            .addr::<Str>(self.stack_cur.rec, self.stack_cur.pos + self.stack_pos)
    }

    #[inline]
    pub fn append_text(&mut self) {
        let text = self.string();
        let pos = *self.code::<u16>();
        if cfg!(debug_assertions) {
            self.text_positions
                .insert(self.stack_cur.pos + self.stack_pos + size_ptr() - u32::from(pos));
        }
        let v1 = self.string_mut(pos - size_ptr() as u16);
        *v1 += text.str();
    }

    #[inline]
    pub fn create_ref(&mut self) {
        let pos = *self.code::<u16>();
        self.put_stack(DbRef {
            store_nr: self.stack_cur.store_nr,
            rec: self.stack_cur.rec,
            pos: self.stack_cur.pos + self.stack_pos - u32::from(pos),
        });
    }

    /**
    Read data from a file
    # Panics
    When the reading was incorrect.
    */
    pub fn get_file_text(&mut self) {
        let r = *self.get_stack::<DbRef>();
        let file = *self.get_stack::<DbRef>();
        if file.rec == 0 {
            return;
        }
        let store = self.database.store(&file);
        let file_path = store.get_str(store.get_int(file.rec, file.pos + 4) as u32);
        let buf = self.database.store_mut(&r).addr_mut::<String>(r.rec, r.pos);
        if let Ok(mut f) = File::open(file_path) {
            f.read_to_string(buf).unwrap();
        }
    }

    #[inline]
    pub fn get_ref_text(&mut self) {
        let r = *self.get_stack::<DbRef>();
        let t: &str = self.database.store(&r).addr::<String>(r.rec, r.pos);
        self.put_stack(Str::new(t));
    }

    #[inline]
    pub fn get_db_ref(&mut self) {
        let r = *self.get_stack::<DbRef>();
        let t = self.database.store(&r).addr::<DbRef>(r.rec, r.pos);
        self.put_stack(*t);
    }

    #[inline]
    pub fn set_db_ref(&mut self) {
        let v1 = *self.get_stack::<DbRef>();
        let r = *self.get_stack::<DbRef>();
        let t = self.database.store_mut(&r).addr_mut::<DbRef>(r.rec, r.pos);
        *t = v1;
    }

    pub fn append_ref_text(&mut self) {
        let text = self.string();
        let r = *self.get_stack::<DbRef>();
        if cfg!(debug_assertions) {
            self.text_positions.insert(r.pos);
        }
        let v1 = self.database.store_mut(&r).addr_mut::<String>(r.rec, r.pos);
        *v1 += text.str();
    }

    pub fn clear_ref_text(&mut self) {
        let r = *self.get_stack::<DbRef>();
        let v1 = self.database.store_mut(&r).addr_mut::<String>(r.rec, r.pos);
        v1.clear();
    }

    #[inline]
    pub fn append_character(&mut self) {
        let pos = *self.code::<u16>();
        let c = *self.get_stack::<char>();
        if c as u32 != 0 {
            self.string_mut(pos - 4).push(c);
        }
    }

    #[inline]
    pub fn text_compare(&mut self) {
        let v2 = char::from_u32(*self.get_stack::<i32>() as u32).unwrap_or('\u{20}');
        let v1 = *self.get_stack::<Str>();
        let mut ch = v1.str().chars();
        self.put_stack(if let Some(f_ch) = ch.next() {
            let res = f_ch.cmp(&v2);
            if res == Ordering::Less {
                -1
            } else {
                i32::from(
                    res == Ordering::Greater || (res == Ordering::Equal && ch.next().is_some()),
                )
            }
        } else {
            -1
        });
    }

    #[must_use]
    pub fn lines_text<'b>(val: &'b str, at: &mut i32) -> &'b str {
        if let Some(to) = val[*at as usize..].find('\n') {
            let r = &val[*at as usize..to];
            *at = to as i32 + 1;
            r
        } else {
            *at = i32::MIN;
            ""
        }
    }

    #[must_use]
    pub fn split_text<'b>(val: &'b str, on: &str, at: &mut i32) -> &'b str {
        if on.is_empty() {
            *at = i32::MIN;
            return "";
        }
        if let Some(to) = val[*at as usize..].find(on) {
            let r = &val[*at as usize..to];
            *at = (to + on.len()) as i32;
            r
        } else {
            *at = i32::MIN;
            ""
        }
    }

    #[inline]
    pub fn get_character(&mut self) {
        let mut from = *self.get_stack::<i32>();
        let v1 = self.string();
        if from < 0 {
            from += v1.len as i32;
        }
        if from < 0 || from >= v1.len as i32 {
            self.put_stack(0);
            return;
        }
        let mut b = v1.str().as_bytes()[from as usize];
        while b & 0xC0 == 0x80 && from > 0 {
            from -= 1;
            b = v1.str().as_bytes()[from as usize];
        }
        self.put_stack(if let Some(ch) = v1.str()[from as usize..].chars().next() {
            ch as i32
        } else {
            0
        });
    }

    #[inline]
    pub fn get_text_sub(&mut self) {
        let mut till = *self.get_stack::<i32>();
        let mut from = *self.get_stack::<i32>();
        let v1 = self.string();
        if from < 0 || from >= v1.len as i32 {
            self.put_stack(Str {
                ptr: v1.ptr,
                len: 0,
            });
            return;
        }
        let mut b = v1.str().as_bytes()[from as usize];
        while b & 0xC0 == 0x80 && from > 0 {
            from -= 1;
            b = v1.str().as_bytes()[from as usize];
        }
        if till == i32::MIN {
            let b = v1.str().as_bytes()[from as usize];
            let ch = if b & 0xE0 == 0xC0 {
                from as u32 + 2
            } else if b & 0xF0 == 0xE0 {
                from as u32 + 3
            } else if b & 0xF0 == 0xF0 {
                from as u32 + 4
            } else {
                from as u32 + 1
            };
            let res = unsafe {
                Str {
                    ptr: v1.ptr.offset(from as isize),
                    len: ch - from as u32,
                }
            };
            self.put_stack(res);
            return;
        }
        if till < 0 {
            till += v1.len as i32;
        }
        let mut len = till - from;
        if len + from > v1.len as i32 {
            len = v1.len as i32 - from;
        } else if till < v1.len as i32 {
            let mut t = till;
            let mut b = v1.str().as_bytes()[t as usize];
            while b & 0xC0 == 0x80 && t < v1.len as i32 {
                t += 1;
                b = v1.str().as_bytes()[t as usize];
                len += 1;
            }
        }
        // TODO allow negative till values or till < from
        unsafe {
            self.put_stack(Str {
                ptr: v1.ptr.offset(from as isize),
                len: len as u32,
            });
        }
    }

    pub fn clear_text(&mut self) {
        let pos = *self.code::<u16>();
        self.string_mut(pos).clear();
    }

    /**
    Free the content of a text variable.
    # Panics
    When the same variable is freed twice.
    */
    pub fn free_text(&mut self) {
        let pos = *self.code::<u16>();
        if cfg!(debug_assertions) {
            let s = self.string_mut(pos);
            s.clear();
            for _ in 0..s.len() {
                *s += "*";
            }
        }
        self.string_mut(pos).shrink_to(0);
        if cfg!(debug_assertions) {
            let var_pos = self.stack_cur.pos + self.stack_pos - u32::from(pos);
            let remove = self.text_positions.remove(&var_pos);
            assert!(remove, "double free");
        }
    }

    /** Get a string reference from a variety of internal string formats.
    # Panics
    When an unknown internal string format is found.
    */
    pub fn string_mut(&mut self, pos: u16) -> &mut String {
        self.database.store_mut(&self.stack_cur).addr_mut::<String>(
            self.stack_cur.rec,
            self.stack_cur.pos + self.stack_pos - u32::from(pos),
        )
    }

    /** Get a value from the byte-code increasing the position to after this value
    # Panics
    When the position is outside the byte-code
    */
    pub fn code<T>(&mut self) -> &T {
        assert!(
            self.code_pos + (size_of::<T>() as u32) <= self.bytecode.len() as u32,
            "Position {} + {} outside generated code {}",
            self.code_pos,
            size_of::<T>(),
            self.bytecode.len()
        );
        unsafe {
            let off = self
                .bytecode
                .as_ptr()
                .offset(self.code_pos as isize)
                .cast::<T>();
            self.code_pos += size_of::<T>() as u32;
            off.as_ref().expect("code")
        }
    }

    pub fn code_str(&mut self) -> &str {
        let len = *self.code::<u8>();
        unsafe {
            let off = self.bytecode.as_ptr().offset(self.code_pos as isize);
            self.code_pos += u32::from(len);
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(off, len as usize))
        }
    }

    pub fn static_str(&mut self) -> &str {
        let from = *self.code::<u32>() as usize;
        let len = *self.code::<u32>() as usize;
        std::str::from_utf8(&self.text_code[from..from + len]).unwrap_or_default()
    }

    /**
    Pull a value from stack
    # Panics
    When the stack has no values left
    */
    #[must_use]
    pub fn get_stack<T>(&mut self) -> &T {
        assert!(
            (size_of::<T>() as u32) < self.stack_pos,
            "No elements left on the stack {} < {}",
            self.stack_pos,
            size_of::<T>() as u32
        );
        self.stack_pos -= size_of::<T>() as u32;
        self.database
            .store(&self.stack_cur)
            .addr::<T>(self.stack_cur.rec, self.stack_cur.pos + self.stack_pos)
    }

    pub fn get_var<T>(&mut self, pos: u16) -> &T {
        self.database.store(&self.stack_cur).addr::<T>(
            self.stack_cur.rec,
            self.stack_cur.pos + self.stack_pos - u32::from(pos),
        )
    }

    pub fn mut_var<T>(&mut self, pos: u16) -> &mut T {
        self.database.store_mut(&self.stack_cur).addr_mut::<T>(
            self.stack_cur.rec,
            self.stack_cur.pos + self.stack_pos - u32::from(pos),
        )
    }

    pub fn put_var<T>(&mut self, pos: u16, value: T) {
        *self.database.store_mut(&self.stack_cur).addr_mut::<T>(
            self.stack_cur.rec,
            self.stack_cur.pos + self.stack_pos + size_of::<T>() as u32 - u32::from(pos),
        ) = value;
    }

    pub fn text(&mut self) {
        if cfg!(debug_assertions) {
            self.text_positions
                .insert(self.stack_cur.pos + self.stack_pos);
        }
        let v = self.string_mut(0);
        let s = String::new();
        unsafe {
            core::ptr::write(v, s);
        }
        self.stack_pos += size_str();
    }

    pub fn put_stack<T>(&mut self, val: T) {
        let m = self
            .database
            .store_mut(&self.stack_cur)
            .addr_mut::<T>(self.stack_cur.rec, self.stack_cur.pos + self.stack_pos);
        *m = val;
        self.stack_pos += size_of::<T>() as u32;
    }

    pub fn stack_pos(&mut self) {
        self.put_stack(self.stack_pos);
    }

    /// Call a function, remember the current code position on the stack.
    ///
    /// * `size` - the amount of stack space maximally needed for the new function.
    /// * `to` - the code position where the called function resides.
    pub fn fn_call(&mut self, _size: u16, to: i32) {
        self.put_stack(self.code_pos);
        // TODO allow to switch stacks
        self.code_pos = to as u32;
    }

    pub fn static_call(&mut self) {
        let call = *self.code::<u16>();
        let mut stack = self.stack_cur;
        stack.pos = self.stack_pos;
        self.library[call as usize](&mut self.database, &mut stack);
        self.stack_pos = stack.pos;
    }

    pub fn var_text(&mut self) {
        let pos = *self.code::<u16>();
        let new_value = Str::new(self.get_var::<String>(pos));
        self.put_stack(new_value);
    }

    pub fn arg_text(&mut self) {
        let pos = *self.code::<u16>();
        let new_value = *self.get_var::<Str>(pos);
        self.put_stack(new_value);
    }

    pub fn format_bool(&mut self) {
        let pos = *self.code::<u16>();
        let dir = *self.code::<i8>();
        let token = *self.code::<u8>();
        let width = *self.get_stack::<i32>();
        let val = *self.get_stack::<bool>();
        let s = self.string_mut(pos - 5);
        external::format_text(s, if val { "true" } else { "false" }, width, dir, token);
    }

    pub fn format_int(&mut self) {
        let pos = *self.code::<u16>();
        let radix = *self.code::<u8>();
        let token = *self.code::<u8>();
        let plus = *self.code::<bool>();
        let note = *self.code::<bool>();
        let width = *self.get_stack::<i32>();
        let val = *self.get_stack::<i32>();
        let s = self.string_mut(pos - 8);
        external::format_int(s, val, radix, width, token, plus, note);
    }

    pub fn format_long(&mut self) {
        let pos = *self.code::<u16>();
        let radix = *self.code::<u8>();
        let token = *self.code::<u8>();
        let plus = *self.code::<bool>();
        let note = *self.code::<bool>();
        let width = *self.get_stack::<i32>();
        let val = *self.get_stack::<i64>();
        let s = self.string_mut(pos - 12);
        external::format_long(s, val, radix, width, token, plus, note);
    }

    pub fn format_float(&mut self) {
        let pos = *self.code::<u16>();
        let precision = *self.get_stack::<i32>();
        let width = *self.get_stack::<i32>();
        let val = *self.get_stack::<f64>();
        let s = self.string_mut(pos - 16);
        external::format_float(s, val, width, precision);
    }

    pub fn format_single(&mut self) {
        let pos = *self.code::<u16>();
        let precision = *self.get_stack::<i32>();
        let width = *self.get_stack::<i32>();
        let val = *self.get_stack::<f32>();
        let s = self.string_mut(pos - 12);
        external::format_single(s, val, width, precision);
    }

    pub fn format_text(&mut self) {
        let pos = *self.code::<u16>();
        let dir = *self.code::<i8>();
        let token = *self.code::<u8>();
        let width = *self.get_stack::<i32>();
        let val = self.string();
        let s = self.string_mut(pos - 4 - size_ptr() as u16);
        external::format_text(s, val.str(), width, dir, token);
    }

    pub fn free_ref(&mut self) {
        let db = *self.get_stack::<DbRef>();
        self.database.free(&db);
    }

    pub fn format_database(&mut self) {
        let pos = *self.code::<u16>();
        let db_tp = *self.code::<u16>();
        let pretty = *self.code::<bool>();
        let val = *self.get_stack::<DbRef>();
        let mut s = String::new();
        ShowDb {
            stores: &self.database,
            store: val.store_nr,
            rec: val.rec,
            pos: val.pos,
            known_type: db_tp,
            pretty,
        }
        .write(&mut s, 0);
        self.string_mut(pos - size_ref() as u16).push_str(&s);
    }

    pub fn append(&mut self) {
        let size = *self.code::<u16>();
        let db_tp = *self.code::<u16>();
        let db = *self.get_stack::<DbRef>();
        let new_value = self.database.claim(&db, u32::from(size));
        self.database.set_default_value(db_tp, &new_value);
        self.put_stack(new_value);
    }

    pub fn database(&mut self) {
        let var = *self.code::<u16>();
        let db_tp = *self.code::<u16>();
        let size = self.database.size(db_tp);
        let db = *self.get_var::<DbRef>(var);
        self.database.clear(&db);
        let r = self.database.claim(&db, u32::from(size));
        self.database.set_default_value(db_tp, &r);
        let db = self.mut_var::<DbRef>(var);
        db.store_nr = r.store_nr;
        db.rec = 1;
        db.pos = 0;
    }

    pub fn new_record(&mut self) {
        let parent_tp = *self.code::<u16>();
        let fld = *self.code::<u16>();
        let data = *self.get_stack::<DbRef>();
        let new_value = self.database.record_new(&data, parent_tp, fld);
        self.database.set_default_value(
            if fld == u16::MAX {
                self.database.content(parent_tp)
            } else {
                self.database
                    .content(self.database.field_type(parent_tp, fld))
            },
            &new_value,
        );
        self.put_stack(new_value);
    }

    pub fn get_record(&mut self) {
        let (db_tp, key) = self.read_key(false);
        let data = *self.get_stack::<DbRef>();
        let res = if data.rec == 0 {
            DbRef {
                store_nr: data.store_nr,
                rec: 0,
                pos: 0,
            }
        } else {
            self.database.find(&data, db_tp, &key)
        };
        self.put_stack(res);
    }

    /**
    Iterate through a data structure from a given key to a given end-key.
    # Panics
    When called on a not implemented data-structure
    */
    pub fn iterate(&mut self) {
        let on = *self.code::<u8>();
        let arg = *self.code::<u16>();
        let keys_size = *self.code::<u8>();
        let mut keys = Vec::new();
        for _ in 0..keys_size {
            keys.push(Key {
                type_nr: *self.code::<i8>(),
                position: *self.code::<u16>(),
            });
        }
        let from_key = *self.code::<u8>();
        let till_key = *self.code::<u8>();
        let till = self.stack_key(till_key, &keys);
        let from = self.stack_key(from_key, &keys);
        let data = *self.get_stack::<DbRef>();
        // Start the loop at the 'till' key and walk to the 'from' key
        let reverse = on & 64 != 0;
        // The 'till' key is exclusive the found key
        let ex = on & 128 == 0;
        let start;
        let finish;
        let all = &self.database.allocations;
        match on & 63 {
            1 => {
                // index points to the record position inside the store
                if reverse {
                    let t = tree::find(&data, ex, arg, all, &keys, &till);
                    start = if ex {
                        t
                    } else {
                        tree::next(keys::store(&data, all), &new_ref(&data, t, arg))
                    };
                    let f = tree::find(&data, ex, arg, all, &keys, &from);
                    finish = tree::next(keys::store(&data, all), &new_ref(&data, f, arg));
                } else {
                    start = tree::find(&data, true, arg, all, &keys, &from);
                    let t = tree::find(&data, ex, arg, all, &keys, &till);
                    finish = if ex {
                        t
                    } else {
                        tree::previous(keys::store(&data, all), &new_ref(&data, t, arg))
                    };
                }
            }
            2 => {
                // sorted points to the position of the record inside the vector
                if reverse {
                    start =
                        vector::sorted_find(&data, ex, arg, all, &keys, &till).0 + u32::from(!ex);
                    finish = vector::sorted_find(&data, ex, arg, all, &keys, &from).0 + 1;
                } else {
                    let s = vector::sorted_find(&data, true, arg, all, &keys, &from).0;
                    start = if s == 0 { u32::MAX } else { s - 1 };
                    let (t, cmp) = vector::sorted_find(&data, ex, arg, all, &keys, &till);
                    finish = if ex || cmp { t } else { t + 1 };
                }
            }
            3 => {
                // ordered points to the position inside the vector of references
                if reverse {
                    start = vector::ordered_find(&data, true, all, &keys, &from).0 + u32::from(!ex);
                    finish = vector::ordered_find(&data, ex, all, &keys, &till).0 + 1;
                } else {
                    let (s, cmp) = vector::ordered_find(&data, ex, all, &keys, &till);
                    start = if cmp || s == 0 { s } else { s - 1 };
                    finish = vector::ordered_find(&data, ex, all, &keys, &from).0 - u32::from(!ex);
                }
            }
            _ => panic!("Not implemented on {on}"),
        }
        self.put_stack(start);
        self.put_stack(finish);
    }

    fn stack_key(&mut self, size: u8, keys: &[Key]) -> Vec<Content> {
        let mut key = Vec::new();
        for (k_nr, k) in keys.iter().enumerate() {
            if k_nr >= size as usize {
                break;
            }
            match k.type_nr.abs() {
                1 => key.push(Content::Long(i64::from(*self.get_stack::<i32>()))),
                2 => key.push(Content::Long(*self.get_stack::<i64>())),
                3 => key.push(Content::Single(*self.get_stack::<f32>())),
                4 => key.push(Content::Float(*self.get_stack::<f64>())),
                5 => key.push(Content::Long(i64::from(*self.get_stack::<bool>()))),
                6 => key.push(Content::Str(self.string())),
                7 => key.push(Content::Long(i64::from(*self.get_stack::<u8>()))),
                _ => panic!("Unknown key type"),
            }
        }
        key
    }

    /**
    Step to the next value for the iterator.
    # Panics
    When requested on a not-implemented iterator.
    */
    pub fn step(&mut self) {
        let state_var = *self.code::<u16>();
        let on = *self.code::<u8>();
        let arg = *self.code::<u16>();
        let cur = *self.get_var::<u32>(state_var);
        let finish = *self.get_var::<u32>(state_var - 4);
        let reverse = on & 64 != 0;
        let data = *self.get_stack::<DbRef>();
        let store = keys::store(&data, &self.database.allocations);
        let cur = if finish == u32::MAX {
            new_ref(&data, 0, 0)
        } else {
            match on & 63 {
                1 => {
                    let rec = new_ref(&data, cur, arg);
                    let n = if reverse {
                        tree::previous(store, &rec)
                    } else {
                        tree::next(store, &rec)
                    };
                    self.put_var(state_var - 8, n);
                    if n == finish {
                        self.put_var(state_var - 12, u32::MAX);
                    }
                    new_ref(&data, n, 0)
                }
                2 => {
                    let mut pos = if cur == u32::MAX {
                        i32::MAX
                    } else {
                        cur as i32
                    };
                    // TODO Implement reverse too
                    vector::vector_step(&data, &mut pos, &self.database.allocations);
                    self.put_var(state_var - 8, pos as u32);
                    if pos as u32 >= finish {
                        pos = i32::MAX;
                        self.put_var(state_var - 12, u32::MAX);
                    }
                    self.database.element_reference(
                        &data,
                        if pos == i32::MAX {
                            i32::MAX
                        } else {
                            8 + pos * i32::from(arg)
                        },
                    )
                }
                3 => {
                    let mut pos = cur as i32;
                    vector::vector_next(&data, &mut pos, 4, &self.database.allocations);
                    let vector = store.get_int(data.rec, data.pos) as u32;
                    let rec = if pos == i32::MAX {
                        0
                    } else {
                        store.get_int(vector, pos as u32) as u32
                    };
                    self.put_var(state_var - 8, pos as u32);
                    DbRef {
                        store_nr: data.store_nr,
                        rec,
                        pos: 0,
                    }
                }
                _ => panic!("Not implemented"),
            }
        };
        self.put_stack(cur);
    }

    /**
    Remove the current value from the iterator. Move the iterator to the previous value.
    # Panics
    When requested on a not-implemented iterator.
    */
    pub fn remove(&mut self) {
        let state_var = *self.code::<u16>();
        let on = *self.code::<u8>();
        let tp = *self.code::<u16>();
        let reverse = on & 64 != 0;
        let cur = *self.get_var::<i32>(state_var);
        let data = *self.get_stack::<DbRef>();
        match on & 63 {
            0 => {
                // vector
                let n = if reverse { cur + 1 } else { cur - 1 };
                vector::remove_vector(
                    &data,
                    u32::from(self.database.size(tp)),
                    cur as u32,
                    &mut self.database.allocations,
                );
                self.put_var(state_var - 8, n);
            }
            1 => {
                let cur = *self.get_var::<u32>(state_var);
                if cur == u32::MAX {
                    return;
                }
                let elm = new_ref(&data, cur, self.database.size(tp));
                let rec = new_ref(&data, cur, self.database.size(tp));
                let store = keys::store(&data, &self.database.allocations);
                let n = if reverse {
                    tree::previous(store, &rec)
                } else {
                    tree::next(store, &rec)
                };
                self.database.remove(&data, &elm, tp);
                self.put_var(state_var - 8, n);
            }
            _ => panic!("Not implemented on {on}"),
        }
    }

    /**
    Clear the given structure on the field
    */
    pub fn clear(&mut self) {
        let tp = *self.code::<u16>();
        let data = *self.get_stack::<DbRef>();
        self.database.remove_claims(&data, tp);
    }

    pub fn append_copy(&mut self) {
        let tp = *self.code::<u16>();
        let multiply = *self.get_stack::<i32>() as u32;
        let data = *self.get_stack::<DbRef>();
        let ctp = self.database.content(tp);
        let size = u32::from(self.database.size(ctp));
        let length = vector::length_vector(&data, &self.database.allocations);
        let v_rec =
            keys::store(&data, &self.database.allocations).get_int(data.rec, data.pos) as u32;
        let from = DbRef {
            store_nr: data.store_nr,
            rec: v_rec,
            pos: 8 + (length * size - size),
        };
        vector::vector_append(&data, multiply - 1, size, &mut self.database.allocations);
        for i in 0..(multiply - 1) {
            let to = DbRef {
                store_nr: data.store_nr,
                rec: v_rec,
                pos: 8 + (length + i) * size,
            };
            self.database.copy_block(&from, &to, size);
            self.database.copy_claims(&data, &to, ctp);
        }
    }

    pub fn copy_record(&mut self) {
        let tp = *self.code::<u16>();
        let to = *self.get_stack::<DbRef>();
        let data = *self.get_stack::<DbRef>();
        let size = u32::from(self.database.size(tp));
        self.database.copy_block(&data, &to, size);
        self.database.copy_claims(&data, &to, tp);
    }

    pub fn hash_add(&mut self) {
        let tp = *self.code::<u16>();
        let rec = *self.get_stack::<DbRef>();
        let data = *self.get_stack::<DbRef>();
        hash::add(
            &data,
            &rec,
            &mut self.database.allocations,
            &self.database.types[tp as usize].keys,
        );
    }

    pub fn validate(&mut self) {
        let tp = *self.code::<u16>();
        let data = *self.get_stack::<DbRef>();
        self.database.validate(&data, tp);
    }

    pub fn hash_find(&mut self) {
        let data = *self.get_stack::<DbRef>();
        let (db_tp, key) = self.read_key(true);
        let res = hash::find(
            &data,
            &self.database.allocations,
            &self.database.types[db_tp as usize].keys,
            &key,
        );
        self.put_stack(res);
    }

    pub fn hash_remove(&mut self) {
        let tp = *self.code::<u16>();
        let rec = *self.get_stack::<DbRef>();
        let data = *self.get_stack::<DbRef>();
        hash::remove(
            &data,
            &rec,
            &mut self.database.allocations,
            &self.database.types[tp as usize].keys,
        );
    }

    fn read_key(&mut self, full: bool) -> (u16, Vec<Content>) {
        let db_tp = *self.code::<u16>();
        let keys = self.database.get_keys(db_tp);
        let no_keys = if full {
            keys.len() as u8
        } else {
            *self.code::<u8>()
        };
        let mut key = Vec::new();
        for (k_nr, k) in keys.iter().enumerate() {
            if k_nr >= no_keys as usize {
                break;
            }
            match k {
                0 => key.push(Content::Long(i64::from(*self.get_stack::<i32>()))),
                1 => key.push(Content::Long(*self.get_stack::<i64>())),
                2 => key.push(Content::Single(*self.get_stack::<f32>())),
                3 => key.push(Content::Float(*self.get_stack::<f64>())),
                4 => key.push(Content::Long(i64::from(*self.get_stack::<bool>()))),
                5 => key.push(Content::Str(self.string())),
                _ => key.push(Content::Long(i64::from(*self.get_stack::<u8>()))),
            }
            // We assume that all none-base types are enumerate types.
        }
        (db_tp, key)
    }

    pub fn finish_record(&mut self) {
        let parent_tp = *self.code::<u16>();
        let fld = *self.code::<u16>();
        let record = *self.get_stack::<DbRef>();
        let data = *self.get_stack::<DbRef>();
        self.database.record_finish(&data, &record, parent_tp, fld);
    }

    pub fn cast_vector_from_text(&mut self) {
        let db_tp = *self.code::<u16>();
        let val = self.string();
        let db = self.database.database(8);
        let into = DbRef {
            store_nr: db.store_nr,
            rec: db.rec,
            pos: 4,
        };
        self.database.set_default_value(db_tp, &into);
        self.database.parse(val.str(), db_tp, &into);
        self.put_stack(into);
    }

    pub fn insert_vector(&mut self) {
        let size = *self.code::<u16>();
        let db_tp = *self.code::<u16>();
        let index = *self.get_stack::<i32>();
        let r = *self.get_stack::<DbRef>();
        let new_value =
            vector::insert_vector(&r, u32::from(size), index, &mut self.database.allocations);
        self.database.set_default_value(db_tp, &new_value);
        self.put_stack(new_value);
    }

    /**
    Returns from a function, the data structures that went out of scope should already have
    been freed at this point.
    * `ret`     - Size of the parameters to get the return address after it.
    * `value`   - Size of the return value.
    * `discard` - The amount of space claimed on the stack at this point.
    # Panics
    When there are claimed texts that are not freed yet.
    */
    pub fn fn_return(&mut self, ret: u16, value: u8, discard: u16) {
        let pos = self.stack_pos;
        self.stack_pos -= u32::from(discard);
        debug_assert!(
            self.text_positions
                .range(self.stack_pos..=pos)
                .next()
                .is_none(),
            "Not freed texts on return: {}",
            self.text_positions
                .range(self.stack_pos..=pos)
                .next()
                .unwrap()
        );
        let fn_stack = self.stack_pos;
        self.stack_pos += u32::from(ret);
        self.code_pos = *self.get_var::<u32>(0);
        self.copy_result(value, pos, fn_stack);
    }

    /**
    Clear the stack of local variables, possibly return a value.
    * `value`   - Size of the return value.
    * `discard` - The amount of space claimed on the stack at this point.
    # Panics
    When texts are not freed from the stack beforehand.
    */
    pub fn free_stack(&mut self, value: u8, discard: u16) {
        let pos = self.stack_pos;
        self.stack_pos -= u32::from(discard);
        debug_assert!(
            self.text_positions
                .range(self.stack_pos..=pos)
                .next()
                .is_none(),
            "Not freed texts"
        );
        self.copy_result(value, pos, self.stack_pos);
    }

    fn copy_result(&mut self, value: u8, pos: u32, fn_stack: u32) {
        let size = u32::from(value);
        if value > 0 {
            let from_pos = self.stack_cur.plus(pos).min(size);
            let to_pos = self.stack_cur.plus(fn_stack);
            self.database.copy_block(&from_pos, &to_pos, size);
        }
        self.stack_pos = fn_stack + size;
    }

    /**
    Write to the byte code.
    # Panics
    When that was problematic
    */
    pub fn code_put<T>(&mut self, on: u32, value: T) {
        unsafe {
            let off = self.bytecode.as_ptr().offset(on as isize) as *mut T;
            *off.as_mut().expect("code") = value;
        }
    }

    /** Remember the stack position for the current code. */
    pub fn remember_stack(&mut self, position: u16) {
        self.stack.insert(self.code_pos, position);
    }

    /**
    Add to the byte code.
    # Panics
    When that was problematic
    */
    pub fn code_add<T: std::fmt::Display>(&mut self, value: T) {
        if self.code_pos as usize + size_of::<T>() > self.bytecode.len() {
            self.bytecode
                .resize(self.code_pos as usize + size_of::<T>(), 0);
        }
        unsafe {
            let off = self.bytecode.as_ptr().offset(self.code_pos as isize) as *mut T;
            self.code_pos += u32::try_from(size_of::<T>()).expect("Problem");
            *off.as_mut().expect("code") = value;
        }
    }

    pub fn code_add_str(&mut self, value: &str) {
        self.code_add(value.len() as u8);
        if self.code_pos as usize + value.len() > self.bytecode.len() {
            self.bytecode
                .resize(self.code_pos as usize + value.len(), 0);
        }
        unsafe {
            let off = self
                .bytecode
                .as_ptr()
                .offset(self.code_pos as isize)
                .cast_mut();
            value.as_ptr().copy_to(off, value.len());
        }
        self.code_pos += value.len() as u32;
    }

    /**
    Define byte code for a function.
    # Panics
    when code cannot be output.
    */
    pub fn def_code(&mut self, def_nr: u32, data: &mut Data, show: bool, writer: &mut dyn Write) {
        let logging = !data.def(def_nr).position.file.starts_with("default/");
        let console = false; //logging;
        let mut stack = Stack::new(data.def(def_nr).variables.clone(), data, def_nr, logging);
        if stack.data.def(def_nr).code == Value::Null {
            let start = self.code_pos;
            self.add_return(&mut stack, start);
            data.definitions[def_nr as usize].code_position = start;
            data.definitions[def_nr as usize].code_length = self.code_pos - start;
            if show {
                self.dump_code(writer, def_nr, data).unwrap();
            }
            return;
        }
        for a in 0..stack.data.def(def_nr).attributes.len() as u16 {
            let n = &stack.data.def(def_nr).attributes[a as usize].name;
            let v = stack.function.var(n);
            stack.position = stack.function.claim(v, stack.position, &Context::Argument);
        }
        let start = self.code_pos;
        self.arguments = stack.position;
        stack.position += 4; // keep space for the code return address
        if console {
            println!("{} ", stack.data.def(def_nr).header(stack.data, def_nr));
            stack.data.dump(def_nr);
        }
        let mut started = HashSet::new();
        for a in stack.data.def(def_nr).variables.arguments() {
            started.insert(a);
        }
        self.generate(&stack.data.def(def_nr).code, &mut stack, true);
        let mut stack_pos = Vec::new();
        for v_nr in 0..stack.function.next_var() {
            stack_pos.push(stack.function.stack(v_nr));
        }
        data.definitions[def_nr as usize].code_position = start;
        data.definitions[def_nr as usize].code_length = self.code_pos - start;
        if let Some(v) = self.calls.get(&def_nr) {
            let old = self.code_pos;
            for pos in v.clone() {
                self.code_pos = pos + 3;
                self.code_add(start as i32);
            }
            self.code_pos = old;
        }
        for (v_nr, pos) in stack_pos.into_iter().enumerate() {
            data.definitions[def_nr as usize]
                .variables
                .set_stack(v_nr as u16, pos);
        }
        if show {
            self.dump_code(writer, def_nr, data).unwrap();
        }
    }

    /**
    Generate the byte code equivalent of a function definition
    # Panics
    On not implemented Value constructions
    */
    fn generate(&mut self, val: &Value, stack: &mut Stack, top: bool) -> Type {
        match val {
            Value::Int(value) => {
                stack.add_op("OpConstInt", self);
                self.code_add(*value);
                I32.clone()
            }
            Value::Enum(value, tp) => {
                self.types.insert(self.code_pos, *tp);
                stack.add_op("OpConstEnum", self);
                self.code_add(*value);
                Type::Enum(0)
            }
            Value::Long(value) => {
                stack.add_op("OpConstLong", self);
                self.code_add(*value);
                Type::Long
            }
            Value::Single(value) => {
                stack.add_op("OpConstSingle", self);
                self.code_add(*value);
                Type::Single
            }
            Value::Float(value) => {
                stack.add_op("OpConstFloat", self);
                self.code_add(*value);
                Type::Float
            }
            Value::Keys(_) => {
                // Should be already part of the search request
                Type::Null
            }
            Value::Boolean(value) => {
                stack.add_op(
                    if *value {
                        "OpConstTrue"
                    } else {
                        "OpConstFalse"
                    },
                    self,
                );
                Type::Boolean
            }
            Value::Text(value) => {
                stack.add_op("OpConstText", self);
                self.code_add_str(value);
                Type::Text(Vec::new())
            }
            Value::Var(v) => self.generate_var(stack, *v),
            Value::Set(v, value) => {
                self.generate_set(stack, *v, value);
                Type::Void
            }
            Value::Loop(lp) => {
                stack.add_loop(self.code_pos);
                let pos = self.code_pos;
                for v in &lp.operators {
                    self.generate(v, stack, false);
                }
                self.clear_stack(stack, 0);
                stack.add_op("OpGotoWord", self);
                self.code_add((i64::from(pos) - i64::from(self.code_pos) - 2) as i16);
                stack.end_loop(self);
                Type::Void
            }
            Value::Insert(_) => panic!("Incorrectly code Insert not rewritten"),
            Value::Break(loop_nr) => {
                let old_pos = stack.position;
                self.clear_stack(stack, *loop_nr);
                stack.add_op("OpGotoWord", self);
                stack.add_break(self.code_pos, *loop_nr);
                self.code_add(0i16); // temporary value to the end of the loop
                stack.position = old_pos;
                Type::Void
            }
            Value::Continue(loop_nr) => {
                let old_pos = stack.position;
                self.clear_stack(stack, *loop_nr);
                stack.add_op("OpGotoWord", self);
                self.code_add(
                    (i64::from(stack.get_loop(*loop_nr)) - i64::from(self.code_pos) - 2) as i16,
                );
                stack.position = old_pos;
                Type::Void
            }
            Value::If(test, t_val, f_val) => {
                self.generate(test, stack, false);
                stack.add_op("OpGotoFalseWord", self);
                let code_step = self.code_pos;
                self.code_add(0i16); // temp step
                let true_pos = self.code_pos;
                let stack_pos = stack.position;
                let tp = self.generate(t_val, stack, false);
                if **f_val == Value::Null {
                    self.code_put(code_step, (self.code_pos - true_pos) as i16); // actual step
                } else {
                    stack.add_op("OpGotoWord", self);
                    let end = self.code_pos;
                    self.code_add(0i16); // temp end
                    let false_pos = self.code_pos;
                    self.code_put(code_step, (self.code_pos - true_pos) as i16); // actual step
                    stack.position = stack_pos;
                    self.generate(f_val, stack, false);
                    self.code_put(end, (self.code_pos - false_pos) as i16); // actual end
                }
                tp
            }
            Value::Return(v) => {
                self.generate(v, stack, false);
                let return_type = &stack.data.def(stack.def_nr).returned;
                if return_type != &Type::Void {
                    let ret_nr = stack.data.type_def_nr(return_type);
                    let known = stack.data.def(ret_nr).known_type;
                    self.types.insert(self.code_pos, known);
                }
                stack.add_op("OpReturn", self);
                self.code_add(self.arguments);
                self.code_add(size(return_type, &Context::Argument) as u8);
                self.code_add(stack.position);
                Type::Void
            }
            Value::Block(bl) => self.generate_block(stack, bl, top),
            Value::Call(op, parameters) => self.generate_call(stack, op, parameters),
            Value::Null => {
                // Ignore, in use as the code on an else clause without code.
                Type::Void
            }
            Value::Drop(val) => {
                self.generate(val, stack, false);
                // get all variables of the current scope.
                let size = stack.size_code(val);
                if size > 0 {
                    stack.add_op("OpFreeStack", self);
                    self.code_add(0u8);
                    self.code_add(size);
                }
                stack.position -= size;
                Type::Void
            }
            Value::Iter(_, _, _) => {
                panic!("Should have rewritten {val:?}");
            }
        }
    }

    fn generate_set(&mut self, stack: &mut Stack, v: u16, value: &Value) {
        self.vars.insert(self.code_pos, v);
        let pos = stack.function.stack(v);
        if pos == u16::MAX {
            stack.function.claim(v, stack.position, &Context::Variable);
            if matches!(*stack.function.tp(v), Type::Text(_)) {
                stack.add_op("OpText", self);
                stack.position += size_str() as u16;
                if let Value::Text(s) = value {
                    if !s.is_empty() {
                        self.set_var(stack, v, value);
                    }
                } else {
                    self.set_var(stack, v, value);
                }
            } else if matches!(stack.function.tp(v), Type::Reference(_, _)) && *value == Value::Null
            {
                if let Type::Reference(_, dep) = stack.function.tp(v).clone() {
                    if dep.is_empty() {
                        stack.add_op("OpConvRefFromNull", self);
                    } else {
                        stack.add_op("OpCreateRef", self);
                        self.code_add(dep[0]);
                    }
                }
            } else if matches!(stack.function.tp(v), Type::Vector(_, _)) && *value == Value::Null {
                if let Type::Vector(elm_tp, dep) = stack.function.tp(v).clone() {
                    if dep.is_empty() {
                        // TODO move this convoluted implementation to a new operator.
                        stack.add_op("OpConvRefFromNull", self);
                        stack.add_op("OpDatabase", self);
                        self.code_add(size_of::<DbRef>() as u16);
                        let name = format!("main_vector<{}>", elm_tp.name(stack.data));
                        let known = stack.data.def_name(&name).known_type;
                        debug_assert_ne!(known, u16::MAX, "Incomplete type {name}");
                        self.code_add(known);
                        stack.add_op("OpVarRef", self);
                        self.code_add(size_of::<DbRef>() as u16);
                        stack.add_op("OpConstInt", self);
                        self.code_add(0);
                        stack.add_op("OpSetInt", self);
                        self.code_add(4u16);
                        stack.add_op("OpCreateRef", self);
                        self.code_add(size_of::<DbRef>() as u16);
                        stack.add_op("OpConstInt", self);
                        self.code_add(4);
                        stack.add_op("OpSetByte", self);
                        self.code_add(4u16);
                        self.code_add(0u16);
                    } else {
                        stack.add_op("OpCreateRef", self);
                        self.code_add(dep[0]);
                    }
                }
            } else {
                self.generate(value, stack, false);
            }
        } else {
            if matches!(stack.function.tp(v), Type::Text(_)) {
                let var_pos = stack.position - pos;
                stack.add_op("OpClearText", self);
                self.code_add(var_pos);
            }
            self.set_var(stack, v, value);
        }
    }

    fn clear_stack(&mut self, stack: &mut Stack, loop_nr: u16) {
        let loop_pos = stack.loop_position(loop_nr);
        if stack.position > loop_pos {
            stack.add_op("OpFreeStack", self);
            self.code_add(0u8);
            self.code_add(stack.position - loop_pos);
            stack.position = loop_pos;
        }
    }

    fn generate_call(&mut self, stack: &mut Stack, op: &u32, parameters: &[Value]) -> Type {
        let mut tps = Vec::new();
        let mut last = 0;
        let mut was_stack = u16::MAX;
        assert!(
            parameters.len() >= stack.data.def(*op).attributes.len(),
            "Too few parameters on {}",
            stack.data.def(*op).name
        );
        for (a_nr, a) in stack.data.def(*op).attributes.iter().enumerate() {
            if a.mutable {
                tps.push(self.generate(&parameters[a_nr], stack, false));
            }
        }
        match &stack.data.def(*op).name as &str {
            "OpGetRecord" => {
                was_stack = stack.position;
                self.gather_key(stack, &parameters, 2, &mut tps);
            }
            "OpStart" => {
                was_stack = stack.position + 4 - size_ref() as u16;
                self.gather_key(stack, &parameters, 2, &mut tps);
            }
            "OpNext" => {
                was_stack = stack.position;
                self.gather_key(stack, &parameters, 3, &mut tps);
            }
            "OpIterate" => {
                was_stack = stack.position + 8 - size_ref() as u16;
                if let Value::Int(parameter_length) = parameters[4] {
                    self.gather_key(stack, &parameters, 4, &mut tps);
                    self.gather_key(stack, &parameters, 5 + parameter_length, &mut tps);
                }
            }
            _ => (),
        }
        if !parameters.is_empty()
            && let Value::Int(n) = parameters[parameters.len() - 1]
        {
            last = n as u16;
        }
        let name = stack.data.def(*op).name.clone();
        if stack.data.def(*op).is_operator() {
            let before_stack = stack.position;
            self.remember_stack(stack.position);
            let code = self.code_pos;
            self.code_add(stack.data.def(*op).op_code as u8);
            stack.operator(*op);
            if was_stack != u16::MAX {
                stack.position = was_stack;
            }
            for (a_nr, a) in stack.data.def(*op).attributes.iter().enumerate() {
                if a.mutable {
                    continue;
                }
                self.add_const(&a.typedef, &parameters[a_nr], stack, before_stack);
            }
            self.op_type(*op, &tps, last, code, stack)
        } else if self.library_names.contains_key(&name) {
            stack.add_op("OpStaticCall", self);
            self.code_add(self.library_names[&name]);
            if !parameters.is_empty() {
                self.gather_key(stack, &parameters, 0, &mut tps);
            }
            for a in &stack.data.def(*op).attributes {
                stack.position -= size(&a.typedef, &Context::Argument);
            }
            // add the result to the stack
            stack.position += size(&stack.data.def(*op).returned, &Context::Argument);
            stack.data.def(*op).returned.clone()
        } else {
            if !self.calls.contains_key(op) {
                self.calls.insert(*op, vec![]);
            }
            self.calls.get_mut(op).unwrap().push(self.code_pos);
            stack.add_op("OpCall", self);
            self.code_add(0u16);
            self.code_add(stack.data.def(*op).code_position as i32);
            // remove the arguments that are already on the stack
            for a in &stack.data.def(*op).attributes {
                stack.position -= size(&a.typedef, &Context::Argument);
            }
            // add the result to the stack
            stack.position += size(&stack.data.def(*op).returned, &Context::Argument);
            stack.data.def(*op).returned.clone()
        }
    }

    fn gather_key(
        &mut self,
        stack: &mut Stack,
        parameters: &&[Value],
        from: i32,
        tps: &mut Vec<Type>,
    ) {
        let no_keys = if let Value::Int(v) = &parameters[from as usize] {
            *v
        } else {
            0
        };
        for k in 0..no_keys {
            tps.push(self.generate(&parameters[(no_keys + from - k) as usize], stack, false));
        }
    }

    fn op_type(&mut self, op: u32, tps: &[Type], last: u16, code: u32, stack: &mut Stack) -> Type {
        match &stack.data.def(op).name as &str {
            "OpDatabase" | "OpAppend" | "OpConvEnumFromNull" | "OpCastEnumFromInt"
            | "OpCastEnumFromText" | "OpGetField" => {
                self.types.insert(code, last);
            }
            "OpGetVector" | "OpInsertVector" | "OpAppendVector" => {
                if let Type::Vector(v, _) = &tps[0] {
                    self.types
                        .insert(code, stack.data.def(stack.data.type_def_nr(v)).known_type);
                    return *v.clone();
                }
            }
            "OpGetHash" => {
                if let Type::Hash(v, _, link) = &tps[0] {
                    return Type::Reference(*v, link.clone());
                }
            }
            "OpGetIndex" => {
                if let Type::Index(v, _, link) = &tps[0] {
                    return Type::Reference(*v, link.clone());
                }
            }
            "OpGetSpacial" => {
                if let Type::Spacial(v, _, link) = &tps[0] {
                    return Type::Reference(*v, link.clone());
                }
            }
            "OpVarEnum" => {
                self.insert_types(tps[0].clone(), code, stack);
            }
            _ => (),
        }
        stack.data.def(op).returned.clone()
    }

    fn insert_types(&mut self, tp: Type, code: u32, stack: &Stack) -> Type {
        match tp {
            Type::Enum(t) => {
                self.types.insert(code, stack.data.def(t).known_type);
            }
            Type::Reference(t, _) => {
                if t < u32::from(u16::MAX) {
                    self.types.insert(code, stack.data.def(t).known_type);
                }
            }
            _ => (),
        }
        tp
    }

    fn generate_var(&mut self, stack: &mut Stack, variable: u16) -> Type {
        assert!(
            stack.function.stack(variable) <= stack.position,
            "Incorrect var {}[{}] versus {} on {}",
            stack.function.name(variable),
            stack.function.stack(variable),
            stack.position,
            stack.data.def(stack.def_nr).name
        );
        let var_pos = stack.position - stack.function.stack(variable);
        let argument = stack.function.is_argument(variable);
        let code = self.code_pos;
        self.vars.insert(code, variable);
        match stack.function.tp(variable) {
            Type::Integer(_, _) | Type::Character => stack.add_op("OpVarInt", self),
            Type::RefVar(_) => stack.add_op("OpVarRef", self),
            Type::Enum(_) => stack.add_op("OpVarEnum", self),
            Type::Boolean => stack.add_op("OpVarBool", self),
            Type::Long => stack.add_op("OpVarLong", self),
            Type::Single => stack.add_op("OpVarSingle", self),
            Type::Float => stack.add_op("OpVarFloat", self),
            Type::Text(_) => stack.add_op(if argument { "OpArgText" } else { "OpVarText" }, self),
            Type::Vector(tp, _) => {
                let typedef: &Type = tp;
                let known = if matches!(typedef, Type::Unknown(_)) {
                    u16::MAX
                } else if matches!(typedef, Type::Text(_)) {
                    self.database.vector(5)
                } else {
                    let name = tp.name(stack.data);
                    self.database.vector(self.database.name(&name))
                };
                if known != u16::MAX {
                    self.types.insert(self.code_pos, known);
                }
                stack.add_op("OpVarVector", self);
            }
            Type::Reference(c, _) => {
                self.types
                    .insert(self.code_pos, stack.data.def(*c).known_type);
                stack.add_op("OpVarRef", self);
            }
            _ => panic!(
                "Unknown var '{}' type {} at {}",
                stack.function.name(variable),
                stack.function.tp(variable).name(stack.data),
                stack.data.def(stack.def_nr).position
            ),
        }
        self.code_add(var_pos);
        if let Type::RefVar(tp) = stack.function.tp(variable) {
            match &**tp {
                Type::Integer(_, _) | Type::Character => stack.add_op("OpGetInt", self),
                Type::Long => stack.add_op("OpGetLong", self),
                Type::Single => stack.add_op("OpGetSingle", self),
                Type::Float => stack.add_op("OpGetFloat", self),
                Type::Enum(_) => stack.add_op("OpGetByte", self),
                Type::Text(_) => stack.add_op("OpGetRefText", self),
                Type::Vector(_, _) | Type::Reference(_, _) => stack.add_op("OpGetDbRef", self),
                _ => panic!("Unknown referenced variable type: {tp}"),
            }
            self.code_add(0u16);
        }
        self.insert_types(stack.function.tp(variable).clone(), code, stack)
    }

    fn generate_block(&mut self, stack: &mut Stack, block: &Block, top: bool) -> Type {
        if block.operators.is_empty() {
            return Type::Void;
        }
        let to = stack.position;
        let mut tp = Type::Void;
        let mut return_expr = 0;
        let mut has_return = false;
        for v in &block.operators {
            let s_pos = self.stack_pos;
            if let Value::Return(expr) = v {
                has_return = true;
                if return_expr == 0 {
                    return_expr = s_pos;
                    self.generate(expr, stack, false);
                }
                self.add_return(stack, return_expr);
            } else {
                has_return = false;
            }
            return_expr = 0;
            tp = self.generate(v, stack, false);
            if self.stack_pos > s_pos && !matches!(v, Value::Set(_, _)) {
                // Normal expressions do not claim stack space (because of Value::Drop)
                // So if there is data left it should be a return expression.
                return_expr = s_pos;
            }
        }
        if top {
            if !has_return {
                self.add_return(
                    stack,
                    if return_expr > 0 {
                        return_expr
                    } else {
                        self.code_pos
                    },
                );
            }
        } else {
            let size = size(&block.result, &Context::Argument);
            let after = to + size;
            if stack.position > after {
                stack.add_op("OpFreeStack", self);
                self.code_add(size as u8);
                self.code_add(stack.position - to);
            }
            stack.position = after;
        }
        tp
    }

    fn add_return(&mut self, stack: &mut Stack, code: u32) {
        let return_type = &stack.data.def(stack.def_nr).returned;
        stack.add_op("OpReturn", self);
        self.code_add(self.arguments);
        self.code_add(size(return_type, &Context::Argument) as u8);
        self.code_add(stack.position);
        if return_type != &Type::Void {
            self.types.insert(code, self.known_type(return_type, stack));
        }
    }

    fn known_type(&self, tp: &Type, stack: &Stack) -> u16 {
        if let Type::Reference(c, _) = tp {
            stack.data.def(*c).known_type
        } else {
            self.database.name(&tp.name(stack.data))
        }
    }

    fn add_const(&mut self, tp: &Type, p: &Value, stack: &Stack, before_stack: u16) {
        match tp {
            Type::Integer(0, 255) => {
                if let Value::Int(nr) = p {
                    self.code_add(*nr as u8);
                }
            }
            Type::Integer(-128, 127) => {
                if let Value::Int(nr) = p {
                    self.code_add(*nr as i8);
                }
            }
            Type::Integer(0, 65535) => {
                if let Value::Int(nr) = p {
                    self.code_add(*nr as u16);
                } else if let Value::Var(v) = p {
                    let r = stack.function.stack(*v);
                    self.code_add(before_stack - r);
                }
            }
            Type::Integer(-32768, 32767) => {
                if let Value::Int(nr) = p {
                    self.code_add(*nr as i16);
                }
            }
            Type::Integer(_, _) => {
                if let Value::Int(nr) = p {
                    self.code_add(*nr);
                }
            }
            Type::Enum(_) => {
                if let Value::Enum(nr, _) = p {
                    self.code_add(*nr);
                }
            }
            Type::Boolean => {
                if let Value::Boolean(v) = p {
                    self.code_add(u8::from(*v));
                }
            }
            Type::Text(_) => {
                if let Value::Text(s) = p {
                    self.code_add_str(s);
                }
            }
            Type::Long => {
                if let Value::Long(val) = p {
                    self.code_add(*val);
                }
            }
            Type::Float => {
                if let Value::Float(val) = p {
                    self.code_add(*val);
                }
            }
            Type::Single => {
                if let Value::Single(val) = p {
                    self.code_add(*val);
                }
            }
            Type::Keys => {
                if let Value::Keys(keys) = p {
                    self.code_add(keys.len() as u8);
                    for k in keys {
                        self.code_add(k.type_nr);
                        self.code_add(k.position);
                    }
                }
            }
            _ => {}
        }
    }

    fn set_var(&mut self, stack: &mut Stack, var: u16, value: &Value) {
        if let Type::RefVar(tp) = stack.function.tp(var).clone() {
            let var_pos = stack.position - stack.function.stack(var);
            stack.add_op("OpVarRef", self);
            self.code_add(var_pos);
            self.generate(value, stack, false);
            match *tp {
                Type::Integer(_, _) | Type::Character => stack.add_op("OpSetInt", self),
                Type::Long => stack.add_op("OpSetLong", self),
                Type::Single => stack.add_op("OpSetSingle", self),
                Type::Float => stack.add_op("OpSetFloat", self),
                Type::Text(_) => stack.add_op("OpAppendRefText", self),
                Type::Enum(_) => stack.add_op("OpSetByte", self),
                Type::Vector(_, _) | Type::Reference(_, _) => stack.add_op("OpSetDbRef", self),
                _ => panic!("Unknown reference variable type"),
            }
            self.code_add(0u16);
            return;
        }
        self.generate(value, stack, false);
        let var_pos = stack.position - stack.function.stack(var);
        match stack.function.tp(var) {
            Type::Integer(_, _) | Type::Character => stack.add_op("OpPutInt", self),
            Type::Enum(_) => stack.add_op("OpPutEnum", self),
            Type::Boolean => stack.add_op("OpPutBool", self),
            Type::Long => stack.add_op("OpPutLong", self),
            Type::Single => stack.add_op("OpPutSingle", self),
            Type::Float => stack.add_op("OpPutFloat", self),
            Type::Text(_) => stack.add_op("OpAppendText", self),
            Type::Vector(_, _) | Type::Reference(_, _) => stack.add_op("OpPutRef", self),
            _ => panic!(
                "Unknown var {} type {} at {}",
                stack.function.name(var),
                stack.function.tp(var).name(stack.data),
                stack.data.def(stack.def_nr).position
            ),
        }
        self.code_add(var_pos);
    }

    /**
    Print the byte-code
    # Panics
    When unknown operators are encountered in the byte-code.
    */
    pub fn print_code(&mut self, d_nr: u32, data: &Data) {
        let mut buf = Vec::new();
        self.dump_code(&mut buf, d_nr, data).unwrap();
        println!("{}", String::from_utf8(buf).unwrap());
    }

    /**
    Write the byte-code generated for the given function definition.
    # Errors
    When the writer had problems.
    # Panics
    When unknown operators are encountered in the byte-code.
    */
    pub fn dump_code(&mut self, f: &mut dyn Write, d_nr: u32, data: &Data) -> Result<(), Error> {
        write!(f, "{}(", data.def(d_nr).name)?;
        let mut stack_pos = 4;
        for a_nr in 0..data.attributes(d_nr) {
            if a_nr > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{}: {}[{stack_pos}]",
                data.attr_name(d_nr, a_nr),
                data.attr_type(d_nr, a_nr)
                    .show(data, &data.def(d_nr).variables)
            )?;
            stack_pos += size(&data.attr_type(d_nr, a_nr), &Context::Argument);
        }
        write!(f, ")")?;
        write!(f, " [{}]", data.def(d_nr).code_position)?;
        if data.def(d_nr).returned != Type::Void {
            write!(
                f,
                " -> {}",
                data.def(d_nr)
                    .returned
                    .show(data, &data.def(d_nr).variables)
            )?;
        }
        writeln!(f)?;
        let start_pos = data.def(d_nr).code_position;
        self.code_pos = start_pos;
        writeln!(
            f,
            "{:4}[{stack_pos}]: return-address",
            self.code_pos - start_pos
        )?;
        while self.code_pos < start_pos + data.def(d_nr).code_length {
            let p = self.code_pos;
            let op = *self.code::<u8>();
            assert!(
                data.has_op(op),
                "Unknown operator {op} in byte_code of {}",
                data.def(d_nr).name
            );
            let def = data.operator(op);
            write!(f, "{:4}", p - start_pos)?;
            if self.stack.contains_key(&p) {
                write!(f, "[{}]", self.stack[&p] + 4)?;
            }
            write!(f, ": {}(", &def.name[2..])?;
            for (a_nr, a) in def.attributes.iter().enumerate() {
                if a_nr > 0 {
                    write!(f, ", ")?;
                }
                if (def.name == "OpGotoFalseWord" || def.name == "OpGotoWord") && a_nr == 0 {
                    let to =
                        i64::from(p) + 3 + i64::from(*self.code::<i16>()) - i64::from(start_pos);
                    write!(f, "jump={to}")?;
                } else if def.name == "OpStaticCall" {
                    let v = *self.code::<u16>();
                    for (n, val) in &self.library_names {
                        if *val == v {
                            write!(f, "{n}")?;
                        }
                    }
                } else if a_nr == 0
                    && !a.mutable
                    && a.name == "pos"
                    && a.typedef == Type::Integer(0, 65535)
                    && self.stack.contains_key(&p)
                {
                    let pos = i32::from(*self.code::<u16>());
                    write!(f, "var[{}]", i32::from(self.stack[&p]) - pos + 4,)?;
                } else if a.mutable {
                    write!(
                        f,
                        "{}: {}",
                        a.name,
                        a.typedef.show(data, &data.def(d_nr).variables)
                    )?;
                } else {
                    write!(f, "{}={}", a.name, self.dump_attribute(a))?;
                }
            }
            write!(f, ")")?;
            if def.returned != Type::Void {
                write!(
                    f,
                    " -> {}",
                    def.returned.show(data, &data.def(d_nr).variables)
                )?;
            }
            if let Some(t) = self.types.get(&p) {
                write!(f, " type={}[{t:}]", self.database.show_type(*t, false))?;
            }
            if let Some(v) = self.vars.get(&p) {
                let vars = &data.def(d_nr).variables;
                write!(
                    f,
                    " var={}[{}]:{}",
                    vars.name(*v),
                    vars.stack(*v) + 4,
                    vars.tp(*v).show(data, vars)
                )?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;
        Ok(())
    }

    /**
    Output the given operator argument to a writer
    # Errors
    When the writer had problems.
    */
    fn dump_attribute(&mut self, a: &Attribute) -> String {
        match a.typedef {
            Type::Integer(min, max) if i64::from(max) - i64::from(min) <= 256 && min == 0 => {
                format!("{}", i32::from(*self.code::<u8>()))
            }
            Type::Integer(min, max) if i64::from(max) - i64::from(min) <= 65536 && min == 0 => {
                format!("{}", i32::from(*self.code::<u16>()))
            }
            Type::Integer(min, max) if i64::from(max) - i64::from(min) <= 256 => {
                format!("{}", i32::from(*self.code::<i8>()))
            }
            Type::Integer(min, max) if i64::from(max) - i64::from(min) <= 65536 => {
                format!("{}", i32::from(*self.code::<i16>()))
            }
            Type::Integer(_, _) => format!("{}", *self.code::<i32>()),
            Type::Boolean => format!("{}", *self.code::<u8>() == 1),
            Type::Enum(_) => format!("{}", *self.code::<u8>()),
            Type::Long => format!("{}", *self.code::<i64>()),
            Type::Single => format!("{}", *self.code::<f32>()),
            Type::Float => format!("{}", *self.code::<f64>()),
            Type::Text(_) => format!("\"{}\"", self.code_str()),
            Type::Character => format!("{}", *self.code::<char>()),
            Type::Keys => {
                let len = *self.code::<u8>();
                let mut keys = Vec::new();
                for _ in 0..len {
                    keys.push(Key {
                        type_nr: *self.code::<i8>(),
                        position: *self.code::<u16>(),
                    });
                }
                format!("{keys:?}")
            }
            _ => "unknown".to_string(),
        }
    }

    /**
    Execute a function inside the `byte_code` with logging each step.
    # Errors
    When the log cannot be written.
    # Panics
    On too many steps or when the stack or claimed structures are not correctly cleared afterward.
    */
    pub fn execute_log(
        &mut self,
        log: &mut dyn Write,
        name: &str,
        data: &Data,
    ) -> Result<(), Error> {
        writeln!(log, "Execute {name}:")?;
        let d_nr = data.def_nr(name);
        assert_ne!(d_nr, u32::MAX, "Unknown routine {name}");
        self.code_pos = data.def(d_nr).code_position;
        self.def_pos = self.code_pos;
        // Write the return address of the main function but do not override the record size.
        self.stack_pos = 4;
        self.put_stack(u32::MAX);
        // TODO Allow command line parameters on main functions
        let mut step = 0;
        while self.code_pos < self.bytecode.len() as u32 {
            let code = self.code_pos;
            let op = *self.code::<u8>();
            self.log_step(log, op, data)?;
            OPERATORS[op as usize](self);
            self.log_result(log, op, code, data)?;
            step += 1;
            assert!(step < 100_000, "Too many operations");
            if self.code_pos == u32::MAX {
                // TODO Validate that all databases & String values are also cleared.
                assert_eq!(self.stack_pos, 4, "Stack not correctly cleared");
                // Free the stack
                self.database.allocations[0].free = true;
                for (s_nr, s) in self.database.allocations.iter().enumerate() {
                    assert!(s.free, "Database {s_nr} not correctly freed");
                }
                writeln!(log, "Finished")?;
                return Ok(());
            }
        }
        Ok(())
    }

    /**
    Execute a function inside the `byte_code`.
    # Panics
    When too many steps were taken, this might indicate an unending loop.
    */
    pub fn execute(&mut self, d_nr: u32, data: &Data) {
        let pos = data.def(d_nr).code_position;
        self.code_pos = pos;
        self.stack_pos = 4;
        // Write the return address of the main function.
        self.put_stack(u32::MAX);
        let mut step = 0;
        // TODO Allow command line parameters on main functions
        while self.code_pos < self.bytecode.len() as u32 {
            let op = *self.code::<u8>();
            OPERATORS[op as usize](self);
            step += 1;
            assert!(step < 10_000_000, "Too many operations");
            if self.code_pos == u32::MAX {
                return;
            }
        }
    }

    fn log_step(&mut self, log: &mut dyn Write, op: u8, data: &Data) -> Result<u8, Error> {
        let cur = self.code_pos;
        let stack = self.stack_pos;
        assert!(data.has_op(op), "Unknown operator {op}");
        let def = data.operator(op);
        let minus = if cur > self.def_pos { self.def_pos } else { 0 };
        write!(
            log,
            "{:5}:[{}] {}(",
            cur - minus - 1,
            self.stack_pos,
            &def.name[2..]
        )?;
        // Inverse the order of reading the attributes correctly from the stack.
        let mut attr = BTreeMap::new();
        for (a_nr, a) in def.attributes.iter().enumerate() {
            if !a.mutable {
                if def.name == "OpStaticCall" {
                    let nr = *self.code::<i16>();
                    write!(log, "{})", FUNCTIONS[nr as usize].0)?;
                    self.code_pos = cur;
                    self.stack_pos = stack;
                    return Ok(op);
                } else if def.name == "OpReturn" && a_nr == 0 {
                    self.return_attr(&mut attr, a_nr);
                } else if def.name.starts_with("OpGoto") && a_nr == 0 {
                    let to = i64::from(cur) + 2 + i64::from(*self.code::<i16>()) - i64::from(minus);
                    attr.insert(a_nr, format!("jump={to}"));
                } else if a_nr == 0 && a.name == "pos" && a.typedef == Type::Integer(0, 65535) {
                    let pos = *self.code::<u16>();
                    assert!(
                        u32::from(pos) <= self.stack_pos,
                        "Variable {pos} outside stack {}",
                        self.stack_pos
                    );
                    attr.insert(a_nr, format!("var[{}]", self.stack_pos - u32::from(pos)));
                } else {
                    attr.insert(a_nr, format!("{}={}", a.name, self.dump_attribute(a)));
                }
            }
        }
        if def.name == "OpGetRecord" {
            let db_tp = u16::from_str(&attr[&1][6..]).unwrap_or(0);
            let no_keys = u8::from_str(&attr[&2][8..]).unwrap_or(0) as usize;
            let keys = self.database.get_keys(db_tp);
            for (idx, key) in keys.iter().enumerate() {
                if idx >= no_keys {
                    break;
                }
                let v = match key {
                    0 => self.dump_stack(&I32, u32::MAX, data),
                    1 => self.dump_stack(&Type::Long, u32::MAX, data),
                    2 => self.dump_stack(&Type::Single, u32::MAX, data),
                    3 => self.dump_stack(&Type::Float, u32::MAX, data),
                    4 => self.dump_stack(&Type::Boolean, u32::MAX, data),
                    5 => self.dump_stack(&Type::Text(Vec::new()), u32::MAX, data),
                    6 => self.dump_stack(&Type::Character, u32::MAX, data),
                    _ => self.dump_stack(&Type::Enum(u32::MAX), u32::from(*key), data),
                };
                attr.insert(idx + 3, format!("key{}={v}[{}]", idx + 1, self.stack_pos));
            }
        }
        for a_nr in (0..def.attributes.len()).rev() {
            let a = &def.attributes[a_nr];
            if a.mutable {
                let v = self.dump_stack(&a.typedef, u32::MAX, data);
                attr.insert(a_nr, format!("{}={v}[{}]", a.name, self.stack_pos));
            }
        }
        // Reverse the argument order again for output.
        for (nr, (_, s)) in attr.iter().enumerate() {
            if nr > 0 {
                write!(log, ", ")?;
            }
            write!(log, "{s}")?;
        }
        write!(log, ")")?;
        self.code_pos = cur;
        self.stack_pos = stack;
        Ok(op)
    }

    fn return_attr(&mut self, attr: &mut BTreeMap<usize, String>, a_nr: usize) {
        let cur_st = self.stack_pos;
        let ret = u32::from(*self.code::<u16>());
        let cur_code = self.code_pos;
        self.code::<u8>();
        let discard = *self.code::<u16>();
        self.stack_pos -= u32::from(discard);
        self.stack_pos += ret;
        let st = self.stack_pos;
        let addr = *self.get_var::<u32>(0);
        self.stack_pos = cur_st;
        self.code_pos = cur_code;
        attr.insert(a_nr, format!("ret={addr}[{st}]"));
    }

    fn log_result(
        &mut self,
        log: &mut dyn Write,
        op: u8,
        code: u32,
        data: &Data,
    ) -> Result<(), Error> {
        let stack = self.stack_pos;
        let def = data.operator(op);
        if def.name == "OpReturn" {
            writeln!(log, "{}", self.dump_result(code))?;
            return Ok(());
        }
        if def.returned == Type::Void {
            writeln!(log)?;
            return Ok(());
        }
        let v = self.dump_stack(&def.returned, code, data);
        writeln!(log, " -> {v}[{}]", self.stack_pos)?;
        self.stack_pos = stack;
        Ok(())
    }

    fn dump_result(&mut self, code: u32) -> String {
        if let Some(k) = self.types.get(&code) {
            let stack = self.stack_pos;
            let known = *k;
            let res = match known {
                0 => format!("{}", *self.get_stack::<i32>()), // integer
                1 => format!("{}", *self.get_stack::<i64>()), // long
                2 => format!("{}", *self.get_stack::<f32>()), // single
                3 => format!("{}", *self.get_stack::<f64>()), // float
                4 => format!("{}", *self.get_stack::<u8>() == 1), // boolean
                5 => format!("\"{}\"", self.string().str().replace('"', "\\\"")), // text
                6 => format!("{}", *self.get_stack::<char>()), // character
                _ => match &self.database.types[known as usize].parts {
                    Parts::Enum(_) => {
                        let val = *self.get_stack::<u8>();
                        format!("{}({val})", self.database.enum_val(known, val))
                    }
                    Parts::Struct(_) | Parts::Vector(_) => {
                        let val = *self.get_stack::<DbRef>();
                        let mut res = format!("ref({},{},{})=", val.store_nr, val.rec, val.pos);
                        self.database.show(&mut res, &val, known, false);
                        res
                    }
                    _ => String::new(),
                },
            };
            let after = self.stack_pos;
            self.stack_pos = stack;
            format!(" -> {res}[{after}]")
        } else {
            String::new()
        }
    }

    // TODO dump of data structures with only the top level records, limited array sizes
    fn dump_stack(&mut self, typedef: &Type, code: u32, data: &Data) -> String {
        match typedef {
            Type::Integer(_, _) => format!("{}", *self.get_stack::<i32>()),
            Type::Character => {
                let c = *self.get_stack::<char>();
                if c as u32 == 0 {
                    "null".to_string()
                } else {
                    format!("{c}")
                }
            }
            Type::Enum(tp) => {
                if code == u32::MAX {
                    format!("{}", *self.get_stack::<u8>())
                } else {
                    let known = if self.types.contains_key(&code) {
                        self.types[&code]
                    } else if *tp == u32::MAX {
                        code as u16
                    } else {
                        data.def(*tp).known_type
                    };
                    let val = *self.get_stack::<u8>();
                    format!("{}({val})", self.database.enum_val(known, val))
                }
            }
            Type::Long => format!("{}", *self.get_stack::<i64>()),
            Type::Single => format!("{}", *self.get_stack::<f32>()),
            Type::Float => format!("{}", *self.get_stack::<f64>()),
            Type::Text(_) => format!("\"{}\"", self.string().str().replace('"', "\\\"")),
            Type::Boolean => format!("{}", *self.get_stack::<u8>() == 1),
            Type::Reference(tp, _) => {
                let known = if self.types.contains_key(&code) {
                    self.types[&code]
                } else {
                    data.def(*tp).known_type
                };
                let val = *self.get_stack::<DbRef>();
                if known == u16::MAX || val.store_nr as usize >= self.database.allocations.len() {
                    return format!("ref({},{},{})", val.store_nr, val.rec, val.pos);
                }
                let mut res = format!("ref({},{},{})=", val.store_nr, val.rec, val.pos);
                self.database.show(&mut res, &val, known, false);
                res
            }
            Type::Vector(_, _) => {
                let val = *self.get_stack::<DbRef>();
                let known = if self.types.contains_key(&code) {
                    self.types[&code]
                } else {
                    return format!("ref({},{},{})", val.store_nr, val.rec, val.pos);
                };
                let mut res = format!("ref({},{},{})=", val.store_nr, val.rec, val.pos);
                self.database.show(&mut res, &val, known, false);
                res
            }
            _ => "unknown".to_string(),
        }
    }
}

#[inline]
#[must_use]
pub fn size_ptr() -> u32 {
    size_of::<Str>() as u32
}

#[inline]
#[must_use]
pub fn size_str() -> u32 {
    size_of::<String>() as u32
}

#[inline]
#[must_use]
pub fn size_ref() -> u32 {
    size_of::<DbRef>() as u32
}
