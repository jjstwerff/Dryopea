// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::float_cmp)]
#![allow(dead_code)]

use crate::database::Str;
use crate::store::Store;
use std::cmp::Ordering;
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Debug, Clone, PartialEq)]
pub struct Key {
    pub type_nr: u16,
    pub descending: bool,
    pub field_pos: u16,
}

#[derive(Clone)]
pub enum Content {
    Long(i64),
    Float(f64),
    Single(f32),
    Str(Str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct DbRef {
    pub store_nr: u16,
    pub rec: u32,
    pub pos: u32,
}

impl DbRef {
    #[must_use]
    pub fn plus(&self, pos: u32) -> DbRef {
        DbRef {
            store_nr: self.store_nr,
            rec: self.rec,
            pos: self.pos + pos,
        }
    }

    #[must_use]
    pub fn min(&self, size: u32) -> DbRef {
        DbRef {
            store_nr: self.store_nr,
            rec: self.rec,
            pos: self.pos - size,
        }
    }

    pub fn push<T>(&mut self, stores: &mut [Store], value: T) {
        *stores[self.store_nr as usize].addr_mut::<T>(self.rec, self.pos) = value;
        self.pos += size_of::<T>() as u32;
    }
}

#[inline]
fn single_cmp(v1: f32, v2: f32) -> Ordering {
    if v1 == v2 {
        Ordering::Equal
    } else if v1 < v2 {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}

#[inline]
fn float_cmp(v1: f64, v2: f64) -> Ordering {
    if v1 == v2 {
        Ordering::Equal
    } else if v1 < v2 {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}

#[must_use]
pub fn store<'a>(r: &DbRef, stores: &'a [Store]) -> &'a Store {
    &stores[r.store_nr as usize]
}

#[must_use]
pub fn mut_store<'a>(r: &DbRef, stores: &'a mut [Store]) -> &'a mut Store {
    &mut stores[r.store_nr as usize]
}

#[must_use]
pub fn compare(rec1: &DbRef, rec2: &DbRef, stores: &[Store], keys: &[Key]) -> Ordering {
    for key in keys {
        let pos1 = rec1.pos + u32::from(key.field_pos);
        let pos2 = rec2.pos + u32::from(key.field_pos);
        let c = compare_ref(rec1, rec2, stores, key, pos1, pos2);
        if c != Ordering::Equal {
            return c;
        }
    }
    Ordering::Equal
}

#[must_use]
pub fn key_compare(
    key: &[Content],
    k_len: u8,
    rec: &DbRef,
    stores: &[Store],
    keys: &[Key],
) -> Ordering {
    for k_nr in 0..k_len {
        let k = &keys[k_nr as usize];
        let pos_r = rec.pos + u32::from(k.field_pos);
        let c = compare_key(&key[k_nr as usize], rec, stores, k, pos_r);
        if c != Ordering::Equal {
            return c;
        }
    }
    Ordering::Equal
}

fn compare_key(k: &Content, record: &DbRef, stores: &[Store], key: &Key, pos: u32) -> Ordering {
    let s = store(record, stores);
    let c = match (k, key.type_nr) {
        (Content::Long(v), 0) => v.cmp(&(i64::from(s.get_int(record.rec, pos)))),
        (Content::Long(v), 1) => v.cmp(&s.get_long(record.rec, pos)),
        (Content::Single(v), 2) => single_cmp(*v, s.get_single(record.rec, pos)),
        (Content::Float(v), 3) => float_cmp(*v, s.get_float(record.rec, pos)),
        (Content::Str(v), 5) => v.str().cmp(s.get_str(s.get_int(record.rec, pos) as u32)),
        (Content::Long(v), _) => v.cmp(&i64::from(s.get_byte(record.rec, pos, 0))),
        _ => panic!("Undefined compare"),
    };
    if key.descending { c.reverse() } else { c }
}

fn compare_ref(r1: &DbRef, r2: &DbRef, stores: &[Store], key: &Key, p1: u32, p2: u32) -> Ordering {
    let s = store(r1, stores);
    let c = match key.type_nr {
        0 => s.get_int(r1.rec, p1).cmp(&s.get_int(r2.rec, p2)),
        1 => s.get_long(r1.rec, p1).cmp(&s.get_long(r2.rec, p2)),
        2 => single_cmp(s.get_single(r1.rec, p1), s.get_single(r2.rec, p2)),
        3 => float_cmp(s.get_float(r1.rec, p1), s.get_float(r2.rec, p2)),
        5 => s
            .get_str(s.get_int(r1.rec, p1) as u32)
            .cmp(s.get_str(s.get_int(r2.rec, p2) as u32)),
        _ => s.get_byte(r1.rec, p1, 0).cmp(&s.get_byte(r2.rec, p2, 0)),
    };
    if key.descending { c.reverse() } else { c }
}

#[must_use]
pub fn get_key(record: &DbRef, stores: &[Store], keys: &[Key]) -> Vec<Content> {
    let mut result = Vec::new();
    for k in keys {
        let p = record.pos + u32::from(k.field_pos);
        match k.type_nr {
            0 => {
                let v = store(record, stores).get_int(record.rec, p);
                result.push(Content::Long(i64::from(v)));
            }
            1 => {
                let v = store(record, stores).get_long(record.rec, p);
                result.push(Content::Long(v));
            }
            5 => {
                let v = store(record, stores)
                    .get_str(store(record, stores).get_int(record.rec, p) as u32);
                result.push(Content::Str(Str::new(v)));
            }
            _ => {
                let v = store(record, stores).get_byte(record.rec, p, 0);
                result.push(Content::Long(i64::from(v)));
            }
        }
    }
    result
}

#[must_use]
pub fn hash(rec: &DbRef, stores: &[Store], keys: &[Key]) -> u64 {
    let mut hasher = DefaultHasher::new();
    for key in keys {
        let pos = rec.pos + u32::from(key.field_pos);
        hash_ref(rec, stores, key, pos, &mut hasher);
    }
    hasher.finish()
}

#[must_use]
pub fn key_hash(key: &[Content]) -> u64 {
    let mut hasher = DefaultHasher::new();
    for k in key {
        match k {
            Content::Long(l) => l.hash(&mut hasher),
            Content::Str(s) => s.str().hash(&mut hasher),
            _ => (),
        }
    }
    hasher.finish()
}

fn hash_ref(r: &DbRef, stores: &[Store], key: &Key, p: u32, hasher: &mut DefaultHasher) {
    let s = store(r, stores);
    match key.type_nr {
        0 => i64::from(s.get_int(r.rec, p)).hash(hasher),
        1 => s.get_long(r.rec, p).hash(hasher),
        2 | 3 => (),
        5 => s.get_str(s.get_int(r.rec, p) as u32).hash(hasher),
        _ => i64::from(s.get_byte(r.rec, p, 0)).hash(hasher),
    }
}
