// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! The standard operations for GameEngine
#![allow(dead_code)]

use crate::store::Store;

pub struct DataStore {
    pub records: Store,
    pub text: Store,
}

impl PartialEq for DataStore {
    fn eq(&self, _: &DataStore) -> bool {
        false
    }
}

impl std::fmt::Debug for DataStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Store")
            .field("records", &self.records.len())
            .field("text", &self.text.len())
            .finish()
    }
}

pub fn op_not(v1: bool) -> bool {
    !v1
}

pub fn op_min_single_int(v1: i32) -> i32 {
    if v1 != i32::MIN {
        -v1
    } else {
        i32::MIN
    }
}

pub fn op_min_single_long(v1: i64) -> i64 {
    -v1
}

pub fn op_min_single_float(v1: f64) -> f64 {
    -v1
}

pub fn op_conv_float_from_int(v1: i32) -> f64 {
    if v1 != i32::MIN {
        v1 as f64
    } else {
        f64::NAN
    }
}

pub fn op_conv_single_from_int(v1: i32) -> f32 {
    if v1 != i32::MIN {
        v1 as f32
    } else {
        f32::NAN
    }
}

pub fn op_conv_bool_from_int(v: i32) -> bool {
    v != i32::MIN
}

pub fn op_conv_bool_from_float(v: f64) -> bool {
    !v.is_nan()
}

pub fn op_conv_bool_from_reference(v: u32) -> bool {
    v > 0
}

pub fn op_conv_bool_from_enum(v: u8) -> bool {
    v > 0
}

pub fn op_conv_bool_from_text(v: u32) -> bool {
    v > 0
}

pub fn op_conv_int_from_null() -> i32 {
    i32::MIN
}

pub fn op_conv_float_from_null() -> f64 {
    f64::NAN
}

pub fn op_conv_single_from_null() -> f32 {
    f32::NAN
}

pub fn op_conv_reference_from_null() -> (u32, u32) {
    (0, 0)
}

pub fn op_conv_enum_from_null() -> u8 {
    0
}

pub fn op_conv_text_from_null() -> u32 {
    0
}

pub fn op_conv_vector_from_null() -> u32 {
    0
}

pub fn op_add_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 + v2
    } else {
        i32::MIN
    }
}

pub fn op_min_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 - v2
    } else {
        i32::MIN
    }
}

pub fn op_mul_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 * v2
    } else {
        i32::MIN
    }
}

pub fn op_div_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 / v2
    } else {
        i32::MIN
    }
}

pub fn op_rem_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 % v2
    } else {
        i32::MIN
    }
}

pub fn op_add_float(v1: f64, v2: f64) -> f64 {
    v1 + v2
}

pub fn op_min_float(v1: f64, v2: f64) -> f64 {
    v1 - v2
}

pub fn op_mul_float(v1: f64, v2: f64) -> f64 {
    v1 * v2
}

pub fn op_div_float(v1: f64, v2: f64) -> f64 {
    if v2 != 0.0 {
        v1 / v2
    } else {
        f64::NAN
    }
}

pub fn op_add_single(v1: f32, v2: f32) -> f32 {
    v1 + v2
}

pub fn op_min_single(v1: f32, v2: f32) -> f32 {
    v1 - v2
}

pub fn op_mul_single(v1: f32, v2: f32) -> f32 {
    v1 * v2
}

pub fn op_div_single(v1: f32, v2: f32) -> f32 {
    if v2 != 0.0 {
        v1 / v2
    } else {
        f32::NAN
    }
}

// OpAnd && OpOr cannot be given as a function because they are lazy

pub fn op_eq_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 == v2
    } else {
        false
    }
}

pub fn op_ne_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 != v2
    } else {
        false
    }
}

pub fn op_lt_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 < v2
    } else {
        false
    }
}

pub fn op_le_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 <= v2
    } else {
        false
    }
}

pub fn op_gt_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 > v2
    } else {
        false
    }
}

pub fn op_ge_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 >= v2
    } else {
        false
    }
}

pub fn op_eq_float(v1: f64, v2: f64) -> bool {
    v1 == v2
}

pub fn op_ne_float(v1: f64, v2: f64) -> bool {
    v1 != v2
}

pub fn op_lt_float(v1: f64, v2: f64) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 < v2
    } else {
        false
    }
}

pub fn op_le_float(v1: f64, v2: f64) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 <= v2
    } else {
        false
    }
}

pub fn op_gt_float(v1: f64, v2: f64) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 > v2
    } else {
        false
    }
}

pub fn op_ge_float(v1: f64, v2: f64) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 >= v2
    } else {
        false
    }
}
pub fn op_eq_single(v1: f32, v2: f32) -> bool {
    v1 == v2
}

pub fn op_ne_single(v1: f32, v2: f32) -> bool {
    v1 != v2
}

pub fn op_lt_single(v1: f32, v2: f32) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 < v2
    } else {
        false
    }
}

pub fn op_le_single(v1: f32, v2: f32) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 <= v2
    } else {
        false
    }
}

pub fn op_gt_single(v1: f32, v2: f32) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 > v2
    } else {
        false
    }
}

pub fn op_ge_single(v1: f32, v2: f32) -> bool {
    if !v1.is_nan() && !v2.is_nan() {
        v1 >= v2
    } else {
        false
    }
}

pub fn op_eq_bool(v1: bool, v2: bool) -> bool {
    v1 == v2
}

pub fn op_ne_bool(v1: bool, v2: bool) -> bool {
    v1 != v2
}

pub fn op_eq_reference(v1: u32, v2: u32) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 == v2
    } else {
        false
    }
}

pub fn op_ne_reference(v1: u32, v2: u32) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 != v2
    } else {
        false
    }
}

pub fn op_eq_enum(v1: u8, v2: u8) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 == v2
    } else {
        false
    }
}

pub fn op_ne_enum(v1: u8, v2: u8) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 != v2
    } else {
        false
    }
}

pub fn op_eq_text(v1: u32, v2: u32) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 == v2
    } else {
        false
    }
}

pub fn op_ne_text(v1: u32, v2: u32) -> bool {
    if v1 != 0 && v2 != 0 {
        v1 != v2
    } else {
        false
    }
}

pub fn op_clear_vector(store: &mut Store, rec: u32, pos: isize) {
    let vec_rec = store.get_int(rec, pos) as u32;
    if vec_rec != 0 {
        store.set_int(rec, vec_rec as isize, 4);
    }
}

pub fn op_get_vector(store: &mut Store, rec: u32, pos: isize, size: u32, index: i32) -> (u32, u32) {
    let vec_rec = store.get_int(rec, pos) as u32;
    if vec_rec == 0 {
        return (0, 0);
    }
    let len = store.get_int(vec_rec, 4);
    let real = if index < 0 { index + len } else { index };
    if real >= 0 && real < len {
        (vec_rec, 8 + real as u32 * size)
    } else {
        (0, 0)
    }
}

pub fn op_append_vector(store: &mut Store, rec: u32, pos: isize, size: u32) -> (u32, u32) {
    let mut vec_rec = store.get_int(rec, pos) as u32;
    let new_rec;
    if vec_rec == 0 {
        vec_rec = store.claim((10 * size + 15) / 8);
        store.set_int(rec, pos, vec_rec as i32);
        new_rec = 0;
    } else {
        new_rec = store.get_int(vec_rec, 4) as u32;
        let st = store.resize(vec_rec, ((new_rec + 1) * size + 15) / 8);
        if st != vec_rec {
            store.set_int(rec, pos, st as i32);
        }
        vec_rec = st;
    };
    store.set_int(vec_rec, 4, new_rec as i32 + 1);
    // Keep room for integers claimed space (0) & vector length (4)
    (vec_rec, 8 + new_rec * size)
}

pub fn op_remove_vector(store: &mut Store, rec: u32, pos: isize, size: u32, index: i32) {
    let len = op_length_vector(store, rec, pos);
    let real = if index < 0 { index + len } else { index } as isize;
    let s = size as isize;
    if real > 0 && real < len as isize {
        store.move_content(rec, s * (real + 1), s * real, (len as isize - real) * s);
        store.set_int(rec, pos, len - 1);
    }
}

pub fn op_insert_vector(
    store: &mut Store,
    rec: u32,
    pos: isize,
    size: u32,
    index: i32,
) -> (u32, u32) {
    let len = op_length_vector(store, rec, pos);
    let real = if index < 0 { index + len } else { index } as isize;
    let s = size as isize;
    if real > 0 && real < len as isize {
        store.move_content(rec, s * real, s * (real + 1), (len as isize - real) * s);
        store.set_int(rec, pos, len + 1);
    }
    // The values on this location still need to be written
    (rec, size * real as u32)
}

pub fn op_length_vector(store: &Store, rec: u32, pos: isize) -> i32 {
    let vec_rec = store.get_int(rec, pos) as u32;
    if vec_rec == 0 {
        0
    } else {
        store.get_int(vec_rec, 4)
    }
}

fn op_remove_record(store: &mut Store, rec: u32) {
    store.delete(rec);
}
