// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! The standard operations for GameEngine
use crate::store::Store;
use std::cmp::Ordering;

#[inline]
pub fn format_text(val: &str, width: i32, dir: i32, token: i32) -> String {
    let mut tokens = width as usize;
    for _ in val.chars() {
        if tokens == 0 {
            break;
        }
        tokens -= 1;
    }
    let mut s = String::new();
    match dir.cmp(&0) {
        Ordering::Less => {
            s += val;
            while tokens > 0 {
                s.push(token as u8 as char);
                tokens -= 1;
            }
        }
        Ordering::Greater => {
            while tokens > 0 {
                s.push(token as u8 as char);
                tokens -= 1;
            }
            s += val;
        }
        Ordering::Equal => {
            let mut ct = 0;
            while ct < tokens / 2 {
                s.push(token as u8 as char);
                ct += 1;
            }
            s += val;
            while ct < tokens {
                s.push(token as u8 as char);
                ct += 1;
            }
        }
    };
    s
}

#[inline]
pub fn format_int(val: i32, radix: i32, width: i32, token: i32, plus: bool, note: bool) -> String {
    let mut res = String::new();
    format_text(
        if val == i32::MIN {
            "null"
        } else {
            match radix {
                2 => {
                    res += if note { "0b" } else { "" };
                    res += &format!("{:b}", val)
                }
                8 => {
                    res += if note { "0o" } else { "" };
                    res += &format!("{:o}", val)
                }
                10 => {
                    res += if val > 0 {
                        if plus {
                            "+"
                        } else {
                            ""
                        }
                    } else {
                        "-"
                    };
                    res += &format!("{:}", val.abs())
                }
                16 => {
                    res += if note { "0x" } else { "" };
                    res += &format!("{:x}", val)
                }
                _ => panic!("Unknown radix"),
            }
            &res
        },
        width,
        1,
        token,
    )
}

#[inline]
pub fn format_long(val: i64, radix: i32, width: i32, token: i32, plus: bool, note: bool) -> String {
    let mut res = String::new();
    format_text(
        if val == i64::MIN {
            "null"
        } else {
            match radix {
                2 => {
                    if note {
                        res += "0b"
                    };
                    res += &format!("{:b}", val);
                }
                8 => {
                    if note {
                        res += "0o"
                    };
                    res += &format!("{:o}", val);
                }
                10 => {
                    res += if val > 0 {
                        if plus {
                            "+"
                        } else {
                            ""
                        }
                    } else {
                        "-"
                    };
                    res += &format!("{:}", val.abs())
                }
                16 => {
                    res += if note { "0x" } else { "" };
                    res += &format!("{:x}", val)
                }
                _ => panic!("Unknown radix"),
            };
            &res
        },
        width,
        1,
        token,
    )
}

pub fn op_clear_vector(store: &mut Store, rec: u32) {
    if rec == 0 {
        return;
    }
    store.set_int(rec, 4, 0);
}

pub fn op_get_vector(store: &Store, rec: u32, pos: isize, size: u32, index: i32) -> (u32, u32) {
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

// Append a single element to a vector pointed to with the given field.
pub fn op_append_vector(store: &mut Store, rec: u32, pos: isize, size: u32) -> (u32, u32) {
    op_append(store, rec, pos, 1, size)
}

fn op_append(store: &mut Store, rec: u32, pos: isize, add: u32, size: u32) -> (u32, u32) {
    let mut vec_rec = store.get_int(rec, pos) as u32;
    let new_length;
    if vec_rec == 0 {
        // claim a new array with minimal 11 elements
        vec_rec = store.claim(((add + 10) * size + 15) / 8);
        store.set_int(rec, pos, vec_rec as i32);
        new_length = add;
    } else {
        new_length = add + store.get_int(vec_rec, 4) as u32;
        let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
        if new_vec != vec_rec {
            store.set_int(rec, pos, new_vec as i32);
            vec_rec = new_vec;
        }
    };
    store.set_int(vec_rec, 4, new_length as i32);
    (vec_rec, 8 + (new_length - add) * size)
}

// This adds an array from one store to another.
// Child records should be copied separately.
// Relations to other records in the original store cannot be correctly copied and should be 0.
pub fn op_add_vector(
    store: &mut Store,
    rec: u32,
    pos: isize,
    size: u32,
    other: &Store,
    o: u32,
    opos: isize,
) {
    let len = op_length_vector(other, o, opos);
    let (vec_rec, new_pos) = op_append(store, rec, pos, len, size);
    let (o_rec, o_pos) = op_get_vector(other, o, opos, size, 0);
    other.copy_block_between(
        o_rec,
        o_pos as isize,
        store,
        vec_rec,
        new_pos as isize,
        (len * size) as isize,
    );
}

// This adds an array inside the same store.
// Child records should be copied separately.
pub fn op_add_vector_same(store: &mut Store, rec: u32, pos: isize, size: u32, o: u32, opos: isize) {
    let len = op_length_vector(store, o, opos);
    let (vec_rec, new_pos) = op_append(store, rec, pos, len, size);
    // (&self, rec: u32, pos: isize, to: isize, len: isize) {
    store.copy_block(o, opos, vec_rec, new_pos as isize, (len * size) as isize);
}

pub fn op_remove_vector(store: &mut Store, rec: u32, pos: isize, size: u32, index: i32) {
    let len = op_length_vector(store, rec, pos) as i32;
    let v_rec = store.get_int(rec, pos) as u32;
    let real = if index < 0 { index + len } else { index } as isize;
    let s = size as isize;
    if real > 0 && real < len as isize {
        store.copy_block(
            v_rec,
            8 + s * (real + 1),
            v_rec,
            8 + s * real,
            (len as isize - real) * s,
        );
        store.set_int(v_rec, 4, len - 1);
    }
}

// Create room for an extra element
pub fn op_insert_vector(
    store: &mut Store,
    rec: u32,
    pos: isize,
    size: u32,
    index: i32,
) -> (u32, u32) {
    let len = op_length_vector(store, rec, pos) as i32;
    let mut v_rec = store.get_int(rec, pos) as u32;
    let real = if index < 0 { index + len } else { index } as isize;
    let s = size as isize;
    if real > 0 && real < len as isize {
        let new_vec = store.resize(v_rec, ((len as u32 + 1) * size + 15) / 8);
        if new_vec != v_rec {
            store.set_int(rec, pos, new_vec as i32);
            v_rec = new_vec;
        }
        store.copy_block(
            v_rec,
            8 + s * real,
            v_rec,
            8 + s * (real + 1),
            (len as isize - real) * s,
        );
        store.set_int(v_rec, 4, len + 1);
    }
    (v_rec, 8 + size * real as u32)
}

pub fn op_length_vector(store: &Store, rec: u32, pos: isize) -> u32 {
    let v_rec = store.get_int(rec, pos) as u32;
    if v_rec == 0 {
        0
    } else {
        store.get_int(v_rec, 4) as u32
    }
}

use std::fmt::Write as _;

pub fn format_float(val: f64, width: i32, precision: i32) -> String {
    let mut s = String::new();
    if precision != 0 {
        write!(
            s,
            "{v:w$.p$}",
            w = width as usize,
            p = precision as usize,
            v = val
        )
        .unwrap();
    } else {
        write!(s, "{v:w$}", w = width as usize, v = val).unwrap();
    };
    s
}

pub fn format_single(val: f32, width: i32, precision: i32) -> String {
    let mut s = String::new();
    if precision != 0 {
        write!(
            s,
            "{v:w$.p$}",
            w = width as usize,
            p = precision as usize,
            v = val
        )
        .unwrap();
    } else {
        write!(s, "{v:w$}", w = width as usize, v = val).unwrap();
    };
    s
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_layouts() {
        assert_eq!("_aa__", format_text("aa", 5, 0, '_' as i32));
        assert_eq!("__aa__", format_text("aa", 6, 0, '_' as i32));
        assert_eq!("0x1234", format_int(0x1234, 16, 0, ' ' as i32, false, true));
        assert_eq!(
            "0x1234567",
            format_long(0x1234567, 16, 0, ' ' as i32, false, true)
        );
        // validate long, float and single
    }

    #[test]
    fn test_vectors() {
        // new vector A
        let mut db_a = Store::new(8);
        let v_a = db_a.claim(1);
        let fld = 4;
        db_a.set_int(v_a, fld, 0);
        // write elements [1,2,3]
        let (vr, vp) = op_append_vector(&mut db_a, v_a, fld, 4);
        db_a.set_int(vr, vp as isize, 1);
        let (vr, vp) = op_append_vector(&mut db_a, v_a, fld, 4);
        db_a.set_int(vr, vp as isize, 2);
        let (vr, vp) = op_append_vector(&mut db_a, v_a, fld, 4);
        db_a.set_int(vr, vp as isize, 3);
        assert_eq!(3, op_length_vector(&db_a, v_a, 4));
        // append element 4
        let (vr, vp) = op_append_vector(&mut db_a, v_a, fld, 4);
        db_a.set_int(vr, vp as isize, 4);
        assert_eq!(4, op_length_vector(&db_a, v_a, fld));
        // new vector B
        let mut db_b = Store::new(8);
        let v_b = db_b.claim(1);
        db_b.set_int(v_b, fld, 0);
        // append elements
        let (vr, vp) = op_append_vector(&mut db_b, v_b, fld, 4);
        db_b.set_int(vr, vp as isize, 6);
        let (vr, vp) = op_append_vector(&mut db_b, v_b, fld, 4);
        db_b.set_int(vr, vp as isize, 8);
        assert_eq!(8, get(&db_b, v_b, 1));
        // append vector B to A
        op_add_vector(&mut db_a, v_a, fld, 4, &db_b, v_b, fld);
        assert_eq!(6, op_length_vector(&db_a, v_a, fld));
        // loop vector A
        assert_eq!(24, calc_sum(&db_a, v_a));
        // remove an element from A
        op_remove_vector(&mut db_a, v_a, fld, 4, 2);
        assert_eq!(5, op_length_vector(&db_a, v_a, fld));
        assert_eq!(6, get(&db_a, v_a, 3));
        // clear vector A
        op_clear_vector(&mut db_a, v_a);
        assert_eq!(0, op_length_vector(&db_a, v_a, fld));
        // append vector B to A
        op_add_vector(&mut db_a, v_a, fld, 4, &db_b, v_b, fld);
        assert_eq!(14, calc_sum(&db_a, v_a));
        assert_eq!(2, op_length_vector(&db_a, v_a, fld));
        assert_eq!(8, get(&db_a, v_a, 1));
        let (e_rec, e_pos) = op_insert_vector(&mut db_a, v_a, fld, 4, 1);
        db_a.set_int(e_rec, e_pos as isize, 1);
        assert_eq!(6, get(&db_a, v_a, 0));
        assert_eq!(1, get(&db_a, v_a, 1));
        assert_eq!(8, get(&db_a, v_a, 2));
        assert_eq!(15, calc_sum(&db_a, v_a));
    }

    fn get(db: &Store, rec: u32, index: i32) -> i32 {
        let (e_rec, e_pos) = op_get_vector(db, rec, 4, 4, index);
        db.get_int(e_rec, e_pos as isize)
    }

    fn calc_sum(db: &Store, rec: u32) -> i32 {
        let mut sum = 0;
        for i in 0..op_length_vector(db, rec, 4) {
            sum += get(db, rec, i as i32);
        }
        sum
    }

    #[test]
    fn test_sorted_vector() {
        // new vector
        // add elements
        // append vector
        // search element on key
        // remove elements
    }
}
