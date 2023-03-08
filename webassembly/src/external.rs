// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! The standard operations for GameEngine
#![allow(dead_code)]
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
pub fn op_abs_int(val: i32) -> i32 {
    if val == i32::MIN {
        val
    } else {
        val.abs()
    }
}

#[inline]
pub fn op_min_single_int(val: i32) -> i32 {
    if val == i32::MIN {
        val
    } else {
        -val
    }
}

#[inline]
pub fn op_conv_long_from_int(val: i32) -> i64 {
    if val == i32::MIN {
        i64::MIN
    } else {
        val as i64
    }
}

#[inline]
pub fn op_conv_float_from_int(val: i32) -> f64 {
    if val == i32::MIN {
        f64::NAN
    } else {
        val as f64
    }
}

#[inline]
pub fn op_conv_single_from_int(val: i32) -> f32 {
    if val == i32::MIN {
        f32::NAN
    } else {
        val as f32
    }
}

#[inline]
pub fn op_conv_bool_from_int(v: i32) -> bool {
    v != i32::MIN
}

#[inline]
pub fn op_add_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 + v2
    } else {
        i32::MIN
    }
}

#[inline]
pub fn op_min_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 - v2
    } else {
        i32::MIN
    }
}

#[inline]
pub fn op_mul_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 * v2
    } else {
        i32::MIN
    }
}

#[inline]
pub fn op_div_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 / v2
    } else {
        i32::MIN
    }
}

#[inline]
pub fn op_rem_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 % v2
    } else {
        i32::MIN
    }
}

#[inline]
pub fn op_eq_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 == v2
    } else {
        false
    }
}

#[inline]
pub fn op_ne_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 != v2
    } else {
        false
    }
}

#[inline]
pub fn op_lt_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 < v2
    } else {
        false
    }
}

#[inline]
pub fn op_le_int(v1: i32, v2: i32) -> bool {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 <= v2
    } else {
        false
    }
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
pub fn op_abs_long(val: i64) -> i64 {
    if val == i64::MIN {
        val
    } else {
        val.abs()
    }
}

#[inline]
pub fn op_min_single_long(val: i64) -> i64 {
    if val == i64::MIN {
        val
    } else {
        -val
    }
}

#[inline]
pub fn op_cast_int_from_long(val: i64) -> i32 {
    if val == i64::MIN {
        i32::MIN
    } else {
        val as i32
    }
}

#[inline]
pub fn op_conv_float_from_long(val: i64) -> f64 {
    if val == i64::MIN {
        f64::NAN
    } else {
        val as f64
    }
}

#[inline]
pub fn op_conv_bool_from_long(val: i64) -> bool {
    val != i64::MIN
}

#[inline]
pub fn op_add_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 + v2
    } else {
        i64::MIN
    }
}

#[inline]
pub fn op_min_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 - v2
    } else {
        i64::MIN
    }
}

#[inline]
pub fn op_mul_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 * v2
    } else {
        i64::MIN
    }
}

#[inline]
pub fn op_div_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 / v2
    } else {
        i64::MIN
    }
}

#[inline]
pub fn op_rem_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 % v2
    } else {
        i64::MIN
    }
}

#[inline]
pub fn op_eq_long(v1: i64, v2: i64) -> bool {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 == v2
    } else {
        false
    }
}

#[inline]
pub fn op_ne_long(v1: i64, v2: i64) -> bool {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 != v2
    } else {
        false
    }
}

#[inline]
pub fn op_lt_long(v1: i64, v2: i64) -> bool {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 < v2
    } else {
        false
    }
}

#[inline]
pub fn op_le_long(v1: i64, v2: i64) -> bool {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 <= v2
    } else {
        false
    }
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

#[inline]
pub fn op_cast_int_from_single(v1: f32) -> i32 {
    if v1.is_nan() {
        i32::MIN
    } else {
        v1 as i32
    }
}

#[inline]
pub fn op_cast_long_from_single(v1: f32) -> i64 {
    if v1.is_nan() {
        i64::MIN
    } else {
        v1 as i64
    }
}

pub fn format_single(val: f32, width: i32, precision: i32) -> String {
    let mut s = String::new();
    if precision != 0 {
        write!(
            s,
            "{val:width$.pre$}",
            width = width as usize,
            pre = precision as usize,
            val = val
        )
            .unwrap();
    } else {
        write!(s, "{val:width$}", width = width as usize, val = val).unwrap();
    };
    s
}

#[inline]
pub fn op_cast_int_from_float(v1: f64) -> i32 {
    if v1.is_nan() {
        i32::MIN
    } else {
        v1 as i32
    }
}

#[inline]
pub fn op_cast_long_from_float(v1: f64) -> i64 {
    if v1.is_nan() {
        i64::MIN
    } else {
        v1 as i64
    }
}

pub fn format_float(val: f64, width: i32, precision: i32) -> String {
    let mut s = String::new();
    if precision != 0 {
        write!(
            s,
            "{val:width$.pre$}",
            width = width as usize,
            pre = precision as usize,
            val = val
        )
            .unwrap();
    } else {
        write!(s, "{val:width$}", width = width as usize, val = val).unwrap();
    };
    s
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

pub fn op_clear_vector(store: &mut Store, rec: u32) {
    if rec == 0 {
        return;
    }
    for (r, pos) in &mut store.references {
        if *r == rec {
            *r = 0;
            *pos = 0;
        }
    }
    store.set_int(rec, 4, 0);
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
    let len = op_length_vector(store, rec);
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
    let len = op_length_vector(store, rec);
    let real = if index < 0 { index + len } else { index } as isize;
    let s = size as isize;
    if real > 0 && real < len as isize {
        store.move_content(rec, s * real, s * (real + 1), (len as isize - real) * s);
        store.set_int(rec, pos, len + 1);
    }
    // The values on this location still need to be written
    (rec, size * real as u32)
}

pub fn op_length_vector(store: &Store, rec: u32) -> i32 {
    if rec == 0 {
        i32::MIN
    } else {
        store.get_int(rec, 4)
    }
}

fn op_remove_record(store: &mut Store, rec: u32) {
    store.delete(rec);
}

use std::fmt::Write as _;

pub fn append(s: &mut String, val: &str) {
    *s += val;
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_layouts() {
        assert_eq!("_aa__", format_text("aa", 5, 0, '_' as i32));
        assert_eq!("__aa__", format_text("aa", 6, 0, '_' as i32));
        assert_eq!("0x1234", format_int(0x1234, 16, 0, ' ' as i32, false, true));
    }
}

