// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! The standard operations for `GameEngine`
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_precision_loss)]
#![allow(dead_code)]
use std::cmp::Ordering;

#[must_use]
pub fn sub_text(val: &str, from: i32, till: i32) -> &str {
    let size = val.len() as i32;
    let mut f = if from < 0 { from + size } else { from };
    let mut t = if till == i32::MIN {
        f + 1
    } else if till < 0 {
        till + size
    } else if till > size {
        size
    } else {
        till
    };
    if f < 0 || f > size || t < f || t > size {
        return "";
    }
    let b = val.as_bytes();
    // when till is inside a UTF-8 token: increase it
    while t < size && b[t as usize] >= 128 && b[t as usize] < 192 {
        t += 1;
    }
    // when from is inside a UTF-8 token: decrease it
    while f > 0 && b[f as usize] >= 128 && b[f as usize] < 192 {
        f -= 1;
    }
    &val[f as usize..t as usize]
}

#[inline]
pub fn format_text(s: &mut String, val: &str, width: i32, dir: i8, token: u8) {
    let mut tokens = width as usize;
    for _ in val.chars() {
        if tokens == 0 {
            break;
        }
        tokens -= 1;
    }
    match dir.cmp(&0) {
        Ordering::Less => {
            *s += val;
            while tokens > 0 {
                s.push(token as char);
                tokens -= 1;
            }
        }
        Ordering::Greater => {
            while tokens > 0 {
                s.push(token as char);
                tokens -= 1;
            }
            *s += val;
        }
        Ordering::Equal => {
            let mut ct = 0;
            while ct < tokens / 2 {
                s.push(token as char);
                ct += 1;
            }
            *s += val;
            while ct < tokens {
                s.push(token as char);
                ct += 1;
            }
        }
    }
}
#[inline]
#[must_use]
pub fn op_abs_long(val: i64) -> i64 {
    if val == i64::MIN { val } else { val.abs() }
}

#[inline]
#[must_use]
pub fn op_min_single_long(val: i64) -> i64 {
    if val == i64::MIN { val } else { -val }
}

#[inline]
#[must_use]
pub fn op_cast_int_from_long(val: i64) -> i32 {
    if val == i64::MIN {
        i32::MIN
    } else {
        val as i32
    }
}

#[inline]
#[must_use]
pub fn op_cast_int_from_single(val: f32) -> i32 {
    if val.is_nan() { i32::MIN } else { val as i32 }
}

#[inline]
#[must_use]
pub fn op_cast_long_from_single(val: f32) -> i64 {
    if val.is_nan() { i64::MIN } else { val as i64 }
}

#[inline]
#[must_use]
pub fn op_cast_int_from_float(val: f64) -> i32 {
    if val.is_nan() { i32::MIN } else { val as i32 }
}

#[inline]
#[must_use]
pub fn op_cast_long_from_float(val: f64) -> i64 {
    if val.is_nan() { i64::MIN } else { val as i64 }
}

#[inline]
#[must_use]
pub fn op_conv_float_from_long(val: i64) -> f64 {
    if val == i64::MIN {
        f64::NAN
    } else {
        val as f64
    }
}

#[inline]
#[must_use]
pub fn op_conv_bool_from_long(val: i64) -> bool {
    val != i64::MIN
}

#[inline]
#[must_use]
pub fn op_add_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 + v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_min_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 - v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_mul_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN {
        v1 * v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_div_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 / v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_rem_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 % v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_logical_and_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 & v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_logical_or_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 | v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_exclusive_or_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 ^ v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_shift_left_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 << v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_shift_right_long(v1: i64, v2: i64) -> i64 {
    if v1 != i64::MIN && v2 != i64::MIN && v2 != 0 {
        v1 >> v2
    } else {
        i64::MIN
    }
}

#[inline]
#[must_use]
pub fn op_abs_int(val: i32) -> i32 {
    if val == i32::MIN { val } else { val.abs() }
}

#[inline]
#[must_use]
pub fn op_min_single_int(val: i32) -> i32 {
    if val == i32::MIN { val } else { -val }
}

#[inline]
#[must_use]
pub fn op_conv_long_from_int(val: i32) -> i64 {
    if val == i32::MIN {
        i64::MIN
    } else {
        i64::from(val)
    }
}

#[inline]
#[must_use]
pub fn op_conv_float_from_int(val: i32) -> f64 {
    if val == i32::MIN {
        f64::NAN
    } else {
        f64::from(val)
    }
}

#[inline]
#[must_use]
pub fn op_conv_single_from_int(val: i32) -> f32 {
    if val == i32::MIN {
        f32::NAN
    } else {
        val as f32
    }
}

#[inline]
#[must_use]
pub fn op_conv_bool_from_int(v: i32) -> bool {
    v != i32::MIN
}

#[inline]
#[must_use]
pub fn op_add_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 + v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_min_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 - v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_mul_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN {
        v1 * v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_div_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 / v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_rem_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 % v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_logical_and_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 & v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_logical_or_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 | v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_exclusive_or_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 ^ v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_shift_left_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 << v2
    } else {
        i32::MIN
    }
}

#[inline]
#[must_use]
pub fn op_shift_right_int(v1: i32, v2: i32) -> i32 {
    if v1 != i32::MIN && v2 != i32::MIN && v2 != 0 {
        v1 >> v2
    } else {
        i32::MIN
    }
}

/**
Format an integer.
# Panics
When unknown radix values are asked.
*/
#[inline]
pub fn format_int(
    s: &mut String,
    val: i32,
    radix: u8,
    width: i32,
    token: u8,
    plus: bool,
    note: bool,
) {
    let mut res = String::new();
    format_text(
        s,
        if val == i32::MIN {
            "null"
        } else {
            match radix {
                2 => {
                    res += if note { "0b" } else { "" };
                    res += &format!("{val:b}");
                }
                8 => {
                    res += if note { "0o" } else { "" };
                    res += &format!("{val:o}");
                }
                10 => {
                    res += if val > 0 {
                        if plus { "+" } else { "" }
                    } else {
                        "-"
                    };
                    res += &format!("{:}", val.abs());
                }
                16 => {
                    res += if note { "0x" } else { "" };
                    res += &format!("{val:x}");
                }
                _ => panic!("Unknown radix"),
            }
            &res
        },
        width,
        1,
        token,
    );
}

/**
Format a long integer.
# Panics
When unknown radix values are asked.
*/
#[inline]
pub fn format_long(
    s: &mut String,
    val: i64,
    radix: u8,
    width: i32,
    token: u8,
    plus: bool,
    note: bool,
) {
    let mut res = String::new();
    format_text(
        s,
        if val == i64::MIN {
            "null"
        } else {
            match radix {
                2 => {
                    if note {
                        res += "0b";
                    };
                    res += &format!("{val:b}");
                }
                8 => {
                    if note {
                        res += "0o";
                    };
                    res += &format!("{val:o}");
                }
                10 => {
                    res += if val > 0 {
                        if plus { "+" } else { "" }
                    } else {
                        "-"
                    };
                    res += &format!("{:}", val.abs());
                }
                16 => {
                    res += if note { "0x" } else { "" };
                    res += &format!("{val:x}");
                }
                _ => panic!("Unknown radix"),
            };
            &res
        },
        width,
        1,
        token,
    );
}

use std::fmt::Write as _;

pub fn format_float(s: &mut String, val: f64, width: i32, precision: i32) {
    if precision != 0 {
        write!(s, "{val:w$.p$}", w = width as usize, p = precision as usize,).unwrap();
    } else {
        write!(s, "{val:w$}", w = width as usize).unwrap();
    };
}

pub fn format_single(s: &mut String, val: f32, width: i32, precision: i32) {
    if precision != 0 {
        write!(s, "{val:w$.p$}", w = width as usize, p = precision as usize,).unwrap();
    } else {
        write!(s, "{val:w$}", w = width as usize).unwrap();
    };
}

#[must_use]
pub fn fix_from(from: i32, s: &str) -> usize {
    let size = s.len() as i32;
    let mut f = if from < 0 { from + size } else { from };
    if f < 0 {
        return 0;
    }
    let b = s.as_bytes();
    // when from is inside a UTF-8 token: decrease it
    while f > 0 && b[f as usize] >= 128 && b[f as usize] < 192 {
        f -= 1;
    }
    f as usize
}

#[must_use]
pub fn fix_till(till: i32, from: usize, s: &str) -> usize {
    let size = s.len() as i32;
    let mut t = if till == i32::MIN {
        from as i32 + 1
    } else if till < 0 {
        till + size
    } else if till > size {
        size
    } else {
        till
    };
    if t < from as i32 || t > size {
        return from;
    }
    let b = s.as_bytes();
    // when till is inside a UTF-8 token: increase it
    while t < size && b[t as usize] >= 128 && b[t as usize] < 192 {
        t += 1;
    }
    t as usize
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_layouts() {
        let mut s = String::new();
        format_text(&mut s, "aa", 5, 0, b'_');
        assert_eq!("_aa__", s);
        s.clear();
        format_text(&mut s, "aa", 6, 0, b'_');
        assert_eq!("__aa__", s);
        s.clear();
        format_int(&mut s, 0x1234, 16, 0, b' ', false, true);
        assert_eq!("0x1234", s);
        s.clear();
        format_long(&mut s, 0x123_4567, 16, 0, b' ', false, true);
        assert_eq!("0x1234567", s);
        // validate long, float and single
    }
}
