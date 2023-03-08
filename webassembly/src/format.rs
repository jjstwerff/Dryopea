// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! The routines to format the different data structures.
#![allow(dead_code)]
use std::cmp::Ordering;

static TOKENS: [u8; 16] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
];

// TODO Let this function correctly under unicode multi-char clusters.
pub fn format_text(s: &mut String, val: &str, width: u32, dir: u32, token: u32) {
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
                s.push(token as u8 as char);
                tokens -= 1;
            }
        }
        Ordering::Greater => {
            while tokens > 0 {
                s.push(token as u8 as char);
                tokens -= 1;
            }
            *s += val;
        }
        Ordering::Equal => {
            let mut ct = 0;
            while ct < tokens / 2 {
                s.push(token as u8 as char);
                ct += 1;
            }
            *s += val;
            while ct < tokens {
                s.push(token as u8 as char);
                ct += 1;
            }
        }
    }
}

pub fn format_int(
    s: &mut String,
    val: i32,
    radix: u32,
    width: u32,
    token: u32,
    plus: u32,
    note: u32,
) {
    let mut data: [u8; 35] = [0; 35];
    let mut v = i32::abs(val);
    let mut p = data.len();
    while v > 0 {
        p -= 1;
        data[p] = TOKENS[(v % radix as i32) as usize];
        v /= radix as i32;
    }
    if note > 0 {
        match radix {
            2 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'b';
            } // "0b"
            8 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'o';
            } // "0o"
            16 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'x';
            } // "0x"
            _ => {}
        }
    }
    if val < 0 {
        p -= 1;
        data[p] = b'-';
    } else if plus > 0 {
        p -= 1;
        data[p] = b'+';
    }
    let mut len = data.len() - p;
    while len < width as usize {
        s.push(token as u8 as char);
        len += 1;
    }
    for t in data.iter().skip(p) {
        s.push(*t as char);
    }
}

pub fn format_long(
    s: &mut String,
    val: i64,
    radix: u32,
    width: u32,
    token: u32,
    plus: u32,
    note: u32,
) {
    let mut data: [u8; 67] = [0; 67];
    let mut v = i64::abs(val);
    let mut p = data.len();
    while v > 0 {
        p -= 1;
        data[p] = TOKENS[(v % radix as i64) as usize];
        v /= radix as i64;
    }
    if note > 0 {
        match radix {
            2 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'b';
            } // "0b"
            8 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'o';
            } // "0o"
            16 => {
                p -= 2;
                data[p] = b'0';
                data[p + 1] = b'x';
            } // "0x"
            _ => {}
        }
    }
    if val < 0 {
        p -= 1;
        data[p] = b'-';
    } else if plus > 0 {
        p -= 1;
        data[p] = b'+';
    }
    let mut len = data.len() - p;
    while len < width as usize {
        s.push(token as u8 as char);
        len += 1;
    }
    for t in data.iter().skip(p) {
        s.push(*t as char);
    }
}

use std::fmt::Write as _;

pub fn format_float(s: &mut String, val: f64, width: u32, precision: u32) {
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
    }
}

pub fn format_single(s: &mut String, val: f32, width: u32, precision: u32) {
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
    }
}

pub fn format_text_str(val: &str, width: u8, dir: i8, token: char) -> String {
    let mut res = String::new();
    format_text(&mut res, val, width as u32, dir as u32, token as u32);
    res
}

pub fn format_int_str(
    val: i32,
    radix: u8,
    width: u8,
    token: char,
    plus: bool,
    note: bool,
) -> String {
    let mut res = String::new();
    format_int(
        &mut res,
        val,
        radix as u32,
        width as u32,
        token as u32,
        plus as u32,
        note as u32,
    );
    res
}

pub fn format_long_str(
    val: i64,
    radix: u8,
    width: u8,
    token: char,
    plus: bool,
    note: bool,
) -> String {
    let mut res = String::new();
    format_long(
        &mut res,
        val,
        radix as u32,
        width as u32,
        token as u32,
        plus as u32,
        note as u32,
    );
    res
}

pub fn format_float_str(val: f64, width: u8, precision: u8) -> String {
    let mut res = String::new();
    format_float(&mut res, val, width as u32, precision as u32);
    res
}

pub fn format_single_str(val: f32, width: u8, precision: u8) -> String {
    let mut res = String::new();
    format_single(&mut res, val, width as u32, precision as u32);
    res
}

pub fn append(s: &mut String, val: &str) {
    *s += val;
}

pub fn force_string_functions(val: &str) -> String {
    let mut b = String::new();
    let s = &mut b;
    format_int(s, 123, 8, 7, 32, 1, 1);
    s.push('(');
    append(s, val);
    s.push(')');
    format_long(s, 12345, 8, 7, 32, 1, 1);
    format_text(s, val, 8, 0, b'*' as u32);
    b
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_layouts() {
        assert_eq!("_aa__", format_text_str("aa", 5, 0, '_'));
        assert_eq!("__aa__", format_text_str("aa", 6, 0, '_'));
        assert_eq!("0x1234", format_int_str(0x1234, 16, 0, ' ', false, true));
    }

    #[test]
    fn test_forced() {
        assert_eq!(
            " +0o173(123)+0o30071**123***",
            force_string_functions("123")
        );
    }
}
