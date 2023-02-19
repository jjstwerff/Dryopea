// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! The routines to format the different data structures.

use std::cmp::Ordering;

static TOKENS: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

/// Format a text
pub fn format_text(val: &str, width: u8, dir: i8, token: char) -> String {
    let mut size = val.len();
    let w = width as usize;
    if size < w {
        let mut res: String;
        match dir.cmp(&0) {
            Ordering::Less => {
                res = val.to_string();
                while size < w {
                    res += &token.to_string();
                    size += 1;
                }
                res
            }
            Ordering::Greater => {
                res = "".to_string();
                while size < w {
                    res += &token.to_string();
                    size += 1;
                }
                res + val
            }
            Ordering::Equal => {
                res = "".to_string();
                let mut sz = size / 2;
                let wd = w / 2;
                while sz < wd {
                    res += &token.to_string();
                    sz += 1;
                }
                res += val;
                sz = size - (size / 2);
                let wd = w - wd;
                while sz < wd {
                    res += &token.to_string();
                    sz += 1;
                }
                res
            }
        }
    } else {
        val.to_string()
    }
}

pub fn format_int(val: i32, radix: u8, width: u8, token: char, plus: bool, note: bool) -> String {
    let mut res = "".to_string();
    let mut data: [u8; 32] = [0; 32];
    let mut v = i32::abs(val);
    let mut p = 31;
    while v > 0 {
        data[p] = (v % radix as i32) as u8;
        v /= radix as i32;
        p -= 1;
    }
    p += 1;
    if val < 0 {
        res += "-";
    } else if plus {
        res += "+";
    }
    if note {
        res += match radix {
            2 => "0b",
            8 => "0o",
            16 => "0x",
            _ => "",
        };
    }
    for t in p..32 {
        res += &TOKENS[data[t] as usize].to_string();
    }
    while res.len() < width as usize {
        res = token.to_string() + &res;
    }
    res
}

pub fn format_long(val: i64, radix: u8, width: u8, token: char, plus: bool, note: bool) -> String {
    let mut res = "".to_string();
    let mut data: [u8; 64] = [0; 64];
    let mut v = i64::abs(val);
    let mut p = 63;
    while v > 0 {
        data[p] = (v % radix as i64) as u8;
        v /= radix as i64;
        p -= 1;
    }
    p += 1;
    if val < 0 {
        res += "-";
    } else if plus {
        res += "+";
    }
    if note {
        res += match radix {
            2 => "0b",
            8 => "0o",
            16 => "0x",
            _ => "",
        }
    }
    for t in p..64 {
        res += &TOKENS[data[t] as usize].to_string();
    }
    while res.len() < width as usize {
        res = token.to_string() + &res;
    }
    res
}

pub fn format_float(val: f64, width: u8, precision: u8) -> String {
    if precision != 0 {
        format!(
            "{val:width$.pre$}",
            width = width as usize,
            pre = precision as usize,
            val = val
        )
    } else {
        format!("{val:width$}", width = width as usize, val = val)
    }
}

pub fn format_single(val: f32, width: u8, precision: u8) -> String {
    if precision != 0 {
        format!(
            "{val:width$.pre$}",
            width = width as usize,
            pre = precision as usize,
            val = val
        )
    } else {
        format!("{val:width$}", width = width as usize, val = val)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_layouts() {
        assert_eq!("_aa__", format_text("aa", 5, 0, '_'));
        assert_eq!("__aa__", format_text("aa", 6, 0, '_'));
        assert_eq!("0x1234", format_int(0x1234, 16, 0, ' ', false, true));
    }
}
