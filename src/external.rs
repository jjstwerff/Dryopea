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
    let v_rec = store.get_int(rec, pos);
    if v_rec == 0 {
        i32::MIN
    } else {
        store.get_int(v_rec as u32, 4)
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
        let v_a = 1;
        // write elements [1,2,3]
        let (vr_a, vp_a) = op_append_vector(&mut db_a, v_a, 4, 4);
        db_a.set_int(vr_a, vp_a as isize, 1);
        let (vr_a, vp_a) = op_append_vector(&mut db_a, v_a, 4, 4);
        db_a.set_int(vr_a, vp_a as isize, 2);
        let (vr_a, vp_a) = op_append_vector(&mut db_a, v_a, 4, 4);
        db_a.set_int(vr_a, vp_a as isize, 3);
        assert_eq!(3, op_length_vector(&db_a, v_a, 4));
        // append element 4
        let (vr_a, vp_a) = op_append_vector(&mut db_a, v_a, 4, 4);
        db_a.set_int(vr_a, vp_a as isize, 4);
        assert_eq!(4, op_length_vector(&db_a, v_a, 4));
        // new vector B
        let db_b = Store::new(8);
        let v_b = 1;
        // append elements
        // append vectors B to A
        // loop vector A
        // remove an element from A
        // clear vector A
        // append vector B to A
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
