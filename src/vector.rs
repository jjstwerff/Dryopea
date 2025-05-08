// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]

use crate::keys;
use crate::keys::{Content, DbRef, Key};
use crate::store::Store;
use std::cmp::Ordering;

// TODO change slice to its own vector on updating it
pub fn insert_vector(db: &DbRef, size: u32, index: i32, stores: &mut [Store]) -> DbRef {
    let len = length_vector(db, stores);
    let real = if index < 0 { index + len as i32 } else { index };
    if real < 0 || real > len as i32 {
        return DbRef {
            store_nr: db.store_nr,
            rec: 0,
            pos: 0,
        };
    }
    let store = keys::mut_store(db, stores);
    let mut vec_rec = store.get_int(db.rec, db.pos) as u32;
    let new_length;
    if vec_rec == 0 {
        // claim a new array with minimal 11 elements
        vec_rec = store.claim((11 * size + 15) / 8);
        store.set_int(db.rec, db.pos, vec_rec as i32);
        new_length = 1;
    } else {
        new_length = len + 1;
        let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
        if new_vec != vec_rec {
            store.set_int(db.rec, db.pos, new_vec as i32);
            vec_rec = new_vec;
        }
        store.copy_block(
            new_vec,
            8 + size as isize * real as isize,
            new_vec,
            8 + size as isize * (real as isize + 1),
            (len as isize - real as isize) * size as isize,
        );
    }
    store.set_int(vec_rec, 4, new_length as i32);
    DbRef {
        store_nr: db.store_nr,
        rec: vec_rec,
        pos: 8 + real as u32 * size,
    }
}

pub fn vector_append(db: &DbRef, add: u32, size: u32, stores: &mut [Store]) -> DbRef {
    let store = keys::mut_store(db, stores);
    let mut vec_rec = store.get_int(db.rec, db.pos) as u32;
    let new_length;
    if vec_rec == 0 {
        // claim a new array with minimal 11 elements
        vec_rec = store.claim(((add + 10) * size + 15) / 8);
        store.set_int(db.rec, db.pos, vec_rec as i32);
        new_length = add;
    } else {
        new_length = add + store.get_int(vec_rec, 4) as u32;
        let new_vec = store.resize(vec_rec, (new_length * size + 15) / 8);
        if new_vec != vec_rec {
            store.set_int(db.rec, db.pos, new_vec as i32);
            vec_rec = new_vec;
        }
    }
    store.set_int(vec_rec, 4, new_length as i32);
    DbRef {
        store_nr: db.store_nr,
        rec: vec_rec,
        pos: 8 + (new_length - add) * size,
    }
}

pub fn sorted_new(db: &DbRef, size: u32, stores: &mut [Store]) -> DbRef {
    // Keep an extra record between the current and the new one.
    // This is needed to allow to create a new open space to move the new record to.
    let store = keys::mut_store(db, stores);
    let mut sorted_rec = store.get_int(db.rec, db.pos) as u32;
    // Claim a record at the back of the current structure or create a new structure.
    if sorted_rec == 0 {
        sorted_rec = store.claim((12 * size + 15) / 8);
        store.set_int(db.rec, db.pos, sorted_rec as i32);
        // Set initial length to 0
        store.set_int(sorted_rec, 4, 0);
        // return the first record
        DbRef {
            store_nr: db.store_nr,
            rec: sorted_rec,
            pos: 8,
        }
    } else {
        let length = store.get_int(sorted_rec, 4) as u32;
        let new_sorted = store.resize(sorted_rec, ((length + 2) * size + 15) / 8);
        if new_sorted != sorted_rec {
            store.set_int(db.rec, db.pos, new_sorted as i32);
            sorted_rec = new_sorted;
        }
        // return the last record inside the allocation
        DbRef {
            store_nr: db.store_nr,
            rec: sorted_rec,
            pos: 8 + (length + 1) * size,
        }
    }
}

pub fn sorted_finish(sorted: &DbRef, size: u32, keys: &[Key], stores: &mut [Store]) {
    let sorted_rec = keys::store(sorted, stores).get_int(sorted.rec, sorted.pos) as u32;
    let length = keys::store(sorted, stores).get_int(sorted_rec, 4) as u32;
    if length == 0 {
        // we do not have to reorder the first inserted record; set length to 1
        keys::mut_store(sorted, stores).set_int(sorted_rec, 4, 1);
        return;
    }
    let latest_pos = 8 + (length + 1) * size;
    let rec = DbRef {
        store_nr: sorted.store_nr,
        rec: sorted_rec,
        pos: latest_pos,
    };
    let key = keys::get_key(&rec, stores, keys);
    let pos = sorted_find(sorted, true, size as u16, stores, keys, &key);
    let store = keys::mut_store(sorted, stores);
    let end_pos = 8 + length * size;
    if pos < end_pos {
        // create space to write the new record to
        store.copy_block(
            sorted_rec,
            pos as isize,
            sorted_rec,
            (pos + size) as isize,
            (end_pos - pos) as isize,
        );
    }
    // move last record to the found correct position
    store.copy_block(
        sorted_rec,
        latest_pos as isize,
        sorted_rec,
        pos as isize,
        size as isize,
    );
    store.set_int(sorted_rec, 4, (length + 1) as i32);
}

pub fn ordered_finish(sorted: &DbRef, rec: &DbRef, keys: &[Key], stores: &mut [Store]) {
    let rec_ref = sorted_new(sorted, 4, stores);
    let sorted_rec = keys::store(sorted, stores).get_int(sorted.rec, sorted.pos) as u32;
    let length = keys::store(sorted, stores).get_int(sorted_rec, 4) as u32;
    if length == 0 {
        // we do not have to reorder the first inserted record, set length to 1
        keys::mut_store(sorted, stores).set_int(sorted_rec, 4, 1);
        keys::mut_store(sorted, stores).set_int(sorted_rec, rec_ref.pos, rec.rec as i32);
        return;
    }
    let key = keys::get_key(rec, stores, keys);
    let p = ordered_find(sorted, true, stores, keys, &key);
    let latest_pos = 8 + length * 4;
    if latest_pos > p {
        keys::mut_store(sorted, stores).copy_block(
            sorted_rec,
            p as isize,
            sorted_rec,
            p as isize + 4,
            (latest_pos - p) as isize,
        );
    }
    keys::mut_store(&rec_ref, stores).set_int(sorted_rec, p, rec.rec as i32);
    keys::mut_store(sorted, stores).set_int(sorted_rec, 4, 1 + length as i32);
}

#[must_use]
#[allow(clippy::cast_sign_loss)]
pub fn length_vector(db: &DbRef, stores: &[Store]) -> u32 {
    if db.rec == 0 || db.pos == 0 {
        return 0;
    }
    let store = keys::store(db, stores);
    let v_rec = store.get_int(db.rec, db.pos) as u32;
    if v_rec == 0 {
        0
    } else {
        store.get_int(v_rec, 4) as u32
    }
}

pub fn clear_vector(db: &DbRef, stores: &mut [Store]) {
    let store = keys::mut_store(db, stores);
    let v_rec = store.get_int(db.rec, db.pos) as u32;
    if v_rec != 0 {
        // Only set size of the vector to 0
        // TODO when the main path to a separate allocated objects: remove these
        // TODO lower string reference counts where needed
        store.set_int(v_rec, 4, 0);
    }
}

#[must_use]
pub fn get_vector(db: &DbRef, size: u32, from: i32, stores: &[Store]) -> DbRef {
    let store = keys::store(db, stores);
    if from == i32::MIN {
        return DbRef {
            store_nr: db.store_nr,
            rec: 0,
            pos: 0,
        };
    }
    let v_rec = store.get_int(db.rec, db.pos) as u32;
    let l = length_vector(db, stores);
    let f = if from < 0 { from + l as i32 } else { from };
    if f < 0 || f >= l as i32 {
        DbRef {
            store_nr: db.store_nr,
            rec: 0,
            pos: 0,
        }
    } else {
        DbRef {
            store_nr: db.store_nr,
            rec: v_rec,
            pos: 8 + size * f as u32,
        }
    }
}

pub fn remove_vector(db: &DbRef, size: u32, index: u32, stores: &mut [Store]) -> bool {
    let len = length_vector(db, stores);
    let store = keys::mut_store(db, stores);
    let vec_rec = store.get_int(db.rec, db.pos) as u32;
    if index >= len || vec_rec == 0 {
        return false;
    }
    store.copy_block(
        vec_rec,
        8 + size as isize * (index as isize + 1),
        vec_rec,
        8 + size as isize * index as isize,
        (len as isize - index as isize) * size as isize,
    );
    store.set_int(vec_rec, 4, len as i32 - 1);
    true
}

#[must_use]
pub fn sorted_find(
    sorted: &DbRef,
    before: bool,
    size: u16,
    stores: &[Store],
    keys: &[Key],
    key: &[Content],
) -> u32 {
    let store = keys::store(sorted, stores);
    let sorted_rec = store.get_int(sorted.rec, sorted.pos) as u32;
    let length = store.get_int(sorted_rec, 4) as u32;
    let mut found = 0;
    let mut result = DbRef {
        store_nr: sorted.store_nr,
        rec: sorted_rec,
        pos: 8 + found * u32::from(size),
    };
    let mut left = 0;
    let mut right = length - 1;
    while left != right {
        found = (left + right + 1) >> 1;
        result.pos = 8 + found * u32::from(size);
        let cmd = keys::key_compare(key, &result, stores, keys);
        if (cmd == Ordering::Less && before) || (cmd != Ordering::Greater && !before) {
            right = found - 1;
        } else {
            left = found;
        }
    }
    result.pos = 8 + left * u32::from(size);
    let cmp = keys::key_compare(key, &result, stores, keys);
    if cmp == Ordering::Greater {
        8 + (left + 1) * u32::from(size)
    } else if cmp == Ordering::Equal && before {
        // TODO this can potentially be below 0 on the returned first record with size > 8
        8 + left * u32::from(size) - u32::from(size)
    } else {
        result.pos
    }
}

#[must_use]
pub fn ordered_find(
    sorted: &DbRef,
    before: bool,
    stores: &[Store],
    keys: &[Key],
    key: &[Content],
) -> u32 {
    let store = keys::store(sorted, stores);
    let sorted_rec = store.get_int(sorted.rec, sorted.pos) as u32;
    let length = store.get_int(sorted_rec, 4) as u32;
    let mut found;
    let mut result = DbRef {
        store_nr: sorted.store_nr,
        rec: 0,
        pos: 0,
    };
    if sorted_rec == 0 {
        return 0;
    }
    let mut left = 0;
    let mut right = length - 1;
    while left != right {
        found = (left + right + 1) >> 1;
        result.rec = store.get_int(sorted_rec, 8 + found * 4) as u32;
        let cmp = keys::key_compare(key, &result, stores, keys);
        if (cmp == Ordering::Less && before) || (cmp != Ordering::Greater && !before) {
            right = found - 1;
        } else {
            left = found;
        }
    }
    result.rec = store.get_int(sorted_rec, 8 + left * 4) as u32;
    let cmp = keys::key_compare(key, &result, stores, keys);
    left * 4
        + if before && cmp == Ordering::Equal {
            4
        } else if cmp == Ordering::Greater {
            12
        } else {
            8
        }
}

pub fn vector_next(data: &DbRef, pos: &mut i32, size: u16, stores: &[Store]) {
    let rec = keys::store(data, stores).get_int(data.rec, data.pos) as u32;
    if rec == 0 {
        *pos = i32::MAX;
        return;
    }
    let length = keys::store(data, stores).get_int(rec, 4);
    if *pos == i32::MAX && length != 0 {
        *pos = 8;
    } else if length != 0 && *pos < 8 + (length - 1) * i32::from(size) {
        *pos += i32::from(size);
    } else {
        *pos = i32::MAX;
    }
}
