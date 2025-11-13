// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]

use crate::keys;
use crate::keys::{Content, DbRef, Key};
use crate::store::Store;
use std::cmp::Ordering;

pub fn add(hash: &DbRef, rec: &DbRef, stores: &mut [Store], keys: &[Key]) {
    let mut claim = keys::store(hash, stores).get_int(hash.rec, hash.pos) as u32;
    let length = if claim == 0 {
        claim = keys::mut_store(hash, stores).claim(9);
        keys::mut_store(hash, stores).zero_fill(claim);
        keys::mut_store(hash, stores).set_int(hash.rec, hash.pos, claim as i32);
        0
    } else {
        keys::store(hash, stores).get_int(claim, 4) as u32
    };
    let room = *keys::store(hash, stores).addr::<i32>(claim, 0) as u32;
    let elms = (room - 1) * 2;
    if (length * 14 / 16) + 1 >= room {
        // rehash
        let mut move_rec = DbRef {
            store_nr: hash.store_nr,
            rec: 0,
            pos: 0,
        };
        let new_claim = keys::mut_store(hash, stores).claim(room * 2 - 1);
        keys::mut_store(hash, stores).zero_fill(new_claim);
        for i in 0..elms {
            let v = keys::store(hash, stores).get_int(claim, 8 + 4 * i) as u32;
            if v == 0 {
                continue;
            }
            move_rec.rec = v;
            move_rec.pos = 8;
            hash_set(new_claim, &move_rec, stores, keys);
        }
        claim = new_claim;
        keys::mut_store(hash, stores).set_int(hash.rec, hash.pos, claim as i32);
    }
    hash_set(claim, rec, stores, keys);
    keys::mut_store(rec, stores).set_int(claim, 4, length as i32 + 1);
    // hash_validate(hash, key, stores, keys);
}

fn hash_set(claim: u32, rec: &DbRef, stores: &mut [Store], keys: &[Key]) {
    let index = hash_free_pos(claim, rec, stores, keys);
    keys::mut_store(rec, stores).set_int(claim, index, rec.rec as i32);
}

fn hash_free_pos(claim: u32, rec: &DbRef, stores: &[Store], keys: &[Key]) -> u32 {
    let room = *keys::store(rec, stores).addr::<i32>(claim, 0) as u32;
    let elms = (room - 1) * 2;
    let hash_val = keys::hash(rec, stores, keys);
    let mut index = (hash_val % u64::from(elms)) as u32;
    for _ in 0..elms {
        if keys::store(rec, stores).get_int(claim, 8 + index * 4) == 0 {
            break;
        }
        index += 1;
        if index >= elms {
            index = 0;
        }
    }
    8 + index * 4
}

#[must_use]
pub fn find(hash_ref: &DbRef, stores: &[Store], keys: &[Key], key: &[Content]) -> DbRef {
    let store = &stores[hash_ref.store_nr as usize];
    let claim = store.get_int(hash_ref.rec, hash_ref.pos) as u32;
    let mut record = DbRef {
        store_nr: hash_ref.store_nr,
        rec: 0,
        pos: 0,
    };
    if claim == 0 {
        return record;
    }
    let room = *store.addr::<i32>(claim, 0) as u32;
    if room == 0 {
        return record;
    }
    let elms = (room - 1) * 2;
    let hash_val = keys::key_hash(key);
    let mut index = (hash_val % u64::from(elms)) as u32;
    let mut rec_pos = store.get_int(claim, 8 + index * 4) as u32;
    'Record: for _ in 0..elms {
        if rec_pos == 0 {
            break;
        }
        record.rec = rec_pos;
        record.pos = 8;
        if keys::key_compare(key, &record, stores, keys) != Ordering::Equal {
            index += 1;
            if index >= elms {
                index = 0;
            }
            rec_pos = store.get_int(claim, 8 + index * 4) as u32;
            continue 'Record;
        }
        break;
    }
    record
}

pub fn remove(hash_ref: &DbRef, rec: &DbRef, stores: &mut [Store], keys: &[Key]) {
    let claim = keys::store(hash_ref, stores).get_int(hash_ref.rec, hash_ref.pos) as u32;
    let length = keys::store(hash_ref, stores).get_int(claim, 4);
    if length == 0 {
        return;
    }
    let room = keys::store(hash_ref, stores).get_int(claim, 0) as u32;
    let elms = (room - 1) * 2;
    // get the index of the element, and set it to 0
    let mut index = hash_free_pos(claim, rec, stores, keys);
    keys::mut_store(hash_ref, stores).set_int(rec.rec, index, 0);
    let mut index_val = 0;
    let mut current = 0;
    // move from there and determine if the next elements could get to a better position
    for _ in 0..elms {
        if index_val == 0 {
            break;
        }
        let next = DbRef {
            store_nr: rec.store_nr,
            rec: index_val,
            pos: 8,
        };
        let to = (8 + (keys::hash(&next, stores, keys) % u64::from(elms)) * 4) as u32;
        let right = if index < to {
            current < index && index <= to
        } else {
            current < index || index <= to
        };
        if !right {
            let val = keys::store(hash_ref, stores).get_int(claim, index);
            keys::mut_store(hash_ref, stores).set_int(claim, current, val);
            current = index;
            keys::mut_store(hash_ref, stores).set_int(claim, index, 0);
        }
        if index + 4 >= elms {
            index = 8;
        } else {
            index += 4;
        }
        index_val = keys::store(hash_ref, stores).get_int(claim, index) as u32;
    }
    keys::mut_store(hash_ref, stores).set_int(claim, 4, length - 1);
}

/**
Check the allocations and structure of the hash table.
# Panics
When the structure is not correctly filled
*/
pub fn validate(hash_ref: &DbRef, stores: &[Store], keys: &[Key]) {
    let claim = keys::store(hash_ref, stores).get_int(hash_ref.rec, hash_ref.pos) as u32;
    let length = keys::store(hash_ref, stores).get_int(claim, 4) as u32;
    let room = *keys::store(hash_ref, stores).addr::<i32>(claim, 0) as u32;
    let elms = (room - 1) * 2;
    let mut record = DbRef {
        store_nr: hash_ref.store_nr,
        rec: 0,
        pos: 0,
    };
    let mut l = 0;
    for i in 0..elms {
        let rec = keys::store(hash_ref, stores).get_int(claim, 8 + i * 4) as u32;
        if rec != 0 {
            record.rec = rec;
            record.pos = 8;
            l += 1;
            let key = keys::get_key(&record, stores, keys);
            assert_eq!(
                find(hash_ref, stores, keys, &key).rec,
                rec,
                "Incorrect entry"
            );
        }
    }
    assert_eq!(length, l, "Incorrect hash length");
}
