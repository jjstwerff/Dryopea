// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(dead_code)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]

use crate::keys::{DbRef, Key, copy_key, key_compare, key_hash, store};
use crate::logger::log::info;
use crate::store::Store;
use std::cmp::Ordering;

fn hash_next(cur: u32, length: u32) -> u32 {
    let res = cur + 4;
    if res >= length { 1 } else { res }
}

fn hash_index(hash: u64, elms: u32) -> u32 {
    (1 + (hash % u64::from(elms)) * 4) as u32
}

fn hash_in_range(start: u32, end: u32, test: u32) -> bool {
    if start < end {
        start < test && test <= end
    } else {
        start < test || test <= end
    }
}

fn hash_set(store: &mut Store, pos: u32, hash: u64, rec: u32) {
    let length = store.get_int(pos - 1, 0) as u32;
    let elms = (length - 1) >> 2;
    let mut to = hash_index(hash, elms);
    while store.get_int(pos, to) > 0 {
        to = hash_next(to, length);
    }
    store.set_int(pos, to, rec as i32);
}

fn hash_get(store: &mut Store, rec: u32, claim: u32) -> u32 {
    if rec == 0 || 1 + 4 * rec >= store.get_int(claim, 0) as u32 {
        info!("Record out of range: {rec}");
        u32::MIN
    } else {
        store.get_int(claim, 1 + 4 * rec) as u32
    }
}

pub fn insert(store: &mut Store, hash: fn(u32) -> u64, rec: u32, hash_ref: u32, hash_fld: u32) {
    let mut claim = store.get_int(hash_ref, hash_fld) as u32;
    let length = store.get_int(claim, 4) as u32;
    let elms = (length - 1) >> 2;
    if length > 7 * elms / 10 {
        // rehash
        let new_claim = store.claim(length * 2);
        store.set_int(hash_ref, hash_fld, new_claim as i32);
        for i in 0..elms {
            let v = store.get_int(claim, 1 + 4 * i) as u32;
            if v == 0 {
                continue;
            }
            hash_set(store, new_claim, hash(v), v);
        }
        claim = new_claim;
    }
    hash_set(store, claim, hash(rec), rec);
    store.set_int(claim, 4, length as i32 + 1);
}

pub fn find<F>(store: &Store, hash: u64, equals: F, position: &mut u32, claim: u32) -> u32
where
    F: Fn(u32) -> bool,
{
    let length = store.get_int(claim, 4);
    let elms = (length - 1) >> 2;
    let mut record = hash_index(hash, elms as u32);
    let mut result = store.get_int(claim, record) as u32;
    while result != 0 {
        if equals(result) {
            *position = record;
            return result;
        }
        record = hash_next(record, length as u32);
        result = store.get_int(claim, record) as u32;
    }
    0
}

pub fn hash_find(
    hash_ref: &DbRef,
    stores: &[Store],
    keys: &[Key],
    key: &DbRef,
    k_len: u8,
) -> DbRef {
    let store = &stores[hash_ref.store_nr as usize];
    let claim = store.get_int(hash_ref.rec, hash_ref.pos) as u32;
    let hash_val = key_hash(key, k_len, stores, keys);
    let mut record = DbRef {
        store_nr: hash_ref.store_nr,
        rec: 0,
        pos: 0,
    };
    let room = store.get_int(claim, 0) as u32;
    if room == 0 {
        return record;
    }
    let elms = (room - 1) * 2;
    let mut index = (hash_val % u64::from(elms)) as u32;
    let mut rec_pos = store.get_int(claim, 8 + index * 4) as u32;
    'Record: for _ in 0..elms {
        if rec_pos == 0 {
            break;
        }
        record.rec = rec_pos;
        if key_compare(key, k_len, &record, stores, keys) != Ordering::Equal {
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

pub fn remove(
    store: &mut Store,
    key_hash: u64,
    hash: fn(u32) -> u64,
    equals: fn(u32) -> bool,
    claim: u32,
) {
    let length = store.get_int(claim, 4) as u32;
    let mut current = 0;
    let f = find(store, key_hash, equals, &mut current, claim);
    if f == 0 {
        // element didn't exist somehow
        return;
    }
    let elms = (length - 1) >> 2;
    store.set_int(claim, current, 0);
    let mut scan = hash_next(current, length);
    let mut scan_val = store.get_int(claim, scan) as u32;
    while scan_val != 0 {
        let to = hash_index(hash(scan_val), elms);
        if !hash_in_range(current, scan, to) {
            let val = store.get_int(claim, scan);
            store.set_int(claim, current, val);
            current = scan;
            store.set_int(claim, current, 0);
        }
        scan = hash_next(scan, length);
        scan_val = store.get_int(claim, scan) as u32;
    }
    store.set_int(claim, 4, length as i32 - 1);
}

pub fn hash_validate(hash_ref: &DbRef, key: &DbRef, stores: &mut [Store], keys: &[Key]) {
    let claim = store(hash_ref, stores).get_int(hash_ref.rec, hash_ref.pos) as u32;
    let length = store(hash_ref, stores).get_int(claim, 4) as u32;
    let room = store(hash_ref, stores).get_int(claim, 0) as u32;
    let elms = (room - 1) * 2;
    println!(
        "dump hash length:{length} elms:{elms} {:.2}%",
        100.0 * f64::from(length) / f64::from(elms)
    );
    let mut record = DbRef {
        store_nr: hash_ref.store_nr,
        rec: 0,
        pos: 0,
    };
    let mut l = 0;
    for i in 0..elms {
        let rec = store(hash_ref, stores).get_int(claim, 8 + i * 4) as u32;
        if rec != 0 {
            record.rec = rec;
            l += 1;
            copy_key(&record, key, stores, keys);
            assert_eq!(
                hash_find(hash_ref, stores, keys, key, keys.len() as u8).rec,
                rec,
                "Incorrect entry"
            );
        }
    }
    assert_eq!(length, l, "Incorrect hash length");
}
