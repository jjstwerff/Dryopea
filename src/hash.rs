// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

#![allow(dead_code)]

use crate::logger::log::info;
use crate::store::Store;

fn hash_next(cur: u32, length: u32) -> u32 {
    let res = cur + 4;
    if res >= length {
        1
    } else {
        res
    }
}

fn hash_index(hash: u32, elms: u32) -> u32 {
    1 + (hash % elms) * 4
}

fn hash_in_range(start: u32, end: u32, test: u32) -> bool {
    if start < end {
        start < test && test <= end
    } else {
        start < test || test <= end
    }
}

fn hash_set(store: &mut Store, pos: u32, hash: u32, rec: u32) {
    let length = store.get_int(pos - 1, 0) as u32;
    let elms = (length - 1) >> 2;
    let mut to = hash_index(hash, elms);
    while store.get_int(pos, to as isize) > 0 {
        to = hash_next(to, length);
    }
    store.set_int(pos, to as isize, rec as i32);
}

fn hash_get(store: &mut Store, rec: u32, claim: u32) -> u32 {
    if rec == 0 || 1 + 4 * rec >= store.get_int(claim, 0) as u32 {
        info!("Record out of range: {}", rec);
        u32::MIN
    } else {
        store.get_int(claim, 1 + 4 * rec as isize) as u32
    }
}

pub fn hash_insert(
    store: &mut Store,
    hash: fn(u32) -> u32,
    rec: u32,
    hash_ref: u32,
    hash_fld: isize,
) {
    let mut claim = store.get_int(hash_ref, hash_fld) as u32;
    let length = store.get_int(claim, 4) as u32;
    let elms = (length - 1) >> 2;
    if length > 7 * elms / 10 {
        // rehash
        let new_claim = store.claim(length * 2);
        store.set_int(hash_ref, hash_fld, new_claim as i32);
        for i in 0..elms {
            let v = store.get_int(claim, 1 + 4 * i as isize) as u32;
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

pub fn hash_find(
    store: &mut Store,
    hash: u32,
    equals: fn(u32) -> bool,
    position: &mut u32,
    claim: u32,
) -> u32 {
    let length = store.get_int(claim, 4);
    let elms = (length - 1) >> 2;
    let mut rec = hash_index(hash, elms as u32);
    let mut res = store.get_int(claim, rec as isize) as u32;
    while res != 0 {
        if equals(res) {
            *position = rec;
            return res;
        }
        rec = hash_next(rec, length as u32);
        res = store.get_int(claim, rec as isize) as u32;
    }
    0
}

pub fn hash_remove(
    store: &mut Store,
    key_hash: u32,
    hash: fn(u32) -> u32,
    equals: fn(u32) -> bool,
    claim: u32,
) {
    let length = store.get_int(claim, 4) as u32;
    let mut current = 0;
    let f = hash_find(store, key_hash, equals, &mut current, claim);
    if f == 0 {
        // element didn't exist somehow
        return;
    }
    let elms = (length - 1) >> 2;
    store.set_int(claim, current as isize, 0);
    let mut scan = hash_next(current, length);
    let mut scan_val = store.get_int(claim, scan as isize) as u32;
    while scan_val != 0 {
        let to = hash_index(hash(scan_val), elms);
        if !hash_in_range(current, scan, to) {
            let val = store.get_int(claim, scan as isize);
            store.set_int(claim, current as isize, val);
            current = scan;
            store.set_int(claim, current as isize, 0);
        }
        scan = hash_next(scan, length);
        scan_val = store.get_int(claim, scan as isize) as u32;
    }
    store.set_int(claim, 4, length as i32 - 1);
}

fn hash_validate(_hash: fn(u32) -> u32, _claim: u32) {
    // TODO REL_0012 validate that elements are as close as possible to their 'wanted' positions
    // at least there should be no holes between their found and preferred positions

    // TODO REL_0012 validate the actual size of the hash
    // TODO REL_0012 validate that there is not too little space in the hash
}
