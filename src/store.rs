// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! An in memory store that can be allocated in small steps.
//! A store has a structure of unclaimed data.
//! There can be a mapped file behind each storage instead of only memory.
//!
//! There is always a specific record as the main record of a store describing vectors and indexes with sub-records.
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]

extern crate mmap_storage;
use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

#[allow(dead_code)]
static A: System = System;
const SIGNATURE: u32 = 0x53_74_6f_31;
pub const PRIMARY: u32 = 1;

pub struct Store {
    // format 0 = SIGNATURE, 4 = free_space_index, 8 = record_size, 12 = content
    pub ptr: *mut u8,
    claims: HashSet<u32>,
    size: u32,
    file: Option<mmap_storage::file::Storage>,
    pub(crate) free: bool,
}

impl Debug for Store {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Store[{}]", self.size))
    }
}

impl Clone for Store {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl PartialEq for Store {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Drop for Store {
    fn drop(&mut self) {
        if self.file.is_none() {
            let l = Layout::from_size_align(self.size as usize * 8, 8).expect("Problem");
            unsafe { A.dealloc(self.ptr, l) };
        }
    }
}

#[allow(dead_code)]
impl Store {
    pub fn new(size: u32) -> Store {
        let l = Layout::from_size_align(size as usize * 8, 8).expect("Problem");
        let ptr = unsafe { A.alloc(l) };
        let mut store = Store {
            ptr,
            size,
            claims: HashSet::new(),
            file: None,
            free: true,
        };
        store.claims.insert(1);
        store.init();
        store
    }

    pub fn open(path: &str) -> Store {
        let mut file = mmap_storage::file::Storage::open(path).expect("Opening file");
        let size = (file.capacity() / 8) as u32;
        let init = if size < 1024 {
            file.resize(8192).unwrap();
            true
        } else {
            false
        };
        let ptr = std::ptr::addr_of!(file.as_slice()[0]).cast_mut();
        let mut store = Store {
            file: Some(file),
            ptr,
            claims: HashSet::new(),
            size,
            free: true,
        };
        if init {
            store.init();
        } else {
            assert_eq!(
                unsafe { store.ptr.cast::<u32>().read_unaligned() },
                SIGNATURE,
                "Unknown file format"
            );
            #[cfg(debug_assertions)]
            store.validate(0);
        }
        store
    }

    pub fn init(&mut self) {
        // The normal routines will not write to rec=0, so we write a signature: StoreV01
        unsafe {
            self.ptr.cast::<u32>().write_unaligned(SIGNATURE);
            // The first empty space
            self.ptr.offset(4).cast::<u32>().write_unaligned(1);
        }
        // Indicate the complete store as empty
        *self.addr_mut(1, 0) = -(self.size as i32) + 1;
    }

    /// Claim the space of a record
    /// # Arguments
    /// * `size` - The requested record size in 8 byte words
    pub fn claim(&mut self, size: u32) -> u32 {
        assert!(size > 0, "Incomplete record");
        let req_size = size as i32;
        // search big enough open space: currently very inefficient
        let mut pos = PRIMARY; // primary record location
        let mut last = pos;
        let mut claim = *self.addr::<i32>(pos, 0);
        while pos < self.size && (claim >= 0 || -claim < req_size) {
            last = pos;
            pos += i32::abs(claim) as u32;
            if pos >= self.size {
                break;
            }
            assert_ne!(pos, last, "Inconsistent database zero sized block {pos}");
            claim = *self.addr::<i32>(pos, 0);
        }
        if pos >= self.size {
            let cur = self.size;
            self.resize_store(if claim < 0 {
                (self.size as i32 + size as i32 + claim) as u32
            } else {
                self.size + size
            });
            let increase = (self.size - cur) as i32;
            if claim < 0 {
                *self.addr_mut(last, 0) = claim - increase;
                pos = last;
            } else {
                *self.addr_mut(cur, 0) = -increase;
                pos = cur;
            }
            // TODO REL_0017 register new free space
            #[cfg(debug_assertions)]
            self.validate(0);
        }
        // when too big we split the open space
        let claim = -self.addr::<i32>(pos, 0);
        assert!(claim >= 0, "Claimed space twice {pos}");
        if claim > size as i32 * 4 / 3 {
            *self.addr_mut(pos, 0) = req_size;
            let new_free = pos + size;
            *self.addr_mut(new_free, 0) = req_size - claim;
            // TODO REL_0016 register new free block
        } else {
            *self.addr_mut(pos, 0) = claim;
            // TODO REL_0016 de-register free block
        }
        self.claims.insert(pos);
        pos
    }

    /// Mutate the claimed size of a record
    pub fn resize(&mut self, rec: u32, size: u32) -> u32 {
        let req_size = size as i32;
        // validate if there is enough space for this size of data
        let claim = *self.addr::<i32>(rec, 0);
        if claim >= req_size {
            return rec;
        }
        // validate if the next claim is free and big enough
        let next = rec + claim as u32;
        // TODO REL_0016 introduce special situation where rec was the last allocation in the store
        if next < self.size {
            let next_size = *self.addr::<i32>(next, 0);
            if next_size < 0 && claim - next_size > req_size {
                let act = req_size * 7 / 4;
                if claim - next_size > act {
                    let new_next = rec + act as u32;
                    let new_size = (-next_size) as u32 + next - new_next;
                    *self.addr_mut(rec, 0) = act;
                    *self.addr_mut(new_next, 0) = -(new_size as i32);
                } else {
                    *self.addr_mut(rec, 0) = claim - next_size;
                }
                return rec;
            }
        }
        // claim a new spot and copy content
        let new = self.claim(size);
        self.copy(rec, new);
        self.delete(rec);
        new
    }

    /// Delete a record, this assumes that all links towards this record are already removed
    pub fn delete(&mut self, rec: u32) {
        self.valid(rec, 4);
        let mut claim = *self.addr::<i32>(rec, 0);
        // Try to combine with possibly free spaces after it
        let mut next = *self.addr::<i32>(rec + claim as u32, 0);
        while next < 0 {
            claim -= next;
            next = *self.addr::<i32>(rec + claim as u32, 0);
        }
        *self.addr_mut(rec, 0) = -claim;
        self.claims.remove(&rec);
    }

    /// Validate the store
    pub fn validate(&self, recs: u32) {
        if !cfg!(debug_assertions) {
            return;
        }
        assert!(!self.free, "Using a freed store");
        let mut pos = PRIMARY;
        let mut alloc = 0;
        while pos < self.size {
            let claim = *self.addr::<i32>(pos, 0);
            assert!(
                pos + i32::abs(claim) as u32 <= self.size,
                "Incorrect record {pos} size {}",
                i32::abs(claim)
            );
            if claim < 0 {
                // ignore the open spaces for now, later we want to check if they are part of the open tree.
                pos += (-claim) as u32;
            } else {
                // check the claimed records
                alloc += 1;
                pos += claim as u32;
            }
        }
        assert_eq!(pos, self.size, "Incorrect {pos} size {}", self.size);
        assert!(
            recs == 0 || alloc == recs as usize,
            "Inconsistent number of records: claimed {alloc} walk {recs}"
        );
    }

    pub fn len(&self) -> u32 {
        self.size
    }

    /// Change the store size, do not mutate content
    fn resize_store(&mut self, to_size: u32) {
        if to_size < self.size {
            return;
        }
        let inc = self.size * 7 / 3;
        let size = if to_size > inc { to_size } else { inc };
        if let Some(f) = &mut self.file {
            f.resize(size as usize * 8).expect("Resize");
            self.ptr = std::ptr::addr_of!(f.as_slice()[0]).cast_mut();
            self.size = size;
            return;
        }
        let bytes = size as usize * 8;
        let l = Layout::from_size_align(1, 8).expect("Problem");
        self.ptr = unsafe { A.realloc(self.ptr, l, bytes) };
        self.size = size;
    }

    #[inline]
    pub fn addr<T>(&self, rec: u32, fld: u32) -> &T {
        unsafe {
            let off = self.ptr.offset(rec as isize * 8 + fld as isize).cast::<T>();
            off.as_mut().expect("Reference")
        }
    }

    #[inline]
    pub fn addr_mut<T>(&mut self, rec: u32, fld: u32) -> &mut T {
        unsafe {
            let off = self.ptr.offset(rec as isize * 8 + fld as isize).cast::<T>();
            off.as_mut().expect("Reference")
        }
    }

    pub fn buffer(&mut self, rec: u32) -> &mut [u8] {
        let size = *self.addr::<u32>(rec, 0) as usize * 8;
        unsafe {
            let p = self.ptr.offset(rec as isize * 8 + 8);
            std::slice::from_raw_parts_mut(p, size)
        }
    }

    /// Try to validate a record reference as much as possible.
    /// Complete validations are only done in 'test' mode.
    pub fn valid(&self, rec: u32, fld: u32) -> bool {
        debug_assert!(self.claims.contains(&rec), "Unknown record {rec}");
        debug_assert!(
            fld >= 4 && fld < 8 * *self.addr::<i32>(rec, 0) as u32,
            "Fld {fld} is outside of record {rec} size {}",
            8 * *self.addr::<i32>(rec, 0) as u32
        );
        debug_assert!(
            rec != 0 && u64::from(rec) * 8 + u64::from(fld) <= u64::from(self.size) * 8,
            "Reading outside store ({rec}.{fld}) > {}",
            self.size
        );
        {
            if fld != 0 {
                let size: i32 = *self.addr(rec, 0);
                // The first 4 positions are reserved for the record size
                debug_assert!(
                    rec + size as u32 <= self.size,
                    "Inconsistent record {rec} size {size} > {}",
                    self.size
                );
                debug_assert!(
                    fld >= 4,
                    "Field {fld} too low, overlapping with size on ({rec}.{fld})"
                );
                debug_assert!(
                    size >= 1 && fld <= size as u32 * 8,
                    "Reading fields outside record ({rec}.{fld}) > {size}"
                );
            }
        }
        true
    }

    #[inline]
    /// Copy only the content of a record, not the claimed size
    fn copy(&self, rec: u32, into: u32) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.ptr.offset(rec as isize * 8 + 4),
                self.ptr.offset(into as isize * 8 + 4),
                *self.addr::<i32>(rec, 0) as usize * 8 - 4,
            );
        }
    }

    #[inline]
    pub fn zero_fill(&self, rec: u32) {
        unsafe {
            std::ptr::write_bytes(
                self.ptr.offset(rec as isize * 8 + 4),
                0,
                *self.addr::<i32>(rec, 0) as usize * 8 - 4,
            );
        }
    }

    #[inline]
    pub fn copy_block(
        &mut self,
        from_rec: u32,
        from_pos: isize,
        to_rec: u32,
        to_pos: isize,
        size: isize,
    ) {
        unsafe {
            std::ptr::copy(
                self.ptr.offset(from_rec as isize * 8 + from_pos),
                self.ptr.offset(to_rec as isize * 8 + to_pos),
                size as usize,
            );
        }
    }

    #[inline]
    pub fn copy_block_between(
        &self,
        from_rec: u32,
        from_pos: isize,
        to_store: &mut Store,
        to_rec: u32,
        to_pos: isize,
        len: isize,
    ) {
        unsafe {
            std::ptr::copy(
                self.ptr.offset(from_rec as isize * 8 + from_pos),
                to_store.ptr.offset(to_rec as isize * 8 + to_pos),
                len as usize,
            );
        }
    }

    #[inline]
    pub fn get_int(&self, rec: u32, fld: u32) -> i32 {
        if rec != 0 && self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_int(&mut self, rec: u32, fld: u32, val: i32) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_long(&self, rec: u32, fld: u32) -> i64 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            i64::MIN
        }
    }

    #[inline]
    pub fn set_long(&mut self, rec: u32, fld: u32, val: i64) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_short(&self, rec: u32, fld: u32, min: i32) -> i32 {
        if self.valid(rec, fld) {
            let read: u16 = *self.addr(rec, fld);
            if read != 0 {
                i32::from(read) + min - 1
            } else {
                i32::MIN
            }
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_short(&mut self, rec: u32, fld: u32, min: i32, val: i32) -> bool {
        if self.valid(rec, fld) {
            if val == i32::MIN {
                *self.addr_mut(rec, fld) = 0;
                true
            } else if val >= min || val <= min + 65536 {
                *self.addr_mut(rec, fld) = (val - min + 1) as u16;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    #[inline]
    pub fn get_byte(&self, rec: u32, fld: u32, min: i32) -> i32 {
        if self.valid(rec, fld) {
            let read: u8 = *self.addr(rec, fld);
            i32::from(read) + min
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_byte(&mut self, rec: u32, fld: u32, min: i32, val: i32) -> bool {
        if self.valid(rec, fld) {
            if val == i32::MIN {
                *self.addr_mut(rec, fld) = 255;
                true
            } else if val >= min || val <= min + 256 {
                *self.addr_mut(rec, fld) = (val - min) as u8;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    #[inline]
    pub fn get_str<'a>(&self, rec: u32) -> &'a str {
        if rec == 0 || rec > i32::MAX as u32 {
            return "";
        }
        let len = self.get_int(rec, 4);
        #[cfg(debug_assertions)]
        assert!(
            len >= 0 && len <= self.addr::<i32>(rec, 0) * 8,
            "Inconsistent text store"
        );
        assert!(
            (len / 8) as u32 + rec <= self.size,
            "Inconsistent text store"
        );
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.ptr.offset(rec as isize * 8 + 8),
                len as usize,
            ))
        }
    }

    #[inline]
    pub fn set_str(&mut self, val: &str) -> u32 {
        let res = self.claim(((val.len() + 15) / 8) as u32);
        self.set_int(res, 4, val.len() as i32);
        unsafe {
            std::ptr::copy_nonoverlapping(
                val.as_ptr(),
                self.ptr.offset(res as isize * 8 + 8),
                val.len(),
            );
        }
        res
    }

    #[inline]
    pub fn set_str_ptr(&mut self, ptr: *const u8, len: usize) -> u32 {
        let res = self.claim(((len + 15) / 8) as u32);
        self.set_int(res, 4, len as i32);
        unsafe {
            std::ptr::copy_nonoverlapping(ptr, self.ptr.offset(res as isize * 8 + 8), len);
        }
        res
    }

    #[inline]
    pub fn append_str(&mut self, record: u32, val: &str) -> u32 {
        let prev = self.get_int(record, 4);
        let result = self.resize(record, (prev as usize + val.len()).div_ceil(8) as u32);
        unsafe {
            std::ptr::copy_nonoverlapping(
                val.as_ptr(),
                self.ptr.offset(result as isize * 8 + 8 + prev as isize),
                val.len(),
            );
        }
        result
    }

    #[inline]
    pub fn get_boolean(&self, rec: u32, fld: u32, mask: u8) -> bool {
        if self.valid(rec, fld) {
            let read: u8 = *self.addr(rec, fld);
            (read & mask) > 0
        } else {
            false
        }
    }

    #[inline]
    pub fn set_boolean(&mut self, rec: u32, fld: u32, mask: u8, val: bool) -> bool {
        if self.valid(rec, fld) {
            let current: u8 = *self.addr(rec, fld);
            let mut write = current & !mask;
            if val {
                write |= mask;
            }
            *self.addr_mut(rec, fld) = write;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_float(&self, rec: u32, fld: u32) -> f64 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            f64::NAN
        }
    }

    #[inline]
    pub fn set_float(&mut self, rec: u32, fld: u32, val: f64) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_single(&self, rec: u32, fld: u32) -> f32 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            f32::NAN
        }
    }

    #[inline]
    pub fn set_single(&mut self, rec: u32, fld: u32, val: f32) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }
}
