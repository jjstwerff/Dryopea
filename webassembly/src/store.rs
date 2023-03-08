// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! An in memory store that can be allocated in small steps.
//! A store has a structure of unclaimed data.
//! There can be a mapped file behind each storage instead of only memory.
//!
//! There is always a specific record as the main record of a store describing vectors and indexes with sub-records.

use std::alloc::{GlobalAlloc, Layout, System};

#[allow(dead_code)]
static A: System = System;
const SIGNATURE: u32 = 0x53_74_6f_31;
pub const PRIMARY: u32 = 1;

pub struct Store {
    // format 0 = SIGNATURE, 4 = free_space_index, 8 = record_size, 12 = content
    ptr: *mut u8,
    size: u32,
}

impl PartialEq for Store {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Drop for Store {
    fn drop(&mut self) {
        let l = Layout::from_size_align(self.size as usize * 8, 8).expect("Problem");
        unsafe { A.dealloc(self.ptr, l) };
    }
}

#[allow(dead_code)]
impl Store {
    pub fn new(size: u32) -> Store {
        let l = Layout::from_size_align(size as usize * 8, 8).expect("Problem");
        let ptr = unsafe { A.alloc(l) };
        let mut store = Store { ptr, size };
        store.init();
        store
    }

    fn init(&mut self) {
        // The normal routines will not write to rec=0 so we write a signature: StoreV01
        unsafe {
            (self.ptr as *mut u32).write(SIGNATURE);
            // The first empty space
            (self.ptr.offset(4) as *mut u32).write(1);
        }
        // Indicate the complete store as empty
        if !self.set_int(1, 0, -(self.size as i32) + 1) {
            panic!("Could not init");
        }
    }

    /// Claim the space of a record
    /// # Arguments
    /// * `size` - The requested record size in 8 byte words
    pub fn claim(&mut self, size: u32) -> u32 {
        let req_size = size as i32;
        // search big enough open space: currently very inefficient
        let mut pos = PRIMARY; // primary record location
        let mut last = pos;
        let mut claim = self.get_int(pos, 0);
        while pos < self.size && (claim >= 0 || -claim < req_size) {
            last = pos;
            pos += i32::abs(claim) as u32;
            if pos >= self.size {
                claim = 0;
                break;
            }
            if pos == last {
                panic!("Inconsistent database zero sized block {}", pos)
            }
            claim = self.get_int(pos, 0);
        }
        if pos + size > self.size {
            let cur = self.size;
            self.resize_store(if claim < 0 {
                (self.size as i32 + size as i32 + claim) as u32
            } else {
                self.size + size
            });
            let increase = (self.size - cur) as i32;
            if claim < 0 {
                self.set_int(last, 0, claim - increase);
                pos = last;
            } else {
                self.set_int(cur, 0, increase);
                pos = cur;
            }
            // TODO REL_0017 register new free space
            #[cfg(debug_assertions)]
            self.validate(0);
            if claim <= 0 {
                return pos;
            }
        }
        // when too big we split the open space
        let claim = -self.get_int(pos, 0);
        if claim < 0 {
            panic!("Claimed space twice {}", pos);
        }
        if claim > size as i32 * 4 / 3 {
            self.set_int(pos, 0, req_size);
            let new_free = pos + size;
            self.set_int(new_free, 0, req_size - claim);
            // TODO REL_0016 register new free block
        } else {
            self.set_int(pos, 0, claim);
            // TODO REL_0016 de-register free block
        }
        pos
    }

    /// Mutate the claimed size of a record
    pub fn resize(&mut self, rec: u32, size: u32) -> u32 {
        let req_size = size as i32;
        // validate if there is enough space for this size of data
        let claim = self.get_int(rec, 0);
        if claim >= req_size {
            return rec;
        }
        // validate if the next claim is free and big enough
        let next = rec + claim as u32;
        // TODO REL_0016 introduce special situation where rec was the last allocation in the store
        if next < self.size {
            let next_size = self.get_int(next, 0);
            if next_size < 0 && claim - next_size > req_size {
                if claim - next_size > req_size * 4 / 3 {
                    let next = claim as u32 + size;
                    self.set_int(rec, 0, req_size);
                    self.set_int(next, 0, next_size - claim - req_size);
                } else {
                    self.set_int(rec, 0, claim + next_size);
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
        let mut claim = self.get_int(rec, 0);
        // Try to combine with possibly free spaces after it
        let mut next = self.get_int(rec + claim as u32, 0);
        while next < 0 {
            claim -= next;
            next = self.get_int(rec + claim as u32, 0);
        }
        self.set_int(rec, 0, -claim);
    }

    /// Validate the store, the all errors are panics for now.
    pub fn validate(&self, recs: u32) {
        let mut pos = PRIMARY;
        let mut alloc = 0;
        while pos < self.size {
            let claim = self.get_int(pos, 0);
            if pos + i32::abs(claim) as u32 > self.size {
                panic!("Incorrect record {} size {}", pos, i32::abs(claim));
            }
            if claim < 0 {
                // ignore the open spaces for now, later we want to check if they are part of the open tree.
                pos += (-claim) as u32;
            } else {
                // check the claimed records
                alloc += 1;
                pos += claim as u32;
            }
        }
        if pos != self.size {
            panic!("Incorrect {} size {}", pos, self.size);
        }
        if recs != 0 && alloc != recs as usize {
            panic!(
                "Inconsistent number of records: claimed {} walk {}",
                alloc, recs
            );
        }
    }

    pub fn len(&self) -> u32 {
        self.size
    }

    /// Change the store size, do not mutate content
    fn resize_store(&mut self, to_size: u32) {
        if to_size < self.size {
            return;
        }
        let inc = self.size * 3 / 2;
        let size = if to_size > inc { to_size } else { inc };
        let bytes = size as usize * 8;
        let l = Layout::from_size_align(1, 8).expect("Problem");
        self.ptr = unsafe { A.realloc(self.ptr, l, bytes) };
        self.size = size;
    }

    #[inline]
    fn addr<T>(&self, rec: u32, fld: isize) -> &T {
        unsafe {
            let off = self.ptr.offset(rec as isize * 8 + fld) as *mut T;
            off.as_mut().expect("Reference")
        }
    }

    #[inline]
    fn addr_mut<T>(&mut self, rec: u32, fld: isize) -> &mut T {
        unsafe {
            let off = self.ptr.offset(rec as isize * 8 + fld) as *mut T;
            off.as_mut().expect("Reference")
        }
    }

    /// Try to validate a record reference as much as possible.
    /// Complete validations are only done in 'test' mode.
    pub fn valid(&self, rec: u32, fld: isize) -> bool {
        #[cfg(debug_assertions)]
        if rec == 0 || rec as u64 * 8 + fld as u64 > self.size as u64 * 8 {
            panic!("Reading outside store ({}.{}) > {}", rec, fld, self.size);
        }
        #[cfg(debug_assertions)]
        {
            if fld != 0 {
                let size: i32 = *self.addr(rec, 0);
                // The first 4 positions are reserved for the record size
                if size < 1 || rec + size as u32 > self.size || fld < 4 {
                    panic!("Reading fields outside record ({}.{}) > {}", rec, fld, size);
                }
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
                self.get_int(rec, 0) as usize * 8 - 4,
            );
        }
    }

    #[inline]
    pub fn move_content(&self, rec: u32, pos: isize, to: isize, len: isize) {
        unsafe {
            std::ptr::copy(
                self.ptr.offset(rec as isize * 8 + 8 + pos),
                self.ptr.offset(rec as isize * 8 + 8 + to),
                len as usize,
            )
        }
    }

    #[inline]
    pub fn get_int(&self, rec: u32, fld: isize) -> i32 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_int(&mut self, rec: u32, fld: isize, val: i32) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_long(&self, rec: u32, fld: isize) -> i64 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            i64::MIN
        }
    }

    #[inline]
    pub fn set_long(&mut self, rec: u32, fld: isize, val: i64) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_short(&self, rec: u32, fld: isize, min: i32) -> i32 {
        if self.valid(rec, fld) {
            let read: u16 = *self.addr(rec, fld);
            if read != 0 {
                read as i32 + min - 1
            } else {
                i32::MIN
            }
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_short(&mut self, rec: u32, fld: isize, min: i32, val: i32) -> bool {
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
    pub fn get_byte(&self, rec: u32, fld: isize, min: i32) -> i32 {
        if self.valid(rec, fld) {
            let read: u8 = *self.addr(rec, fld);
            if read != 0 {
                read as i32 + min - 1
            } else {
                i32::MIN
            }
        } else {
            i32::MIN
        }
    }

    #[inline]
    pub fn set_byte(&mut self, rec: u32, fld: isize, min: i32, val: i32) -> bool {
        if self.valid(rec, fld) {
            if val == i32::MIN {
                *self.addr_mut(rec, fld) = 0;
                true
            } else if val >= min || val <= min + 256 {
                *self.addr_mut(rec, fld) = (val - min + 1) as u8;
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
        let len = self.get_int(rec, 4);
        #[cfg(debug_assertions)]
        if len < 0 || len > self.get_int(rec, 0) * 8 {
            panic!("Inconsistent text store")
        }
        if (len / 8) as u32 + rec > self.size {
            panic!("Inconsistent text store")
        }
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
    pub fn append_str(&mut self, rec: u32, val: &str) -> u32 {
        let prev = self.get_int(rec, 4);
        let res = self.resize(rec, ((prev as usize + val.len() + 7) / 8) as u32);
        unsafe {
            std::ptr::copy_nonoverlapping(
                val.as_ptr(),
                self.ptr.offset(res as isize * 8 + 8 + prev as isize),
                val.len(),
            );
        }
        res
    }

    #[inline]
    pub fn get_boolean(&self, rec: u32, fld: isize, mask: u8) -> bool {
        if self.valid(rec, fld) {
            let read: u8 = *self.addr(rec, fld);
            (read & mask) > 0
        } else {
            false
        }
    }

    #[inline]
    pub fn set_boolean(&mut self, rec: u32, fld: isize, mask: u8, val: bool) -> bool {
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
    pub fn get_float(&self, rec: u32, fld: isize) -> f64 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            f64::NAN
        }
    }

    #[inline]
    pub fn set_float(&mut self, rec: u32, fld: isize, val: f64) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_single(&self, rec: u32, fld: isize) -> f32 {
        if self.valid(rec, fld) {
            *self.addr(rec, fld)
        } else {
            f32::NAN
        }
    }

    #[inline]
    pub fn set_single(&mut self, rec: u32, fld: isize, val: f32) -> bool {
        if self.valid(rec, fld) {
            *self.addr_mut(rec, fld) = val;
            true
        } else {
            false
        }
    }
}
