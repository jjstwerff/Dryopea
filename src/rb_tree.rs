// Copyright (c) 2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
#![allow(dead_code)]

use crate::logger::log::error;
use crate::store::Store;
use std::cmp::Ordering;

static RB_LEFT: isize = 0;
static RB_RIGHT: isize = 4;
static RB_FLAG: isize = 8;
static RB_MAX_DEPTH: u32 = 100;

/// Get the lowest record with test(rec)
pub fn rb_search(
    store: &mut Store,
    top: u32,
    fld: isize,
    rec_pos: isize,
    test: fn(u32) -> bool,
) -> u32 {
    let mut e = store.get_int(top, fld);
    let mut rec = 0;
    while e > 0 {
        rec = e as u32;
        e = store.get_int(rec, rec_pos + if test(rec) { RB_LEFT } else { RB_RIGHT });
    }
    rec
}

struct RbTree<'a> {
    store: &'a mut Store,
    rec_pos: isize,
    lower: fn(u32, u32) -> bool,
}

/// Insert a new element
/// Always the same: store, rec_pos, top_pos, lower
pub fn rb_insert(
    store: &mut Store,
    top: u32,
    top_pos: isize,
    rec: u32,
    rec_pos: isize,
    lower: fn(u32, u32) -> bool,
) {
    store.set_byte(rec, rec_pos + RB_FLAG, 0, 0);
    store.set_int(rec, rec_pos + RB_LEFT, 0);
    store.set_int(rec, rec_pos + RB_RIGHT, 0);
    let new_top = if store.get_int(top, top_pos) == 0 {
        rec
    } else {
        let top_rec = store.get_int(top, top_pos);
        RbTree {
            store,
            rec_pos,
            lower,
        }
        .put(0, top_rec, rec, 0, 0) as u32
    };
    if new_top == 0 {
        return; // problem encountered: probably duplicate key
    }
    store.set_int(top, top_pos, new_top as i32);
    store.set_byte(new_top, rec_pos + RB_FLAG, 0, 0);
}

/// Return the first element in the tree
pub fn rb_first(store: &mut Store, top: u32, top_pos: isize, pos: isize) -> u32 {
    let mut i = store.get_int(top, top_pos);
    while i > 0 && store.get_int(i as u32, pos + RB_LEFT) > 0 {
        i = store.get_int(i as u32, pos + RB_LEFT);
    }
    i as u32
}

/// Return the last element in the tree
pub fn rb_last(store: &mut Store, top: u32, top_pos: isize, pos: isize) -> u32 {
    let mut i = store.get_int(top, top_pos);
    while i > 0 && store.get_int(i as u32, pos + RB_RIGHT) > 0 {
        i = store.get_int(i as u32, pos + RB_RIGHT)
    }
    i as u32
}

impl<'a> RbTree<'a> {
    fn left(&self, rec: u32) -> i32 {
        self.store.get_int(rec, self.rec_pos + RB_LEFT)
    }

    fn right(&self, rec: u32) -> i32 {
        self.store.get_int(rec, self.rec_pos + RB_RIGHT)
    }

    fn flag(&self, rec: u32) -> bool {
        self.store.get_byte(rec, self.rec_pos + RB_FLAG, 0) == 1
    }

    fn set_left(&mut self, rec: u32, to: i32) {
        self.store.set_int(rec, self.rec_pos + RB_LEFT, to);
    }

    fn set_right(&mut self, rec: u32, to: i32) {
        self.store.set_int(rec, self.rec_pos + RB_RIGHT, to);
    }

    fn set_flag(&mut self, rec: u32, to: bool) {
        self.store
            .set_byte(rec, self.rec_pos + RB_FLAG, 0, if to { 1 } else { 0 });
    }

    /// Find the correct position to insert the element
    fn put(&mut self, depth: u32, pos: i32, rec: u32, l: i32, r: i32) -> i32 {
        if depth > RB_MAX_DEPTH {
            return 0;
        }
        if pos <= 0 {
            self.set_flag(rec, true);
            self.set_left(rec, -l);
            self.set_right(rec, -r);
            return rec as i32;
        }
        let un_p = pos as u32;
        if un_p == rec {
            // duplicate record
            return rec as i32;
        }
        if (self.lower)(rec, un_p) {
            let next = self.left(un_p);
            let p = self.put(depth + 1, next, rec, l, pos);
            if p < 0 {
                return -1;
            }
            self.set_left(un_p, p);
        } else if (self.lower)(un_p, rec) {
            let next = self.right(un_p);
            let p = self.put(depth + 1, next, rec, pos, r);
            if p < 0 {
                return -1;
            }
            self.set_right(un_p, p);
        } else {
            // double keys
            return -1;
        }
        self.rb_balance(un_p)
    }

    /// When the position is found, re-balance the tree after inserting
    fn rb_balance(&mut self, rec: u32) -> i32 {
        if self.flag(rec) {
            return rec as i32;
        }
        let l = self.left(rec);
        let r = self.right(rec);
        if l > 0 && self.flag(l as u32) {
            let ll = self.left(l as u32);
            if ll > 0 && self.flag(ll as u32) {
                return self.fix_ll(rec, l, ll);
            }
            let lr = self.right(l as u32);
            if lr > 0 && self.flag(lr as u32) {
                return self.fix_lr(rec, l, lr);
            }
        }
        if r > 0 && self.flag(r as u32) {
            let rl = self.left(r as u32);
            if rl > 0 && self.flag(rl as u32) {
                return self.fix_rl(rec, r, rl);
            }
            let rr = self.right(r as u32);
            if rr > 0 && self.flag(rr as u32) {
                return self.fix_rl(rec, r, rr);
            }
        }
        rec as i32
    }

    /// Black, rec, Node (Red, l, Node (Red, ll, a, b), c), d
    /// -> Red, l, Node (Black, ll, a, b), Node (Black, rec, c, d)
    fn fix_ll(&mut self, rec: u32, l: i32, ll: i32) -> i32 {
        let c = self.right(l as u32);
        self.set_right(l as u32, rec as i32);
        self.set_flag(ll as u32, false);
        self.set_left(rec, if c < 0 { -l } else { c });
        l
    }

    /// Black, rec, Node (Red, l, a, Node (Red, lr, b, c)), d
    /// -> Red, lr, Node (Black, l, a, b), Node (Black, rec, c, d)
    fn fix_lr(&mut self, rec: u32, l: i32, lr: i32) -> i32 {
        let b = self.left(lr as u32);
        let c = self.right(lr as u32);
        self.set_left(lr as u32, l);
        self.set_right(lr as u32, rec as i32);
        self.set_flag(l as u32, false);
        self.set_right(l as u32, if b < 0 { -lr } else { b });
        self.set_left(rec, if c < 0 { -lr } else { c });
        lr
    }

    /// Black, p, a, Node (Red, r, Node (Red, rl, b, c), d)
    /// -> Red, rl, Node (Black, p, a, b), Node (Black, r, c, d)
    fn fix_rl(&mut self, rec: u32, r: i32, rl: i32) -> i32 {
        let b = self.left(rl as u32);
        let c = self.right(rl as u32);
        self.set_left(rl as u32, rec as i32);
        self.set_right(rl as u32, r);
        self.set_right(rec, if b < 0 { -rl } else { b });
        self.set_flag(r as u32, false);
        self.set_left(r as u32, if c < 0 { -rl } else { c });
        rl
    }

    /// Black, p, a, Node (Red, r, b, Node (Red, rr, c, d))
    /// -> Red, r, Node (Black, p, a, b), Node (Black, rr, c, d)
    fn fix_rr(&mut self, rec: u32, r: i32, rr: i32) -> i32 {
        let b = self.left(r as u32);
        self.set_left(r as u32, rec as i32);
        self.set_right(rec, if b < 0 { -r } else { b });
        self.set_flag(rr as u32, false);
        r
    }

    /// Walk to the element to remove
    fn rb_remove_iter(&mut self, rec: u32, depth: u32, pos: u32, black: &mut bool) -> u32 {
        if depth > RB_MAX_DEPTH || pos == 0 {
            return 0;
        }
        let mut p = pos;
        let compare_to;
        if pos != rec && (self.lower)(rec, pos) {
            compare_to = -1;
            let cl = self.left(pos);
            if cl < 0 {
                // should be a normal node
                return 0;
            }
            let mut l = self.rb_remove_iter(rec, depth + 1, cl as u32, black) as i32;
            if l == 0 {
                l = self.left(rec);
            }
            self.set_left(pos, l);
        } else if pos != rec && (self.lower)(pos, rec) {
            compare_to = 1;
            let cr = self.right(pos);
            if cr < 0 {
                // should be a normal node
                return 0;
            }
            let mut r = self.rb_remove_iter(rec, depth + 1, cr as u32, black) as i32;
            if r == 0 {
                r = self.right(rec);
            }
            self.set_right(pos, r);
        } else {
            p = self.rb_remove_elm(pos, depth, black);
            if p == 0 {
                return 0;
            }
            if self.left(p) <= 0 && self.right(p) <= 0 {
                if !*black {
                    // Cannot change node to black twice in rb_remove()
                    return 0;
                }
                if !self.flag(p) {
                    // Child of single-child node should be red
                    return 0;
                }
                self.set_flag(p, false);
                *black = false;
                return p;
            }
            compare_to = -1;
        }
        if *black {
            self.rb_repair(p, black, compare_to);
        }
        p
    }

    fn rb_remove_elm(&mut self, rec: u32, depth: u32, black: &mut bool) -> u32 {
        let l = self.left(rec);
        let r = self.right(rec);
        let rd = self.flag(rec);
        // left is empty: return right as replacement
        if l <= 0 {
            *black = !rd;
            if r <= 0 {
                return 0;
            }
            if rd {
                // Expected node with single-child to be black
                return 0;
            }
            if self.left(r as u32) < 0 {
                self.set_left(r as u32, l);
            }
            return r as u32;
        }
        // left is empty: return left as replacement
        if r <= 0 {
            *black = !rd;
            // Expected node with single-child to be black
            if rd {
                return 0;
            }
            if self.right(l as u32) < 0 {
                self.set_right(l as u32, r);
            }
            return l as u32;
        }
        // both left and right as not empty: remove previous element in tree
        // (=max(l)) and then make that the replacement
        let pos = self.rb_max(l as u32);
        let mut new_left = self.rb_remove_iter(pos, depth + 1, l as u32, black) as i32;
        if new_left == 0 {
            new_left = self.left(pos);
        }
        self.set_right(pos, r);
        self.set_flag(pos, rd);
        self.set_left(pos, new_left);
        let pv = rb_previous(self.store, pos, self.rec_pos);
        let nx = rb_next(self.store, pos, self.rec_pos);
        if pv > 0 && self.right(pv) < 0 {
            self.set_right(pv, -(pos as i32));
        }
        if nx > 0 && self.left(nx) < 0 {
            self.set_left(nx, -(pos as i32));
        }
        pos
    }

    fn rb_repair(&mut self, rec: u32, black: &mut bool, compare_to: i32) -> u32 {
        let mut repair1 = 0;
        let mut repair2 = 0;
        match compare_to.cmp(&0) {
            Ordering::Less => {
                let r = self.right(rec);
                if r < 0 {
                    // Expecting a normal node
                    return 0;
                }
                self.rb_child_to_red(r as u32, &mut repair1, &mut repair2)
            }
            Ordering::Greater => {
                let l = self.left(rec);
                if l < 0 {
                    // Expecting a normal node
                    return 0;
                }
                self.rb_child_to_red(l as u32, &mut repair1, &mut repair2)
            }
            Ordering::Equal => {}
        }
        if self.flag(rec) {
            self.set_flag(rec, false);
            *black = false;
        }
        let mut p = rec;
        if repair1 != 0 {
            p = self.rb_repair2(repair1, p, 0);
        }
        if repair2 != 0 {
            p = self.rb_repair2(repair2, p, 0);
        }
        if *black && self.flag(p) {
            self.set_flag(p, false);
            *black = false;
        }
        p
    }

    fn rb_child_to_red(&mut self, rec: u32, l: &mut u32, r: &mut u32) {
        if self.flag(rec) {
            let cl = self.left(rec);
            if cl < 0 {
                // expecting a normal node
                return;
            }
            *l = cl as u32;
            let cr = self.right(rec);
            if cr < 0 {
                // expecting a normal node
                return;
            }
            *r = cr as u32;
            if *l == 0 || *r == 0 {
                // childToRed() called for invalid situation; red-black tree invariants possibly broken
                return;
            }
            self.set_flag(*l, true);
            self.set_flag(*r, true);
        } else {
            self.set_flag(rec, true);
            *l = rec;
        }
    }

    /// Find rec starting at pos.
    /// Balance pos for each level down.
    fn rb_repair2(&mut self, rec: u32, pos: u32, depth: u32) -> u32 {
        if depth > RB_MAX_DEPTH {
            // Too many iterations
            return 0;
        }
        let bal = self.rb_balance(pos);
        if bal < 0 {
            // Expecting a normal node
            return 0;
        }
        let result = bal as u32;
        if result != rec {
            if (self.lower)(rec, result) {
                let l = self.left(result);
                if l < 0 {
                    // expect a normal node
                    return 0;
                }
                let r = self.rb_repair2(rec, l as u32, depth + 1) as i32;
                self.set_left(result, r);
            } else {
                let r = self.right(result);
                if r < 0 {
                    // expect a normal node
                    return 0;
                }
                let r = self.rb_repair2(rec, r as u32, depth + 1) as i32;
                self.set_right(result, r);
            }
        }
        result
    }

    /// Safe validation version of rb_first
    fn rb_min(&self, rec: u32) -> u32 {
        let mut depth = 0;
        if rec == 0 {
            return 0;
        }
        let mut p = rec;
        loop {
            let l = self.left(p);
            if l <= 0 || depth > RB_MAX_DEPTH {
                return p;
            }
            p = l as u32;
            depth += 1;
        }
    }

    /// Safe validation version of rb_last
    fn rb_max(&self, pos: u32) -> u32 {
        let mut depth = 0;
        if pos == 0 {
            return 0;
        }
        let mut p = pos;
        loop {
            let l = self.right(p);
            if l <= 0 || depth > RB_MAX_DEPTH {
                return p;
            }
            p = l as u32;
            depth += 1;
        }
    }

    /// Walk through the tree validating ordering, returning the number of elements.
    /// Validating that each branch has the same number of black elements.
    /// Check if no two elements into the tree are both red.
    fn rb_verify(&self, rec: u32, blacks: u32, max_blacks: &mut u32) -> u32 {
        let mut nb = blacks;
        if nb > RB_MAX_DEPTH {
            log::error!("Too deep structure");
        }
        let l = self.left(rec);
        let r = self.right(rec);
        if l.unsigned_abs() == rec || r.unsigned_abs() == rec {
            log::error!("Linked to self on {}", rec);
        }
        if !self.flag(rec) {
            nb += 1;
        }
        1 + self.rb_v_side(rec, l as u32, true, nb, max_blacks)
            + self.rb_v_side(rec, r as u32, false, nb, max_blacks)
    }

    fn rb_v_side(&self, rec: u32, side: u32, left: bool, depth: u32, max_blacks: &mut u32) -> u32 {
        if side > 0 {
            let sl = (self.lower)(side, rec);
            let sh = (self.lower)(rec, side);
            if !sl && !sh && rec != side {
                log::error!("Duplicate key {} and {}", rec, side);
            }
            if (left && sh) || (!left && sl) {
                log::error!("Ordering not correct");
            }
            if self.flag(rec) && self.flag(side) {
                log::error!("Two adjacent red nodes {} and {}", rec, side);
            }
            self.rb_verify(side, depth, max_blacks)
        } else if *max_blacks == 0 {
            *max_blacks = depth;
            0
        } else if *max_blacks != depth {
            log::error!("Not balanced");
            0
        } else {
            0
        }
    }

    /// Move from the first to the last element and back checking ordering.
    /// Same fields as insert/remove
    fn rb_verify_list(&self, top: u32, top_pos: isize, size: u32) {
        let rec = self.store.get_int(top, top_pos) as u32;
        let min = self.rb_min(rec);
        let max = self.rb_max(rec);
        if rb_next(self.store, max, self.rec_pos) != 0 {
            error!("Incorrect max element");
        }
        if rb_previous(self.store, min, self.rec_pos) != 0 {
            error!("Incorrect max element");
        }
        self.verify_walk(min, max, true, size);
        self.verify_walk(max, min, false, size);
    }

    fn verify_walk(&self, start: u32, end: u32, dir: bool, size: u32) {
        let mut step = 1;
        let mut elm = start;
        while elm != end {
            if step > size {
                error!("List too long at {}", elm);
            }
            let n = if dir {
                rb_next(self.store, elm, self.rec_pos)
            } else {
                rb_previous(self.store, elm, self.rec_pos)
            };
            if n == 0 {
                error!("Incorrect list at {}", elm);
            }
            if (self.lower)(n, elm) ^ dir {
                error!("Not ascending list at {}", elm);
            }
            elm = n;
            step += 1;
        }
        if step < size {
            error!("List too short");
        }
    }
}

/// Step to the next element in the tree
pub fn rb_next(store: &Store, rec: u32, rec_pos: isize) -> u32 {
    if rec == 0 {
        return 0;
    }
    let mut r = store.get_int(rec, rec_pos + RB_RIGHT);
    let mut depth = 0;
    while r > 0 && store.get_int(r as u32, rec_pos + RB_LEFT) > 0 {
        r = store.get_int(r as u32, rec_pos + RB_LEFT);
        if depth > RB_MAX_DEPTH {
            return 0;
        }
        depth += 1;
    }
    r.unsigned_abs()
}

/// Step to the previous element in the tree
pub fn rb_previous(store: &Store, rec: u32, rec_pos: isize) -> u32 {
    if rec == 0 {
        return 0;
    }
    let mut r = store.get_int(rec, rec_pos + RB_LEFT);
    let mut depth = 0;
    while r > 0 && store.get_int(r as u32, rec_pos + RB_RIGHT) > 0 {
        r = store.get_int(r as u32, rec_pos + RB_RIGHT);
        if depth > RB_MAX_DEPTH {
            return 0;
        }
        depth += 1;
    }
    r.unsigned_abs()
}

/// Validate the tree
pub fn rb_validate(
    store: &mut Store,
    top: u32,
    top_pos: isize,
    rec_pos: isize,
    lower: fn(u32, u32) -> bool,
) {
    let pos = store.get_int(top, top_pos);
    if pos == 0 {
        return;
    }
    if pos < 0 || store.get_byte(pos as u32, rec_pos + RB_FLAG, 0) == 1 {
        log::error!("Root is not black");
    }
    let mut max_blacks = 0;
    let tree = RbTree {
        store,
        rec_pos,
        lower,
    };
    let v = tree.rb_verify(pos as u32, 0, &mut max_blacks);
    tree.rb_verify_list(top, top_pos, v);
}

/// Remove an element from the tree.
pub fn rb_remove(
    store: &mut Store,
    rec: u32,
    rec_pos: isize,
    top: u32,
    top_pos: isize,
    lower: fn(u32, u32) -> bool,
) {
    let mut black = false;
    let t_pos = store.get_int(top, top_pos) as u32;
    let t = RbTree {
        store,
        rec_pos,
        lower,
    }
    .rb_remove_iter(rec, 0, t_pos, &mut black);
    if t > 0 {
        store.set_int(top, top_pos, t as i32);
        if t != 0 {
            store.set_byte(t, rec_pos + RB_FLAG, 0, 0);
        }
    }
}
