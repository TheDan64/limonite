use rustc_hash::FxHashMap;

use std::{mem, collections::HashMap};

// Based on https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/
/// A fast and efficient String interner.
pub struct Interner {
    map: FxHashMap<&'static str, StrId>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<StrId> {
        self.map.get(name).copied()
    }

    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let name = unsafe { self.alloc(name) };
        let id = StrId(self.map.len() as u32);
        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    pub fn lookup(&self, id: StrId) -> &str {
        self.vec[id.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        &*(interned as *const str)
    }
}

/// An id newtype representing a string. Can be used for fast string comparisons.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StrId(u32);

impl StrId {
    pub const DUMMY: StrId = StrId(u32::max_value());
}
