const LEN: usize = 1000;

/// An `F64Allocator` is a heap that only stores f64s.  When we allocate an f64,
/// it returns a `*const f64`. Since pointers are 32-bits wide in WebAssembly,
/// we can store it in a 64-bit integer value, and still have room left for tag
/// bits. Thus, we store `*const f64` in our `Any` values, which allows an `Any`
/// to be a 64-bit integer that does not need to be heap allocated.
///
/// TODO(arjun): We need to implement garbage collection.
pub struct F64Allocator {
    // TODO(arjun): No more that one thousand floats.
    current_space: Box<[f64; LEN]>,
    other_space: Box<[f64; LEN]>,
    next_slot: usize,
}

impl F64Allocator {
    pub fn new() -> Self {
        let next_slot = 0;
        let current_space = Box::new([0.0; LEN]);
        let other_space = Box::new([0.0; LEN]);
        return F64Allocator {
            current_space,
            other_space,
            next_slot,
        };
    }

    /// Note that we don't have a function to read the value of a float.
    pub fn alloc(&mut self, value: f64) -> Option<*const f64> {
        assert!(self.next_slot <= LEN);
        let index = self.next_slot;
        self.next_slot += 1;
        if self.next_slot == LEN {
            return None;
        }
        let f64_ref = unsafe { self.current_space.get_unchecked_mut(index) };
        *f64_ref = value;
        return Some(f64_ref as *const f64);
    }

    /// Swaps semispaces. All subsequent allocations will occur in the other
    /// space, and start from the first slot.
    pub fn semispace_swap(&mut self) {
        std::mem::swap(&mut self.current_space, &mut self.other_space);
        self.next_slot = 0;
    }

    /// check if a pointer is to an f64 pointer by being in our heap
    pub fn is_f64<T>(&self, ptr: *const T) -> bool {
        let c = &*self.current_space as *const f64 as usize;
        let o = &*self.other_space as *const f64 as usize;
        let p = ptr as usize;
        (p > c && p < c + LEN) || (p > o && p < o + LEN)
    }
}
