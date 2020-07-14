const LEN : usize = 1000;

/// An `F64Allocator` is a heap that only stores f64s.  When we allocate an f64,
/// it returns a `*const f64`. Since pointers are 32-bits wide in WebAssembly,
/// we can store it in a 64-bit integer value, and still have room left for tag
/// bits. Thus, we store `*const f64` in our `Any` values, which allows an `Any`
/// to be a 64-bit integer that does not need to be heap allocated.
///
/// TODO(arjun): We need to implement garbage collection.
pub struct F64Allocator {
    // TODO(arjun): No more that one thousand floats.
    buf: [f64; LEN],
    next_slot: usize
}

impl F64Allocator {

    pub fn new() -> Self {
        let next_slot = 0;
        let buf = [0.0; LEN];
        return F64Allocator { buf, next_slot };
    }

    /// Note that we don't have a function to read the value of a float.
    pub fn alloc(&mut self, value: f64) -> *const f64 {
        let index = self.next_slot;
        self.next_slot += 1;
        assert!(self.next_slot < LEN, "no space left for floats");
        let f64_ref = unsafe {
            self.buf.get_unchecked_mut(index)
        };
        *f64_ref = value;
        return f64_ref as *const f64;
    }

}