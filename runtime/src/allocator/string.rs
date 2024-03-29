use super::constants::DATA_OFFSET;
use super::heap_values::*;
use super::Heap;
use crate::heap;
use std::ops::Deref;

/// A managed thin pointer to a string. Interned strings are also tagged
/// despite not being managed so this can always be used
///
/// Tag | Size (LE 32) | str (utf-8)
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct StringPtr {
    ptr: *mut Tag,
}

impl StringPtr {
    /// # Safety
    ///
    /// ptr should point to a valid String tag, followed by a little-endian
    /// 4-byte length encoding in memory; it should be aligned, then followed
    /// by strictly that length of a utf-8 encoded string. if the string is
    /// not utf-8 encoded, Deref will turn it to a &str unchecked, and
    /// undefined behavior will result
    pub const unsafe fn new(ptr: *mut Tag) -> Self {
        Self { ptr }
    }
    pub fn len(&self) -> usize {
        u32::from_le(unsafe { *(self.ptr.add(DATA_OFFSET) as *const u32) }) as usize
    }
    pub fn slice(&self, a: i32, b: i32) -> Self {
        let a = if a < 0 { todo!() } else { a as usize };
        let b = if b < 0 { todo!() } else { b as usize };
        let a = if a > self.len() { self.len() } else { a };
        let b = if b > self.len() {
            self.len()
        } else if b < a {
            a
        } else {
            b
        };
        let rust_str = unsafe {
            let start = self.ptr.add(DATA_OFFSET + 1) as *const u8;
            // TODO(luna): unicode....
            let first_char = start.add(a);
            let length = b - a;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(first_char, length))
        };
        heap().alloc_str_or_gc(rust_str)
    }
}
impl HeapPtr for StringPtr {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        self.len() + 4
    }
}
/// gain all the methods of string slices
impl Deref for StringPtr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        // SAFETY: [StringPtr::new] being unsafe guarantees that we must be
        // well-formed
        unsafe {
            let ptr = self.ptr.add(DATA_OFFSET + 1) as *const u8;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, self.len()))
        }
    }
}
impl std::fmt::Debug for StringPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &**self)
    }
}
impl std::fmt::Display for StringPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &**self)
    }
}
impl PartialEq for StringPtr {
    fn eq(&self, other: &StringPtr) -> bool {
        if self.ptr == other.ptr {
            true
        } else {
            **self == **other
        }
    }
}
impl Eq for StringPtr {}
impl std::hash::Hash for StringPtr {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        (**self).hash(h)
    }
}
