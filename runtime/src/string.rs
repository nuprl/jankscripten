//! create managed strings and manipulate them

use super::heap;
use super::heap_types::StringPtr;
use std::{slice, str};

/// # Panics
///
/// if from is not utf8
#[no_mangle]
pub extern "C" fn string_from_ptr(from: StrPtr) -> StringPtr<'static> {
    // can't use String::from_raw_parts because data for that needs to be
    // known to the allocator
    let str_ref = str_from_ptr(from);
    heap().alloc(String::from(str_ref)).unwrap()
}

/// # Panics
///
/// if from is not utf8
fn str_from_ptr<'a>(from: StrPtr) -> &'a str {
    // safety: StrPtr::new being unsafe guarantees that StrPtr must be
    // well-formed
    let slice = unsafe {
        let len = u32::from_le(*from.ptr()) as usize;
        let ptr = from.ptr().add(1) as *const u8;
        slice::from_raw_parts(ptr, len)
    };
    str::from_utf8(slice).expect("not utf8")
}

#[no_mangle]
pub extern "C" fn string_len(string: StringPtr) -> i32 {
    string.len() as i32
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct StrPtr {
    ptr: *const u32,
}
impl From<StrPtr> for *const u32 {
    fn from(ptr: StrPtr) -> *const u32 {
        ptr.ptr
    }
}
impl PartialEq for StrPtr {
    fn eq(&self, other: &StrPtr) -> bool {
        str_from_ptr(*self) == str_from_ptr(*other)
    }
}
impl std::hash::Hash for StrPtr {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        str_from_ptr(*self).hash(h)
    }
}
impl Eq for StrPtr {}
impl StrPtr {
    /// # Safety
    ///
    /// from should point to a little-endian 4-byte length encoding in memory;
    /// it should be aligned, then followed by strictly that length of a string
    unsafe fn new(ptr: *const u32) -> Self {
        Self { ptr }
    }
    fn ptr(&self) -> *const u32 {
        self.ptr
    }
}
impl std::fmt::Debug for StrPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", str_from_ptr(*self))
    }
}

pub fn str_as_ptr(s: &str) -> StrPtr {
    let length = s.len() as u32;
    let length_bytes: [u8; 4] = unsafe { std::mem::transmute(length.to_le()) };
    let data = Box::leak(Box::new(Vec::new()));
    data.extend_from_slice(&length_bytes);
    data.extend_from_slice(s.as_bytes());
    let ptr = &**data as *const [u8] as *const u32;
    unsafe { StrPtr::new(ptr) }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use wasm_bindgen_test::*;
    // composing string_from_ptr and as_ptr is safe because of each's guarantees
    fn from_rust_str(s: &str) -> StringPtr {
        string_from_ptr(str_as_ptr(s))
    }
    #[test]
    #[wasm_bindgen_test]
    fn to_string_len() {
        init();
        assert_eq!(string_len(from_rust_str("spinel")), 6);
    }
    #[test]
    #[wasm_bindgen_test]
    fn string_eq() {
        init();
        assert_eq!(*from_rust_str("pearl"), String::from("pearl"));
    }
}
