//! create managed strings and manipulate them

use super::heap;
use super::heap_types::StringPtr;
use std::{slice, str};

/// # Safety
///
/// from should point to a little-endian 4-byte length encoding in memory;
/// it should be aligned, then followed by strictly that length of a string
///
/// # Panics
///
/// if from is not utf8
#[no_mangle]
pub unsafe extern "C" fn string_from_str(from: *const u32) -> StringPtr<'static> {
    // can't use String::from_raw_parts because data for that needs to be
    // known to the allocator
    let len = u32::from_le(*from) as usize;
    let ptr = from.add(1) as *const u8;
    let slice = slice::from_raw_parts(ptr, len);
    let str_ref: &str = str::from_utf8(slice).expect("not utf8");
    heap().alloc(String::from(str_ref)).unwrap()
}

#[no_mangle]
pub extern "C" fn string_len(string: StringPtr) -> i32 {
    string.len() as i32
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use wasm_bindgen_test::*;
    fn as_ptr(s: &str) -> *const u32 {
        let length = s.len() as u32;
        let mut length_bytes: [u8; 4] = unsafe { std::mem::transmute(length.to_le()) };
        let data = Box::leak(Box::new(Vec::new()));
        data.extend_from_slice(&length_bytes);
        data.extend_from_slice(s.as_bytes());
        &**data as *const [u8] as *const u32
    }
    // composing string_from_str and as_ptr is safe because of each's guarantees
    fn from_rust_str(s: &str) -> StringPtr {
        unsafe { string_from_str(as_ptr(s)) }
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
