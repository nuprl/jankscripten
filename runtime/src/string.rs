//! create managed strings and manipulate them

pub use super::heap_types::StringPtr;
use crate::heap;

#[no_mangle]
pub extern "C" fn string_length(string: StringPtr) -> i32 {
    string.len() as i32
}

/// Append the given strings
#[no_mangle]
pub extern "C" fn string_concat(a: StringPtr, b: StringPtr) -> StringPtr {
    // combine them
    let combined: String = format!("{}{}", &*a, &*b);

    // allocate this into a string
    heap().alloc_str_or_gc(combined.as_str())
}

#[no_mangle]
pub extern "C" fn string_slice(s: StringPtr, a: i32, b: i32) -> StringPtr {
    s.slice(a, b)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{heap, init};
    use wasm_bindgen_test::*;
    #[test]
    #[wasm_bindgen_test]
    fn to_string_length() {
        init();
        assert_eq!(string_length(heap().alloc_str_or_gc("spinel")), 6);
    }
    #[test]
    #[wasm_bindgen_test]
    fn string_eq() {
        init();
        assert_eq!(&*heap().alloc_str_or_gc("pearl"), "pearl");
    }
    #[test]
    #[wasm_bindgen_test]
    fn alloc_and_read() {
        init();
        assert_eq!(&*heap().alloc_str_or_gc("lapis"), "lapis");
    }
    #[test]
    #[wasm_bindgen_test]
    fn string_append_hello_world() {
        init();
        let a = heap().alloc_str_or_gc("Hello");
        let b = heap().alloc_str_or_gc(" ");
        let c = heap().alloc_str_or_gc("world!");

        let combined = string_concat(a, b);
        let combined = string_concat(combined, c);

        assert_eq!(&*combined, "Hello world!");
    }
}
