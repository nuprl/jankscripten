//! create managed strings and manipulate them

pub use super::heap_types::StringPtr;
use crate::heap;

#[no_mangle]
pub extern "C" fn string_len(string: StringPtr) -> i32 {
    string.len() as i32
}

pub extern "C" fn string_append(a: StringPtr, b: StringPtr) -> StringPtr {
    let a: &str = &a;
    let b: &str = &b;

    // combine them
    // TODO(luna): this technically could avoid an intermediate
    // allocation. it's probly not a high priority
    let combined = format!("{}{}", a, b);

    // allocate this into a string
    heap().alloc_str_or_gc(&combined)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{heap, init};
    use wasm_bindgen_test::*;
    #[test]
    #[wasm_bindgen_test]
    fn to_string_len() {
        init();
        assert_eq!(string_len(heap().alloc_str_or_gc("spinel")), 6);
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
}
