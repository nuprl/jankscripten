//! create managed strings and manipulate them

pub use super::heap_types::StringPtr;
use crate::heap;

#[no_mangle]
pub extern "C" fn string_len(string: StringPtr) -> i32 {
    string.len() as i32
}

/// Append the given strings
#[no_mangle]
pub extern "C" fn string_append(a: StringPtr, b: StringPtr) -> StringPtr {
    // combine them
    let combined: String = format!("{}{}", &*a, &*b);

    // allocate this into a string
    heap().alloc_str_or_gc(combined.as_str())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::heap_types::StringPtr;
    use crate::init;
    use wasm_bindgen_test::*;
    #[test]
    #[wasm_bindgen_test]
    fn to_string_len() {
        init();
        assert_eq!(string_len("spinel".into()), 6);
    }
    #[test]
    #[wasm_bindgen_test]
    fn string_eq() {
        init();
        assert_eq!(&*StringPtr::from("pearl"), "pearl");
    }
    #[test]
    #[wasm_bindgen_test]
    fn alloc_and_read() {
        init();
        assert_eq!(&*StringPtr::from("lapis"), "lapis");
    }
    #[test]
    #[wasm_bindgen_test]
    fn string_append_hello_world() {
        init();
        let a = StringPtr::from("Hello");
        let b = StringPtr::from(" ");
        let c = StringPtr::from("world!");

        let combined = string_append(a, b);
        let combined = string_append(combined, c);

        assert_eq!(&*combined, "Hello world!");
    }
}
