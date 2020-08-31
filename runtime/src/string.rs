//! create managed strings and manipulate them

pub use super::heap_types::StringPtr;

#[no_mangle]
pub extern "C" fn string_len(string: StringPtr) -> i32 {
    string.len() as i32
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
}
