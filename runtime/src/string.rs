use super::heap;
use super::heap_types::StringPtr;

#[no_mangle]
pub extern "C" fn string_from_str<'a>(from: &str) -> StringPtr<'a> {
    heap().alloc_string(from.into()).unwrap()
}

#[no_mangle]
pub extern "C" fn string_len<'a>(string: StringPtr<'a>) -> i32 {
    string.len() as i32
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use wasm_bindgen_test::*;
    #[test]
    #[wasm_bindgen_test]
    fn to_string_len() {
        init();
        assert_eq!(string_len(string_from_str("spinel")), 6);
    }
}
