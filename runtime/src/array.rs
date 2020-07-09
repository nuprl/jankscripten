//! create managed Arrays

use super::{heap, heap_types::ArrayPtr};
use crate::AnyValue;

#[no_mangle]
pub extern "C" fn array_new<'a>() -> ArrayPtr<'a> {
    heap().alloc_or_gc(Vec::new())
}

#[no_mangle]
pub extern "C" fn array_index<'a>(array: ArrayPtr<'a>, index: i32) -> AnyValue<'a> {
    array[index as usize].clone()
}

#[no_mangle]
pub extern "C" fn array_len<'a>(array: ArrayPtr<'a>) -> i32 {
    array.len() as i32
}

#[no_mangle]
pub extern "C" fn array_push<'a>(mut array: ArrayPtr<'a>, value: AnyValue<'a>) -> i32 {
    array.push(value);
    array.len() as i32
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::AnyEnum;
    use wasm_bindgen_test::wasm_bindgen_test;
    #[test]
    #[wasm_bindgen_test]
    fn push_index() {
        crate::init();
        let arr = array_new();
        assert_eq!(array_push(arr, AnyEnum::I32(135).into()), 1);
        assert_eq!(array_push(arr, AnyEnum::I32(7).into()), 2);
        assert_eq!(array_push(arr, AnyEnum::I32(98).into()), 3);
        assert_eq!(array_index(arr, 2), AnyEnum::I32(98).into());
    }
}
