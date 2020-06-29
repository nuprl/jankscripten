//! create managed Arrays

use super::{
    heap,
    heap_types::{AnyJSPtr, ArrayPtr},
};

#[no_mangle]
pub extern "C" fn array_new_any<'a>() -> ArrayPtr<'a, AnyJSPtr<'a>> {
    heap().alloc(Vec::new()).unwrap()
}
#[no_mangle]
pub extern "C" fn array_new_i32<'a>() -> ArrayPtr<'a, i32> {
    heap().alloc(Vec::new()).unwrap()
}

fn array_index_generic<T: Clone>(array: ArrayPtr<T>, index: i32) -> T {
    array[index as usize].clone()
}
#[no_mangle]
pub extern "C" fn array_index_any<'a>(
    array: ArrayPtr<'a, AnyJSPtr<'a>>,
    index: i32,
) -> AnyJSPtr<'a> {
    array_index_generic(array, index)
}
#[no_mangle]
pub extern "C" fn array_index_i32(array: ArrayPtr<i32>, index: i32) -> i32 {
    array_index_generic(array, index)
}

fn array_len_generic<T>(array: ArrayPtr<T>) -> i32 {
    array.len() as i32
}
#[no_mangle]
pub extern "C" fn array_len_any<'a>(array: ArrayPtr<'a, AnyJSPtr<'a>>) -> i32 {
    array_len_generic(array)
}
#[no_mangle]
pub extern "C" fn array_len_i32<'a>(array: ArrayPtr<'a, i32>) -> i32 {
    array_len_generic(array)
}

fn array_push_generic<'a, T>(mut array: ArrayPtr<'a, T>, value: T) -> i32 {
    array.push(value);
    array.len() as i32
}
#[no_mangle]
pub extern "C" fn array_push_any<'a>(
    array: ArrayPtr<'a, AnyJSPtr<'a>>,
    value: AnyJSPtr<'a>,
) -> i32 {
    array_push_generic(array, value)
}
#[no_mangle]
pub extern "C" fn array_push_i32(array: ArrayPtr<i32>, value: i32) -> i32 {
    array_push_generic(array, value)
}

#[cfg(test)]
mod test {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;
    #[test]
    #[wasm_bindgen_test]
    fn push_index() {
        crate::init();
        let arr = array_new_i32();
        assert_eq!(array_push_i32(arr, 135), 1);
        assert_eq!(array_push_i32(arr, 7), 2);
        assert_eq!(array_push_i32(arr, 98), 3);
        assert_eq!(array_index_i32(arr, 2), 98);
    }
}
