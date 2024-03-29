//! create managed Arrays

use super::{heap, heap_types::ArrayPtr};
use crate::AnyValue;

#[no_mangle]
pub extern "C" fn array_new() -> ArrayPtr {
    heap().alloc_or_gc(Vec::new())
}

#[no_mangle]
pub extern "C" fn array_index(array: ArrayPtr, index: i32) -> AnyValue {
    array[index as usize].clone()
}

#[no_mangle]
pub extern "C" fn array_set(mut array: ArrayPtr, index: i32, val: AnyValue) -> AnyValue {
    array[index as usize] = val;
    val
}

#[no_mangle]
pub extern "C" fn array_length(array: ArrayPtr) -> i32 {
    array.len() as i32
}

#[no_mangle]
pub extern "C" fn array_slice(array: ArrayPtr, a: i32, b: i32) -> ArrayPtr {
    let a = if a < 0 { todo!() } else { a as usize };
    let b = if b < 0 { todo!() } else { b as usize };
    heap().alloc_or_gc(array[a..b].to_vec())
}

#[no_mangle]
pub extern "C" fn array_push(mut array: ArrayPtr, value: AnyValue) -> i32 {
    array.push(value);
    array.len() as i32
}

#[no_mangle]
pub extern "C" fn array_concat(a: ArrayPtr, b: ArrayPtr) -> ArrayPtr {
    heap().alloc_or_gc(a.iter().chain(b.iter()).cloned().collect::<Vec<_>>())
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
