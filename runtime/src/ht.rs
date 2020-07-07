//! create (currently unmanaged) HashMaps and manage them

use super::{
    heap,
    heap_types::{AnyJSPtr, HTPtr},
};
use crate::Key;
use std::collections::HashMap;

#[no_mangle]
pub extern "C" fn ht_new_any<'a>() -> HTPtr<'a, AnyJSPtr<'a>> {
    heap().alloc_or_gc(HashMap::new())
}
#[no_mangle]
pub extern "C" fn ht_new_i32<'a>() -> HTPtr<'a, i32> {
    heap().alloc_or_gc(HashMap::new())
}
#[no_mangle]
pub extern "C" fn ht_new_f64<'a>() -> HTPtr<'a, f64> {
    heap().alloc_or_gc(HashMap::new())
}

fn ht_get_generic<T: Clone>(ht: HTPtr<T>, field: Key) -> T {
    HashMap::get(&ht, &field).unwrap().clone()
}
#[no_mangle]
pub extern "C" fn ht_get_any<'a>(ht: HTPtr<'a, AnyJSPtr<'a>>, field: Key) -> AnyJSPtr<'a> {
    ht_get_generic(ht, field)
}
#[no_mangle]
pub extern "C" fn ht_get_i32(ht: HTPtr<i32>, field: Key) -> i32 {
    ht_get_generic(ht, field)
}
#[no_mangle]
pub extern "C" fn ht_get_f64(ht: HTPtr<f64>, field: Key) -> f64 {
    ht_get_generic(ht, field)
}

fn ht_set_generic<'a, T: Clone>(mut ht: HTPtr<'a, T>, field: Key, value: T) -> T {
    ht.insert(field, value.clone());
    value
}
#[no_mangle]
pub extern "C" fn ht_set_any<'a>(
    ht: HTPtr<'a, AnyJSPtr<'a>>,
    field: Key,
    value: AnyJSPtr<'a>,
) -> AnyJSPtr<'a> {
    ht_set_generic(ht, field, value)
}
#[no_mangle]
pub extern "C" fn ht_set_i32(ht: HTPtr<i32>, field: Key, value: i32) -> i32 {
    ht_set_generic(ht, field, value)
}
#[no_mangle]
pub extern "C" fn ht_set_f64(ht: HTPtr<f64>, field: Key, value: f64) -> f64 {
    ht_set_generic(ht, field, value)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use crate::string::str_as_ptr;
    use wasm_bindgen_test::wasm_bindgen_test;
    #[test]
    #[wasm_bindgen_test]
    fn string_keys() {
        init();
        let k1 = str_as_ptr("key_1");
        let k2 = str_as_ptr("key_2");
        let ht = ht_new_i32();
        ht_set_i32(ht, k1, 3);
        ht_set_i32(ht, k2, 2);
        ht_set_i32(ht, k1, 1);
        assert_eq!(ht_get_i32(ht, k2), 2);
        assert_eq!(ht_get_i32(ht, k1), 1);
    }
}
