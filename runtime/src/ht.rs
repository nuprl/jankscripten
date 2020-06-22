//! create (currently unmanaged) HashMaps and manage them

use super::{
    heap,
    heap_types::{AnyJSPtr, HTPtr},
};
use crate::Key;
use std::collections::HashMap;

#[no_mangle]
pub extern "C" fn ht_new_any<'a>() -> HTPtr<'a, AnyJSPtr<'a>> {
    heap().alloc(HashMap::new()).unwrap()
}
#[no_mangle]
pub extern "C" fn ht_new_i32<'a>() -> HTPtr<'a, i32> {
    heap().alloc(HashMap::new()).unwrap()
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
