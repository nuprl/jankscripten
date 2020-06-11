use crate::any::Any;
use crate::Key;
use std::collections::HashMap;

fn ht_new_generic<T>() -> Box<HashMap<Key, T>> {
    Box::new(HashMap::new())
}
#[no_mangle]
pub extern "C" fn ht_new_any() -> Box<HashMap<Key, Box<Any>>> {
    ht_new_generic()
}
#[no_mangle]
pub extern "C" fn ht_new_i32() -> Box<HashMap<Key, i32>> {
    ht_new_generic()
}

fn ht_get_generic<T: Clone>(ht: &HashMap<Key, T>, field: Key) -> T {
    ht.get(&field).unwrap().clone()
}
#[no_mangle]
pub extern "C" fn ht_get_any(ht: &HashMap<Key, Box<Any>>, field: Key) -> Box<Any> {
    ht_get_generic(ht, field)
}
#[no_mangle]
pub extern "C" fn ht_get_i32(ht: &HashMap<Key, i32>, field: Key) -> i32 {
    ht_get_generic(ht, field)
}

fn ht_set_generic<T: Clone>(ht: &mut HashMap<Key, T>, field: Key, value: T) -> T {
    ht.insert(field, value.clone());
    value
}
#[no_mangle]
pub extern "C" fn ht_set_any(
    ht: &mut HashMap<Key, Box<Any>>,
    field: Key,
    value: Box<Any>,
) -> Box<Any> {
    ht_set_generic(ht, field, value.clone())
}
#[no_mangle]
pub extern "C" fn ht_set_i32(ht: &mut HashMap<Key, i32>, field: Key, value: i32) -> i32 {
    ht_set_generic(ht, field, value)
}
