use std::collections::HashMap;

type I32HashMap = HashMap<i32, i32>;

#[no_mangle]
pub extern "C" fn ht_new() -> Box<I32HashMap> {
    Box::new(HashMap::new())
}
#[no_mangle]
pub extern "C" fn ht_get(ht: &I32HashMap, field: i32) -> i32 {
    *ht.get(&field).unwrap()
}
#[no_mangle]
pub extern "C" fn ht_set(ht: &mut I32HashMap, field: i32, value: i32) -> i32 {
    ht.insert(field, value);
    value
}
