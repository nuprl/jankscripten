//! create (currently unmanaged) HashMaps and manage them

use super::{heap, heap_types::HTPtr};
use crate::{AnyValue, Key};
use std::collections::HashMap;

#[no_mangle]
pub extern "C" fn ht_new<'a>() -> HTPtr<'a> {
    heap().alloc_or_gc(HashMap::new())
}

#[no_mangle]
pub extern "C" fn ht_get<'a>(ht: HTPtr<'a>, field: Key) -> AnyValue<'a> {
    HashMap::get(&ht, &field).unwrap().clone()
}

#[no_mangle]
pub extern "C" fn ht_set<'a>(mut ht: HTPtr<'a>, field: Key, value: AnyValue<'a>) -> AnyValue<'a> {
    ht.insert(field, value.clone());
    value
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use crate::string::str_as_ptr;
    use crate::AnyEnum;
    use wasm_bindgen_test::wasm_bindgen_test;
    #[test]
    #[wasm_bindgen_test]
    fn string_keys() {
        init();
        let k1 = str_as_ptr("key_1");
        let k2 = str_as_ptr("key_2");
        let ht = ht_new();
        ht_set(ht, k1, AnyEnum::I32(3).into());
        ht_set(ht, k2, AnyEnum::I32(2).into());
        ht_set(ht, k1, AnyEnum::I32(1).into());
        assert_eq!(ht_get(ht, k2), AnyEnum::I32(2).into());
        assert_eq!(ht_get(ht, k1), AnyEnum::I32(1).into());
    }
}
