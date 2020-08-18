//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::heap;
use crate::heap_types::*;
use crate::string::StrPtr;
use crate::AnyValue;

#[no_mangle]
pub extern "C" fn object_empty<'a>() -> ObjectPtr<'a> {
    heap().alloc_object_or_gc(0)
}

#[no_mangle]
pub extern "C" fn object_set<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: AnyValue,
    cache: &mut isize,
) -> AnyValue {
    object.insert(heap(), field, value, cache);
    value
}

#[no_mangle]
pub extern "C" fn object_get<'a>(
    object: ObjectPtr<'a>,
    field: StrPtr,
    cache: &mut isize,
) -> AnyValue {
    object.get(heap(), field, cache).unwrap().into()
}
