//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::allocator::HeapRefView;
use crate::heap;
use crate::heap_types::*;
use crate::string::StrPtr;

#[no_mangle]
pub extern "C" fn object_empty<'a>() -> ObjectPtr<'a> {
    heap().alloc_object(0).unwrap()
}

#[no_mangle]
pub extern "C" fn object_set<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: AnyJSPtr<'a>,
) -> AnyJSPtr<'a> {
    object.insert(heap(), field, value);
    value
}

#[no_mangle]
pub extern "C" fn object_get<'a>(object: ObjectPtr<'a>, field: StrPtr) -> AnyJSPtr<'a> {
    match object.get(heap(), field).unwrap().view() {
        HeapRefView::Any(p) => p,
        _ => panic!("not an any. TODO: might want to use AnyPtr as Any"),
    }
}
