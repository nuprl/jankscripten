//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::any::Any;
use crate::heap;
use crate::heap_types::*;
use crate::string::StrPtr;

#[no_mangle]
pub extern "C" fn object_empty<'a>() -> ObjectPtr<'a> {
    heap().alloc_object_or_gc(0)
}

#[no_mangle]
pub extern "C" fn object_set_any<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: AnyJSPtr<'a>,
    cache: &mut isize,
) -> AnyJSPtr<'a> {
    object.insert(heap(), field, value.get().clone(), cache);
    value
}

#[no_mangle]
pub extern "C" fn object_set_f64<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: f64,
    cache: &mut isize,
) -> f64 {
    object.insert(heap(), field, Any::F64(value), cache);
    value
}

#[no_mangle]
pub extern "C" fn object_get_any<'a>(
    object: ObjectPtr<'a>,
    field: StrPtr,
    cache: &mut isize,
) -> AnyJSPtr<'a> {
    let x: Any<'a> = object.get(heap(), field, cache).unwrap();
    heap().alloc_or_gc(x)
}

#[no_mangle]
pub extern "C" fn object_get_f64<'a>(
    object: ObjectPtr<'a>,
    field: StrPtr,
    cache: &mut isize,
) -> f64 {
    let x: Any<'a> = object.get(heap(), field, cache).unwrap();
    match x {
        Any::F64(f) => f,
        _ => panic!("not an f64"),
    }
}
