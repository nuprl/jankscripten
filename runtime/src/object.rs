//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::box_f64;
use crate::heap;
use crate::heap_types::*;
use crate::string::StrPtr;
use crate::{AnyEnum, AnyValue};

#[no_mangle]
pub extern "C" fn object_empty<'a>() -> ObjectPtr<'a> {
    heap().alloc_object_or_gc(0)
}

#[no_mangle]
pub extern "C" fn object_set_any<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: AnyValue<'a>,
    cache: &mut isize,
) -> AnyValue<'a> {
    object.insert(heap(), field, value, cache);
    value
}

#[no_mangle]
pub extern "C" fn object_set_f64<'a>(
    mut object: ObjectPtr<'a>,
    field: StrPtr,
    value: f64,
    cache: &mut isize,
) -> f64 {
    object.insert(heap(), field, AnyEnum::F64(box_f64(value)).into(), cache);
    value
}

#[no_mangle]
pub extern "C" fn object_get_any<'a>(
    object: ObjectPtr<'a>,
    field: StrPtr,
    cache: &mut isize,
) -> AnyValue<'a> {
    object.get(heap(), field, cache).unwrap().into()
}

#[no_mangle]
pub extern "C" fn object_get_f64<'a>(
    object: ObjectPtr<'a>,
    field: StrPtr,
    cache: &mut isize,
) -> f64 {
    let x = object.get(heap(), field, cache).unwrap();
    match x {
        AnyEnum::F64(f) => *f,
        _ => panic!("not an f64"),
    }
}
