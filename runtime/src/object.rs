//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::heap;
use crate::heap_types::*;
use crate::string::StrPtr;
use crate::{AnyEnum, AnyValue, HeapRefView};

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

/// global.Object.create
#[no_mangle]
pub extern "C" fn object_create(maybe_prototype_chain: AnyValue) -> ObjectPtr<'static> {
    match *maybe_prototype_chain {
        AnyEnum::Null => object_empty(),
        AnyEnum::Ptr(p) => match p.view() {
            HeapRefView::ObjectPtrPtr(_o) => {
                // TODO(luna): support prototype chain. this should probably
                // look something like
                // object_empty().insert("__proto__", o)
                object_empty()
            }
            _ => panic!("not an object or null"),
        },
        _ => panic!("not an object or null"),
    }
}
