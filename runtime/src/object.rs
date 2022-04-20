//! Bindings to heap_type's ObjectPtr: instances of hidden classes

use crate::allocator::no_cache;
use crate::heap;
use crate::heap_types::*;
use crate::static_strings::static_strings;
use crate::{AnyEnum, AnyValue, HeapPtr, HeapRefView};

#[no_mangle]
pub extern "C" fn object_empty() -> ObjectPtr {
    heap().alloc_object_or_gc(0)
}

#[no_mangle]
pub extern "C" fn object_set(
    mut object: ObjectPtr,
    field: StringPtr,
    value: AnyValue,
    cache: &mut (u16, u16),
) -> AnyValue {
    object.insert(heap(), field, value, cache);
    value
}

#[no_mangle]
pub extern "C" fn object_get(
    object: ObjectPtr,
    field: StringPtr,
    cache: &mut (u16, u16),
) -> AnyValue {
    object.get(heap(), field, cache).into()
}

/// Object.create
/// TODO(luna): presumably there should be some way to type this correctly
/// as returning a DynObject even though it's ultimately stored in a DynObject
/// and thus is coerced into an Any::Fun
#[no_mangle]
pub extern "C" fn object_create(
    _env: EnvPtr,
    _this: AnyValue,
    maybe_prototype_chain: AnyValue,
) -> AnyValue {
    AnyEnum::Ptr(
        match *maybe_prototype_chain {
            // Create a legitimately empty object. No properties or prototype.
            AnyEnum::Null => object_empty(),
            AnyEnum::Ptr(p) => match p.view() {
                HeapRefView::ObjectPtrPtr(_o) => {
                    // Create new object that inherits from the given prototype
                    let mut new_object = object_empty();
                    // TODO(luna): we could avoid allocating this by interning it in the first place
                    new_object.insert(
                        heap(),
                        static_strings().__proto__,
                        maybe_prototype_chain,
                        &mut no_cache(),
                    );
                    new_object
                }
                _ => log_panic!("not an object or null: {:?}", *maybe_prototype_chain),
            },
            _ => log_panic!("not an object or null: {:?}", *maybe_prototype_chain),
        }
        .as_any_ptr(),
    )
    .into()
}

#[cfg(test)]
mod test {
    use wasm_bindgen_test::wasm_bindgen_test;

    use crate::{
        allocator::no_cache,
        any_value::any_from_i32,
        object::{object_empty, object_get, object_set},
        wasm32::heap,
    };

    #[wasm_bindgen_test]
    fn inline_cache_invalidation() {
        let o = object_empty();
        let an_x = heap().alloc_str_or_gc("x");
        object_set(o, an_x, any_from_i32(10), &mut no_cache());
        let mut cache = no_cache();
        assert_eq!(
            any_from_i32(10),
            object_get(o, heap().alloc_str_or_gc("x"), &mut cache)
        );
        assert_eq!(cache, (1, 0));
        let o2 = object_empty();
        object_set(
            o2,
            heap().alloc_str_or_gc("y"),
            any_from_i32(11),
            &mut no_cache(),
        );
        object_set(o2, an_x, any_from_i32(12), &mut no_cache());
        assert_eq!(
            any_from_i32(12),
            object_get(o2, heap().alloc_str_or_gc("x"), &mut cache)
        );
    }
}
