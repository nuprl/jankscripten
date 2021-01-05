//! janky ops: see libjankscripten/rts_function.rs

use crate::any_value::{AnyValue as Any, *};
use crate::coercions::*;
use crate::heap;
use crate::heap_types::ObjectPtr;
use crate::static_strings::static_strings;
use crate::string::*;
use crate::util::unwrap_log;
use crate::HeapPtr;

/// A helper function for the JavaScript `+` operator. This is called
/// by `jnks_plus` in the NotWasm runtime, which is the full implementation
/// of `+`.
///
/// This function performs `+` ASSUMING its arguments are primitives
/// (i.e. not objects). This rules down its cases into string concatenation
/// and math.
#[no_mangle]
pub extern "C" fn janky_primitive_plus(a: Any, b: Any) -> Any {
    // First check: are either `a` or `b` pointers? If so, they'll be coerced
    // to strings and we'll perform string concatenation.
    match (*a, *b) {
        (AnyEnum::Ptr(_), AnyEnum::Ptr(_)) | (_, AnyEnum::Ptr(_)) | (AnyEnum::Ptr(_), _) => {
            // coerce arguments to strings
            let a_string = any_to_string(a);
            let b_string = any_to_string(b);

            // combine them
            let combined = format!("{}{}", a_string, b_string);

            // allocate this into a string
            let combined = heap().alloc_str_or_gc(&combined);
            unsafe { AnyEnum::Ptr(std::mem::transmute(combined)).into() }
        }
        // We have two primitive values. Try to perform numeric addition on them.
        (_, _) => {
            if let Some(res) = i32s_or_as_f64s_any(a, b, |a, b| a + b, |a, b| a + b) {
                res
            } else {
                // TODO(luna): these panics in this file should be exceptions, when we support those
                log_panic!("unsupported for +: {:?}, {:?}", a, b)
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn janky_minus(a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(a, b, |a, b| a - b, |a, b| a - b).expect("unsupported for -")
}
#[no_mangle]
pub extern "C" fn janky_times(a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(a, b, |a, b| a * b, |a, b| a * b).expect("unsupported for *")
}
#[no_mangle]
pub extern "C" fn janky_over(a: Any, b: Any) -> f64 {
    i32s_or_as_f64s(a, b, |a, b| a / b, |a, b| a as f64 / b as f64).expect("unsupported for /")
}
#[no_mangle]
pub extern "C" fn janky_mod(a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(a, b, |a, b| a % b, |a, b| a % b).expect("unsupported for %")
}
#[no_mangle]
pub extern "C" fn janky_mod_f64(a: f64, b: f64) -> f64 {
    a % b
}
#[no_mangle]
pub extern "C" fn janky_strict_equal(a: Any, b: Any) -> bool {
    a == b
}
#[no_mangle]
pub extern "C" fn janky_equal(a: Any, b: Any) -> bool {
    abstract_eq(*a, *b)
}
#[no_mangle]
pub extern "C" fn janky_strict_not_equal(a: Any, b: Any) -> bool {
    a != b
}
#[no_mangle]
pub extern "C" fn janky_not_equal(a: Any, b: Any) -> bool {
    !abstract_eq(*a, *b)
}
/// TODO(luna): one could intern these values in our own interning style
/// to avoid needing to allocate for this
///
/// the best imaginable way would be to make most of alloc_str_or_gc a `const
/// fn` and then do const NAME: [u8; _] = intern_str("hello");
#[no_mangle]
pub extern "C" fn janky_typeof(a: Any) -> StringPtr {
    heap().alloc_str_or_gc(typeof_as_str(a))
}
#[no_mangle]
pub extern "C" fn janky_delete(_a: Any, _b: Any) -> bool {
    todo!()
}
#[no_mangle]
pub extern "C" fn janky_void(_: Any) -> Any {
    AnyEnum::Undefined.into()
}

#[no_mangle]
pub extern "C" fn instance_of(a: Any, b: Any) -> bool {
    if let AnyEnum::Closure(constructor) = *b {
        // `Constructor.prototype instanceof Constructor` is false. to make
        // this algorithm much cleaner, and avoid constant casts to Any, the
        // helper function inclusive_instance_of is used
        if let Some(obj) = match_object(*a) {
            // two step process needed because of packed borrows
            let constructor_env = constructor.0;
            let constructor_obj = constructor_env.fn_obj();
            let constructor_prototype =
                constructor_obj.get(heap(), static_strings().prototype, &mut -1);
            let should_match = unwrap_log(match_object(constructor_prototype), "non-obj prototype");
            let proto_shim = obj.get(heap(), static_strings().__proto__, &mut -1);
            if let Some(other) = match_object(proto_shim) {
                inclusive_instance_of(other, should_match)
            } else {
                // if obj has no / bogus prototype, it's prototype cannot match anything
                false
            }
        } else {
            // if obj is not an obj, `obj instanceof _` is false
            false
        }
    } else {
        log_panic!("TypeError: invalid 'instanceof' operand {:?}", b)
    }
}

/// helper function for instance_of
///
/// though `Constructor.prototype instanceof Constructor` is false,
/// `inclusive_instance_of(Constructor.prototype, Constructor)` is true
fn inclusive_instance_of(obj: ObjectPtr, should_match: ObjectPtr) -> bool {
    // this is heavily adapted from Mark's ObjectDataPtr::get, which
    // presents the algorithm for walking the prototype chain
    //
    // however, instead of walking until a value is found, we walk until a
    // prototype is pointer-equal to should_match
    //
    // There are 3 cases to consider for `obj instanceof Constructor`:
    //
    // 1. `obj === should_match` ~~> `true`. (a prototype of) the object matches the constructor provided
    // 2. `obj.__proto__` exists ~~> `obj.__proto__ instanceof Prototype`
    //                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //                                 this will recursively search
    //                                      the prototype chain
    // 3. "__proto__" field is missing or null ~~> `false`. the leaf has been reached
    //
    // Test Case 1: `obj === should_match`. (ObjectPtr == means javascript ===)
    if obj == should_match {
        // This is Case 1
        true
    } else {
        // Test Case 2: `obj` has a field named "__proto__".
        let proto_shim = obj.get(heap(), static_strings().__proto__, &mut -1);
        // Is it a real object that we can read from? As opposed to `null`
        // or any other type of value.
        if let Some(proto_obj) = match_object(proto_shim) {
            // this is Case 2. Perform the same read on the proto obj.
            // -1 because we don't cache reads on the prototype chain
            inclusive_instance_of(proto_obj, should_match)
        } else {
            // This is case 3. `__proto__ is missing or not an object
            false
        }
    }
}

fn typeof_as_str(a: Any) -> &'static str {
    match *a {
        AnyEnum::I32(_) | AnyEnum::F64(_) => "number",
        AnyEnum::Bool(_) => "boolean",
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::String(_) => "string",
            HeapRefView::HT(_) | HeapRefView::Array(_) | HeapRefView::ObjectPtrPtr(_) => "object",
            HeapRefView::Any(what) => typeof_as_str(*what),
            HeapRefView::Class(_) => panic!("shouldn't be able to typeof non-value object data"),
            HeapRefView::MutF64(_) => "number",
            HeapRefView::NonPtr32(_) | HeapRefView::Ptr(_) | HeapRefView::Env(_) => {
                panic!("not a value")
            }
        },
        AnyEnum::Closure(_) => "function",
        AnyEnum::Undefined => "undefined",
        AnyEnum::Null => "object",
    }
}
