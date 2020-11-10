//! janky ops: see libjankscripten/rts_function.rs

use crate::any_value::{AnyValue as Any, *};
use crate::coercions::*;
use crate::heap;
use crate::string::StringPtr;
// use crate::heap_types::*;

/// The JavaScript `+` operator. This isn't implemented to spec:
/// https://www.ecma-international.org/ecma-262/5.1/#sec-11.6.1
/// because it doesn't first try to coerce its arguments to primitives.
/// It instead uses a simple trick to emulate most of the behavior in the spec:
/// if either of the arguments are NotWasm ptrs, both will be coerced into
/// strings, and string concatenation will happen instead. If both of the
/// arguments are NOT pointers, then they're both primitives and
/// the user is expecting mathematical plus.
#[no_mangle]
pub extern "C" fn janky_plus(a: Any, b: Any) -> Any {
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
#[no_mangle]
pub extern "C" fn janky_typeof(a: Any) -> StringPtr {
    typeof_as_str(a).into()
}
#[no_mangle]
pub extern "C" fn janky_delete(_a: Any, _b: Any) -> bool {
    todo!()
}
#[no_mangle]
pub extern "C" fn janky_void(_: Any) -> Any {
    AnyEnum::Undefined.into()
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
            HeapRefView::NonPtr32(_)
            | HeapRefView::MutF64(_)
            | HeapRefView::Ptr(_)
            | HeapRefView::Env(_) => panic!("not a value"),
        },
        AnyEnum::Closure(_) => "function",
        AnyEnum::Undefined => "undefined",
        AnyEnum::Null => "object",
    }
}
