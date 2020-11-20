//! janky ops: see libjankscripten/rts_function.rs

use crate::any_value::{AnyValue as Any, *};
use crate::coercions::*;
use crate::heap;
use crate::string::*;
use crate::HeapPtr;
// use crate::heap_types::*;

#[no_mangle]
pub extern "C" fn janky_plus(a: Any, b: Any) -> Any {
    if let Some(res) = i32s_or_as_f64s_any(a, b, |a, b| a + b, |a, b| a + b) {
        res
    } else if let Some(a) = is_string(*a) {
        if let Some(b) = is_string(*b) {
            AnyEnum::Ptr(string_append(a, b).as_any_ptr()).into()
        } else {
            // coerce b to string and then append
            AnyEnum::Ptr(string_append(a, any_to_string(b)).as_any_ptr()).into()
        }
    } else {
        // TODO(luna): these panics in this file should be exceptions, when we support those
        log_panic!("unsupported for +: {:?}, {:?}", a, b)
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
