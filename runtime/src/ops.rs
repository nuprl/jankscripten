//! janky ops: see libjankscripten/rts_function.rs

use crate::any_value::{AnyValue as Any, *};
use crate::coercions::*;
use crate::string::StringPtr;

#[no_mangle]
pub extern "C" fn janky_plus(a: Any, b: Any) -> Any {
    if let Some(res) = i32s_or_as_f64s_any(a, b, |a, b| a + b, |a, b| a + b) {
        res
    } else if let (AnyEnum::Ptr(_a), AnyEnum::Ptr(_b)) = (*a, *b) {
        todo!("strings +")
    } else {
        // TODO(luna): these panics in this file should be exceptions, when we support those
        panic!("unsupported for +: {:?}, {:?}", a, b)
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
        AnyEnum::Fn(_) => "function",
        AnyEnum::Undefined => "undefined",
        AnyEnum::Null => "object",
    }
}
