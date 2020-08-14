//! janky ops: see libjankscripten/rts_function.rs

use super::any_value::*;
use super::string::*;

type Any = AnyValue<'static>;

fn i32s_or_as_f64s<T, F, I>(a: Any, b: Any, floats: F, ints: I) -> Option<T>
where
    F: FnOnce(f64, f64) -> T,
    I: FnOnce(i32, i32) -> T,
{
    match (*a, *b) {
        (AnyEnum::F64(a), AnyEnum::F64(b)) => Some(floats(unsafe { *a }, unsafe { *b })),
        (AnyEnum::I32(a), AnyEnum::F64(b)) => Some(floats(a as f64, unsafe { *b })),
        (AnyEnum::F64(a), AnyEnum::I32(b)) => Some(floats(unsafe { *a }, b as f64)),
        (AnyEnum::I32(a), AnyEnum::I32(b)) => Some(ints(a, b)),
        _ => None,
    }
}

fn i32s_or_as_f64s_any(
    a: Any,
    b: Any,
    floats: fn(f64, f64) -> f64,
    ints: fn(i32, i32) -> i32,
) -> Option<Any> {
    i32s_or_as_f64s(
        a,
        b,
        |a, b| f64_to_any(floats(a, b)),
        |a, b| any_from_i32(ints(a, b)),
    )
}

#[no_mangle]
pub extern "C" fn janky_plus(a: Any, b: Any) -> Any {
    if let Some(res) = i32s_or_as_f64s_any(a, b, |a, b| a + b, |a, b| a + b) {
        res
    } else if let (AnyEnum::StrPtr(_a), AnyEnum::StrPtr(_b)) = (*a, *b) {
        todo!("strings +")
    } else {
        // TODO(luna): these panics in this file should be exceptions, when we support those
        panic!("unsupported for +: {:?}, {:?}", a, b)
    }
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
pub extern "C" fn janky_equal(a: Any, b: Any) -> bool {
    a == b
}
#[no_mangle]
pub extern "C" fn janky_typeof(a: Any) -> StrPtr {
    str_as_ptr(typeof_as_str(a))
}
#[no_mangle]
pub extern "C" fn janky_delete(_a: Any, _b: Any) -> bool {
    todo!()
}

fn typeof_as_str(a: Any) -> &'static str {
    match *a {
        AnyEnum::I32(_) | AnyEnum::F64(_) => "number",
        AnyEnum::Bool(_) => "boolean",
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::I32(_) => "number",
            HeapRefView::String(_) => "string",
            HeapRefView::HT(_) | HeapRefView::Array(_) | HeapRefView::ObjectPtrPtr(_) => "object",
            HeapRefView::Any(what) => typeof_as_str(*what),
            HeapRefView::Class(_) => panic!("shouldn't be able to typeof non-value object data"),
        },
        AnyEnum::StrPtr(_) => "string",
        AnyEnum::Fn(_) => "function",
        AnyEnum::Undefined => "undefined",
        AnyEnum::Null => "object",
    }
}
