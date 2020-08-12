//! janky ops: see libjankscripten/rts_function.rs

use super::any_value::*;

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
pub extern "C" fn janky_over(a: Any, b: Any) -> f64 {
    i32s_or_as_f64s(a, b, |a, b| a / b, |a, b| a as f64 / b as f64).expect("unsupported for /")
}
pub extern "C" fn janky_mod(a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(a, b, |a, b| a % b, |a, b| a % b).expect("unsupported for %")
}
pub extern "C" fn janky_equal(a: Any, b: Any) -> bool {
    a == b
}
