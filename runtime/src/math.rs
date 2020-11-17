//! wouldn't it be so nice if these didn't have to be ground types

use crate::any_value::{AnyValue as Any, *};
use crate::coercions::i32s_or_as_f64s_any;
use crate::heap_types::EnvPtr;

#[no_mangle]
pub extern "C" fn math_plus(a: Any, b: Any) -> Any {
    if let Some(res) = i32s_or_as_f64s_any(a, b, |a, b| a + b, |a, b| a + b) {
        res
    } else {
        log_panic!("unsupported for +: {:?}, {:?}", a, b)
    }
}

#[no_mangle]
pub extern "C" fn math_sqrt(_: EnvPtr, _this: Any, a: Any) -> Any {
    f64_to_any(any_to_f64(a).sqrt())
}
#[no_mangle]
pub extern "C" fn math_sin(_: EnvPtr, _this: Any, a: Any) -> Any {
    f64_to_any(any_to_f64(a).sin())
}
#[no_mangle]
pub extern "C" fn math_abs(_: EnvPtr, _this: Any, a: Any) -> Any {
    f64_to_any(any_to_f64(a).abs())
}
#[no_mangle]
pub extern "C" fn math_min(_: EnvPtr, _this: Any, a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(
        a,
        b,
        |a, b| if a < b { a } else { b },
        |a, b| std::cmp::min(a, b),
    )
    .unwrap()
}
#[no_mangle]
pub extern "C" fn math_max(_: EnvPtr, _this: Any, a: Any, b: Any) -> Any {
    i32s_or_as_f64s_any(
        a,
        b,
        |a, b| if a > b { a } else { b },
        |a, b| std::cmp::max(a, b),
    )
    .unwrap()
}
