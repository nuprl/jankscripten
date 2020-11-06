use crate::any_value::{AnyValue as Any, *};
use crate::heap_types::EnvPtr;
use crate::wasm32::heap;

/// this returns either I32(truncated result) or F64(f64::NAN)
#[no_mangle]
pub extern "C" fn parse_int(env: EnvPtr, this: Any, a: Any) -> Any {
    match *a {
        AnyEnum::I32(i) => AnyEnum::I32(i).into(),
        AnyEnum::F64(f) => AnyEnum::I32(f as i32).into(),
        AnyEnum::Bool(_) | AnyEnum::Closure(_) | AnyEnum::Undefined | AnyEnum::Null => {
            heap().f64_to_any(f64::NAN)
        }
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::String(s) => match s.parse() {
                Ok(o) => AnyEnum::I32(o).into(),
                Err(_) => todo!("support partial parse"),
            },
            HeapRefView::HT(_) | HeapRefView::ObjectPtrPtr(_) => heap().f64_to_any(f64::NAN),
            HeapRefView::Array(a) => parse_int(env, this, a[0]),
            HeapRefView::Any(what) => parse_int(env, this, *what),
            HeapRefView::Class(_)
            | HeapRefView::NonPtr32(_)
            | HeapRefView::MutF64(_)
            | HeapRefView::Ptr(_)
            | HeapRefView::Env(_) => panic!("not a value"),
        },
    }
}

/// we return undefined for type messiness for now
#[no_mangle]
pub extern "C" fn console_log(_: EnvPtr, _this: Any, a: Any) -> Any {
    log!("{}", *a);
    AnyEnum::Undefined.into()
}
