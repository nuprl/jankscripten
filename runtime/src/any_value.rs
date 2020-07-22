//! An enum that can store any type known to the runtime

pub use crate::allocator::AnyPtr;
use crate::closure::{AnyClosure, AnyClosureVal};
use crate::heap;
use crate::i64_val::*;
use crate::string::StrPtr;

/// this is the actual Any type, however it should never be returned or
/// accepted as a parameter, because rust will refuse to turn it into an i64
///
/// These (along with itself) are the values of NotWasm. A value is something
/// that can be stored in a variable or put on the operand stack.
///
/// don't put anything bigger than 32 bits in here. in order for Any to be
/// an immediate value we need them to be 64 bits due to wasm restrictions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AnyEnum<'a> {
    I32(i32),
    F64(*const f64),
    Bool(bool),
    Ptr(AnyPtr<'a>),
    StrPtr(StrPtr),
    Fn(AnyClosure),
}
impl<'a> AsI64 for AnyEnum<'a> {}
pub type AnyValue<'a> = I64Val<AnyEnum<'a>>;

macro_rules! decl_proj_fns {
    ($from_name:ident, $to_name:ident, $any_name:ident, $ty:ty) => {
        #[no_mangle]
        pub extern "C" fn $from_name<'a>(val: $ty) -> AnyValue<'a> {
            AnyEnum::$any_name(val).into()
        }
        #[no_mangle]
        pub extern "C" fn $to_name<'a>(val: AnyValue<'a>) -> $ty {
            if let AnyEnum::$any_name(inner) = *val {
                inner.into()
            } else {
                panic!("unwrap incorrect type {}", stringify!($any_name));
            }
        }
    };
}

#[no_mangle]
pub extern "C" fn any_to_f64(any: AnyValue) -> f64 {
    if let AnyEnum::F64(ptr) = *any {
        return unsafe { *ptr };
    }
    panic!("any_to_f64 did not receive an AnyEnum::F64");
}

#[no_mangle]
pub extern "C" fn f64_to_any(x: f64) -> AnyValue<'static> {
    return heap().f64_to_any(x);
}

#[no_mangle]
pub extern "C" fn any_from_any_closure<'a>(val: AnyClosureVal) -> AnyValue<'a> {
    AnyEnum::Fn(*val).into()
}
#[no_mangle]
pub extern "C" fn any_to_any_closure<'a>(val: AnyValue<'a>) -> AnyClosureVal {
    if let AnyEnum::Fn(inner) = *val {
        inner.into()
    } else {
        panic!("unwrap incorrect type {}", stringify!(Fn));
    }
}

decl_proj_fns!(any_from_i32, any_to_i32, I32, i32);
decl_proj_fns!(any_from_bool, any_to_bool, Bool, bool);
decl_proj_fns!(any_from_ptr, any_to_ptr, Ptr, AnyPtr<'a>);

#[cfg(test)]
mod test {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn any_size_is_64() {
        assert_eq!(std::mem::size_of::<AnyValue>(), 8);
        assert_eq!(std::mem::size_of::<AnyEnum>(), 8);
        assert_eq!(std::mem::size_of::<Option<AnyEnum>>(), 8);
    }
    #[test]
    fn any_size_is_128() {
        assert_eq!(std::mem::size_of::<AnyValue>(), 16);
        assert_eq!(std::mem::size_of::<AnyEnum>(), 16);
        assert_eq!(std::mem::size_of::<Option<AnyEnum>>(), 16);
    }
}
