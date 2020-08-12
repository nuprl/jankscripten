//! An enum that can store any type known to the runtime

pub use crate::allocator::AnyPtr;
use crate::heap;
use crate::string::StrPtr;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// this is the actual Any type, however it should never be returned or
/// accepted as a parameter, because rust will refuse to turn it into an i64
///
/// These (along with itself) are the values of NotWasm. A value is something
/// that can be stored in a variable or put on the operand stack.
///
/// don't put anything bigger than 32 bits in here. in order for Any to be
/// an immediate value we need them to be 64 bits due to wasm restrictions.
#[derive(Clone, Copy, PartialEq)]
pub enum AnyEnum<'a> {
    I32(i32),
    F64(*const f64),
    Bool(bool),
    Ptr(AnyPtr<'a>),
    StrPtr(StrPtr),
    Fn(u32),
    Undefined,
}

impl std::fmt::Debug for AnyEnum<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "I32({})", n),
            F64(ptr) => write!(f, "F64({})", unsafe { ptr.read() }),
            Bool(b) => write!(f, "Bool({})", b),
            Ptr(ptr) => write!(f, "{:?}", ptr.view()),
            StrPtr(s) => write!(f, "StrPtr({})", s),
            Fn(n) => write!(f, "Fn({})", n),
            Undefined => write!(f, "undefined"),
        }
    }
}
impl std::fmt::Display for AnyEnum<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "{}", n),
            F64(ptr) => write!(f, "{}", unsafe { ptr.read() }),
            Bool(b) => write!(f, "{}", b),
            Ptr(_) => write!(f, "{:?}", self), // TODO: impl Display for HeapPtr
            StrPtr(s) => write!(f, "{}", s),
            Fn(_) => write!(f, "{:?}", self),
            Undefined => write!(f, "{:?}", self),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct AnyValue<'a> {
    #[cfg(target_pointer_width = "32")]
    val: u64,
    #[cfg(target_pointer_width = "64")]
    val: u128,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Deref for AnyValue<'a> {
    type Target = AnyEnum<'a>;
    fn deref(&self) -> &Self::Target {
        let ptr = self as *const Self as *const Self::Target;
        unsafe { &*ptr }
    }
}
impl<'a> DerefMut for AnyValue<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let ptr = self as *mut Self as *mut Self::Target;
        unsafe { &mut *ptr }
    }
}
impl Debug for AnyValue<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?} ({})", **self, self.val)
    }
}
impl<'a> From<AnyEnum<'a>> for AnyValue<'a> {
    fn from(val: AnyEnum) -> Self {
        unsafe { std::mem::transmute(val) }
    }
}
impl<'a> PartialEq<Self> for AnyValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

macro_rules! decl_proj_fns {
    ($from_name:ident, $to_name:ident, $any_name:ident, $ty:ty) => {
        #[no_mangle]
        pub extern "C" fn $from_name<'a>(val: $ty) -> AnyValue<'a> {
            AnyEnum::$any_name(val).into()
        }
        #[no_mangle]
        pub extern "C" fn $to_name<'a>(val: AnyValue<'a>) -> $ty {
            if let AnyEnum::$any_name(inner) = *val {
                inner
            } else {
                panic!("unwrap incorrect type {}", stringify!($any_name));
            }
        }
    };
}

#[no_mangle]
pub extern "C" fn any_to_f64(any: AnyValue) -> f64 {
    let any: AnyEnum = unsafe { std::mem::transmute(any) };
    if let AnyEnum::F64(ptr) = any.into() {
        return unsafe { *ptr };
    }
    panic!("any_to_f64 did not receive an AnyEnum::F64");
}

#[no_mangle]
pub extern "C" fn f64_to_any(x: f64) -> AnyValue<'static> {
    return heap().f64_to_any(x);
}

decl_proj_fns!(any_from_i32, any_to_i32, I32, i32);
decl_proj_fns!(any_from_bool, any_to_bool, Bool, bool);
decl_proj_fns!(any_from_ptr, any_to_ptr, Ptr, AnyPtr<'a>);

pub extern "C" fn get_undefined() -> AnyValue<'static> {
    AnyEnum::Undefined.into()
}

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
