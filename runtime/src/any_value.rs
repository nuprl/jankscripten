//! An enum that can store any type known to the runtime

pub use crate::allocator::{AnyPtr, HeapRefView};
use crate::heap;
use std::fmt::{Debug, Formatter, Result as FmtResult};
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
pub enum AnyEnum {
    I32(i32),
    F64(*const f64),
    Bool(bool),
    Ptr(AnyPtr),
    Fn(u32),
    Undefined,
    Null,
}

impl std::fmt::Debug for AnyEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "I32({})", n),
            F64(ptr) => write!(f, "F64({})", unsafe { ptr.read() }),
            Bool(b) => write!(f, "Bool({})", b),
            Ptr(ptr) => write!(f, "{:?}", ptr.view()),
            Fn(n) => write!(f, "Fn({})", n),
            Undefined => write!(f, "undefined"),
            Null => write!(f, "null"),
        }
    }
}
impl std::fmt::Display for AnyEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "{}", n),
            F64(ptr) => write!(f, "{}", unsafe { ptr.read() }),
            Bool(b) => write!(f, "{}", b),
            Ptr(p) => write!(f, "{}", p.view()),
            Fn(_) | Undefined | Null => write!(f, "{:?}", self),
        }
    }
}
impl std::fmt::Display for HeapRefView {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use HeapRefView::*;
        match *self {
            I32(_) => panic!("ref inside any"),
            String(s) => write!(f, "{}", &*s),
            HT(ht) => write!(f, "{:?}", *ht),
            Array(a) => write!(f, "{:?}", *a),
            Any(a) => write!(f, "{}", **a),
            Class(_) => panic!("shouldn't have object data as value"),
            ObjectPtrPtr(_o) => todo!("TODO(luna): toString"),
        }
    }
}
impl std::fmt::Debug for HeapRefView {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use HeapRefView::*;
        match *self {
            I32(_) => panic!("ref inside any"),
            String(_) => write!(f, "String({})", self),
            HT(_) => write!(f, "HT({})", self),
            Array(_) => write!(f, "Array({})", self),
            Any(_) => write!(f, "Any({})", self),
            Class(_) => panic!("shouldn't have object data as value"),
            ObjectPtrPtr(_) => write!(f, "DynObject({})", self),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct AnyValue {
    #[cfg(target_pointer_width = "32")]
    val: u64,
    #[cfg(target_pointer_width = "64")]
    val: u128,
}

impl Deref for AnyValue {
    type Target = AnyEnum;
    fn deref(&self) -> &Self::Target {
        let ptr = self as *const Self as *const Self::Target;
        unsafe { &*ptr }
    }
}
impl DerefMut for AnyValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let ptr = self as *mut Self as *mut Self::Target;
        unsafe { &mut *ptr }
    }
}
impl Debug for AnyValue {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?} ({})", **self, self.val)
    }
}
impl From<AnyEnum> for AnyValue {
    fn from(val: AnyEnum) -> Self {
        unsafe { std::mem::transmute(val) }
    }
}
impl PartialEq<Self> for AnyValue {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

macro_rules! decl_proj_fns {
    ($from_name:ident, $to_name:ident, $any_name:ident, $ty:ty) => {
        #[no_mangle]
        pub extern "C" fn $from_name(val: $ty) -> AnyValue {
            AnyEnum::$any_name(val).into()
        }
        #[no_mangle]
        pub extern "C" fn $to_name(val: AnyValue) -> $ty {
            if let AnyEnum::$any_name(inner) = *val {
                inner
            } else {
                log!("cannot unwrap {:?} as {}", *val, stringify!($any_name));
                panic!();
            }
        }
    };
}

#[no_mangle]
pub extern "C" fn any_to_f64(any: AnyValue) -> f64 {
    match *any {
        AnyEnum::I32(i) => i as f64,
        AnyEnum::F64(f) => unsafe { *f },
        AnyEnum::Bool(b) => b as i32 as f64,
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::I32(i) => *i as f64,
            HeapRefView::String(s) => s.parse().unwrap_or(f64::NAN),
            _ => f64::NAN,
        },
        AnyEnum::Fn(_) => f64::NAN,
        AnyEnum::Undefined => f64::NAN,
        AnyEnum::Null => 0.,
    }
}

#[no_mangle]
pub extern "C" fn f64_to_any(x: f64) -> AnyValue {
    return heap().f64_to_any(x);
}

decl_proj_fns!(any_from_i32, any_to_i32, I32, i32);
decl_proj_fns!(any_from_bool, any_to_bool, Bool, bool);
decl_proj_fns!(any_from_ptr, any_to_ptr, Ptr, AnyPtr);
decl_proj_fns!(any_from_fn, any_to_fn, Fn, u32);

#[no_mangle]
pub extern "C" fn get_undefined() -> AnyValue {
    AnyEnum::Undefined.into()
}
#[no_mangle]
pub extern "C" fn get_null() -> AnyValue {
    AnyEnum::Null.into()
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
