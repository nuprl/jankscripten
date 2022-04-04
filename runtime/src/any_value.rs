//! An enum that can store any type known to the runtime

pub use crate::allocator::{heap_types::EnvPtr, AnyPtr, HeapRefView};
use crate::closure::{closure_env, Closure, ClosureVal};
use crate::i64_val::*;
use crate::string::StringPtr;
use crate::wasm32::heap;
use crate::HeapPtr;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

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
    /// How to hold functions in Any needs to be further discussed. should
    /// arity be included to arity-mismatch correctly? for now, no
    ///
    /// also, eventually we will distinguish functions and closures in any,
    /// because some functions will not be closure-converted. but not yet
    Closure(Closure),
    Undefined,
    Null,
}

impl Debug for AnyEnum {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "I32({})", n),
            F64(ptr) => write!(f, "F64({})", unsafe { ptr.read() }),
            Bool(b) => write!(f, "Bool({})", b),
            Ptr(ptr) => write!(f, "{:?}", ptr.view()),
            Closure(n) => write!(f, "Closure({:?})", n),
            Undefined => write!(f, "undefined"),
            Null => write!(f, "null"),
        }
    }
}

/// Note: The `Display` trait on NotWasm structs should implement the
/// internal `ToString` operation described in the ECMAScript spec:
/// https://www.ecma-international.org/ecma-262/5.1/#sec-9.8
impl Display for AnyEnum {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use AnyEnum::*;
        match self {
            I32(n) => write!(f, "{}", n),
            F64(ptr) => write!(f, "{}", unsafe { ptr.read() }),
            // TODO(luna): when we get our fancy rust-runtime-interning system,
            // use that here
            Bool(b) => write!(f, "{}", b),
            Closure(closure) => write!(f, "{}", closure),
            Ptr(p) => write!(f, "{}", p.view()),
            Undefined => write!(f, "undefined"),
            Null => write!(f, "null"),
        }
    }
}

/// Note: The `Display` trait on NotWasm structs should implement the
/// internal `ToString` operation described in the ECMAScript spec:
/// https://www.ecma-international.org/ecma-262/5.1/#sec-9.8
impl Display for HeapRefView {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use HeapRefView::*;
        match *self {
            String(s) => write!(f, "{}", &*s),
            Any(a) => write!(f, "{}", **a),
            Array(a) => {
                write!(f, "[")?;
                for elem in &*a {
                    write!(f, "{}, ", **elem)?;
                }
                write!(f, "]")
            }
            Class(_) => log_panic!("shouldn't have object data as value"),
            ObjectPtrPtr(_o) => log_panic!("TODO(luna): toString"),
            NonPtr32(_) | MutF64(_) | Ptr(_) => log_panic!("ref inside any"),
            Env(_) => log_panic!("not a value"),
            HT(_) | Array(_) => log_panic!("Display trait not implemented"),
        }
    }
}
impl Debug for HeapRefView {
    // please observe carefully that heap refs that are not values / should
    // never be printed begin with a ! but are still printed because this
    // is for *debugging*
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use HeapRefView::*;
        match *self {
            String(_) => write!(f, "String({})", self),
            HT(_) => write!(f, "HT({})", self),
            Array(_) => write!(f, "Array({})", self),
            Any(_) => write!(f, "Any({})", self),
            Class(_) => write!(f, "!ObjData"),
            ObjectPtrPtr(o) => write!(f, "DynObject({:?})", o),
            NonPtr32(v) => write!(f, "!Ref({})", *v),
            MutF64(v) => write!(f, "!F64({})", *v),
            Ptr(p) => write!(f, "!Ref({:?})", p),
            Env(e) => write!(f, "Env({:?})", e),
        }
    }
}

impl AsI64 for AnyEnum {}
pub type AnyValue = I64Val<AnyEnum>;

// This macro automatically generates simple projection functions.
macro_rules! decl_proj_fns {
    ($from_name:ident, $to_name:ident, $any_name:ident, $ty:ty) => {
        #[no_mangle]
        pub extern "C" fn $from_name(val: $ty) -> AnyValue {
            AnyEnum::$any_name(val).into()
        }
        #[no_mangle]
        pub extern "C" fn $to_name(val: AnyValue) -> $ty {
            if let AnyEnum::$any_name(inner) = *val {
                inner.into()
            } else {
                log!("cannot unwrap {:?} as {}", *val, stringify!($any_name));
                panic!("");
            }
        }
    };
}

// Automatically generate these simple projection functions.
// The rest will be specified manually.
decl_proj_fns!(any_from_i32, any_to_i32, I32, i32);

#[no_mangle]
pub extern "C" fn any_to_f64(any: AnyValue) -> f64 {
    match *any {
        AnyEnum::I32(i) => i as f64,
        AnyEnum::F64(f) => unsafe { *f },
        AnyEnum::Bool(b) => b as i32 as f64,
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::NonPtr32(_) => panic!("ref is not a value"),
            HeapRefView::String(s) => s.parse().unwrap_or(f64::NAN),
            _ => f64::NAN,
        },
        AnyEnum::Closure(_) => f64::NAN,
        AnyEnum::Undefined => f64::NAN,
        AnyEnum::Null => 0.,
    }
}

#[no_mangle]
pub extern "C" fn f64_to_any(x: f64) -> AnyValue {
    return heap().f64_to_any(x);
}

#[no_mangle]
pub extern "C" fn any_from_closure<'a>(val: ClosureVal) -> AnyValue {
    AnyEnum::Closure(*val).into()
}
#[no_mangle]
pub extern "C" fn any_to_closure<'a>(val: AnyValue) -> ClosureVal {
    if let AnyEnum::Closure(inner) = *val {
        inner.into()
    } else {
        panic!("unwrap incorrect type {}", stringify!(Fn));
    }
}

#[no_mangle]
pub extern "C" fn any_from_fn<'a>(val: u32) -> AnyValue {
    AnyEnum::Closure(Closure(unsafe { EnvPtr::null() }, val as u16)).into()
}

#[no_mangle]
pub extern "C" fn any_to_ptr<'a>(val: AnyValue) -> AnyPtr {
    match *val {
        AnyEnum::Ptr(ptr) => ptr.into(),
        AnyEnum::Closure(clos) => closure_env(clos.into()).fn_obj().as_any_ptr(),
        unknown_val => {
            log!("cannot unwrap {:?} as Ptr", unknown_val);
            panic!("");
        }
    }
}

#[no_mangle]
pub extern "C" fn any_from_ptr<'a>(val: AnyPtr) -> AnyValue {
    AnyEnum::Ptr(val).into()
}

#[no_mangle]
pub extern "C" fn any_to_bool<'a>(val: AnyValue) -> bool {
    match *val {
        AnyEnum::I32(i) => i != 0,
        AnyEnum::F64(p) => {
            let f = unsafe { *p };
            !f.is_nan() && f != 0.0
        }
        AnyEnum::Bool(b) => b,
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::NonPtr32(_) => panic!("ref is not a value"),
            HeapRefView::String(s) => &*s != "",
            HeapRefView::Array(_) => true,
            HeapRefView::ObjectPtrPtr(_) => true,
            _ => log_panic!("TODO: any_to_bool {:?}", val),
        },
        AnyEnum::Closure(_) => true,
        AnyEnum::Undefined => false,
        AnyEnum::Null => false,
    }
}
#[no_mangle]
pub extern "C" fn any_from_bool<'a>(val: bool) -> AnyValue {
    AnyEnum::Bool(val).into()
}

/// Converts the given value to a Rust string. This should implement the
/// internal `ToString` operation described in the ECMAScript standard:
/// https://www.ecma-international.org/ecma-262/5.1/#sec-9.8
///
/// `any_to_string` reuses the `fmt::Display` trait on NotWasm structs,
/// which should be implemented according to the above JS spec.
#[no_mangle]
pub fn any_to_string(val: AnyValue) -> StringPtr {
    let string = val.to_string();
    heap().alloc_str_or_gc(string.as_str())
}

#[no_mangle]
pub extern "C" fn get_undefined() -> AnyValue {
    AnyEnum::Undefined.into()
}

#[no_mangle]
pub extern "C" fn get_null() -> AnyValue {
    AnyEnum::Null.into()
}

/// Is the given any value an object?
#[no_mangle]
pub extern "C" fn any_is_object(val: AnyValue) -> bool {
    match *val {
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::ObjectPtrPtr(_) => true,
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    /// This serves as documentation of the discriminants and a test that they
    /// are as expected. Keeping them in the same place avoids divergence
    /// This is stable across compiles in the same compiler as long as AnyEnum
    /// isn't changed. This seems to closely follow just being sequential by
    /// the definition
    /// https://doc.rust-lang.org/std/mem/fn.discriminant.html
    #[wasm_bindgen_test]
    fn abi_any_discriminants_stable() {
        assert_disc(AnyEnum::I32(0), 0);
        assert_disc(AnyEnum::F64(&0.0 as *const f64), 1);
        assert_disc(AnyEnum::Bool(false), 2);
        assert_disc(AnyEnum::Ptr(heap().alloc_str_or_gc(".").as_any_ptr()), 3);
        assert_disc(AnyEnum::Closure(Closure(unsafe { EnvPtr::null() }, 0)), 4);
        assert_disc(AnyEnum::Undefined, 5);
        assert_disc(AnyEnum::Null, 6);
    }
    #[wasm_bindgen_test]
    fn any_size_is_64() {
        assert_eq!(std::mem::size_of::<AnyValue>(), 8, "AnyValue");
        assert_eq!(std::mem::size_of::<AnyEnum>(), 8, "AnyEnum");
        assert_eq!(std::mem::size_of::<Option<AnyEnum>>(), 8, "Option<AnyEnum>");
    }
    #[test]
    fn any_size_is_128() {
        assert_eq!(std::mem::size_of::<AnyValue>(), 16);
        assert_eq!(std::mem::size_of::<AnyEnum>(), 16);
        assert_eq!(std::mem::size_of::<Option<AnyEnum>>(), 16);
    }
    /// as an optimization in libjankscripten, we avoid certain runtime calls
    /// by making some assumptions about the structure of an any_value. this
    /// is technically undefined behavior in rust. this test ensures that the
    /// undefined behavior is the behavior we expect. since we literally only
    /// target one platform, that seems good enough
    /// the assumed structure for 32-bit is:
    /// [32-bit payload data][32-bit descriminant/padding]
    #[wasm_bindgen_test]
    fn any_value_has_predicted_structure_32() {
        use crate::allocator::Tag;
        assert_eq!(18, cast_predicted_structure_32(AnyEnum::I32(18)));
        let f = 7.34;
        let point_f = &f as *const _;
        assert_eq!(
            point_f,
            cast_predicted_structure_32(AnyEnum::F64(point_f)) as *const _
        );
        assert_eq!(true, cast_predicted_structure_32(AnyEnum::Bool(true)) != 0);
        // this is the only way to construct a tag from outside allocator
        let mut tag = Tag::object(5);
        let point_tag = &mut tag as *mut _;
        let anyptr = unsafe { AnyPtr::new(point_tag) };
        assert_eq!(
            point_tag,
            cast_predicted_structure_32(AnyEnum::Ptr(anyptr)) as *mut _
        );
    }
    fn cast_predicted_structure_32(a: AnyEnum) -> u32 {
        (AnyValue::from(a).raw_val() >> 32) as u32
    }
    /// similarly to [any_value_has_predicted_structure_32], with 48-bit
    /// structs, the predicted structure is:
    /// an I64Val<Closure> looks like this:
    /// [16-bit padding][48-bit closure]
    /// an I64Val<AnyEnum::Closure> looks like this:
    /// [48-bit closure][16-bit descriminant/padding]
    #[wasm_bindgen_test]
    fn any_value_has_predicted_structure_48() {
        use crate::closure::*;
        use crate::env::*;
        use crate::object::*;
        crate::init();
        let fake_env = unsafe {
            let fake_env = env_alloc(1, object_empty());
            env_init_at(fake_env, 0, AnyEnum::Undefined.into());
            fake_env
        };
        let fake_closure = closure_new(fake_env, 13);
        let into_any = any_from_closure(fake_closure);
        log!("{:064b}", fake_closure.raw_val());
        log!("{:064b}", into_any.raw_val());
        let raw_data_of_shifted = into_any.raw_val() >> 16;
        let calculated_closure: ClosureVal = unsafe { std::mem::transmute(raw_data_of_shifted) };
        assert_eq!(calculated_closure, fake_closure);
    }
    fn assert_disc(any: AnyEnum, expected: usize) {
        use std::mem::discriminant;
        use std::mem::Discriminant;
        union DescInt<T> {
            opaque: Discriminant<T>,
            int: usize,
        }
        let un = DescInt {
            opaque: discriminant(&any),
        };
        let as_int = unsafe { un.int };
        assert_eq!(as_int, expected);
        // Memory layout test
        assert_eq!(0x0000_00ff & as_int, expected);
    }
}
