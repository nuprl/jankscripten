use super::heap_types::EnvPtr;
use super::i64_val::*;
use super::Tag;

/// this is a closure-converted closure, it represents a cons of an environment
/// (a fixed array) and a function pointer. these two pointers can be combined
/// into one i64 to be used as an immediate value. this closure has had its
/// arity erased, for example if it is not stuck within an any
///
/// you must not obtain a reference to any of its fields, since it is
/// unaligned. https://github.com/rust-lang/rust/issues/27060 it is unaligned
/// because of the need to fit it in Any
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(packed(2))]
pub struct Closure(pub EnvPtr, pub u16);
impl AsI64 for Closure {}
pub type ClosureVal = I64Val<Closure>;

/// Note: The `Display` trait on NotWasm structs should implement the
/// internal `ToString` operation described in the ECMAScript spec:
/// https://www.ecma-international.org/ecma-262/5.1/#sec-9.8
impl std::fmt::Display for Closure {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // one could compile the source function into the wasm strings table
        // to call it back, but we probably never print functions

        // Note from Mark: please avoid implementing this at all costs.
        // This is one of the jankiest features of JavaScript, and none
        // of our benchmarks should rely on this behavior.

        log_panic!("Display not implemented for Closures")
    }
}

/// webassembly has no 16-bit value, but func should no higher than 2^16. it
/// is truncated
#[no_mangle]
pub extern "C" fn closure_new(ptr: EnvPtr, func: u32) -> ClosureVal {
    Closure(ptr, func as u16).into()
}
#[no_mangle]
pub extern "C" fn closure_env(closure: ClosureVal) -> EnvPtr {
    closure.0
}
#[no_mangle]
pub extern "C" fn closure_func(closure: ClosureVal) -> u32 {
    closure.1 as u32
}

/// this is a closure with any arity, which holds its arity along with
/// it
///
/// this is to allow arity mismatches to be correctly handled even when the
/// arity of the closure would otherwise not be known (for example, in an Any,
/// or if an argument type was specified as AnyFunc)
///
/// you must not obtain a reference to any of its fields, since it is
/// unaligned. https://github.com/rust-lang/rust/issues/27060 it is unaligned
/// because of the need to fit it in Any
///
/// note that this is not being used for now. arity mismatches are not allowed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(packed)]
pub struct AnyClosure(*const Tag, u16, u8);
impl AsI64 for AnyClosure {}
pub type AnyClosureVal = I64Val<AnyClosure>;

#[no_mangle]
pub extern "C" fn any_closure_new(ptr: *const Tag, func: u16, arity: u8) -> AnyClosureVal {
    AnyClosure(ptr, func, arity).into()
}
#[no_mangle]
pub extern "C" fn any_closure_ptr(closure: AnyClosureVal) -> *const Tag {
    closure.0
}
#[no_mangle]
pub extern "C" fn any_closure_func(closure: AnyClosureVal) -> u32 {
    closure.1 as u32
}
#[no_mangle]
pub extern "C" fn any_closure_arity(closure: AnyClosureVal) -> u32 {
    closure.2 as u32
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::env::*;
    use crate::object::object_empty;
    use crate::AnyEnum;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn closure_size_is_64() {
        assert_eq!(std::mem::size_of::<ClosureVal>(), 8);
        assert_eq!(std::mem::size_of::<Closure>(), 6);
        assert_eq!(std::mem::size_of::<AnyClosureVal>(), 8);
        assert_eq!(std::mem::size_of::<AnyClosure>(), 7);
    }
    #[test]
    fn closure_size_is_128() {
        assert_eq!(std::mem::size_of::<ClosureVal>(), 16);
        assert_eq!(std::mem::size_of::<Closure>(), 10);
        assert_eq!(std::mem::size_of::<AnyClosureVal>(), 16);
        assert_eq!(std::mem::size_of::<AnyClosure>(), 11);
    }
    #[wasm_bindgen_test]
    fn as_seen_on_notwasm() {
        let env = unsafe {
            // Expr::Closure
            let fn_obj = object_empty();
            let env = env_alloc(3, fn_obj);
            env_init_at(env, 0, AnyEnum::I32(5).into());
            env_init_at(env, 1, AnyEnum::I32(6).into());
            env_init_at(env, 2, AnyEnum::I32(7).into())
        };
        let clos = closure_new(env, 0);

        // Expr::ClosureCall
        let back_out = closure_env(clos);
        assert_eq!(env, back_out);
        // this could be finished with an EnvGet, but i found the bug already
    }
    #[wasm_bindgen_test]
    fn closure_to_object() {
        use crate::any_value::*;
        crate::init();
        let env = unsafe {
            // Expr::Closure
            let fn_obj = object_empty();
            let env = env_alloc(3, fn_obj);
            env_init_at(env, 0, AnyEnum::I32(5).into());
            env_init_at(env, 1, AnyEnum::I32(6).into());
            env_init_at(env, 2, AnyEnum::I32(7).into())
        };
        let clos = closure_new(env, 0); // dummy 0
        let mut as_obj: crate::heap_types::ObjectPtr =
            unsafe { std::mem::transmute(any_to_ptr(any_from_closure(clos))) };
        as_obj.insert(
            crate::heap(),
            crate::heap().alloc_str_or_gc("x"),
            AnyEnum::I32(11).into(),
            &mut -1,
        );
        assert_eq!(
            as_obj.get(crate::heap(), crate::heap().alloc_str_or_gc("x"), &mut -1),
            AnyEnum::I32(11)
        );

        // Expr::ClosureCall
        let back_out = closure_env(clos);
        assert_eq!(env, back_out);
        let as_obj: crate::heap_types::ObjectPtr =
            unsafe { std::mem::transmute(any_to_ptr(any_from_closure(clos))) };
        assert_eq!(
            as_obj.get(crate::heap(), crate::heap().alloc_str_or_gc("x"), &mut -1),
            AnyEnum::I32(11)
        );
        // this could be finished with an EnvGet, but i found the bug already
    }
}
