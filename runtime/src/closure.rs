use super::i64_val::*;
use super::Tag;

/// this is a closure, it represents a cons of an environment (a fixed array)
/// and a function pointer
///
/// these two pointers can be combined into one i64 to be used as an immediate
/// value. this closure has had its arity erased, for example if it is not
/// stuck within an any
///
/// if there is no closure environment paired with the function (it is
/// already 1st order), the pointer can be null
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Closure(*const Tag, u16);
impl AsI64 for Closure {}
pub type ClosureVal = I64Val<Closure>;

#[no_mangle]
pub extern "C" fn closure_new(ptr: *const Tag, func: u16) -> ClosureVal {
    Closure(ptr, func).into()
}
#[no_mangle]
pub extern "C" fn closure_ptr(closure: ClosureVal) -> *const Tag {
    closure.0
}
#[no_mangle]
pub extern "C" fn closure_func(closure: ClosureVal) -> u32 {
    closure.1 as u32
}

/// this is a closure with any arity, which holds its arity along with
/// it. this is to allow arity mismatches to be correctly handled even when the
/// arity of the closure would otherwise not be known (for example, in an Any,
/// or if an argument type was specified as AnyFunc)
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
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn closure_size_is_64() {
        assert_eq!(std::mem::size_of::<ClosureVal>(), 8);
        assert_eq!(std::mem::size_of::<Closure>(), 8);
        assert_eq!(std::mem::size_of::<AnyClosureVal>(), 8);
        assert_eq!(std::mem::size_of::<AnyClosure>(), 7);
    }
    #[test]
    fn closure_size_is_128() {
        assert_eq!(std::mem::size_of::<ClosureVal>(), 16);
        assert_eq!(std::mem::size_of::<Closure>(), 16);
        assert_eq!(std::mem::size_of::<AnyClosureVal>(), 16);
        assert_eq!(std::mem::size_of::<AnyClosure>(), 11);
    }
}