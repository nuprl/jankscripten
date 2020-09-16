//! create refs
//!
//! Refs in notwasm are similar to refs in OCaml. They are essentially boxes
//! that hold values.
//!
//! Only ref *creation* is implemented in the runtime. Dereferencing and
//! mutation are implemented using raw WebAssembly instructions.
//!
//! there's a ref for kind of *immediate value*, and one for all pointers

use super::{any_value::AnyValue, heap, heap_types::*, AnyPtr};

/// also used for bool, fn. FFI boundary lets us do this type pun
#[no_mangle]
pub extern "C" fn ref_new_non_ptr_32(val: i32) -> NonPtr32Ptr {
    heap().alloc_or_gc(val)
}
// TODO(luna): avoid allowing ourselves to allocate f64 using the shorthand
// notation when they're NOT trying to make a special f64, cause they should
// use the f64 heap
#[no_mangle]
pub extern "C" fn ref_new_f64(val: f64) -> MutF64Ptr {
    heap().alloc_or_gc(val)
}
#[no_mangle]
pub extern "C" fn ref_new_any(val: AnyValue) -> AnyJSPtr {
    // object could use a more generic name or something maybe
    heap().alloc_or_gc(val)
}
// TODO(luna): same deal, these shouldn't USUALLY be allocated
#[no_mangle]
pub extern "C" fn ref_new_ptr(val: AnyPtr) -> PtrPtr {
    // object could use a more generic name or something maybe
    heap().alloc_or_gc(val)
}

// no tests here because dereferencing and storing are not implemented in the
// runtime. See the notwasm test suite for tests on refs.
