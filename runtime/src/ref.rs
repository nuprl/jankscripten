//! create refs
//! Refs in notwasm are similar to refs in OCaml. They are essentially boxes 
//! that hold values.
//! Only ref *creation* is implemented in the runtime. Dereferencing and 
//! mutation are implemented using raw WebAssembly instructions.

use super::{
    heap,
    heap_types::I32Ptr,
};

// construct a new reference, copying and boxing the given value.
#[no_mangle]
pub extern "C" fn ref_new<'a>(val: i32) -> I32Ptr<'a> {
    heap().alloc(val).unwrap()
}

// no tests here because dereferencing and storing are not implemented in the 
// runtime. See the notwasm test suite for tests on refs.