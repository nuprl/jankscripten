//! create refs
//!
//! Refs in notwasm are similar to refs in OCaml. They are essentially boxes
//! that hold values.
//!
//! Only ref *creation* is implemented in the runtime. Dereferencing and
//! mutation are implemented using raw WebAssembly instructions.
//!
//! the runtime system needs to know the following about any type that can
//! be heap allocated:
//!
//! - how big is this data?
//! - what pointers to heap managed types are inside this data?
//!
//! each environment will know the first using the array using what we use
//! in StringPtr. for the second, we need a way to traverse the entries
//! of an environment, which means for each entry we need to know the same two
//! points. i have a few different ideas about how to store this info:
//!
//! 1. group the entries by their kind:
//!
//!     - non-pointers, with only a total size
//!     - pointers, 32 bits
//!     - anys, 64 bits, which each need to be checked for pointers
//!     - closures, 64 bits, with component env pointers
//!
//!     each would have their total size, and non-pointers could be skipped
//!     when marking
//!
//! 2. add a tag to each entry of the closure, which states either:
//!
//!     - 32-bit non-pointer
//!     - 64-bit non-pointer (difference needed to traverse)
//!     - pointer, 32 bits
//!     - any, 64 bits
//!     - closure, 64 bits
//!
//!     this is more space efficient for small closures, which i think is more
//!     common except for the janky "global variables" case; however it MAY be
//!     less efficient to traverse. it's also easier to add new types to
//!     conceptually
//!
//! 3. box ALL anys and closures, not just those that are assigned to,
//!     then follow Plan 1 OR 2; but anys and closures would now be pointers
//!     and could be ignored
//!
//!     this has the benefit of reduced complexity, but adds an additional
//!     deref to all deref / stores
//!
//! can anys store Ref? i propose no. at first glance, a Ref is not a value,
//! so no. but consider:
//!
//! ```javascript
//! function f(whatType) {
//!     function g(thisIsAny) {
//!         whatType = thisIsAny;
//!     }
//! }
//! ```
//!
//! `whatType` has type Any because all parameters to functions do. but it
//! must be a ref. so therefore it has to be Any::Ref. however, if strong
//! update is supported, it would be impossible to update the Any. so i
//! propose that whatType must be type Ref(Any); and this has to be supported,
//! meaning we need to support coercions to Ref(Any) and allow a parameters
//! type to be Ref(Any)

use super::{any_value::AnyValue, heap, heap_types::*, AnyPtr};

/// also used for bool, fn. FFI boundary lets us do this type pun
#[no_mangle]
pub extern "C" fn ref_new_non_ptr_32(val: i32) -> NonPtr32Ptr {
    heap().alloc_or_gc(val)
}
// /// this is all kinds of messiness
// ///#[no_mangle]
// ///pub extern "C" fn ref_new_f64(val: f64) -> MutF64 {
// ///    heap().alloc_mut_f64_or_gc(val)
// ///}
// /// this is all kinds of messiness
// ///
// /// this isn't really a *const Tag i'm lying to the compiler it's a *const T
//#[no_mangle]
//pub extern "C" fn ref_new_ptr(val: AnyPtr) -> PtrPtr {
//    // object could use a more generic name or something maybe
//    heap().alloc_or_gc(val)
//}

// no tests here because dereferencing and storing are not implemented in the
// runtime. See the notwasm test suite for tests on refs.
