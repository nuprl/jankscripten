//! the runtime for programs compiled to wasm by jankscripten. it
//! externs public non-mangled functions intended to be compiled to wasm
//! and dynamically linked
//!
//! - tag all public functions with `extern "C"` *and* `#[no_mangle]`
//! - names become flattened, so name using mod_ (eg num::num_add instead
//!   of num::add)
//! - all externed return values must be <= 64 bits

type Key = StrPtr;

pub mod any_value;
pub mod array;
pub mod ht;
pub mod object;
pub mod string;
pub mod r#ref;
mod util;

mod allocator;
use allocator::*;
use any_value::AnyEnum;
use any_value::AnyValue;
use string::StrPtr;
use crate::allocator::Tag;
static mut HEAP: Option<Heap> = None;

#[no_mangle]
pub static JNKS_STRINGS: [u8; 65536] = [0; 65536];

/// needs to be called before most other code. it initializes the managed heap
#[no_mangle]
pub extern "C" fn init() {
    unsafe {
        HEAP = Some(Heap::new(65535));
    }
}

#[no_mangle]
pub extern "C" fn gc_enter_fn(slots: usize) {
    heap().push_shadow_frame(slots);
}

#[no_mangle]
pub unsafe extern "C" fn gc_exit_fn() {
    heap().pop_shadow_frame();
}

#[no_mangle]
pub fn set_in_current_shadow_frame_slot(ptr: *mut Tag, slot: usize) {
    heap().set_in_current_shadow_frame_slot(slot, ptr);
}

fn heap() -> &'static Heap {
    unsafe { &HEAP }.as_ref().unwrap()
}

