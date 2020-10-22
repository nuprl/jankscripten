//! the runtime for programs compiled to wasm by jankscripten. it
//! externs public non-mangled functions intended to be compiled to wasm
//! and dynamically linked
//!
//! - tag all public functions with `extern "C"` *and* `#[no_mangle]`
//! - names become flattened, so name using mod_ (eg num::num_add instead
//!   of num::add)
//! - all externed return values must be <= 64 bits

#[allow(unused)]
macro_rules! log {
    ($($t:tt)*) => (
        crate::util::log(&format!($($t)*))
    )
}

use crate::allocator::Tag;
use crate::allocator::*;
use crate::any_value::AnyEnum;
use crate::any_value::AnyValue;
use crate::closure::ClosureVal;

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

#[no_mangle]
pub fn set_any_in_current_shadow_frame_slot(any: AnyValue, slot: usize) {
    heap().set_any_in_current_shadow_frame_slot(slot, any);
}

#[no_mangle]
pub fn set_closure_in_current_shadow_frame_slot(closure: ClosureVal, slot: usize) {
    let env = closure.0;
    heap().set_in_current_shadow_frame_slot(slot, env.get_ptr());
}

/// returns Any::I32(42) because jankyscript requires return values
#[no_mangle]
pub fn log_any(_this: AnyValue, any: AnyValue) -> AnyValue {
    let any: AnyEnum = *any;
    log!("{:?}", any);
    AnyEnum::I32(42).into()
}

pub fn heap() -> &'static Heap {
    unsafe { &HEAP }.as_ref().unwrap()
}