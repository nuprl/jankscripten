//! The implementation of the jankscripten runtime system relies on being
//! compiled to WebAssembly. For example, it defines several 'extern' functions
//! that work with `u64` data, when compiled to Wasm, but `u128` data when
//! compiled ot 64-bit architectures. Rust does not have stable support for
//! extern functions that work with `u128`. Therefore, most modules below
//! are setup to only compile with the `wasm32` target.

#![allow(improper_ctypes_definitions)]

#[cfg(all(target_arch = "wasm32"))]
type Key = crate::heap_types::StringPtr;

#[allow(unused)]
macro_rules! log {
    ($($t:tt)*) => (
        crate::util::log(&format!($($t)*))
    )
}

#[allow(unused)]
macro_rules! error {
    ($($t:tt)*) => (
        crate::util::error(&format!($($t)*))
    )
}

#[allow(unused)]
macro_rules! log_panic {
    ($($t:tt)*) => (
        {
            log!($($t)*);
            std::panic!("log panic")
        }
    )
}

#[cfg(all(target_arch = "wasm32"))]
mod allocator;
#[cfg(all(target_arch = "wasm32"))]
pub mod any_value;
#[cfg(all(target_arch = "wasm32"))]
pub mod array;
#[cfg(all(target_arch = "wasm32"))]
pub mod closure;
#[cfg(all(target_arch = "wasm32"))]
mod coercions;
#[cfg(all(target_arch = "wasm32"))]
pub mod env;
#[cfg(all(target_arch = "wasm32"))]
pub mod ht;
#[cfg(all(target_arch = "wasm32"))]
pub mod math;
#[cfg(all(target_arch = "wasm32"))]
pub mod object;
#[cfg(all(target_arch = "wasm32"))]
pub mod ops;
#[cfg(all(target_arch = "wasm32"))]
pub mod r#ref; // Rust raw identifier syntax
#[cfg(all(target_arch = "wasm32"))]
pub mod static_strings;
#[cfg(all(target_arch = "wasm32"))]
pub mod std_lib;
#[cfg(all(target_arch = "wasm32"))]
pub mod string;

#[cfg(all(target_arch = "wasm32"))]
mod i64_val;
#[cfg(all(target_arch = "wasm32"))]
mod util;
#[cfg(all(target_arch = "wasm32"))]
mod wasm32;

#[cfg(all(target_arch = "wasm32"))]
use crate::allocator::Tag;
#[cfg(all(target_arch = "wasm32"))]
use allocator::*;
#[cfg(all(target_arch = "wasm32"))]
use any_value::AnyEnum;
#[cfg(all(target_arch = "wasm32"))]
use any_value::AnyValue;

#[cfg(all(target_arch = "wasm32"))]
use wasm32::*;
