//! the runtime for programs compiled to wasm by jankscripten. it itself
//! externs public non-mangled functions intended to by compiled to wasm
//! and dynamically linked
//!
//! currently only implements Num and add_num
//!
//! things i've learned messing around:
//!
//! - tag all public functions with `extern "C"` *and* `#[no_mangle]`. without
//!   it no code will be generated at all fsr
//! - you can't really use impl productively, just stick with flat
//!   procedures. you can use mods, but they become flattened, so name
//!   using mod_ (eg num::num_add instead of num::add)
//! - because of no multiple returns, it's much easier to think about the
//!   generated code if types are <=64 bits
//! - when not, always expect a reference and return a box if
//!   allocating. otherwise wasm will ask you to pack from memory
//! - build on release if you wanna read the generated code, it's 1000% better
//! - wasm_bindgen / wasm-pack is unneccessary and gets in the way since
//!   it's all about js bindings and disallows enum structs

pub mod ht;
pub mod num;