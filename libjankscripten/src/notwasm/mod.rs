//! NotWasm: It is not WebAssembly, but quite close to it.
mod compile;
#[allow(dead_code)]
mod constructors;
mod intern;
mod notwasm_rt;
mod parser;
mod rt_bindings;
mod translation;
mod walk;

mod elim_gotos;
mod from_jankyscript;
mod label_apps;

use intern::intern;
use translation::translate;

pub mod pretty;
pub mod syntax;
pub mod type_checking;

pub use compile::compile;
pub use notwasm_rt::get_notwasm_rt;
pub use parser::parse;

#[cfg(test)]
pub mod test_wasm;

pub use from_jankyscript::*;
