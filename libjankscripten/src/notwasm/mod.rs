//! NotWasm: It is not WebAssembly, but quite close to it.
mod compile;
mod constructors;
mod index;
mod intern;
mod rt_bindings;
mod translation;
mod walk;
mod parser;

use index::index;
use translation::translate;

pub mod syntax;

pub use parser::parse;
pub use compile::compile;

#[cfg(test)]
mod test_wasm;
