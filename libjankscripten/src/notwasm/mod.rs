//! NotWasm: It is not WebAssembly, but quite close to it.
mod compile;
#[allow(unused)]
mod constructors;
mod index;
mod index_labels;
mod intern;
mod parser;
mod rt_bindings;
mod translation;
mod walk;

use index::index;
use index_labels::index_labels;
use intern::intern;
use translation::translate;

pub mod syntax;

pub use compile::compile;
pub use parser::parse;

#[cfg(test)]
mod test_wasm;
