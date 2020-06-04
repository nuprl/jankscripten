mod compile;
mod index;
mod translation;
mod walk;

use index::index;
use translation::translate;

pub mod syntax;

pub use compile::compile;

#[cfg(test)]
mod test_wasm;
