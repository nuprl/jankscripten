//! NotWasm: It is not WebAssembly, but quite close to it.
mod compile;
#[allow(dead_code)]
mod constructors;
mod intern;
pub mod parser;
mod rt_bindings;
mod translation;
mod walk;

mod elim_gotos;
mod from_jankyscript;
mod label_apps;

lrlex::lrlex_mod!("notwasm/lexer.l"); // produces lexer_l.rs
lrpar::lrpar_mod!("notwasm/parser.y"); // produces parser_y.rs

use intern::intern;
use translation::translate;

pub mod pretty;
pub mod syntax;
pub mod type_checking;

pub use compile::compile;
pub use parser::parse;

#[cfg(test)]
pub mod test_wasm;

pub use from_jankyscript::*;
