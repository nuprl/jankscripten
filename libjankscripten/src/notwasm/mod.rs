//! NotWasm: It is not WebAssembly, but quite close to it.
mod compile;
#[allow(dead_code)]
mod constructors;
mod intern;
mod parser;
mod rt_bindings;
mod translation;
mod walk;

mod elim_gotos;
mod from_jankyscript;
mod label_apps;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("notwasm/lexer.l");
lrpar_mod!("notwasm/parser.y");

pub mod parser2;

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
