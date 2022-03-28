//! A toolkit for working with JavaScript, including parsing, pretty-printing,
//! and desugaring.

mod add_blocks;
pub mod constructors;
mod desugar;
mod desugar_bracket_str;
mod desugar_function_applications;
mod desugar_function_stmts;
mod desugar_logical;
mod desugar_loops;
mod desugar_switch;
mod desugar_this;
mod desugar_updates;
mod desugar_vardecls;
mod lift_vars;
mod normalize_std_lib_calls;
mod parser;
mod resugar_method_call;
pub mod syntax;
pub mod walk;

pub use crate::shared::NameGen;
pub use desugar::*;
pub use parser::*;
pub use syntax::*;
pub use walk::*;

mod pretty_ast;

#[cfg(test)]
mod testing;
