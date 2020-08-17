//! A toolkit for working with JavaScript, including parsing, pretty-printing,
//! and desugaring.

pub mod constructors;
mod desugar;
mod desugar_function_applications;
mod desugar_function_stmts;
mod desugar_logical;
mod desugar_loops;
mod desugar_switch;
mod desugar_this;
mod desugar_updates;
mod desugar_vardecls;
mod lift_vars;
pub mod name_gen;
mod parser;
pub mod syntax;
pub mod walk;

pub use desugar::*;
pub use name_gen::*;
pub use parser::*;
pub use syntax::*;
pub use walk::*;

mod pretty_ast;

#[cfg(test)]
mod testing;
