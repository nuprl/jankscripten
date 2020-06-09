//! the AST, transformations, and utils for compiling javascript to jankyscript

pub mod constructors;
pub mod desugar;
pub mod desugar_logical;
pub mod desugar_loops;
pub mod desugar_functions;
pub mod name_gen;
pub mod parser;
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
