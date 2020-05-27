//! the AST, transformations, and utils for compiling javascript to jankyscript

pub mod compile_for;
pub mod constructors;
pub mod desugar_logical;
pub mod name_gen;
pub mod parser;
pub mod syntax;
pub mod walk;

pub use compile_for::*;
pub use desugar_logical::*;
pub use name_gen::*;
pub use parser::*;
pub use syntax::*;
pub use walk::*;

mod pretty_ast;

#[cfg(test)]
mod testing;
