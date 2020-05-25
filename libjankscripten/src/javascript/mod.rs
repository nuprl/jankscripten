//! the AST, transformations, and utils for compiling javascript to jankyscript

pub mod compile_for;
pub mod parser;
pub mod syntax;
pub mod walk;

pub use compile_for::*;
pub use parser::*;
pub use syntax::*;
pub use walk::*;

mod constructors;
mod name_gen;
mod pretty_ast;

use name_gen::*;
