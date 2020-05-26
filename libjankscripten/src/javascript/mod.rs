//! the AST, transformations, and utils for compiling javascript to jankyscript

pub mod compile_for;
pub mod parser;
pub mod syntax;
pub mod walk;

pub use compile_for::*;
pub use parser::*;
pub use syntax::*;
pub use walk::*;

pub mod constructors;
pub mod name_gen;
mod pretty_ast;
mod simpl_fancy_updates;

use name_gen::*;
