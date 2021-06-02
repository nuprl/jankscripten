mod box_assigns;
mod closure_convert;
mod collect_assigns;
mod compile;
pub mod constructors;
pub mod from_js;
mod fv;
mod pretty;
pub mod syntax;
mod type_checking;
mod typeinf;
mod typeinf_z3;
mod uninit_vars;
mod walk;

pub use compile::compile;