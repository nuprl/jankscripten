use super::box_assigns::box_assigns;
use super::fv::free_vars;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};

pub fn compile(janky_ast: &mut Stmt) -> Result<(), TypeCheckingError> {
    let (_fvs, assigns) = free_vars(janky_ast);
    type_check(janky_ast)?;
    box_assigns(janky_ast, assigns);
    Ok(())
}
