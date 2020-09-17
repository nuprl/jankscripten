use super::box_assigns::box_assigns;
use super::collect_assigns::collect_assigns;
use super::fv::free_vars;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};

pub fn compile(janky_ast: &mut Stmt) -> Result<(), TypeCheckingError> {
    free_vars(janky_ast);
    type_check(janky_ast)?;
    let should_box_globals = collect_assigns(janky_ast);
    box_assigns(janky_ast, should_box_globals);
    Ok(())
}
