use super::box_assigns::box_assigns;
use super::closure_convert::closure_convert;
use super::collect_assigns::collect_assigns;
use super::fv::free_vars;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};

pub fn compile<F>(janky_ast: &mut Stmt, inspect_janky: F) -> Result<(), TypeCheckingError>
where
    F: FnOnce(&Stmt) -> (),
{
    type_check(janky_ast)?;
    // TODO(luna): maybe the runtime should be added in jankierscript or
    // jankyscript. this would mean we could assert free_vars == \emptyset
    free_vars(janky_ast);
    let should_box_globals = collect_assigns(janky_ast);
    box_assigns(janky_ast, should_box_globals);
    closure_convert(janky_ast);
    inspect_janky(&janky_ast);
    Ok(())
}