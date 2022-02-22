use crate::opts::Opts;

use super::box_assigns::box_assigns;
use super::closure_convert::closure_convert;
use super::coercions::reify_coercions;
use super::collect_assigns::collect_assigns;
use super::fv::free_vars;
use super::insert_returns::insert_returns;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};
use super::typeinf::typeinf;

pub fn compile<F>(
    opts: &Opts,
    janky_ast: &mut Stmt,
    inspect_janky: F,
) -> Result<(), TypeCheckingError>
where
    F: FnOnce(&Stmt) -> (),
{
    insert_returns(janky_ast);
    if opts.typeinf {
        typeinf(janky_ast);
        reify_coercions(janky_ast);
    }
    type_check(janky_ast)?;
    // TODO(luna): maybe the runtime should be added in jankierscript or
    // jankyscript. this would mean we could assert free_vars == \emptyset
    free_vars(janky_ast);
    let should_box_globals = collect_assigns(janky_ast);
    box_assigns(janky_ast, should_box_globals);
    closure_convert(janky_ast);
    // type-checking should succeed after every phase.
    type_check(janky_ast)?;
    // Inspect after type-checking, so that all type annotations are present.
    inspect_janky(&janky_ast);
    Ok(())
}
