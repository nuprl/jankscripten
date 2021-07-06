use super::box_assigns::box_assigns;
use super::closure_convert::closure_convert;
use super::collect_assigns::collect_assigns;
use super::fv::free_vars;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};
use super::super::opts;
use im_rc::HashMap;
use std::rc::Rc;

pub fn compile<F>(
    opts: &mut opts::Opts,
    janky_ast: &mut Stmt, 
    inspect_janky: F
) -> Result<(), TypeCheckingError>
where
    F: FnOnce(&Stmt) -> (),
{
    let opts = opts.borrow_stdlib();
    let mut env: HashMap<String, Type> = Default::default();
    for (x, t) in opts.rts_fn_imports.iter() {
        env.insert(x.clone(), t.janky());
    }
    let env = Rc::new(env);
    type_check(env.clone(), janky_ast)?;
    // TODO(luna): maybe the runtime should be added in jankierscript or
    // jankyscript. this would mean we could assert free_vars == \emptyset
    free_vars(janky_ast);
    let should_box_globals = collect_assigns(janky_ast);
    box_assigns(janky_ast, should_box_globals);
    closure_convert(janky_ast);
    // type-checking should succeed after every phase.
    type_check(env, janky_ast)?;
    // Inspect after type-checking, so that all type annotations are present.
    inspect_janky(&janky_ast);
    Ok(())
}
