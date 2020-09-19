//! change higher order functions into closures that capture both environment
//! and code
//!
//! the relevant prerequisites are:
//! - free variables that are assigned anywhere should be boxed so that the
//!   environments will capture by name
//!
//! the process is:
//!
//! - accepting an environment in a parameter in each function
//! - referencing free variables through the passed environment
//! - modifying each call site to load the function pointer and call it with
//!   the environment as the first parameter
//!
//! after this step, functions can be lifted out of their environments
//! into the flat list of functions wasm uses. this is already done in
//! [crate::notwasm::from_jankyscript]
//!
//! however, there are some optimizations that we plan to do. they are not
//! yet implemented
//!
//! - dont closure convert functions that never escape (need escape analysis)
//! - don't consider some variables free (top-level should become globals
//!   or something)
//! - don't closure convert functions that have no free variables... kind of
//!   - note that this can't be done as stated because you can't know from
//!     the call site which type of function you have unless you:
//!     - differentiate closures and functions in the static and runtime
//!       type system (any)
//!     - actually just don't do it for certain types of functions that
//!       never have free variables, like top-level functions in a function
//!       with no fields used as a "module" (most prototypes, most top-level
//!       objects)

use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use im_rc::HashMap;

pub fn closure_convert(program: &mut Stmt) {
    let mut v = ClosureConversion::new();
    program.walk(&mut v);
}

struct ClosureConversion {
    // this could probly be in Loc but it's not used anywhere else (yet)
    // and no, we can't avoid cloning into a stack without not using a
    // visitor. since we always have a &mut, we can't have some list of &s
    // sitting around somewhere
    free_vars_stack: Vec<HashMap<Id, (u32, Type)>>,
}

impl Visitor for ClosureConversion {
    fn enter_fn(&mut self, func: &mut Func, _: &Loc) {
        self.free_vars_stack.push(
            func.free_vars
                .iter()
                .enumerate()
                .map(|(i, (k, v))| (k.clone(), (i as u32, v.clone())))
                .collect(),
        );
    }
    fn exit_fn(&mut self, func: &mut Func, _: &Loc) {
        self.free_vars_stack.pop();
        // TODO(luna): Type::Env
        func.args_with_typs
            .insert(0, (Id::Generated("env", 0), Type::Int));
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Id(id, _) if self.free(id) => {
                *expr = Expr::EnvGet(id.clone());
            }
            Expr::Assign(lv, to) => match &**lv {
                LValue::Id(id, _) if self.free(id) => *expr = env_set_(id.clone(), to.take()),
                // container covered by Expr::Id
                _ => (),
            },
            _ => (),
        }
    }
}
impl ClosureConversion {
    fn new() -> Self {
        Self {
            // simply don't "closure convert" *any* variables at the global
            free_vars_stack: vec![HashMap::new()],
        }
    }
    fn free(&self, id: &Id) -> bool {
        self.free_vars_stack.last().unwrap().contains_key(id)
    }
}
