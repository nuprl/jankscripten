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

use super::syntax::*;
use super::walk::*;
use crate::pos::Pos;
use im_rc::HashMap;

pub fn closure_convert(program: &mut Stmt) {
    let mut v = ClosureConversion::new();
    program.walk(&mut v);
}

struct ClosureConversion {
    free_vars_stack: Vec<HashMap<Id, u32>>,
}

impl Visitor for ClosureConversion {
    fn enter_fn(&mut self, func: &mut Func, _: &Loc) {
        self.free_vars_stack.push(
            func.free_vars
                .iter()
                .enumerate()
                .map(|(i, (k, _))| (k.clone(), i as u32))
                .collect(),
        );
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Id(id, ty, s) => {
                if let Some(e) = self.compile_id(id, ty.clone(), s.clone()) {
                    *expr = e;
                }
            }
            Expr::Func(func, s) => {
                // this has to happen first so that free_vars will reflect
                // the scope of the containing function not the function itself
                self.free_vars_stack.pop();
                // remember which variables should be passed on from env vs
                // from stack. im_rc hashmap iter is guaranteed to be
                // consistent for the same data
                let s_copy = s.clone();
                let env = func
                    .free_vars
                    .iter()
                    .map(
                        |(id, ty)| match self.compile_id(id, ty.clone(), s.clone()) {
                            Some(e) => (e, ty.clone()),
                            None => (Expr::Id(id.clone(), ty.clone(), s.clone()), ty.clone()),
                        },
                    )
                    .collect();
                // you might think that here is where we want to insert the environment
                // as a parameter. but if we did that we would have to rewrite all our
                // coercions as well! so instead, we just translate *any* FnType in
                // the whole program with an environmented type in
                // from_jankyscript. when we no longer closure-convert all functions,
                // this may need to change
                *expr = Expr::Closure(func.take(), env, s_copy);
            }
            // assigns should have been boxed so they will never be assigned
            // to free
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
    fn free_vars(&self) -> &HashMap<Id, u32> {
        self.free_vars_stack.last().unwrap()
    }
    /// if id is free, then turn it into an EnvGet, otherwise return None
    fn compile_id(&self, id: &Id, ty: Type, s: Pos) -> Option<Expr> {
        self.free_vars()
            .get(id)
            .and_then(|i| Some(Expr::EnvGet(*i, ty.clone(), s)))
    }
}
