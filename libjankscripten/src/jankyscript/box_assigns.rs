//! box variables in certain circumstances
//!
//! we box a variable completely if it is captured and assigned to:
//!
//! - in this or another closure; and it's read in another closure or after
//!   this closure's creation
//! - after this closure is created in program flow
//!
//! if it isn't boxed, assignments after the environment creation (or in
//! another closure that could be called at any time) would not update the
//! closure environment; also, assignments into the closure environment would
//! not update the surrounding environment
//!
//! now this is a pretty complex set of rules, so as a first pass i'm
//! implementing a more lenient superset that is still correct but not
//! as performant. i think when we implement escape analysis implementing
//! this part might be easier. the superset says we box if it is captured
//! in this closure and assigned to at any time

use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use im_rc::HashSet as ImmHashSet;
use std::collections::HashSet;

/// box relevant variables on the provided program (see module-level)
///
/// we follow a double-recursion process: for each variable declared, we
/// check the scope to determine if it meets the criteria (CriteriaVisitor)
/// and if so, add it to the list of ids to convert
pub fn box_assigns(program: &mut Stmt) {
    let mut v = BoxVisitor::new();
    program.walk(&mut v);
}

/// we visit everything that refers to an id, and replace it with the boxed
/// equivalent
/// - Var -> Var(NewRef)
/// - Id -> Deref(Id)
/// - Assign -> Store
struct BoxVisitor {
    ids: HashSet<Id>,
    // TODO(luna): check if ever assigned to
    fv_stack: Vec<ImmHashSet<Id>>,
}
impl Visitor for BoxVisitor {
    fn enter_fn(&mut self, func: &mut Func, _: &Loc) {
        self.fv_stack.push(func.free_vars.clone());
    }
    fn exit_fn(&mut self, _: &mut Func, _: &Loc) {
        self.fv_stack.pop();
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Id(id) if self.ids.contains(id) => *expr = deref_(expr.take()),
            Expr::Assign(lv, to) => {
                match &**lv {
                    LValue::Id(id) if self.ids.contains(id) => {
                        *expr = store_(id.clone(), to.take())
                    }
                    // []/. => boxed already!
                    _ => (),
                }
            }
            _ => (),
        }
    }
    fn exit_stmt(&mut self, stmt: &mut Stmt, _: &Loc) {
        match stmt {
            Stmt::Var(id, ty, expr) if self.fv_stack.last().unwrap().contains(id) => {
                *expr = Box::new(new_ref_(expr.take(), ty.clone()));
                *ty = Type::Ref(Box::new(ty.clone()));
                self.ids.insert(id.clone());
            }
            _ => (),
        }
    }
}
impl BoxVisitor {
    fn new() -> Self {
        Self {
            ids: HashSet::new(),
            // you can't have free variables at the top level
            fv_stack: vec![ImmHashSet::new()],
        }
    }
}
