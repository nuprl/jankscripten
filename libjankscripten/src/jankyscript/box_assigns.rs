//! box variables in certain circumstances
//!
//! we box a variable completely if it is captured and assigned to in
//! a closure, because if it isn't boxed, assignments into the
//! closure-converted environment will not affect the captured variable

use super::constructors::*;
use super::syntax::*;
use super::walk::*;
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
}
impl Visitor for BoxVisitor {
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
            // TODO(luna): CriteriaVisitor
            // as a first pass, it actually is fine to box all variables
            // and then test that
            Stmt::Var(id, ty, expr) => {
                *expr = Box::new(new_ref_(expr.take()));
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
        }
    }
}
