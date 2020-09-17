//! box variables determined by [super::collect_assigns]

use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use im_rc::HashSet as ImmHashSet;

/// box relevant variables on the provided program
///
/// should_box_globals should be provided by the output of
/// [super::collect_assigns::collect_assigns()]
pub fn box_assigns(program: &mut Stmt, should_box_globals: ImmHashSet<Id>) {
    let mut v = BoxVisitor::new(should_box_globals);
    program.walk(&mut v);
}

/// we visit everything that refers to an id, and replace it with the boxed
/// equivalent if its in func.assigned_free_children
/// - Var -> Var(NewRef)
/// - Id -> Deref(Id)
/// - Assign -> Store
struct BoxVisitor {
    to_box_stack: Vec<ImmHashSet<Id>>,
}
impl Visitor for BoxVisitor {
    fn enter_fn(&mut self, func: &mut Func, _: &Loc) {
        // we combine variables to box from up the stack down, so that we
        // can pop off new ones, but we can lookup ALL the appropriate
        // variables
        let old = self.to_box_stack.last().unwrap().clone();
        let new = func.assigned_free_children.clone();
        self.to_box_stack.push(old.union(new));
    }
    fn exit_fn(&mut self, _: &mut Func, _: &Loc) {
        self.to_box_stack.pop();
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Id(id) if self.should_box(id) => *expr = deref_(expr.take()),
            Expr::Assign(lv, to) => {
                match &**lv {
                    LValue::Id(id) if self.should_box(id) => *expr = store_(id.clone(), to.take()),
                    // []/. => boxed already!
                    _ => (),
                }
            }
            _ => (),
        }
    }
    fn exit_stmt(&mut self, stmt: &mut Stmt, _: &Loc) {
        match stmt {
            Stmt::Var(id, ty, expr) if self.should_box(id) => {
                *expr = Box::new(new_ref_(expr.take(), ty.clone()));
                *ty = Type::Ref(Box::new(ty.clone()));
            }
            _ => (),
        }
    }
}
impl BoxVisitor {
    fn new(to_box_global: ImmHashSet<Id>) -> Self {
        Self {
            to_box_stack: vec![to_box_global],
        }
    }
    fn should_box(&self, id: &Id) -> bool {
        self.to_box_stack.last().unwrap().contains(id)
    }
}
