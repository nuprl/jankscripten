//! box variables determined by [super::collect_assigns]

use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use crate::shared::NameGen;
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
    ng: NameGen,
}
impl Visitor for BoxVisitor {
    fn enter_fn(&mut self, func: &mut Func, _: &Loc) {
        // we combine variables to box from up the stack down, so that we
        // can pop off new ones, but we can lookup ALL the appropriate
        // variables
        let old = self.to_box_stack.last().unwrap().clone();
        let new = func.assigned_free_children.clone();
        let to_box = old.union(new);
        self.to_box_stack.push(to_box.clone());
        // we'll be changing the types of some of our free variables to ref,
        // and we need those types so let's record that change
        for (k, v) in func.free_vars.iter_mut() {
            if self.should_box(k) {
                *v = Type::Ref(Box::new(v.clone()));
            }
        }
        // now we want to box up parameters. we can't expect the parameter
        // to be boxed because how would we know? parameters are always any. so
        // what we do is change the parameter to a fresh name, and assign the
        // original name to it. we don't box it because it'll be handled by the
        // rest of the visitor
        for (name, ty) in &mut func.args_with_typs {
            if self.should_box(name) {
                let real_name = name.clone();
                *name = self.ng.fresh("to_box");
                func.body = Box::new(block_(vec![
                    var_(real_name, ty.clone(), Expr::Id(name.clone(), ty.clone())),
                    (*func.body).take(),
                ]));
            }
        }
    }
    fn exit_fn(&mut self, _: &mut Func, _: &Loc) {
        self.to_box_stack.pop();
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Id(id, ty) if self.should_box(id) => {
                let old_ty = ty.clone();
                let new_ty = ref_ty_(old_ty.clone());
                *ty = new_ty.clone();
                *expr = deref_(expr.take(), old_ty)
            }
            Expr::Assign(lv, to) => {
                match &mut **lv {
                    LValue::Id(id, ty) if self.should_box(id) => {
                        *expr = store_(id.clone(), to.take(), ty.clone())
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
            Stmt::Var(id, ty, expr) if self.should_box(id) => {
                *stmt = var_to_new_ref(id.clone(), ty, expr.take());
            }
            _ => (),
        }
    }
}
impl BoxVisitor {
    fn new(to_box_global: ImmHashSet<Id>) -> Self {
        Self {
            to_box_stack: vec![to_box_global],
            ng: NameGen::default(),
        }
    }
    fn should_box(&self, id: &Id) -> bool {
        self.to_box_stack.last().unwrap().contains(id)
    }
}

/// provide: the name to assign to; the type of the expr unboxed, and the
/// expr the var is assigned to
fn var_to_new_ref(id: Id, ty: &Type, expr: Expr) -> Stmt {
    var_(
        id,
        Type::Ref(Box::new(ty.clone())),
        new_ref_(expr, ty.clone()),
    )
}
