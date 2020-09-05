use super::syntax::*;
use super::walk::*;

pub fn box_assigns(program: &mut Stmt) {
    //
}

struct BoxVisitor;

impl Visitor for BoxVisitor {
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Assign(lv, to) => {
                match &**lv {
                    LValue::Id(id) => *expr = Expr::Store(id.clone(), Box::new(to.take())),
                    // boxed assign already!
                    _ => (),
                }
            }
            _ => (),
        }
    }
}
