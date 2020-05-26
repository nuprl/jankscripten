use super::syntax::*;
use super::walk::*;
use resast::BinaryOp;

struct MyVisitor { }

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => Expr::Id(x.clone()),
        LValue::Dot(e, x) => Expr::Dot(Box::new(e.clone()), x.clone()),
        LValue::Bracket(e1, e2) => Expr::Bracket(Box::new(e1.clone()), Box::new(e2.clone()))
    }
}

impl Visitor for MyVisitor {

    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => { }
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                // `lv += rhs` should turn into `lv = lv + rhs`
                // Unfortunately, lv is not an expression ...
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Plus),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            _ => { 
                // Nothing to do
            }
        }

    }

}


pub fn simpl(program: &mut Stmt) {
    let mut v = MyVisitor { };
    program.walk(&mut v);
}