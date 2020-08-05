use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use super::*;
use resast::BinaryOp;

//note: will depend on function naming from desugar_function_applications in order to handle f().x += 1; cases
// x += 1;    =>    x = x + 1;

struct DesugarFancyUpdates();

impl Visitor for DesugarFancyUpdates {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => {}
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Plus, lv, rhs);
            }
            Expr::Assign(AssignOp::MinusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Minus, lv, rhs);
            }
            Expr::Assign(AssignOp::TimesEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Times, lv, rhs);
            }
            Expr::Assign(AssignOp::DivEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Over, lv, rhs);
            }
            Expr::Assign(AssignOp::ModEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Mod, lv, rhs);
            }
            Expr::Assign(AssignOp::LeftShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::LeftShift, lv, rhs);
            }
            Expr::Assign(AssignOp::RightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::RightShift, lv, rhs);
            }
            Expr::Assign(AssignOp::UnsignedRightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::UnsignedRightShift, lv, rhs);
            }
            Expr::Assign(AssignOp::OrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Or, lv, rhs);
            }
            Expr::Assign(AssignOp::XOrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::XOr, lv, rhs);
            }
            Expr::Assign(AssignOp::AndEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::And, lv, rhs);
            }
            Expr::Assign(AssignOp::PowerOfEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::PowerOf, lv, rhs);
            }
            Expr::UnaryAssign(_op, _e) => {
                // There are four essential cases
                //
                // ++x => x = x + 1, x
                //
                // ++e.x => tmp = e, tmp.x = tmp.x + 1, tmp.x      introduce tmp
                //
                // x++ => tmp = x, x = x + 1, tmp                  introduce tmp
                //
                // e.x++ => tmp1 = e, tmp2 = tmp1.x,              introduce tmp1
                //           tmp1.x = tmp1.x + 1, tmp2                  and tmp2
                //
                // The decrement operators are similar to the increment
                // operators. Computed field lookups (e1[e2]) may require a new
                // temporary variable too.
                todo!("unary assignment operators")
            }
            _ => {
                //not an assignment, proceed as usual
            }
        }
    }
}

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => id_(x.clone()),
        LValue::Dot(e, x) => {
            assert!(e.is_essentially_atom(), "potentially duplicating effects");
            dot_(e.clone(), x.clone())
        }
        LValue::Bracket(e1, e2) => {
            assert!(e1.is_essentially_atom(), "potentially duplicating effects");
            assert!(e2.is_essentially_atom(), "potentially duplicating effects");
            bracket_(e1.clone(), e2.clone())
        }
    }
}

fn desugar_assign_op(bin_op: BinaryOp, lv: &mut LValue, rhs: &mut Expr) -> Expr {
    // TODO(arjun): This is broken. lv cloning is definitely an issue
    assign_(
        lv.clone(),
        binary_(BinOp::BinaryOp(bin_op), lval_to_expr(lv), rhs.take()),
    )
}

pub fn desugar_updates(program: &mut Stmt, _ng: &mut NameGen) {
    let mut v = DesugarFancyUpdates();
    program.walk(&mut v);
}
