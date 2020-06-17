use super::syntax::*;
use super::walk::*;
use super::*;
use resast::BinaryOp;
use super::constructors::*;

//note: will depend on function naming from desugar_function_applications in order to handle f().x += 1; cases
// x += 1;    =>    x = x + 1;

struct DesugarFancyUpdates();

impl Visitor for DesugarFancyUpdates {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => { }
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Plus, lv, rhs);
            },
            Expr::Assign(AssignOp::MinusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Minus, lv, rhs);
            },
            Expr::Assign(AssignOp::TimesEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Times, lv, rhs);
            },
            Expr::Assign(AssignOp::DivEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Over, lv, rhs);
            },
            Expr::Assign(AssignOp::ModEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Mod, lv, rhs);
            },
            Expr::Assign(AssignOp::LeftShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::LeftShift, lv, rhs);
            },
            Expr::Assign(AssignOp::RightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::RightShift, lv, rhs);
            },
            Expr::Assign(AssignOp::UnsignedRightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::UnsignedRightShift, lv, rhs);
            },
            Expr::Assign(AssignOp::OrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Or, lv, rhs);
            },
            Expr::Assign(AssignOp::XOrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::XOr, lv, rhs);
            },
            Expr::Assign(AssignOp::AndEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::And, lv, rhs);
            },
            Expr::Assign(AssignOp::PowerOfEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::PowerOf, lv, rhs);
            },
            _ => {
                //not an assignment, proceed as usual
            }
        }
    }
}

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => id_(x.clone()),
        LValue::Dot(e, x) => dot_(e.clone(), x.clone()),
        LValue::Bracket(e1, e2) => bracket_(e1.clone(), e2.clone())
    }
}

fn desugar_assign_op(bin_op: BinaryOp, lv: &mut LValue, rhs: &mut Expr) -> Expr {
    assign_(
        lv.clone(),
        binary_(
            BinOp::BinaryOp(bin_op),
            lval_to_expr(lv),
            rhs.take()))
}

pub fn desugar_updates(program: &mut Stmt, _ng: &mut NameGen) {
    let mut v = DesugarFancyUpdates();
    program.walk(&mut v);
}
