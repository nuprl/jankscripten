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

pub fn simpl(program: &mut Stmt, _ng: &mut NameGen) {
    let mut v = DesugarFancyUpdates();
    program.walk(&mut v);
}

// TODO: more testing with function calls after desugar_functions complete?
#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::*;
    
    #[test]
    fn parse_pluseq() {    
        let prog = r#"
            var MyObject =  { x: 1};
            function f () {
                return MyObject;
            }
            f().x += 1;
        "#;
        
        desugar_okay(prog, simpl)
    }

    #[test]
    fn parse_minuseq() {
        let prog = r#"
            var x = 5;
            x -= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_timeseq() {
        let prog = r#"
            var x = 5;
            x *= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_diveq() {
        let prog = r#"
            var x = 5;
            x /= 1;
        "#;
        
        desugar_okay(prog, simpl)
    }

    #[test]
    fn parse_modeq() {
        let prog = r#"
            var x = 5;
            x %= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_lshifteq() {
        let prog = r#"
            var x = 5;
            x <<= 1;
        "#;

        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_rshifteq() {
        let prog = r#"
            var x = 5;
            x >>= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_unsignedrshifteq() {
        let prog = r#"
            var x = 5;
            x >>>= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_expeq() {
        let prog = r#"
            var x = 5;
            x **= 1;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_oreq() {
        let prog = r#"
            var x = false;
            x |= true;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_xoreq() {
        let prog = r#"
            var x = true;
            x ^= true;
        "#;
        
        desugar_okay(prog, simpl);
    }

    #[test]
    fn parse_andeq() {
        let prog = r#"
            var x = false;
            x &= true;
        "#;

        desugar_okay(prog, simpl);
    }
}
