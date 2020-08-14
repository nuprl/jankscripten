use super::constructors::*;
use super::syntax::*;
use super::walk::*;
use super::*;
use resast::BinaryOp;

//note: will depend on function naming from desugar_function_applications in order to handle f().x += 1; cases
// x += 1;    =>    x = x + 1;

struct DesugarFancyUpdates<'a> {
    ng: &'a mut NameGen,
}

impl Visitor for DesugarFancyUpdates<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => {}
            Expr::Assign(op, lv, rhs) => {
                *expr = self.desugar_assign_op(
                    match op {
                        AssignOp::Equal => unreachable!(),
                        AssignOp::PlusEqual => BinaryOp::Plus,
                        AssignOp::MinusEqual => BinaryOp::Minus,
                        AssignOp::TimesEqual => BinaryOp::Times,
                        AssignOp::DivEqual => BinaryOp::Over,
                        AssignOp::ModEqual => BinaryOp::Mod,
                        AssignOp::LeftShiftEqual => BinaryOp::LeftShift,
                        AssignOp::RightShiftEqual => BinaryOp::RightShift,
                        AssignOp::UnsignedRightShiftEqual => BinaryOp::UnsignedRightShift,
                        AssignOp::OrEqual => BinaryOp::Or,
                        AssignOp::XOrEqual => BinaryOp::XOr,
                        AssignOp::AndEqual => BinaryOp::And,
                        AssignOp::PowerOfEqual => BinaryOp::PowerOf,
                    },
                    lv,
                    rhs,
                    loc,
                )
            }
            Expr::UnaryAssign(op, lv) => {
                // There are four essential cases
                //
                // ++x => x = x + 1, x
                //
                // ++e.x => tmp = e, tmp.x = tmp.x + 1, tmp.x                              tmp fresh
                //
                // x++ => tmp = x, x = x + 1, tmp                                          tmp fresh
                //
                // e.x++ => tmp1 = e, tmp2 = tmp1.x, tmp1.x = tmp1.x + 1, tmp    tmp1 and tmp2 fresh
                //
                // The decrement operators are similar to the increment operators. Computed field
                // lookups (e1[e2]) may require a new temporary variable too.
                //
                // We can do a bit better than cases above suggest by using the  is_essentially_atom
                // methods to avoid introducing unnecessary temporary variables.
                let block = loc.enclosing_block().unwrap();
                let e = self.lval_to_expr(lv, loc);
                // Insert the statement 'atom = atom + 1' immediately before this expression.
                block.insert(
                    block.index,
                    expr_(assign_(
                        lv.take(),
                        binary_(op.binop(), e.clone(), Expr::Lit(Lit::Num(Num::Int(1)))),
                    )),
                );
                if op.is_prefix() {
                    *expr = e;
                } else {
                    // atom = atom - 1
                    *expr = binary_(
                        op.other_binop(),
                        e.clone(),
                        Expr::Lit(Lit::Num(Num::Int(1))),
                    )
                }
            }
            _ => {
                //not an assignment, proceed as usual
            }
        }
    }
}

impl DesugarFancyUpdates<'_> {
    fn lval_to_expr(&mut self, lv: &mut LValue, loc: &Loc) -> Expr {
        match lv {
            LValue::Id(x) => id_(x.clone()),
            LValue::Dot(e, x) => {
                let cxt = loc.enclosing_block().unwrap();
                self.lift_to_id(cxt, e);
                dot_(e.clone(), x.clone())
            }
            LValue::Bracket(e1, e2) => {
                let cxt = loc.enclosing_block().unwrap();
                self.lift_to_id(cxt, e1);
                self.lift_to_id(cxt, e2);
                bracket_(e1.clone(), e2.clone())
            }
        }
    }
    fn lift_to_id(&mut self, cxt: &BlockContext, expr: &mut Expr) {
        if !expr.is_essentially_atom() {
            let e_name = self.ng.fresh("update_assign");
            cxt.insert(cxt.index, vardecl1_(e_name.clone(), expr.take()));
            *expr = id_(e_name);
        }
    }
    fn desugar_assign_op(
        &mut self,
        bin_op: BinaryOp,
        lv: &mut LValue,
        rhs: &mut Expr,
        loc: &Loc,
    ) -> Expr {
        let expr = self.lval_to_expr(lv, loc);
        assign_(
            lv.take(),
            binary_(BinOp::BinaryOp(bin_op), expr, rhs.take()),
        )
    }
}

pub fn desugar_updates(program: &mut Stmt, ng: &mut NameGen) {
    let mut v = DesugarFancyUpdates { ng };
    program.walk(&mut v);
}
