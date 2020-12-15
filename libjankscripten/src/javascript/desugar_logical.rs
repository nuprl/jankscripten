//! desugar &&, ||, ?:, and , (seq)

use super::constructors::*;
use super::syntax::LogicalOp;
use super::*;

/// desugar &&, ||, ?:, and , (seq)
pub fn desugar_logical(stmt: &mut Stmt, ng: &mut NameGen) {
    stmt.walk(&mut DesugarLogical(ng));
}

/// visitor for all logical desugars
struct DesugarLogical<'a>(&'a mut NameGen);
impl Visitor for DesugarLogical<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        let ctx = loc.enclosing_block().expect("expected block context");
        match expr {
            Expr::Binary(BinOp::LogicalOp(op), left, right, ref s) => {
                let left_name = self.0.fresh("left");
                let (cons, alt, op_name) = match op {
                    LogicalOp::And => (right.take(), id_(left_name.clone(), s.clone()), "and"),
                    LogicalOp::Or => (id_(left_name.clone(), s.clone()), right.take(), "or"),
                };
                let result = self.0.fresh(op_name);
                let if_stmt = if_(
                    id_(left_name.clone(), s.clone()),
                    expr_(assign_(result.clone(), cons, s.clone()), s.clone()),
                    expr_(assign_(result.clone(), alt, s.clone()), s.clone()),
                    s.clone(),
                );
                ctx.insert(ctx.index, vardecl1_(result.clone(), UNDEFINED_, s.clone()));
                ctx.insert(ctx.index, vardecl1_(left_name, left.take(), s.clone()));
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result, s.clone());
            }
            Expr::If(cond, cons, alt, s) => {
                let result = self.0.fresh("if_expr");
                ctx.insert(ctx.index, vardecl1_(result.clone(), UNDEFINED_, s.clone()));
                let if_stmt = if_(
                    cond.take(),
                    expr_(assign_(result.clone(), cons.take(), s.clone()), s.clone()),
                    expr_(assign_(result.clone(), alt.take(), s.clone()), s.clone()),
                    s.clone(),
                );
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result, s.clone());
            }
            Expr::Seq(es, s) => {
                let last = es.pop().expect("sequence with no exprs");
                for e in es {
                    ctx.insert(ctx.index, expr_(e.take(), s.clone()));
                }
                *expr = last;
            }
            _ => (),
        }
    }
}
