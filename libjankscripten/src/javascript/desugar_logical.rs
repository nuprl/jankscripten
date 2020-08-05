//! desugar &&, ||, ?:, and , (seq)

use super::constructors::*;
use super::*;
use resast::LogicalOp;

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
            Expr::Binary(BinOp::LogicalOp(op), left, right) => {
                let left_name = self.0.fresh("left");
                let (cons, alt, op_name) = match op {
                    LogicalOp::And => (right.take(), id_(left_name.clone()), "and"),
                    LogicalOp::Or => (id_(left_name.clone()), right.take(), "or"),
                };
                let result = self.0.fresh(op_name);
                let if_stmt = if_(
                    id_(left_name.clone()),
                    expr_(assign_(result.clone(), cons)),
                    expr_(assign_(result.clone(), alt)),
                );
                ctx.insert(ctx.index, vardecl1_(result.clone(), UNDEFINED_));
                ctx.insert(ctx.index, vardecl1_(left_name, left.take()));
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result);
            }
            Expr::If(cond, cons, alt) => {
                let result = self.0.fresh("if_expr");
                ctx.insert(ctx.index, vardecl1_(result.clone(), UNDEFINED_));
                let if_stmt = if_(
                    cond.take(),
                    expr_(assign_(result.clone(), cons.take())),
                    expr_(assign_(result.clone(), alt.take())),
                );
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result);
            }
            Expr::Seq(es) => {
                let last = es.pop().expect("sequence with no exprs");
                for e in es {
                    ctx.insert(ctx.index, expr_(e.take()));
                }
                *expr = last;
            }
            _ => (),
        }
    }
}
