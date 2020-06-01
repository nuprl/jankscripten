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
        let ctx = if let Loc::Node(Context::Block(ctx), ..) = loc {
            ctx
        } else {
            panic!("expected block context");
        };
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::desugar_okay;
    #[test]
    fn ops() {
        let program = "var x = true && false ? true || false : false; x";
        let mut unlabeled = parse(program).unwrap();
        let labeled = parse(
            "var $jen_and_0 = undefined;
            var $jen_left_0 = true;
            if ($jen_left_0)
                $jen_and_0 = false;
            else
                $jen_and_0 = $jen_left_0;
            var $jen_or_0 = undefined;
            var $jen_left_1 = true;
            if ($jen_left_1)
                $jen_or_0 = $jen_left_1;
            else
                $jen_or_0 = false;
            var $jen_if_expr_0 = undefined;
            if ($jen_and_0)
                $jen_if_expr_0 = $jen_or_0;
            else
                $jen_if_expr_0 = false;
            var x = $jen_if_expr_0;
            x
            ",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut DesugarLogical(&mut ng));
        println!("input:\n{}\noutput:\n{}", unlabeled, labeled);
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
        desugar_okay(program, desugar_logical);
    }
    #[test]
    fn if_expr() {
        let program = "var x = true ? 1 : 2; x";
        desugar_okay(program, desugar_logical);
    }
    #[test]
    fn seq() {
        desugar_okay(
            "var x = true;
            var r = true;
            while (x = false, x) {
                // shouldn't happen
                r = false;
                break;
            }
            r;",
            desugar_logical,
        );
    }
}
