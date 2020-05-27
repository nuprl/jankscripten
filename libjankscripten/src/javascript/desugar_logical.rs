//! desugar &&, ||, ?:

use super::constructors::*;
use super::*;
use resast::LogicalOp;

pub fn desugar_logical(stmt: &mut Stmt, ng: &mut NameGen) {
    stmt.walk(&mut DesugarLogical(ng));
}

struct DesugarLogical<'a>(&'a mut NameGen);
impl Visitor for DesugarLogical<'_> {
    fn enter_expr(&mut self, expr: &mut Expr, loc: &mut Loc) {
        match expr {
            Expr::Binary(BinOp::LogicalOp(op), left, right) => {
                let left_name = self.0.fresh("left");
                let (cons, alt, op_name) = match op {
                    LogicalOp::And => (right.take(), id_(left_name.clone()), "and"),
                    LogicalOp::Or => (id_(left_name.clone()), right.take(), "or"),
                };
                let result = self.0.fresh(op_name);
                let mut if_stmt = if_(
                    id_(left_name.clone()),
                    expr_(assign_(result.clone(), cons)),
                    expr_(assign_(result.clone(), alt)),
                );
                // need to recurse ourselves when inserting
                // if statement includes right but not left
                let mut v = VisitorState::new(self);
                v.walk_stmt(&mut if_stmt, loc);
                let mut left = left.take();
                v.walk_expr(&mut left, loc);
                let ctx = expect_block(loc);
                ctx.insert(ctx.index, vardecl1_(result.clone(), UNDEFINED_));
                ctx.insert(ctx.index, vardecl1_(left_name, left));
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result);
            }
            Expr::If(cond, cons, alt) => {
                let result = self.0.fresh("if_expr");
                let decl = vardecl1_(result.clone(), UNDEFINED_);
                let mut if_stmt = if_(
                    cond.take(),
                    expr_(assign_(result.clone(), cons.take())),
                    expr_(assign_(result.clone(), alt.take())),
                );
                // insert doesn't automatically recurse its statements
                let mut v = VisitorState::new(self);
                // if_stmt includes cond, cons, alt so we're good
                v.walk_stmt(&mut if_stmt, loc);
                let ctx = expect_block(loc);
                ctx.insert(ctx.index, decl);
                ctx.insert(ctx.index, if_stmt);
                *expr = id_(result);
            }
            _ => (),
        }
    }
}

fn expect_block<'a, 'b, 'c>(loc: &'b mut Loc<'a>) -> &'c mut BlockContext
where
    'a: 'c,
    'b: 'c,
{
    if let Loc::Node(Context::Block(ctx), ..) = loc {
        ctx
    } else {
        panic!("expected block context");
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn if_expr() {
        let mut unlabeled = parse("var x = true ? 1 : 2; var garbage = 2;").unwrap();
        let labeled = parse(
            "var $jen_if_expr_0 = undefined;
            if (true)
                $jen_if_expr_0 = 1;
            else
                $jen_if_expr_0 = 2;
            var x = $jen_if_expr_0;
            var garbage = 2;",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut DesugarLogical(&mut ng));
        println!("input:\n{}\noutput:\n{}", unlabeled, labeled);
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
    #[test]
    fn ops() {
        let mut unlabeled =
            parse("var x = true && false ? true || false : false; var garbage = 2;").unwrap();
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
            var garbage = 2;
            ",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut DesugarLogical(&mut ng));
        println!("input:\n{}\noutput:\n{}", unlabeled, labeled);
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
}
