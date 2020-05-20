use super::parse;
use super::{Expr, Stmt};
use std::collections::HashMap;

/// called by js_quote::stmt!, so it'd better be in scope
pub(crate) fn unplaceholder(
    template_str: &str,
    mut stmts: HashMap<&'static str, Stmt>,
    mut exprs: HashMap<&'static str, Expr>,
) -> Stmt {
    let init_parse = cancel_single_block(parse(template_str).unwrap()); // TODO(luna)
    println!("{:?}", exprs);
    init_parse.replace_all(&mut |n| on_stmt(n, &mut stmts), &mut |n| {
        on_expr(n, &mut exprs)
    })
}
pub(crate) fn unplaceholder_expr(
    template_str: &str,
    mut stmts: HashMap<&'static str, Stmt>,
    mut exprs: HashMap<&'static str, Expr>,
) -> Expr {
    let init_parse = cancel_single_block(parse(template_str).unwrap()); // TODO(luna)
    if let Stmt::Expr(expr_box) = init_parse {
        expr_box.replace_all(&mut |n| on_stmt(n, &mut stmts), &mut |n| {
            on_expr(n, &mut exprs)
        })
    } else {
        panic!("not expr");
    }
}

fn cancel_single_block(mut stmt: Stmt) -> Stmt {
    if let Stmt::Block(ref mut block) = stmt {
        if block.len() == 1 {
            return block.pop().expect("len is 1");
        }
    }
    stmt
}

fn on_stmt(node: Stmt, stmts: &mut HashMap<&'static str, Stmt>) -> Stmt {
    match node {
        Stmt::Throw(ref what) => match &**what {
            Expr::Id(ref x) => {
                if let Some(stmt) = stmts.remove(x.as_str()) {
                    println!("woo");
                    stmt
                } else {
                    node
                }
            }
            _ => node,
        },
        _ => node,
    }
}

fn on_expr(node: Expr, exprs: &mut HashMap<&'static str, Expr>) -> Expr {
    match node {
        Expr::Id(ref x) => {
            println!("id exists");
            if let Some(expr) = exprs.remove(x.as_str()) {
                println!("wooexpr");
                expr
            } else {
                node
            }
        }
        _ => node,
    }
}

#[cfg(test)]
mod test {
    use crate::javascript::cons::*;
    use crate::javascript::parser;
    use crate::javascript::{BinOp, Expr, Lit, Num, Stmt};
    use js_quote::{expr, stmt};
    use resast::BinaryOp;
    #[test]
    fn js_quote() {
        let y = Stmt::Empty;
        let quoted = stmt!(throw x; #y);
        assert_eq!(
            quoted,
            Stmt::Block(vec![throw_(Expr::Id("x".to_string())), Stmt::Empty])
        )
    }
    #[test]
    fn expr() {
        let quoted = expr!(5 + 6);
        assert_eq!(
            quoted,
            binary_(
                BinOp::BinaryOp(BinaryOp::Plus),
                Expr::Lit(Lit::Num(Num::Int(5))),
                Expr::Lit(Lit::Num(Num::Int(6)))
            )
        );
    }
    #[test]
    fn block() {
        let quoted = stmt!({
            throw x;
            throw y
        });
        assert_eq!(
            quoted,
            Stmt::Block(vec![throw_(id_("x")), throw_(id_("y"))])
        );
    }
    #[test]
    fn empty_block() {
        let quoted = stmt!({});
        assert_eq!(quoted, Stmt::Block(vec![]));
    }
    #[test]
    fn substs() {
        let quoted = expr!(5 + 6);
        let expected_expr = binary_(
            BinOp::BinaryOp(BinaryOp::Plus),
            Expr::Lit(Lit::Num(Num::Int(5))),
            Expr::Lit(Lit::Num(Num::Int(6))),
        );
        assert_eq!(quoted, expected_expr);
        let quoted = stmt!(let x = @quoted);
        let expected_assign = Stmt::VarDecl(vardecl1_("x", expected_expr));
        assert_eq!(quoted, expected_assign);
        let quoted = stmt!({
            #quoted
            {}
        });
        assert_eq!(
            quoted,
            Stmt::Block(vec![expected_assign, Stmt::Block(vec![])])
        );
    }
    #[test]
    fn unbox() {
        let boxed = Box::new(Expr::This);
        let x = stringify!(*@test);
        println!("{}", x);
        let quoted = stmt!(let x = @*boxed);
        assert_eq!(quoted, Stmt::VarDecl(vardecl1_("x", Expr::This)));
    }
    #[test]
    fn parse_expr_expectations() {
        let expr = parser::parse("5").unwrap();
        assert_eq!(
            expr,
            Stmt::Block(vec![Stmt::Expr(Box::new(Expr::Lit(Lit::Num(Num::Int(5)))))])
        );
    }
    #[test]
    #[should_panic]
    fn eq_fails_sometimes() {
        assert_eq!(stmt!(x = 5), stmt!(x = 6));
    }
}
