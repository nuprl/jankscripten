use super::parse;
use super::{Expr, Stmt};
use std::collections::HashMap;

/// called by js_quote::stmt!, so it'd better be in scope
pub(crate) fn unplaceholder(
    template_str: &str,
    mut stmts: HashMap<&'static str, Stmt>,
    mut exprs: HashMap<&'static str, Expr>,
) -> Stmt {
    let init_parse = parse(template_str).unwrap(); // TODO(luna)
    init_parse.replace_all(&mut |n| on_stmt(n, &mut stmts), &mut |n| {
        on_expr(n, &mut exprs)
    })
}
pub(crate) fn unplaceholder_expr(
    template_str: &str,
    mut stmts: HashMap<&'static str, Stmt>,
    mut exprs: HashMap<&'static str, Expr>,
) -> Expr {
    let mut init_parse = parse(template_str).unwrap(); // TODO(luna)
    if let Stmt::Block(mut block) = init_parse {
        if block.len() == 1 {
            if let Some(Stmt::Expr(expr_box)) = block.pop() {
                expr_box.replace_all(&mut |n| on_stmt(n, &mut stmts), &mut |n| {
                    on_expr(n, &mut exprs)
                })
            } else {
                panic!("not expr");
            }
        } else {
            panic!("not expr");
        }
    } else {
        panic!("not expr");
    }
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
    use crate::javascript::parser;
    use crate::javascript::{Expr, Lit, Stmt};
    use js_quote::stmt;
    #[test]
    fn js_quote() {
        let y = Stmt::Empty;
        let quoted = stmt!((); #y);
        assert_eq!(quoted, Stmt::Block(vec![Stmt::Empty, Stmt::Empty]))
    }
    #[test]
    fn parse_expr() {
        let expr = parser::parse("5").unwrap();
        assert_eq!(
            expr,
            Stmt::Block(vec![Stmt::Expr(Box::new(Expr::Lit(Lit::Num(
                "5".to_string()
            ))))])
        );
    }
}
