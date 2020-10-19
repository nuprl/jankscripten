use super::syntax::*;
use crate::shared::ops::BinOp;
use crate::shared::types::Type;

// Expressions

pub fn lit_(l: Lit, s: Span) -> Expr {
    Expr::Lit(l, s)
}

pub fn binary_(op: BinOp, e1: Expr, e2: Expr, s: Span) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2), s)
}

// Statements

// use default arguments for t.
pub fn var_(x: &str, e: Expr, s: Span) -> Stmt {
    Stmt::Var(x.to_string(), None, e, s)
}

pub fn block_(stmts: Vec<Stmt>, s: Span) -> Stmt {
    Stmt::Block(stmts, s)
}
