use super::syntax::*;
use crate::shared::types::Type;

// Expressions

pub fn lit_(l: Lit) -> Expr {
    Expr::Lit(l)
}

pub fn binary_(op: BinOp, e1: Expr, e2: Expr) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2))
}

// Statements

// use default arguments for t. 
pub fn var_(x: &str, t: Option<Type>, e: Expr) -> Stmt {
    Stmt::Var(x.to_string(), t, e)
}

pub fn block_(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(stmts)
}