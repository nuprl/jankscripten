use super::syntax::*;
use crate::shared::coercions::Coercion;
use crate::shared::types::Type;

// Lit

pub fn num_(n: Num) -> Lit {
    Lit::Num(n)
}

// Expressions

pub fn lit_(l: Lit) -> Expr {
    Expr::Lit(l)
}

pub fn binary_(op: super::super::notwasm::syntax::BinaryOp, e1: Expr, e2: Expr) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2))
}

pub fn coercion_(c: Coercion, e: Expr) -> Expr {
    Expr::Coercion(c, Box::new(e))
}

// Statements

pub fn var_(x: Id, t: Type, e: Expr) -> Stmt {
    Stmt::Var(x, t, Box::new(e))
}

pub fn block_(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(stmts)
}

pub fn if_(c: Expr, t: Stmt, e: Stmt) -> Stmt {
    Stmt::If(Box::new(c), Box::new(t), Box::new(e))
}

pub fn while_(cond: Expr, body: Stmt) -> Stmt {
    Stmt::While(Box::new(cond), Box::new(body))
}

pub fn empty_() -> Stmt {
    Stmt::Empty
}