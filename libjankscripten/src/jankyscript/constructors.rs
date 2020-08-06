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

pub fn dot_<I: Into<Id>>(a: Expr, b: I) -> Expr {
    Expr::Dot(Box::new(a), b.into())
}

pub fn bracket_(a: Expr, b: Expr) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b))
}

pub fn binary_(op: super::super::notwasm::syntax::BinaryOp, e1: Expr, e2: Expr) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2))
}

pub fn assign_(lv: LValue, e: Expr) -> Expr {
    Expr::Assign(Box::new(lv), Box::new(e))
}
pub fn assign_var_(x: Id, e: Expr) -> Expr {
    Expr::Assign(Box::new(LValue::Id(x)), Box::new(e))
}

pub fn unary_(op: super::super::notwasm::syntax::UnaryOp, e1: Expr) -> Expr {
    Expr::Unary(op, Box::new(e1))
}

pub fn coercion_(c: Coercion, e: Expr) -> Expr {
    match c {
        Coercion::Id(_) => e,
        _ => Expr::Coercion(c, Box::new(e)),
    }
}

// Statements

pub fn var_(x: Id, t: Type, e: Expr) -> Stmt {
    Stmt::Var(x, t, Box::new(e))
}

pub fn block_(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(stmts)
}

pub fn expr_(e: Expr) -> Stmt {
    Stmt::Expr(Box::new(e))
}

pub fn if_(c: Expr, t: Stmt, e: Stmt) -> Stmt {
    Stmt::If(Box::new(c), Box::new(t), Box::new(e))
}

pub fn loop_(body: Stmt) -> Stmt {
    Stmt::Loop(Box::new(body))
}

pub fn empty_() -> Stmt {
    Stmt::Empty
}
