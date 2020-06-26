use super::syntax::*;
use crate::shared::coercions::Coercion;

// Lit

pub fn num_(n: &str) -> Lit {
    Lit::Num(n.to_string())
}

// Expressions

pub fn lit_(l: Lit) -> Expr {
    Expr::Lit(l)
}

pub fn binary_(op: BinOp, e1: Expr, e2: Expr) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2))
}

pub fn coercion_(c: Coercion, e: Expr) -> Expr {
    Expr::Coercion(c, Box::new(e))
}