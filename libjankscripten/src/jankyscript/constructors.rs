use super::syntax::*;

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