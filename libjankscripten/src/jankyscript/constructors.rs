use super::syntax::*;
use crate::shared::coercions::Coercion;
use crate::shared::Type;

// Lit

pub fn num_(n: Num, s: Span) -> Lit {
    Lit::Num(n)
}

// Expressions

pub fn func(args_with_typs: Vec<(Id, Type)>, result_typ: Type, body: Stmt, s: Span) -> Expr {
    Expr::Func(Func::new(args_with_typs, result_typ, body), s)
}

pub fn lit_(l: Lit, s: Span) -> Expr {
    Expr::Lit(l, s)
}

pub fn dot_<I: Into<Id>>(a: Expr, b: I, s: Span) -> Expr {
    Expr::Dot(Box::new(a), b.into(), s)
}

pub fn bracket_(a: Expr, b: Expr, s: Span) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b), s)
}

pub fn binary_(op: super::super::notwasm::syntax::BinaryOp, e1: Expr, e2: Expr, s: Span) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2), s)
}

pub fn assign_(lv: LValue, e: Expr, s: Span) -> Expr {
    Expr::Assign(Box::new(lv), Box::new(e), s)
}
pub fn assign_var_(x: Id, ty: Type, e: Expr, s: Span) -> Expr {
    Expr::Assign(Box::new(LValue::Id(x, ty)), Box::new(e), s)
}

pub fn unary_(op: super::super::notwasm::syntax::UnaryOp, e1: Expr, s: Span) -> Expr {
    Expr::Unary(op, Box::new(e1), s)
}

pub fn coercion_(c: Coercion, e: Expr, s: Span) -> Expr {
    match c {
        Coercion::Id(_) => e,
        _ => Expr::Coercion(c, Box::new(e), s),
    }
}

pub fn new_ref_(e1: Expr, ty: Type, s: Span) -> Expr {
    Expr::NewRef(Box::new(e1), ty, s)
}

pub fn deref_(e1: Expr, ty: Type, s: Span) -> Expr {
    Expr::Deref(Box::new(e1), ty, s)
}

pub fn store_(id: Id, e1: Expr, ty: Type, s: Span) -> Expr {
    Expr::Store(
        Box::new(Expr::Id(id, ref_ty_(ty.clone(), s), s)),
        Box::new(e1),
        ty,
        s,
    )
}

// Statements

pub fn var_(x: Id, t: Type, e: Expr, s: Span) -> Stmt {
    Stmt::Var(x, t, Box::new(e), s)
}

pub fn block_(stmts: Vec<Stmt>, s: Span) -> Stmt {
    Stmt::Block(stmts, s)
}

pub fn expr_(e: Expr, s: Span) -> Stmt {
    Stmt::Expr(Box::new(e), s)
}

pub fn if_(c: Expr, t: Stmt, e: Stmt, s: Span) -> Stmt {
    Stmt::If(Box::new(c), Box::new(t), Box::new(e), s)
}

pub fn loop_(body: Stmt, s: Span) -> Stmt {
    Stmt::Loop(Box::new(body), s)
}

pub fn empty_() -> Stmt {
    Stmt::Empty
}

pub fn return_(e: Expr, s: Span) -> Stmt {
    Stmt::Return(Box::new(e), s)
}

// Types

pub fn ref_ty_(ty: Type, s: Span) -> Type {
    Type::Ref(Box::new(ty))
}
