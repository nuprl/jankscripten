use super::syntax::*;
use crate::shared::coercions::Coercion;
use crate::shared::Type;
use crate::pos::Pos;

// Lit

pub fn num_(n: Num, _s: Pos) -> Lit {
    Lit::Num(n)
}

// Expressions

pub fn func(args_with_typs: Vec<(Id, Type)>, result_typ: Type, body: Stmt, s: Pos) -> Expr {
    Expr::Func(Func::new(args_with_typs, result_typ, body), s)
}

pub fn lit_(l: Lit, s: Pos) -> Expr {
    Expr::Lit(l, s)
}

pub fn dot_<I: Into<Id>>(a: Expr, b: I, s: Pos) -> Expr {
    Expr::Dot(Box::new(a), b.into(), s)
}

pub fn bracket_(a: Expr, b: Expr, s: Pos) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b), s)
}

pub fn binary_(op: super::super::notwasm::syntax::BinaryOp, e1: Expr, e2: Expr, s: Pos) -> Expr {
    Expr::Binary(op, Box::new(e1), Box::new(e2), s)
}

pub fn assign_(lv: LValue, e: Expr, s: Pos) -> Expr {
    Expr::Assign(Box::new(lv), Box::new(e), s)
}
pub fn assign_var_(x: Id, ty: Type, e: Expr, s: Pos) -> Expr {
    Expr::Assign(Box::new(LValue::Id(x, ty)), Box::new(e), s)
}

pub fn unary_(op: super::super::notwasm::syntax::UnaryOp, e1: Expr, s: Pos) -> Expr {
    Expr::Unary(op, Box::new(e1), s)
}

pub fn coercion_(c: Coercion, e: Expr, s: Pos) -> Expr {
    match c {
        Coercion::Id(_) => e,
        _ => Expr::Coercion(c, Box::new(e), s),
    }
}

pub fn new_ref_(e1: Expr, ty: Type, s: Pos) -> Expr {
    Expr::NewRef(Box::new(e1), ty, s)
}

pub fn deref_(e1: Expr, ty: Type, s: Pos) -> Expr {
    Expr::Deref(Box::new(e1), ty, s)
}

pub fn store_(id: Id, e1: Expr, ty: Type, s: Pos) -> Expr {
    Expr::Store(
        Box::new(Expr::Id(id, ref_ty_(ty.clone(), s.clone()), s.clone())),
        Box::new(e1),
        ty,
        s,
    )
}

// Statements

pub fn var_(x: Id, t: Type, e: Expr, s: Pos) -> Stmt {
    Stmt::Var(x, t, Box::new(e), s)
}

pub fn block_(stmts: Vec<Stmt>, s: Pos) -> Stmt {
    Stmt::Block(stmts, s)
}

pub fn expr_(e: Expr, s: Pos) -> Stmt {
    Stmt::Expr(Box::new(e), s)
}

pub fn if_(c: Expr, t: Stmt, e: Stmt, s: Pos) -> Stmt {
    Stmt::If(Box::new(c), Box::new(t), Box::new(e), s)
}

pub fn loop_(body: Stmt, s: Pos) -> Stmt {
    Stmt::Loop(Box::new(body), s)
}

pub fn empty_() -> Stmt {
    Stmt::Empty
}

pub fn return_(e: Expr, s: Pos) -> Stmt {
    Stmt::Return(Box::new(e), s)
}

// Types

pub fn ref_ty_(ty: Type, _s: Pos) -> Type {
    Type::Ref(Box::new(ty))
}
