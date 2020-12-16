//! Constructor functions, that facilitate building ASTs in code.
use super::syntax::*;
use crate::pos::Pos;

// Statements:
pub fn expr_(a: Expr, s: Pos) -> Stmt {
    Stmt::Expr(Box::new(a), s)
}

pub fn if_(a: Expr, b: Stmt, c: Stmt, s: Pos) -> Stmt {
    Stmt::If(Box::new(a), Box::new(b), Box::new(c), s)
}

pub fn switch_(a: Expr, b: Vec<(Expr, Stmt)>, c: Stmt, s: Pos) -> Stmt {
    Stmt::Switch(Box::new(a), b, Box::new(c), s)
}

pub fn while_(a: Expr, b: Stmt, s: Pos) -> Stmt {
    Stmt::While(Box::new(a), Box::new(b), s)
}

pub fn dowhile_(a: Stmt, b: Expr, s: Pos) -> Stmt {
    Stmt::DoWhile(Box::new(a), Box::new(b), s)
}

pub fn for_(a: ForInit, b: Expr, c: Expr, d: Stmt, s: Pos) -> Stmt {
    Stmt::For(a, Box::new(b), Box::new(c), Box::new(d), s)
}

pub fn forin_<I: Into<Id>>(a: bool, b: I, c: Expr, d: Stmt, s: Pos) -> Stmt {
    Stmt::ForIn(a, b.into(), Box::new(c), Box::new(d), s)
}

pub fn label_<I: Into<Id>>(a: I, b: Stmt, s: Pos) -> Stmt {
    Stmt::Label(a.into(), Box::new(b), s)
}

pub fn break_<I: Into<Id>>(a: Option<I>, s: Pos) -> Stmt {
    Stmt::Break(a.map(Into::into), s)
}

pub fn continue_<I: Into<Id>>(a: Option<I>, s: Pos) -> Stmt {
    Stmt::Continue(a.map(Into::into), s)
}

pub fn catch_<I: Into<Id>>(a: Stmt, b: I, c: Stmt, s: Pos) -> Stmt {
    Stmt::Catch(Box::new(a), b.into(), Box::new(c), s)
}

pub fn finally_(a: Stmt, b: Stmt, s: Pos) -> Stmt {
    Stmt::Finally(Box::new(a), Box::new(b), s)
}

pub fn throw_(a: Expr, s: Pos) -> Stmt {
    Stmt::Throw(Box::new(a), s)
}

pub fn vardecl1_<T: Into<Id>>(name: T, val: Expr, s: Pos) -> Stmt {
    Stmt::VarDecl(
        vec![VarDecl {
            name: name.into(),
            named: Box::new(val),
        }],
        s,
    )
}

pub fn func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt, s: Pos) -> Stmt {
    Stmt::Func(
        a.into(),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
        s,
    )
}

pub fn return_(a: Expr, p: Pos) -> Stmt {
    Stmt::Return(Box::new(a), p)
}

// Expressions
pub const TRUE_: Expr = Expr::Lit(Lit::Bool(true), Pos::UNKNOWN);
pub const FALSE_: Expr = Expr::Lit(Lit::Bool(false), Pos::UNKNOWN);
pub const UNDEFINED_: Expr = Expr::Lit(Lit::Undefined, Pos::UNKNOWN);

pub fn str_(st: impl Into<String>, s: Pos) -> Expr {
    Expr::Lit(Lit::String(st.into()), s)
}

pub fn id_<I: Into<Id>>(id: I, s: Pos) -> Expr {
    Expr::Id(id.into(), s)
}

pub fn dot_<I: Into<Id>>(a: Expr, b: I, s: Pos) -> Expr {
    Expr::Dot(Box::new(a), b.into(), s)
}

pub fn bracket_(a: Expr, b: Expr, s: Pos) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b), s)
}

pub fn new_(a: Expr, b: Vec<Expr>, s: Pos) -> Expr {
    Expr::New(Box::new(a), b, s)
}

pub fn unary_(a: UnaryOp, b: Expr, s: Pos) -> Expr {
    Expr::Unary(a, Box::new(b), s)
}

pub fn not_(b: Expr, s: Pos) -> Expr {
    unary_(UnaryOp::Not, b, s)
}

pub fn binary_(a: BinOp, b: Expr, c: Expr, s: Pos) -> Expr {
    Expr::Binary(a, Box::new(b), Box::new(c), s)
}

pub fn or_(b: Expr, c: Expr, s: Pos) -> Expr {
    binary_(BinOp::LogicalOp(LogicalOp::Or), b, c, s)
}

pub fn unaryassign_(a: UnaryAssignOp, b: LValue, s: Pos) -> Expr {
    Expr::UnaryAssign(a, Box::new(b), s)
}

pub fn if_expr_(a: Expr, b: Expr, c: Expr, s: Pos) -> Expr {
    Expr::If(Box::new(a), Box::new(b), Box::new(c), s)
}

pub fn op_assign_<L: Into<LValue>>(a: AssignOp, b: L, c: Expr, s: Pos) -> Expr {
    Expr::Assign(a, Box::new(b.into()), Box::new(c), s)
}

pub fn assign_<L: Into<LValue>>(b: L, c: Expr, s: Pos) -> Expr {
    op_assign_(AssignOp::Equal, b, c, s)
}

pub fn call_(a: Expr, b: Vec<Expr>, s: Pos) -> Expr {
    Expr::Call(Box::new(a), b, s)
}

pub fn expr_func_<I: Into<Id>, J: Into<Id>>(a: Option<I>, b: Vec<J>, c: Stmt, s: Pos) -> Expr {
    Expr::Func(
        a.map(|x| x.into()),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
        s,
    )
}

pub fn named_func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt, s: Pos) -> Expr {
    expr_func_(Some(a.into()), b, c, s)
}

pub fn lambda<I: Into<Id>>(b: Vec<I>, c: Stmt, s: Pos) -> Expr {
    expr_func_(Option::<I>::None, b, c, s)
}

// lvals
pub fn lval_id_<I: Into<Id>>(a: I, _s: Pos) -> LValue {
    LValue::Id(a.into())
}

pub fn lval_dot_<I: Into<Id>>(a: Expr, b: I, _s: Pos) -> LValue {
    LValue::Dot(a, b.into())
}
