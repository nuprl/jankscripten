//! Constructor functions, that facilitate building ASTs in code.
use super::syntax::*;

// Statements:
pub fn expr_(a: Expr) -> Stmt {
    Stmt::Expr(Box::new(a))
}

pub fn if_(a: Expr, b: Stmt, c: Stmt) -> Stmt {
    Stmt::If(Box::new(a), Box::new(b), Box::new(c))
}

pub fn switch_(a: Expr, b: Vec<(Expr, Stmt)>, c: Stmt) -> Stmt {
    Stmt::Switch(Box::new(a), b, Box::new(c))
}

pub fn while_(a: Expr, b: Stmt) -> Stmt {
    Stmt::While(Box::new(a), Box::new(b))
}

pub fn dowhile_(a: Stmt, b: Expr) -> Stmt {
    Stmt::DoWhile(Box::new(a), Box::new(b))
}

pub fn for_(a: ForInit, b: Expr, c: Expr, d: Stmt) -> Stmt {
    Stmt::For(a, Box::new(b), Box::new(c), Box::new(d))
}

pub fn forin_<I: Into<Id>>(a: bool, b: I, c: Expr, d: Stmt) -> Stmt {
    Stmt::ForIn(a, b.into(), Box::new(c), Box::new(d))
}

pub fn label_<I: Into<Id>>(a: I, b: Stmt) -> Stmt {
    Stmt::Label(a.into(), Box::new(b))
}

pub fn break_<I: Into<Id>>(a: Option<I>) -> Stmt {
    Stmt::Break(a.map(Into::into))
}

pub fn continue_<I: Into<Id>>(a: Option<I>) -> Stmt {
    Stmt::Continue(a.map(Into::into))
}

pub fn catch_<I: Into<Id>>(a: Stmt, b: I, c: Stmt) -> Stmt {
    Stmt::Catch(Box::new(a), b.into(), Box::new(c))
}

pub fn finally_(a: Stmt, b: Stmt) -> Stmt {
    Stmt::Finally(Box::new(a), Box::new(b))
}

pub fn throw_(a: Expr) -> Stmt {
    Stmt::Throw(Box::new(a))
}

pub fn vardecl1_<T: Into<Id>>(name: T, val: Expr) -> Stmt {
    Stmt::VarDecl(vec![VarDecl {
        name: name.into(),
        named: Box::new(val),
    }])
}

pub fn func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt) -> Stmt {
    Stmt::Func(
        a.into(),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
    )
}

pub fn return_(a: Expr) -> Stmt {
    Stmt::Return(Box::new(a))
}

// Expressions
pub const TRUE_: Expr = Expr::Lit(Lit::Bool(true));
pub const FALSE_: Expr = Expr::Lit(Lit::Bool(false));
pub const UNDEFINED_: Expr = Expr::Lit(Lit::Undefined);

pub fn str_(s: impl Into<String>) -> Expr {
    Expr::Lit(Lit::String(s.into()))
}

pub fn id_<I: Into<Id>>(id: I) -> Expr {
    Expr::Id(id.into())
}

pub fn dot_<I: Into<Id>>(a: Expr, b: I) -> Expr {
    Expr::Dot(Box::new(a), b.into())
}

pub fn bracket_(a: Expr, b: Expr) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b))
}

pub fn new_(a: Expr, b: Vec<Expr>) -> Expr {
    Expr::New(Box::new(a), b)
}

pub fn unary_(a: UnaryOp, b: Expr) -> Expr {
    Expr::Unary(a, Box::new(b))
}

pub fn not_(b: Expr) -> Expr {
    unary_(UnaryOp::Not, b)
}

pub fn binary_(a: BinOp, b: Expr, c: Expr) -> Expr {
    Expr::Binary(a, Box::new(b), Box::new(c))
}

pub fn or_(b: Expr, c: Expr) -> Expr {
    binary_(BinOp::LogicalOp(LogicalOp::Or), b, c)
}

pub fn unaryassign_(a: UnaryAssignOp, b: LValue) -> Expr {
    Expr::UnaryAssign(a, Box::new(b))
}

pub fn if_expr_(a: Expr, b: Expr, c: Expr) -> Expr {
    Expr::If(Box::new(a), Box::new(b), Box::new(c))
}

pub fn op_assign_<L: Into<LValue>>(a: AssignOp, b: L, c: Expr) -> Expr {
    Expr::Assign(a, Box::new(b.into()), Box::new(c))
}

pub fn assign_<L: Into<LValue>>(b: L, c: Expr) -> Expr {
    op_assign_(AssignOp::Equal, b, c)
}

pub fn call_(a: Expr, b: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(a), b)
}

pub fn expr_func_<I: Into<Id>, J: Into<Id>>(a: Option<I>, b: Vec<J>, c: Stmt) -> Expr {
    Expr::Func(
        a.map(|x| x.into()),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
    )
}

pub fn named_func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt) -> Expr {
    expr_func_(Some(a.into()), b, c)
}

pub fn lambda<I: Into<Id>>(b: Vec<I>, c: Stmt) -> Expr {
    expr_func_(Option::<I>::None, b, c)
}

// lvals
pub fn lval_id_<I: Into<Id>>(a: I) -> LValue {
    LValue::Id(a.into())
}

pub fn lval_dot_<I: Into<Id>>(a: Expr, b: I) -> LValue {
    LValue::Dot(a, b.into())
}
