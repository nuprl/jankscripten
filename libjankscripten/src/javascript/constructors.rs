use super::syntax::*;

// Statements:
pub(crate) fn expr_(a: Expr) -> Stmt {
    Stmt::Expr(Box::new(a))
}
pub(crate) fn if_(a: Expr, b: Stmt, c: Stmt) -> Stmt {
    Stmt::If(Box::new(a), Box::new(b), Box::new(c))
}
pub(crate) fn switch_(a: Expr, b: Vec<(Expr, Stmt)>, c: Stmt) -> Stmt {
    Stmt::Switch(Box::new(a), b, Box::new(c))
}
pub(crate) fn while_(a: Expr, b: Stmt) -> Stmt {
    Stmt::While(Box::new(a), Box::new(b))
}
pub(crate) fn dowhile_(a: Stmt, b: Expr) -> Stmt {
    Stmt::DoWhile(Box::new(a), Box::new(b))
}
pub(crate) fn for_(a: ForInit, b: Expr, c: Expr, d: Stmt) -> Stmt {
    Stmt::For(a, Box::new(b), Box::new(c), Box::new(d))
}
pub(crate) fn forin_<I: Into<Id>>(a: bool, b: I, c: Expr, d: Stmt) -> Stmt {
    Stmt::ForIn(a, b.into(), Box::new(c), Box::new(d))
}
pub(crate) fn label_<I: Into<Id>>(a: I, b: Stmt) -> Stmt {
    Stmt::Label(a.into(), Box::new(b))
}
pub(crate) fn break_<I: Into<Id>>(a: Option<I>) -> Stmt {
    Stmt::Break(a.map(Into::into))
}
pub(crate) fn continue_<I: Into<Id>>(a: Option<I>) -> Stmt {
    Stmt::Continue(a.map(Into::into))
}
pub(crate) fn catch_<I: Into<Id>>(a: Stmt, b: I, c: Stmt) -> Stmt {
    Stmt::Catch(Box::new(a), b.into(), Box::new(c))
}
pub(crate) fn finally_(a: Stmt, b: Stmt) -> Stmt {
    Stmt::Finally(Box::new(a), Box::new(b))
}
pub(crate) fn throw_(a: Expr) -> Stmt {
    Stmt::Throw(Box::new(a))
}
pub(crate) fn vardecl1_<T: Into<Id>>(name: T, val: Expr) -> Stmt {
    Stmt::VarDecl(vec![VarDecl {
        name: name.into(),
        named: Box::new(val),
    }])
}
pub(crate) fn func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt) -> Stmt {
    Stmt::Func(
        a.into(),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
    )
}
pub(crate) fn return_(a: Expr) -> Stmt {
    Stmt::Return(Box::new(a))
}

// Expressions
pub(crate) const TRUE_: Expr = Expr::Lit(Lit::Bool(true));
pub(crate) const FALSE_: Expr = Expr::Lit(Lit::Bool(false));
pub(crate) const UNDEFINED_: Expr = Expr::Lit(Lit::Undefined);
pub(crate) fn id_<I: Into<Id>>(id: I) -> Expr {
    Expr::Id(id.into())
}
pub(crate) fn dot_<I: Into<Id>>(a: Expr, b: I) -> Expr {
    Expr::Dot(Box::new(a), b.into())
}
pub(crate) fn bracket_(a: Expr, b: Expr) -> Expr {
    Expr::Bracket(Box::new(a), Box::new(b))
}
pub(crate) fn new_(a: Expr, b: Vec<Expr>) -> Expr {
    Expr::New(Box::new(a), b)
}
pub(crate) fn unary_(a: UnaryOp, b: Expr) -> Expr {
    Expr::Unary(a, Box::new(b))
}
pub(crate) fn binary_(a: BinOp, b: Expr, c: Expr) -> Expr {
    Expr::Binary(a, Box::new(b), Box::new(c))
}
pub(crate) fn or_(b: Expr, c: Expr) -> Expr {
    Expr::Binary(
        BinOp::BinaryOp(resast::BinaryOp::Or),
        Box::new(b),
        Box::new(c),
    )
}
pub(crate) fn unaryassign_(a: UnaryAssignOp, b: LValue) -> Expr {
    Expr::UnaryAssign(a, Box::new(b))
}
pub(crate) fn if_expr_(a: Expr, b: Expr, c: Expr) -> Expr {
    Expr::If(Box::new(a), Box::new(b), Box::new(c))
}
pub(crate) fn op_assign_(a: AssignOp, b: LValue, c: Expr) -> Expr {
    Expr::Assign(a, Box::new(b), Box::new(c))
}
pub(crate) fn assign_(b: LValue, c: Expr) -> Expr {
    op_assign_(AssignOp::Equal, b, c)
}
pub(crate) fn call_(a: Expr, b: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(a), b)
}
pub(crate) fn expr_func_<I: Into<Id>, J: Into<Id>>(a: Option<I>, b: Vec<J>, c: Stmt) -> Expr {
    Expr::Func(
        a.map(|x| x.into()),
        b.into_iter().map(Into::into).collect(),
        Box::new(c),
    )
}
pub(crate) fn named_func_<I: Into<Id>, J: Into<Id>>(a: I, b: Vec<J>, c: Stmt) -> Expr {
    expr_func_(Some(a.into()), b, c)
}
pub(crate) fn lambda<I: Into<Id>>(b: Vec<I>, c: Stmt) -> Expr {
    expr_func_(Option::<I>::None, b, c)
}

// lvals
pub(crate) fn lval_id_<I: Into<Id>>(a: I) -> LValue {
    LValue::Id(a.into())
}
pub(crate) fn lval_dot_<I: Into<Id>>(a: Expr, b: I) -> LValue {
    LValue::Dot(a, b.into())
}
