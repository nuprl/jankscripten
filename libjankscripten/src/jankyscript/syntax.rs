//! The JankierScript language

pub use crate::shared::coercions::Coercion;
pub use crate::shared::types::Type;

pub type Id = super::super::javascript::Id;
pub type Lit = super::super::javascript::Lit;
pub type Num = super::super::javascript::Num;
pub type Key = super::super::javascript::Key;

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Array(Vec<Expr>),
    Object(Vec<(Key, Expr)>),
    This,
    Id(Id),
    Dot(Box<Expr>, Id),
    Bracket(Box<Expr>, Box<Expr>),
    Unary(super::super::notwasm::syntax::UnaryOp, Box<Expr>),
    Binary(super::super::notwasm::syntax::BinaryOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    New(Type, Box<Expr>, Vec<Expr>),
    Func(Type, Vec<(Id, Type)>, Box<Stmt>),
    Coercion(Coercion, Box<Expr>)
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Type, Box<Expr>),
    Block(Vec<Stmt>),
    Empty,
    Expr(Box<Expr>),
    Assign(Box<LValue>, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
}
