//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.

pub use crate::shared::Id;
pub use crate::shared::Type;

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

pub type BinOp = super::super::javascript::BinaryOp;

#[derive(Debug)]
pub enum Expr {
    Lit(super::super::javascript::Lit, Span),
    Array(Vec<Expr>, Span),
    Object(Vec<(super::super::javascript::Key, Expr, Span)>, Span),
    Id(Id, Span),
    Dot(Box<Expr>, Id, Span),
    Bracket(Box<Expr>, Box<Expr>, Span),
    Unary(super::super::javascript::UnaryOp, Box<Expr>, Span),
    Binary(BinOp, Box<Expr>, Box<Expr>, Span),
    Assign(Box<LValue>, Box<Expr>, Span),
    Call(Box<Expr>, Vec<Expr>, Span),
    Func(Option<Type>, Vec<(Id, Option<Type>, Span)>, Box<Stmt>, Span),
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Option<Type>, Box<Expr>, Span),
    Block(Vec<Stmt>, Span),
    Expr(Box<Expr>, Span),
    Empty,
    If(Box<Expr>, Box<Stmt>, Box<Stmt>, Span),
    Loop(Box<Stmt>, Span),
    Label(Id, Box<Stmt>, Span),
    Break(Id, Span),
    Catch(Box<Stmt>, Id, Box<Stmt>, Span),
    Finally(Box<Stmt>, Box<Stmt>, Span),
    Throw(Box<Expr>, Span),
    Return(Box<Expr>, Span),
}
