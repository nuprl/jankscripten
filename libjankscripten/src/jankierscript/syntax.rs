//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.
use crate::shared::types::Type;

pub type Id = super::super::javascript::Id;

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

pub type BinOp = resast::BinaryOp;

#[derive(Debug)]
pub enum Expr {
    Lit(super::super::javascript::Lit),
    Array(Vec<Expr>),
    Object(Vec<(super::super::javascript::Key, Expr)>),
    Id(Id),
    Dot(Box<Expr>, Id),
    Bracket(Box<Expr>, Box<Expr>),
    Unary(super::super::javascript::UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assign(Box<LValue>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Func(Option<Type>, Vec<(Id, Option<Type>)>, Box<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Option<Type>, Box<Expr>),
    Block(Vec<Stmt>),
    Expr(Box<Expr>),
    Empty,
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Loop(Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
}
