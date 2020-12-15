//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.

pub use crate::shared::Id;
pub use crate::shared::Type;
use crate::pos::Pos;

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

pub type BinOp = super::super::javascript::BinaryOp;

#[derive(Debug)]
pub enum Expr {
    Lit(super::super::javascript::Lit, Pos),
    Array(Vec<Expr>, Pos),
    Object(Vec<(super::super::javascript::Key, Expr)>, Pos),
    Id(Id, Pos),
    Dot(Box<Expr>, Id, Pos),
    Bracket(Box<Expr>, Box<Expr>, Pos),
    Unary(super::super::javascript::UnaryOp, Box<Expr>, Pos),
    Binary(BinOp, Box<Expr>, Box<Expr>, Pos),
    Assign(Box<LValue>, Box<Expr>, Pos),
    Call(Box<Expr>, Vec<Expr>, Pos),
    Func(Option<Type>, Vec<(Id, Option<Type>)>, Box<Stmt>, Pos),
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Option<Type>, Box<Expr>, Pos),
    Block(Vec<Stmt>, Pos),
    Expr(Box<Expr>, Pos),
    Empty,
    If(Box<Expr>, Box<Stmt>, Box<Stmt>, Pos),
    Loop(Box<Stmt>, Pos),
    Label(Id, Box<Stmt>, Pos),
    Break(Id, Pos),
    Catch(Box<Stmt>, Id, Box<Stmt>, Pos),
    Finally(Box<Stmt>, Box<Stmt>, Pos),
    Throw(Box<Expr>, Pos),
    Return(Box<Expr>, Pos),
}
