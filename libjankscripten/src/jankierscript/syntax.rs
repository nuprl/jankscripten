//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.

pub use crate::javascript::Lit;
use crate::pos::Pos;
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
    Lit(Lit, Pos),
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
    /// TODO(luna):
    /// 1. make sure the bool from javascript has been successfully
    ///    desugared
    /// 2. wouldn't it be nice if there was like, a ForInEntries Expr (or even
    ///    __JNKS.forInEntries) and we could desugar this to loop? because
    ///    doing this to jankyscript and notwasm stuff is going to be annoying
    ForIn(Id, Box<Expr>, Box<Stmt>, Pos),
    Label(Id, Box<Stmt>, Pos),
    Break(Id, Pos),
    Catch(Box<Stmt>, Id, Box<Stmt>, Pos),
    Finally(Box<Stmt>, Box<Stmt>, Pos),
    Throw(Box<Expr>, Pos),
    Return(Box<Expr>, Pos),
}
