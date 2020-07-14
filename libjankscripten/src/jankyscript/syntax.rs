//! The JankierScript language

use crate::shared::types::Type;
use crate::shared::ops::*;

pub type Id = super::super::javascript::Id;
pub type Lit = super::super::javascript::Lit;
pub type Key = super::super::javascript::Key;

#[derive(Debug)]
/// Coercion : S -> T
/// 
/// Every coercion turns into a call to a function in the runtime system.
pub enum Coercion {
    /// Coercion::Tag(t) : t -> Any
    /// Note that not all values may be injected in Any.
    Tag(Type),
    /// Coercion::Untag(t) : Any -> t
    /// Note that untagging will fail if the Any-typed value contains an element
    /// that does not have the type t.
    Untag(Type),
    /// Coercion::Fun(args, ret)
    /// 
    /// Assume exactly one argument:
    /// 
    /// Coercion::Fun([arg], ret) where arg : S -> S' and ret : T -> T'
    /// Coercion::Fun([arg], ret) : (S' -> T) -> (S -> T')
    Fun(Vec<Coercion>, Box<Coercion>),
    // Coercion::Id(t) : t -> t
    Id(Type),
    /// Coercion::Seq(t2, t1) where t1 : S -> U and t2 : U -> T
    /// has the type S -> Ts
    Seq(Box<Coercion>, Box<Coercion>)
}

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
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    New(Type, Box<Expr>, Vec<Expr>),
    Func(Type, Vec<(Id, Type)>, Box<Stmt>),
    Coercion(Coercion, Box<Expr>)
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Type, Expr),
    Block(Vec<Stmt>),
    Empty,
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
