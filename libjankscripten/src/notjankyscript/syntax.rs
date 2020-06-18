//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.

use crate::shared::types::Type;

#[derive(Debug)]
pub enum BinOp {
    Plus(Type, Type), // initialize as any
    // TODO: others
}

#[derive(Debug)]
pub enum UnaryOp {
    Increment(Type) // initialize as any
    // TODO: others
}

#[derive(Debug)]
pub enum AssignOp {
    PlusEqual(Type, Type)
    // TODO: others
}

#[derive(Debug)]
pub enum Lit {
    String(String),
    Regex(String, String),
    Bool(bool),
    Null,
    Num(String), // TODO(arjun): parse
    Undefined,
}

#[derive(Debug)]
pub enum Key {
    Int(i32),
    Str(String),
}

pub type Id = String;

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
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Option<Type>, Expr), // initialize to None (recommendation is to add default behavior to constructor)
    Block(Vec<Stmt>),
    Empty,
    Assign(AssignOp, Box<LValue>, Box<Expr>),
    Call(Id, Box<Expr>, Vec<Expr>),
    New(Id, Option<Type>, Box<Expr>, Vec<Expr>),
    Func(Id, Option<Type>, Vec<(Id, Option<Type>)>, Box<Stmt>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
}
