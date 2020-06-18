//! The JankyScript language definition

use crate::shared::types::Type;
use crate::shared::coercions::Coercion;

#[derive(Debug)]
pub enum BinOp {
    Plus(Type, Type),
    // TODO: others
}

#[derive(Debug)]
pub enum UnaryOp {
    Increment(Type),
    // TODO: others
}

#[derive(Debug)]
pub enum AssignOp {
    PlusEqual(Type, Type),
    // TODO: others
}

#[derive(Debug)]
pub enum Lit {
    String(String),
    Regex(String, String),
    Bool(bool),
    Null,
    Num(String),
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
    Var(Id, Type, Expr),
    Block(Vec<Stmt>),
    Empty,
    Assign(AssignOp, Box<LValue>, Box<Expr>),
    Call(Id, Box<Expr>, Vec<Expr>),
    New(Id, Type, Box<Expr>, Vec<Expr>),
    Func(Id, Type, Vec<(Id, Type)>, Box<Stmt>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
    Coercion(Coercion, Box<Expr>)
}
