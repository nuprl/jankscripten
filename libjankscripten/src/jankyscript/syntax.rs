//! The source (and target) language for type inference.
//!
//! The JavaScript AST goes through several stages of desugaring before we
//! produce a program in this language.

/// Deliberately excludes logical operators.
type BinOp = resast::BinaryOp;

pub type UnaryOp = resast::UnaryOp;

pub type AssignOp = resast::AssignOp;

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
    Var(Id, Expr),
    Block(Vec<Stmt>),
    Empty,
    Assign(AssignOp, Box<LValue>, Box<Expr>),
    Call(Id, Box<Expr>, Vec<Expr>),
    New(Id, Box<Expr>, Vec<Expr>),
    Func(Id, Vec<Id>, Box<Stmt>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
}
