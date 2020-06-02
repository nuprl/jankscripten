//! The abstract syntax for NotWasm, which reflects the wasm representation
//! with garbage collected structures, expressions, and statements
//!
//! From the design document:
//! NotWasm is a lower-level IR than JankyScript, but higher-level than Wasm. It has the following features:
//! - First-order functions
//! - Garbage-collected data structures, including classes, arrays, and hash tables
//! - An environment data structure that can store addresses of local variables.
//! - All local variables are annotated with a flag that determines if they can be stored in an environment.
//! - Unlike WebAssembly, NotWasm does not have an operand stack. Instead, it has compound expressions. We can easily introduce the operand stack later.
//! - What is the type system?
//!   - T ::= i32 | f64 | num | C | ht | bool | AnyClass | (T1, … Tn) -> T
//! A Program is:
//! - A set of classes (just fields, not methods, all public)
//! - A set of functions
//! - A set of global variables that are initialized to …whataever Wasm supports

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub classes: HashMap<Id, Class>,
    pub functions: HashMap<Id, Function>,
    // Expr must be const as defined by wasm
    pub globals: HashMap<Id, Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    Var(Id, Expr),
    If(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    // TODO: indexes for labels too
    Label(Id, Box<Stmt>),
    Break(Id),
    // Break value as return?
    Return(Expr),
    Block(Vec<Stmt>),
    Expr(Expr),
    Assign(Id, Expr),
    Call(Id, Id, Vec<Id>, Type),
    New(Id, Id, Vec<Id>, Type),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(Lit, Type),
    //Array(Vec<Expr>, Type),
    //HT(Vec<(Key, Expr, Type)>, Type),
    // HTGet / HTSet / ClassGet / etc VS Dot / Bracket
    // TODO: classes
    Id(Id, Type),
    // only negative float is unary and in JS
    //Unary(UnaryOp, Box<Expr>, Type),
    Binary(BinaryOp, Box<Expr>, Box<Expr>, Type),
}

impl Expr {
    pub fn get_type(&self) -> Type {
        use Expr::*;
        match self {
            Lit(.., ty) | /*Array(.., ty) | HT(.., ty) | */Id(.., ty) | Binary(.., ty) => ty.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32,
    F64,
    Num,
    Class,
    HT,
    Bool,
    AnyClass,
    Fn(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Id {
    Named(String),
    Index(u32),
    // functions are indexed separately than locals
    Func(u32),
    // labels are indexed by depth above statement
    Label(u32),
}

// TODO
#[derive(Debug, PartialEq)]
pub struct Class;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub locals: Vec<Type>,
    pub body: Stmt,
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    F64(f64),
}

/// binary ops needed for JS and defined by wasm (everything else should
/// probably be desugared(?))
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    LeftShift,
    RightShift,
    UnsignedRightShift,
    Plus,
    Minus,
    Times,
    Over,
    Mod,
    Or,
    XOr,
    And,
}

#[derive(Debug, PartialEq)]
pub enum Key {
    I32(i32),
    // TODO?
    //Str(String),
}
