//! The abstract syntax for NotWasm, which reflects the wasm representation
//! with garbage collected structures, expressions, and statements
//!
//! From the design document:
//! NotWasm is a lower-level IR than JankyScript, but higher-level than
//!   Wasm. It has the following features:
//! - First-order functions
//! - Garbage-collected data structures, including classes, arrays, and
//!   hash tables
//! - An environment data structure that can store addresses of local
//!   variables.
//! - All local variables are annotated with a flag that determines if they
//!   can be stored in an environment.
//! - Unlike WebAssembly, NotWasm does not have an operand stack. Instead,
//!   it has compound expressions. We can easily introduce the operand stack later.
//! - What is the type system?
//!   - T ::= i32 | f64 | num | C | ht | bool | AnyClass | (T1, … Tn) -> T
//! A Program is:
//! - A set of classes (just fields, not methods, all public)
//! - A set of functions
//! - A set of global variables that are initialized to …whataever Wasm
//!   supports

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub classes: HashMap<Id, Class>,
    pub functions: HashMap<Id, Function>,
    // Expr must be const as defined by wasm
    pub globals: HashMap<Id, Atom>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    Var(Id, Expr, Type),
    Assign(Id, Expr),
    If(Atom, Box<Stmt>, Box<Stmt>),
    Loop(Box<Stmt>),
    // TODO: indexes for labels too
    Label(Id, Box<Stmt>),
    Break(Id),
    // Break value as return?
    Return(Atom),
    Block(Vec<Stmt>),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Lit(Lit),
    // TODO: String instead of i32
    HTGet(Box<Atom>, Key, Type),
    // HTGet / HTSet / ClassGet / etc VS Dot / Bracket
    // TODO: classes
    Id(Id),
    // only negative float is unary and in JS
    //Unary(UnaryOp, Box<Expr>, Type),
    Binary(BinaryOp, Box<Atom>, Box<Atom>, Type),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    //Array(Vec<Expr>, Type),
    HT(Type),
    // TODO: String instead of i32
    HTSet(Box<Atom>, Key, Box<Atom>, Type),
    Call(Id, Vec<Id>, Vec<Type>, Type),
    //New(Id, Vec<Id>, Type),
    Atom(Atom),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32,
    F64,
    Class,
    HT(Box<Type>),
    Bool,
    AnyClass,
    Fn(Vec<Type>, Box<Type>),
    Any,
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
impl Id {
    pub fn index(&self) -> u32 {
        use Id::*;
        match self {
            Index(i) => *i,
            _ => panic!("non-indexed or non-local id"),
        }
    }
}

// TODO
#[derive(Debug, PartialEq)]
pub struct Class;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub locals: Vec<Type>,
    pub body: Stmt,
    pub ret_ty: Type,
    pub params_tys: Vec<(Id, Type)>,
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    F64(f64),
}

/// only operations not in wasm are:
/// - Equal
/// - NotEqual
/// - In
/// - InstanceOf
/// - PowerOf
pub type BinaryOp = resast::BinaryOp;

//#[derive(Debug, PartialEq)]
/*pub enum Key {
    I32(i32),
    // TODO?
    //Str(String),
}*/
pub type Key = i32;

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        write!(
            f,
            "{}",
            match self {
                I32 => "i32",
                F64 => "f64",
                Class => "class",
                HT(..) => "ht",
                Bool => "bool",
                AnyClass => "anyclass",
                Fn(..) => "fn",
                Any => "any",
            }
        )
    }
}
