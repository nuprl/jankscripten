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

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    I32Eq,
    I32Add,
    I32Sub,
    I32Mul,
    I32GT, // signed
    I32Ge, // signed
    I32Le, // signed
    I32And,
    I32Or,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub classes: HashMap<Id, Class>,
    pub functions: HashMap<Id, Function>,
    /// Atom must be const as defined by wasm
    pub globals: HashMap<Id, Atom>,
    /// no need to initialize, populated by intern
    pub data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    // type really only needs to be is_f64 (vs i32)
    Var(Id, Expr, Type),
    Assign(Id, Expr),
    If(Atom, Box<Stmt>, Box<Stmt>),
    Loop(Box<Stmt>),
    Label(Label, Box<Stmt>),
    Break(Label),
    // Break value as return?
    Return(Atom),
    Block(Vec<Stmt>),
    Trap,
    /// these don't exist in NotWasm, only GotoWasm. if you try to [translate]
    /// a goto, it will panic
    Goto(Label),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Lit(Lit),
    HTGet(Box<Atom>, Box<Atom>, Type),
    Index(Box<Atom>, Box<Atom>, Type),
    // HTGet / HTSet / ClassGet / etc VS Dot / Bracket
    // TODO: classes
    Id(Id),
    StringLen(Box<Atom>),
    // only negative float is unary and in JS
    //Unary(UnaryOp, Box<Expr>, Type),
    Binary(BinaryOp, Box<Atom>, Box<Atom>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    HT(Type),
    Array(Type),
    Push(Atom, Atom, Type),
    HTSet(Atom, Atom, Atom, Type),
    CallDirect(Id, Vec<Id>),
    CallIndirect(Id, FnType, Vec<Id>),
    //New(Id, Vec<Id>, Type),
    ToString(Atom),
    Atom(Atom),
}

#[derive(Debug, PartialEq)]
pub struct FnType {
    pub args: Vec<Type>,
    pub result: Option<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32,
    F64,
    String,
    StrRef,
    Class,
    HT(Box<Type>),
    Array(Box<Type>),
    Bool,
    AnyClass,
    Fn(Vec<Type>, Box<Option<Type>>),
    Any,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Id {
    Named(String),
}

impl Id {

    pub fn into_name(self) -> String {
        if let Id::Named(s) = self {
            s
        } else {
            panic!("it's not a name")
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Label {
    Named(String),
    /// in GotoWasm applications are labeled, but they shouldn't make it out
    App(i32),
}
impl<S: Into<String>> From<S> for Label {
    fn from(s: S) -> Self {
        Self::Named(s.into())
    }
}

// TODO
#[derive(Debug, PartialEq)]
pub struct Class;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub body: Stmt,
    pub fn_type: FnType,
    pub params: Vec<Id>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    F64(f64),
    String(String),
    Interned(u32),
}

//#[derive(Debug, PartialEq)]
/*pub enum Key {
    I32(i32),
    // TODO?
    //Str(String),
}*/
// TODO: String instead of i32
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
                String => "string",
                StrRef => "str",
                Class => "class",
                HT(..) => "ht",
                Array(..) => "array",
                Bool => "bool",
                AnyClass => "anyclass",
                Fn(..) => "fn",
                Any => "any",
            }
        )
    }
}
