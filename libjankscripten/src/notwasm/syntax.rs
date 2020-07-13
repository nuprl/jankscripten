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

/// The types of NotWam. Every value has a unique type, thus we *do not* support
/// subtyping. The comment for each variant describes the shape of the value
/// with the associated type. Note we refer to several types defined in the
/// runtime system (e.g., `TypeTag`, `Tag`, and `AnyEnum`).
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    /// If `v : I32` then `v` is an `i32`.
    I32,
    /// If `v : F64`, then `v` is an `f64`.
    F64,
    /// If `v : String` then `v` is a `*const Tag` ...
    String,
    /// NOTE(arjun): I think we can (and need) to combine String and StrRef.
    StrRef,
    /// If `v : HT` then `v` is a `*const Tag`, where
    ///  `v.type_tag === TypeTag::HT`.
    HT,
    /// If `v : HT` then `v` is a `*const Tag`, where `v.type_tag == Array`.
    Array,
    /// If `v : Bool` then `v` is an `i32` that is either `1` or `0`.
    Bool,
    /// If `v : AnyClass` then `v` is a `*const Tag` where
    /// `v.type_tag == Class`.
    AnyClass,
    /// If `v : Fn(fn_type)` then `v` is an `i32`, which is an index of a
    /// function with the type `fn_type`.
    Fn(FnType),
    /// If `v : Any` then `v` is an `AnyEnum`.
    Any,
}

/// Binary operators that correspond to primitive WebAssembly instructions.
/// Other than `PtrEq`, none of these operators can be applied to `Any`-typed
/// values.
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    PtrEq,
    I32Eq,
    I32Add,
    I32Sub,
    I32Mul,
    I32GT, // signed
    I32LT, // signed
    I32Ge, // signed
    I32Le, // signed
    I32And,
    I32Or,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
}

/// Unary operators that correspond to primitive WebAssembly instructions.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Sqrt,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub functions: HashMap<Id, Function>,
    /// Atom must be const as defined by wasm
    pub globals: HashMap<Id, Global>,
    /// no need to initialize, populated by intern
    pub data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    // type really only needs to be is_f64 (vs i32)
    Var(Id, Expr, Type),
    Expression(Expr),
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
    /// the from type
    ToAny(Box<Atom>, Type),
    /// the to type
    FromAny(Box<Atom>, Type),
    HTGet(Box<Atom>, Box<Atom>),
    ObjectGet(Box<Atom>, Box<Atom>),
    Index(Box<Atom>, Box<Atom>),
    ArrayLen(Box<Atom>),
    Id(Id),
    StringLen(Box<Atom>),
    Unary(UnaryOp, Box<Atom>),
    Binary(BinaryOp, Box<Atom>, Box<Atom>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    HT,
    Array,
    Push(Atom, Atom),
    HTSet(Atom, Atom, Atom),
    Call(Id, Vec<Id>),
    ObjectEmpty,
    /// ObjectSet(obj, field_name, value, typ) is obj.field_name: typ = value;
    ObjectSet(Atom, Atom, Atom),
    ToString(Atom),
    Atom(Atom),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnType {
    pub args: Vec<Type>,
    pub result: Option<Box<Type>>,
}

impl FnType {
    pub fn to_type(self) -> Type {
        Type::Fn(self)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Id {
    Named(String),
}

impl Id {
    pub fn into_name(self) -> String {
        match self {
            Id::Named(s) => s,
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

#[derive(Debug, PartialEq)]
pub struct Function {
    pub body: Stmt,
    pub fn_type: FnType,
    pub params: Vec<Id>,
}

#[derive(Debug, PartialEq)]
pub struct Global {
    pub is_mut: bool,
    pub ty: Type,
    /// restricted to a const expression.
    /// also, parity_wasm restricts it to one instruction (this is not a
    /// wasm restriction and could theoretically be fixed)
    pub atom: Atom,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    F64(f64),
    String(String),
    Interned(u32),
}

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
                HT => "ht",
                Array => "array",
                Bool => "bool",
                AnyClass => "anyclass",
                Fn(..) => "fn",
                Any => "any",
            }
        )
    }
}
