//! The JankierScript language

use crate::rts_function::RTSFunction;
pub use crate::shared::coercions::Coercion;
pub use crate::shared::types::Type;
use im_rc::HashSet as ImmHashSet;

pub type Id = super::super::javascript::Id;
pub type Lit = super::super::javascript::Lit;
pub type Num = super::super::javascript::Num;
pub type Key = super::super::javascript::Key;

pub type BinaryOp = super::super::notwasm::syntax::BinaryOp;
pub type UnaryOp = super::super::notwasm::syntax::UnaryOp;

impl BinaryOp {
    pub fn janky_typ(self: &BinaryOp) -> (Type, Type) {
        use super::super::notwasm::syntax::BinaryOp::*;
        match self {
            PtrEq => (Type::Any, Type::Bool), // for completeness; should be special-cased
            I32Eq | I32Ne | I32GT | I32LT | I32Ge | I32Le => (Type::Int, Type::Bool),
            F64Eq | F64Ne | F64LT | F64GT | F64Ge | F64Le => (Type::Float, Type::Bool),
            I32Add | I32Sub | I32Mul | I32Div | I32Rem | I32And | I32Or | I32Shl | I32Shr => {
                (Type::Int, Type::Int)
            }
            F64Add | F64Sub | F64Mul | F64Div => (Type::Float, Type::Float),
        }
    }
}

impl UnaryOp {
    pub fn janky_typ(self: &UnaryOp) -> (Type, Type) {
        match self {
            UnaryOp::Sqrt => (Type::Float, Type::Float),
            UnaryOp::Neg => (Type::Float, Type::Float),
            UnaryOp::Eqz => (Type::Bool, Type::Bool),
        }
    }
}

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

#[derive(Debug)]
pub struct Func {
    pub result_typ: Type,
    pub args_with_typs: Vec<(Id, Type)>,
    pub body: Box<Stmt>,
    pub free_vars: ImmHashSet<Id>,
    // NOTE(arjun): E..g, add variables assigned to here, in fv.rs
}

impl Func {
    pub fn new(args_with_typs: Vec<(Id, Type)>, result_typ: Type, body: Stmt) -> Self {
        let body = Box::new(body);
        let free_vars = ImmHashSet::new();
        Func {
            args_with_typs,
            result_typ,
            body,
            free_vars,
        }
    }

    pub fn arg_typs(&self) -> impl Iterator<Item = &Type> {
        self.args_with_typs.iter().map(|(_, t)| t)
    }

    pub fn arg_names(&self) -> impl Iterator<Item = &Id> {
        self.args_with_typs.iter().map(|(x, _)| x)
    }
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Array(Vec<Expr>),
    Object(Vec<(Key, Expr)>),
    Id(Id),
    Dot(Box<Expr>, Id),
    Bracket(Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Assign(Box<LValue>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    PrimCall(RTSFunction, Vec<Expr>),
    Func(Func),
    Coercion(Coercion, Box<Expr>),
    /// Create a new heap-allocated box, with contents of type T.
    NewRef(Box<Type>, Box<Expr>),
    /// Read from a heap-allocated box
    Deref(Box<Expr>),
    /// Update the contents of a heap-allocated box
    Store(Id, Box<Expr>),
}

#[derive(Debug)]
pub enum Stmt {
    Var(Id, Type, Box<Expr>),
    Block(Vec<Stmt>),
    Empty,
    Expr(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Loop(Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Id),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    Return(Box<Expr>),
}
