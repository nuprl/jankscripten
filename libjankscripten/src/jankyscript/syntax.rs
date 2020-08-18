//! The JankierScript language

use crate::rts_function::RTSFunction;
pub use crate::shared::coercions::Coercion;
pub use crate::shared::types::Type;

pub type Id = super::super::javascript::Id;
pub type Lit = super::super::javascript::Lit;
pub type Num = super::super::javascript::Num;
pub type Key = super::super::javascript::Key;

pub type BinaryOp = super::super::notwasm::syntax::BinaryOp;
pub type UnaryOp = super::super::notwasm::syntax::UnaryOp;

impl BinaryOp {
    pub fn janky_typ(self: &BinaryOp) -> (Type, Type) {
        match self {
            BinaryOp::PtrEq => (Type::Any, Type::Bool), // for completeness; should be special-cased
            BinaryOp::I32Eq
            | BinaryOp::I32GT
            | BinaryOp::I32LT
            | BinaryOp::I32Ge
            | BinaryOp::I32Le => (Type::Int, Type::Bool),
            BinaryOp::F64Eq | BinaryOp::F64LT => (Type::Float, Type::Bool),
            BinaryOp::I32Add
            | BinaryOp::I32Sub
            | BinaryOp::I32Mul
            | BinaryOp::I32Div
            | BinaryOp::I32Rem
            | BinaryOp::I32And
            | BinaryOp::I32Or => (Type::Int, Type::Int),
            BinaryOp::F64Add | BinaryOp::F64Sub | BinaryOp::F64Mul | BinaryOp::F64Div => {
                (Type::Float, Type::Float)
            }
        }
    }
}

impl UnaryOp {
    pub fn janky_typ(self: &UnaryOp) -> (Type, Type) {
        match self {
            UnaryOp::Sqrt => (Type::Float, Type::Float),
            UnaryOp::Neg => (Type::Float, Type::Float),
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
    New(Box<Expr>, Vec<Expr>),
    Func(Type, Vec<(Id, Type)>, Box<Stmt>),
    Coercion(Coercion, Box<Expr>),
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
