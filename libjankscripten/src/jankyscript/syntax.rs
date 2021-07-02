//! The JankyScript language

use super::super::javascript as js;
use super::operators::NotwasmOp;
use crate::pos::Pos;
use crate::rts_function::RTSFunction;
pub use crate::shared::coercions::Coercion;
pub use crate::shared::Type;
use im_rc::HashMap;
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
            I32Add | I32Sub | I32Mul | I32Div | I32Rem | I32And | I32Or | I32Xor | I32Shl
            | I32Shr | I32ShrU => (Type::Int, Type::Int),
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

#[derive(Debug, PartialEq)]
pub enum LValue {
    Id(Id, Type),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub result_typ: Type,
    pub args_with_typs: Vec<(Id, Type)>,
    pub body: Box<Stmt>,
    pub free_vars: HashMap<Id, Type>,
    /// this represents variables which may or may not be defined in this
    /// function that are free (and read) in any child function AND are assigned
    /// at some point in this function or children. these should be boxed
    pub assigned_free_children: ImmHashSet<Id>,
}

impl Func {
    pub fn new(args_with_typs: Vec<(Id, Type)>, result_typ: Type, body: Stmt) -> Self {
        let body = Box::new(body);
        let free_vars = HashMap::new();
        let assigned_free_children = ImmHashSet::new();
        Func {
            args_with_typs,
            result_typ,
            body,
            free_vars,
            assigned_free_children,
        }
    }

    pub fn arg_typs(&self) -> impl Iterator<Item = &Type> {
        self.args_with_typs.iter().map(|(_, t)| t)
    }

    pub fn arg_names(&self) -> impl Iterator<Item = &Id> {
        self.args_with_typs.iter().map(|(x, _)| x)
    }
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Func::new(Vec::new(), Type::Any, Stmt::Empty))
    }
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub enum JsOp {
    Binary(js::BinaryOp),
    Unary(js::UnaryOp),
}

impl From<js::BinaryOp> for JsOp {
    fn from(op: js::BinaryOp) -> JsOp {
        JsOp::Binary(op)
    }
}

impl From<js::UnaryOp> for JsOp {
    fn from(op: js::UnaryOp) -> JsOp {
        JsOp::Unary(op)
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct JsOpTypeinf {
    /// During type inference, holds type-metavariables with the types of each argument.
    pub arg_ts: Vec<Type>,
    /// During type inference, holds a metavariable that resolved to the inferred NotwasmOp.
    pub op_metavar: NotwasmOp,
    pub any_case: Option<crate::z3ez::Bool>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(Lit, Pos),
    Array(Vec<Expr>, Pos),
    Object(Vec<(Key, Expr)>, Pos),
    Id(Id, Type, Pos),
    Dot(Box<Expr>, Id, Pos),
    Bracket(Box<Expr>, Box<Expr>, Pos),
    /// A JavaScript operator.
    JsOp(JsOp, Vec<Expr>, JsOpTypeinf, Pos),
    Unary(UnaryOp, Box<Expr>, Pos),
    Binary(BinaryOp, Box<Expr>, Box<Expr>, Pos),
    Assign(Box<LValue>, Box<Expr>, Pos),
    Call(Box<Expr>, Vec<Expr>, Pos),
    PrimCall(RTSFunction, Vec<Expr>, Pos),
    Func(Func, Pos),
    Closure(Func, Vec<(Expr, Type)>, Pos),
    Coercion(Coercion, Box<Expr>, Pos),
    /// Create a new heap-allocated box, with contents of type T.
    NewRef(Box<Expr>, Type, Pos),
    /// Read from a heap-allocated box
    Deref(Box<Expr>, Type, Pos),
    /// Update the contents of a heap-allocated box.
    /// stores .1 into the location indicated by .0
    Store(Box<Expr>, Box<Expr>, Type, Pos),
    /// the index of the variable
    EnvGet(u32, Type, Pos),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Var(Id, Type, Box<Expr>, Pos),
    Block(Vec<Stmt>, Pos),
    Empty,
    Expr(Box<Expr>, Pos),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>, Pos),
    Loop(Box<Stmt>, Pos),
    ForIn(Id, Box<Expr>, Box<Stmt>, Pos),
    Label(Id, Box<Stmt>, Pos),
    Break(Id, Pos),
    Catch(Box<Stmt>, Id, Box<Stmt>, Pos),
    Finally(Box<Stmt>, Box<Stmt>, Pos),
    Throw(Box<Expr>, Pos),
    Return(Box<Expr>, Pos),
}
