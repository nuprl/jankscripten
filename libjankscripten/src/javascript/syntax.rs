#[derive(Debug, PartialEq)]
pub enum BinOp {
    BinaryOp(resast::BinaryOp),
    LogicalOp(resast::LogicalOp),
}

pub type UnaryOp = resast::UnaryOp;

pub type AssignOp = resast::AssignOp;

#[derive(Debug, PartialEq)]
pub enum UnaryAssignOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialEq)]
pub enum Num {
    Int(i32),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    String(String),
    Regex(String, String), // TODO(arjun): The Regex is not properly parsed
    Bool(bool),
    Null,
    Num(Num),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Key {
    Int(i32),
    Str(String),
}

pub type Id = String;

#[derive(Debug, PartialEq)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Array(Vec<Expr>),
    Object(Vec<(Key, Expr)>),
    This,
    Id(String),
    Dot(Box<Expr>, Id),
    Bracket(Box<Expr>, Box<Expr>),
    New(Box<Expr>, Vec<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    UnaryAssign(UnaryAssignOp, Box<LValue>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Assign(AssignOp, Box<LValue>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Func(Option<Id>, Vec<Id>, Box<Stmt>),
    Seq(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: Id,
    pub named: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum ForInit {
    Expr(Box<Expr>),
    Decl(Vec<VarDecl>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Empty,
    Expr(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Switch(Box<Expr>, Vec<(Expr, Stmt)>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    For(ForInit, Box<Expr>, Box<Expr>, Box<Stmt>),
    /// true = declare variable, false = assign to existing
    ForIn(bool, Id, Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Option<Id>),
    Continue(Option<Id>),
    Catch(Box<Stmt>, Id, Box<Stmt>),
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    VarDecl(Vec<VarDecl>),
    Func(Id, Vec<Id>, Box<Stmt>),
    Return(Box<Expr>),
}

pub(crate) mod cons {
    use super::Stmt::{self, *};
    use super::{BinOp, Expr, ForInit, VarDecl};
    pub(crate) fn for_(init: ForInit, cond: Expr, advance: Expr, body: Stmt) -> Stmt {
        For(init, Box::new(cond), Box::new(advance), Box::new(body))
    }
    pub(crate) fn throw_(e: Expr) -> Stmt {
        Stmt::Throw(Box::new(e))
    }
    pub(crate) fn vardecl1_<T: Into<String>>(name: T, val: Expr) -> Vec<VarDecl> {
        vec![VarDecl {
            name: name.into(),
            named: Box::new(val),
        }]
    }
    pub(crate) fn dot_<T: Into<String>>(exp: Expr, field: T) -> Expr {
        Expr::Dot(Box::new(exp), field.into())
    }
    pub(crate) fn binary_(op: BinOp, a: Expr, b: Expr) -> Expr {
        Expr::Binary(op, Box::new(a), Box::new(b))
    }
    pub(crate) fn id_<T: Into<String>>(id: T) -> Expr {
        Expr::Id(id.into())
    }
}
