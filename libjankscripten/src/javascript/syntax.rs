//! A simplified AST for JavaScript.
//! 
//! This AST supports most of ECMAScript-262, 3rd edition, but excludes some
//! annoying features, such as `with`.

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    BinaryOp(resast::BinaryOp),
    LogicalOp(resast::LogicalOp),
}

pub type UnaryOp = resast::UnaryOp;

pub type AssignOp = resast::AssignOp;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryAssignOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    Int(i32),
    Float(f64),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Id {
    Named(String),
    Generated(&'static str, usize),
}

impl Id {
    // TODO(arjun): We should eliminate this method. It is only used in the parser, and a cleaner
    // approach would be to have a parser combinator that returns a String instead of an Id.
    pub fn into_name(self) -> String {
        match self {
            Id::Named(s) => s,
            _ => panic!("into_name on Id::Generated"),
        }
    }
}

impl<T: Into<String>> From<T> for Id {
    fn from(i: T) -> Self {
        Id::Named(i.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LValue {
    Id(Id),
    Dot(Expr, Id),
    Bracket(Expr, Expr),
}

impl<T: Into<Id>> From<T> for LValue {
    fn from(i: T) -> Self {
        LValue::Id(i.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Lit),
    Array(Vec<Expr>),
    Object(Vec<(Key, Expr)>),
    This,
    Id(Id),
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

#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl {
    pub name: Id,
    pub named: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForInit {
    Expr(Box<Expr>),
    Decl(Vec<VarDecl>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Empty,
    Expr(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Switch(Box<Expr>, Vec<(Expr, Stmt)>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    For(ForInit, Box<Expr>, Box<Expr>, Box<Stmt>),
    /// `ForIn(true, x, ..)` indicates `for (var x ...`.
    /// `ForIn(true, x, ..)` indicates `for (x ...`.
    ForIn(bool, Id, Box<Expr>, Box<Stmt>),
    Label(Id, Box<Stmt>),
    Break(Option<Id>),
    Continue(Option<Id>),
    /// `Catch(body, x, catch_block)` is
    /// `try { body } catch(x) { catch_block }
    Catch(Box<Stmt>, Id, Box<Stmt>),
    // `Finally(body, finally_block)`
    // `try { body } finally { finally_block }`
    Finally(Box<Stmt>, Box<Stmt>),
    Throw(Box<Expr>),
    /// Could be:
    /// `var x = 10, y = 30;`
    VarDecl(Vec<VarDecl>),
    Func(Id, Vec<Id>, Box<Stmt>),
    Return(Box<Expr>),
}
