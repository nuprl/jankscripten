//! A simplified AST for JavaScript.
//!
//! This AST supports most of ECMAScript-262, 3rd edition, but excludes some
//! annoying features, such as `with`.

pub use crate::shared::Id;
pub use crate::shared::Span;
pub use swc_common::DUMMY_SP;

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    BinaryOp(BinaryOp),
    LogicalOp(LogicalOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Minus,
    Plus,
    Not,
    Tilde,
    TypeOf,
    Void,
    Delete,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
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
    In,
    InstanceOf,
    PowerOf,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AssignOp {
    Equal,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    ModEqual,
    LeftShiftEqual,
    RightShiftEqual,
    UnsignedRightShiftEqual,
    OrEqual,
    XOrEqual,
    AndEqual,
    PowerOfEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryAssignOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Lit(Lit, Span),
    Array(Vec<Expr>, Span),
    Object(Vec<(Key, Expr)>, Span),
    This,
    Id(Id, Span),
    Dot(Box<Expr>, Id, Span),
    Bracket(Box<Expr>, Box<Expr>, Span),
    New(Box<Expr>, Vec<Expr>, Span),
    Unary(UnaryOp, Box<Expr>, Span),
    Binary(BinOp, Box<Expr>, Box<Expr>, Span),
    UnaryAssign(UnaryAssignOp, Box<LValue>, Span),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    Assign(AssignOp, Box<LValue>, Box<Expr>, Span),
    Call(Box<Expr>, Vec<Expr>, Span),
    Func(Option<Id>, Vec<Id>, Box<Stmt>, Span),
    Seq(Vec<Expr>, Span),
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
    Block(Vec<Stmt>, Span),
    Empty,
    Expr(Box<Expr>, Span),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>, Span),
    Switch(Box<Expr>, Vec<(Expr, Stmt)>, Box<Stmt>, Span),
    While(Box<Expr>, Box<Stmt>, Span),
    DoWhile(Box<Stmt>, Box<Expr>, Span),
    For(ForInit, Box<Expr>, Box<Expr>, Box<Stmt>, Span),
    /// `ForIn(true, x, .., Span)` indicates `for (var x ...`.
    /// `ForIn(false, x, .., Span)` indicates `for (x ...`.
    ForIn(bool, Id, Box<Expr>, Box<Stmt>, Span),
    Label(Id, Box<Stmt>, Span),
    Break(Option<Id>, Span),
    Continue(Option<Id>, Span),
    /// `Catch(body, x, catch_block, Span)` is
    /// `try { body } catch(x, Span) { catch_block }
    Catch(Box<Stmt>, Id, Box<Stmt>, Span),
    // `Finally(body, finally_block, Span)`
    // `try { body } finally { finally_block }`
    Finally(Box<Stmt>, Box<Stmt>, Span),
    Throw(Box<Expr>, Span),
    /// Could be:
    /// `var x = 10, y = 30;`
    VarDecl(Vec<VarDecl>, Span),
    Func(Id, Vec<Id>, Box<Stmt>, Span),
    Return(Box<Expr>, Span),
}

impl Expr {
    /// Produces `true` if the expression can be freely copied without altering the order of
    /// effects or invalidating object identities.
    pub fn is_essentially_atom(&self) -> bool {
        match self {
            Expr::Lit(_, _) => true,
            Expr::This => true,
            Expr::Id(_, _) => true,
            Expr::Dot(e, _, _) => e.is_essentially_atom(),
            Expr::Bracket(e1, e2, _) => e1.is_essentially_atom() && e2.is_essentially_atom(),
            // Copying an object, array, or function will alter their identity.
            Expr::Array(_, _) => false,
            Expr::Object(_, _) => false,
            Expr::Func(..) => false,
            _ => false,
        }
    }
}

impl LValue {
    pub fn is_essentially_atom(&self) -> bool {
        match self {
            LValue::Id(_) => true,
            LValue::Dot(e, _) => e.is_essentially_atom(),
            LValue::Bracket(e1, e2) => e1.is_essentially_atom() && e2.is_essentially_atom(),
        }
    }

    pub fn take(&mut self) -> Self {
        std::mem::replace(
            self,
            LValue::Id(Id::Bogus("bogus identifier inserted by LValue::take")),
        )
    }
}

impl UnaryAssignOp {
    pub fn is_prefix(&self) -> bool {
        use UnaryAssignOp::*;
        match self {
            PreDec | PreInc => true,
            PostDec | PostInc => false,
        }
    }

    pub fn binop(&self) -> BinOp {
        use UnaryAssignOp::*;
        match self {
            PostInc | PreInc => BinOp::BinaryOp(BinaryOp::Plus),
            PostDec | PreDec => BinOp::BinaryOp(BinaryOp::Minus),
        }
    }

    pub fn other_binop(&self) -> BinOp {
        use UnaryAssignOp::*;
        match self {
            PostInc | PreInc => BinOp::BinaryOp(BinaryOp::Minus),
            PostDec | PreDec => BinOp::BinaryOp(BinaryOp::Plus),
        }
    }
}
