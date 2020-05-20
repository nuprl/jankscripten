#[derive(Debug)]
pub enum BinOp {
    BinaryOp(resast::BinaryOp),
    LogicalOp(resast::LogicalOp),
}

pub type UnaryOp = resast::UnaryOp;

pub type AssignOp = resast::AssignOp;

#[derive(Debug)]
pub enum UnaryAssignOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialEq)]
pub enum Num {
    Int(i32),
    Float(f64)
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct VarDecl {
    pub name: Id,
    pub named: Box<Expr>,
}

#[derive(Debug)]
pub enum ForInit {
    Expr(Box<Expr>),
    Decl(Vec<VarDecl>),
}

#[derive(Debug)]
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

impl Stmt {
    /// rest recurses / gone the curses
    ///
    /// something i found useful last time when only one kind of statement
    /// is needed, but nested statements might need to be found
    ///
    /// strictly depth-first, ltr
    ///
    /// ```
    /// # use libjankscripten::javascript::{Stmt, Expr};
    /// # let mut stmt = Stmt::Empty;
    /// stmt = stmt.replace_all(&mut |stmt| match stmt {
    ///     Stmt::Break(_) => Stmt::Empty, // ...
    ///     _ => stmt,
    /// }, &mut |expr| match expr {
    ///     Expr::Lit(_) => Expr::This, // or whatever,
    ///     _ => expr
    /// });
    /// ```
    pub fn replace_all<'a, F, G>(mut self, f: &mut F, g: &mut G) -> Self
    where
        F: FnMut(Stmt) -> Stmt,
        G: FnMut(Expr) -> Expr,
    {
        use Stmt::*;
        // visit node
        self = f(self);
        // recurse
        match self {
            // 0
            Empty | Break(_) | Continue(_) => self,
            VarDecl(vds) => VarDecl(
                vds.into_iter()
                    .map(|super::VarDecl { name, named }| super::VarDecl {
                        name,
                        named: named.box_replace(f, g),
                    })
                    .collect(),
            ),
            // 1xStmt
            Label(x, a) => Label(x, a.box_replace(f, g)),
            Func(x, y, a) => Func(x, y, a.box_replace(f, g)),
            // 2xStmt
            Catch(a, x, b) => Catch(a.box_replace(f, g), x, b.box_replace(f, g)),
            Finally(a, b) => Finally(a.box_replace(f, g), b.box_replace(f, g)),
            // 1x[Stmt]
            Block(ss) => Block(ss.into_iter().map(|s| s.replace_all(f, g)).collect()),
            // 1xExpr
            Expr(a) => Expr(a.box_replace(f, g)),
            Throw(a) => Throw(a.box_replace(f, g)),
            Return(a) => Return(a.box_replace(f, g)),
            // 1xExpr, 1xStmt
            While(e, s) => While(e.box_replace(f, g), s.box_replace(f, g)),
            DoWhile(s, e) => DoWhile(s.box_replace(f, g), e.box_replace(f, g)),
            ForIn(x, y, e, s) => ForIn(x, y, e.box_replace(f, g), s.box_replace(f, g)),
            // 1xExpr, 2xStmt
            If(e, sa, sb) => If(
                e.box_replace(f, g),
                sa.box_replace(f, g),
                sb.box_replace(f, g),
            ),
            // 1xExpr, 1xStmt, 1x[(Expr,Stmt)]
            Switch(e, es_ss, s) => Switch(
                e.box_replace(f, g),
                es_ss
                    .into_iter()
                    .map(|(e, s)| (e.replace_all(f, g), s.replace_all(f, g)))
                    .collect(),
                s.box_replace(f, g),
            ),
            // 2xExpr, 1xStmt
            For(x, ea, eb, s) => For(
                x,
                ea.box_replace(f, g),
                eb.box_replace(f, g),
                s.box_replace(f, g),
            ),
        }
    }
    /// should actually be called box_replace_all, just boxes a replace all
    fn box_replace<'a, F, G>(self, f: &mut F, g: &mut G) -> Box<Self>
    where
        F: FnMut(Stmt) -> Stmt,
        G: FnMut(Expr) -> Expr,
    {
        Box::new(self.replace_all(f, g))
    }
    /// like [replace_all] but only worries about statements
    pub(crate) fn replace<'a, F>(self, f: &mut F) -> Self
    where
        F: FnMut(Stmt) -> Stmt,
    {
        self.replace_all(f, &mut |node| node)
    }
}

impl Expr {
    /// like [Stmt::replace_all], but as a method on Expr. does the *exact*
    /// same thing
    pub fn replace_all<'a, F, G>(mut self, f: &mut F, g: &mut G) -> Self
    where
        F: FnMut(Stmt) -> Stmt,
        G: FnMut(Expr) -> Expr,
    {
        use Expr::*;
        self = g(self);
        match self {
            Lit(_) | This | Id(_) => self,
            Array(es) => Array(es.into_iter().map(|e| e.replace_all(f, g)).collect()),
            Object(ks_es) => Object(
                ks_es
                    .into_iter()
                    .map(|(k, e)| (k, e.replace_all(f, g)))
                    .collect(),
            ),
            Dot(e, x) => Dot(e.box_replace(f, g), x),
            Bracket(ea, eb) => Bracket(ea.box_replace(f, g), eb.box_replace(f, g)),
            New(e, es) => New(
                e.box_replace(f, g),
                es.into_iter().map(|e| e.replace_all(f, g)).collect(),
            ),
            Unary(x, e) => Unary(x, e.box_replace(f, g)),
            Binary(x, ea, eb) => Binary(x, ea.box_replace(f, g), eb.box_replace(f, g)),
            UnaryAssign(x, lv) => UnaryAssign(x, Box::new(lv.replace_all(f, g))),
            If(ea, eb, ec) => If(
                ea.box_replace(f, g),
                eb.box_replace(f, g),
                ec.box_replace(f, g),
            ),
            Assign(x, lv, e) => Assign(x, Box::new(lv.replace_all(f, g)), e.box_replace(f, g)),
            Call(ea, es) => Call(
                ea.box_replace(f, g),
                es.into_iter().map(|e| e.replace_all(f, g)).collect(),
            ),
            Func(x, y, s) => Expr::Func(x, y, s.box_replace(f, g)),
            Seq(es) => Seq(es.into_iter().map(|e| e.replace_all(f, g)).collect()),
        }
    }
    /// should actually be called box_replace_all, just boxes a replace all
    fn box_replace<'a, F, G>(self, f: &mut F, g: &mut G) -> Box<Self>
    where
        F: FnMut(Stmt) -> Stmt,
        G: FnMut(Expr) -> Expr,
    {
        Box::new(self.replace_all(f, g))
    }
    /// like [replace_all] but only replacing Exprs
    pub(crate) fn replace<'a, G>(self, g: &mut G) -> Self
    where
        G: FnMut(Expr) -> Expr,
    {
        self.replace_all(&mut |node| node, g)
    }
}

impl LValue {
    /// like [Stmt::replace_all], but as a method on LValue. does the *exact*
    /// same thing
    fn replace_all<'a, F, G>(self, f: &mut F, g: &mut G) -> Self
    where
        F: FnMut(Stmt) -> Stmt,
        G: FnMut(Expr) -> Expr,
    {
        use LValue::*;
        match self {
            Id(_) => self,
            Dot(e, x) => Dot(e.replace_all(f, g), x),
            Bracket(ea, eb) => Bracket(ea.replace_all(f, g), eb.replace_all(f, g)),
        }
    }
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
