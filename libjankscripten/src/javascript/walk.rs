//! walk the statements / now you've made it

use super::syntax::*;

/// a visitor is passed to [Stmt::walk] to describe what happens when walking
///
/// each method has a default implementation of doing nothing, so you only have to specify what you need. the way to make a Visitor to pass to [Stmt::walk] is to define a newtype:
///
/// ```
/// use libjankscripten::javascript::Visitor;
/// struct WalkOn;
/// impl Visitor for WalkOn {
///     // ...
/// }
/// ```
pub trait Visitor {
    /// called before recursing on a statement
    fn enter_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called before recursing on an expression
    fn enter_expr(&mut self, _expr: &mut Expr) {}
}

impl Stmt {
    /// walk the ast, calling relevant visitor methods when appropriate
    ///
    /// strictly depth-first, ltr. see [Visitor] for more info
    ///
    /// ```
    /// # use libjankscripten::javascript::{Stmt, Expr};
    /// # let mut stmt = Stmt::Empty;
    /// use libjankscripten::javascript::Visitor;
    /// struct EmptyToBlock;
    /// impl Visitor for EmptyToBlock {
    ///     fn enter_stmt(&mut self, stmt: &mut Stmt) {
    ///         match stmt {
    ///             Stmt::Empty => {
    ///                 let old = stmt.take();
    ///                 *stmt = Stmt::Block(vec![]);
    ///             }
    ///             _ => (),
    ///         }
    ///     }
    /// }
    /// ```
    pub fn walk(&mut self, v: &mut impl Visitor) {
        use Stmt::*;
        v.enter_stmt(self);
        // recurse
        match self {
            // 0
            Empty | Break(_) | Continue(_) => (),
            // 1xStmt
            Label(.., a) | Func(.., a) => a.walk(v),
            // 2xStmt
            Finally(a, b) | Catch(a, .., b) => {
                a.walk(v);
                b.walk(v);
            }
            // 1x[Stmt]
            Block(ss) => ss.iter_mut().for_each(|s| s.walk(v)),
            // 1x{ .., Stmt }
            VarDecl(vds) => {
                for super::VarDecl { name: _, named } in vds {
                    named.walk(v);
                }
            }
            // 1xExpr
            Throw(a) | Return(a) | Expr(a) => a.walk(v),
            // 1xExpr, 1xStmt
            DoWhile(s, e) | ForIn(.., e, s) | While(e, s) => {
                e.walk(v);
                s.walk(v);
            }
            // 1xExpr, 2xStmt
            If(e, sa, sb) => {
                e.walk(v);
                sa.walk(v);
                sb.walk(v);
            }
            // 1xExpr, 1xStmt, 1x[(Expr,Stmt)]
            Switch(e, es_ss, s) => {
                e.walk(v);
                es_ss.iter_mut().for_each(|(e, s)| {
                    e.walk(v);
                    s.walk(v)
                });
                s.walk(v);
            }
            // 2xExpr, 1xStmt
            For(_, ea, eb, s) => {
                ea.walk(v);
                eb.walk(v);
                s.walk(v);
            }
        }
    }
    /// replace this statement with `;` and return its old value. this is
    /// used to gain ownership of a mutable reference, especially in [Stmt::walk]
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Stmt::Empty)
    }
}

impl Expr {
    /// like [Stmt::walk], but as a method on Expr. does the *exact*
    /// same thing
    pub fn walk(&mut self, v: &mut impl Visitor) {
        use Expr::*;
        v.enter_expr(self);
        match self {
            // 0
            Lit(_) | This | Id(_) => (),
            // 1xLValue
            UnaryAssign(.., lv) => lv.walk(v),
            // 1xStmt
            Func(.., s) => s.walk(v),
            // 1x[Expr]
            Array(es) | Seq(es) => {
                for e in es {
                    e.walk(v);
                }
            }
            // 1x[(_, Expr)]
            Object(ks_es) => {
                for (_, e) in ks_es {
                    e.walk(v);
                }
            }
            // 1xExpr
            Dot(e, ..) | Unary(.., e) => e.walk(v),
            // 1xExpr, 1xLValue
            Assign(.., lv, e) => {
                lv.walk(v);
                e.walk(v);
            }
            // 1xExpr, 1x[Expr]
            New(e, es) | Call(e, es) => {
                e.walk(v);
                for e in es {
                    e.walk(v);
                }
            }
            // 2xExpr
            Bracket(ea, eb) | Binary(.., ea, eb) => {
                ea.walk(v);
                eb.walk(v);
            }
            // 3xExpr
            If(ea, eb, ec) => {
                ea.walk(v);
                eb.walk(v);
                ec.walk(v);
            }
        }
    }
    /// replace this statement with `undefined` and return its old
    /// value. this is used to gain ownership of a mutable reference,
    /// especially in [Expr::walk]
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Expr::Lit(Lit::Undefined))
    }
}

impl LValue {
    /// like [Stmt::walk], but as a method on LValue. does the *exact*
    /// same thing
    fn walk(&mut self, v: &mut impl Visitor) {
        use LValue::*;
        match self {
            Id(_) => (),
            Dot(e, ..) => e.walk(v),
            Bracket(ea, eb) => {
                ea.walk(v);
                eb.walk(v);
            }
        }
    }
}
