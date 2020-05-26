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
    /// called after recursing on a statement, with the new value
    fn exit_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called after recursing on an expression, with the new value
    fn exit_expr(&mut self, _expr: &mut Expr) {}
}

struct VisitorState<'a, V> {
    visitor: &'a mut V
}

impl<'a, V> VisitorState<'a, V> where V : Visitor {

    fn new(visitor: &'a mut V) -> Self {
        return VisitorState { visitor };
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        self.visitor.enter_stmt(stmt);
        // recurse
        match stmt {
            // 0
            Empty | Break(_) | Continue(_) => (),
            // 1xStmt
            Label(.., a) | Func(.., a) => self.walk_stmt(a),
            // 2xStmt
            Finally(a, b) | Catch(a, .., b) => {
                self.walk_stmt(a);
                self.walk_stmt(b);
            }
            // 1x[Stmt]
            Block(ss) => ss.iter_mut().for_each(|s| self.walk_stmt(s)),
            // 1x{ .., Stmt }
            VarDecl(vds) => {
                for super::VarDecl { name: _, named } in vds {
                    self.walk_expr(named);
                }
            }
            // 1xExpr
            Throw(a) | Return(a) | Expr(a) => self.walk_expr(a),
            // 1xExpr, 1xStmt
            DoWhile(s, e) | ForIn(.., e, s) | While(e, s) => {
                self.walk_expr(e);
                self.walk_stmt(s);
            }
            // 1xExpr, 2xStmt
            If(e, sa, sb) => {
                self.walk_expr(e);
                self.walk_stmt(sa);
                self.walk_stmt(sb);
            }
            // 1xExpr, 1xStmt, 1x[(Expr,Stmt)]
            Switch(e, es_ss, s) => {
                self.walk_expr(e);
                es_ss.iter_mut().for_each(|(e, s)| {
                    self.walk_expr(e);
                    self.walk_stmt(s);
                });
                self.walk_stmt(s);
            }
            // 2xExpr, 1xStmt
            For(_, ea, eb, s) => {
                self.walk_expr(ea);
                self.walk_expr(eb);
                self.walk_stmt(s);
            }
        }
        self.visitor.exit_stmt(stmt);
    }

    pub fn walk_expr(&mut self, expr: &mut Expr) {
        use Expr::*;
        self.visitor.enter_expr(expr);
        match expr {
            // 0
            Lit(_) | This | Id(_) => (),
            // 1xLValue
            UnaryAssign(.., lv) => self.walk_lval(lv),
            // 1xStmt
            Func(.., s) => self.walk_stmt(s),
            // 1x[Expr]
            Array(es) | Seq(es) => {
                for e in es {
                    self.walk_expr(e);
                }
            }
            // 1x[(_, Expr)]
            Object(ks_es) => {
                for (_, e) in ks_es {
                    self.walk_expr(e);
                }
            }
            // 1xExpr
            Dot(e, ..) | Unary(.., e) => self.walk_expr(e),
            // 1xExpr, 1xLValue
            Assign(.., lv, e) => {
                self.walk_lval(lv);
                self.walk_expr(e);
            }
            // 1xExpr, 1x[Expr]
            New(e, es) | Call(e, es) => {
                self.walk_expr(e);
                for e in es {
                    self.walk_expr(e);
                }
            }
            // 2xExpr
            Bracket(ea, eb) | Binary(.., ea, eb) => {
                self.walk_expr(ea);
                self.walk_expr(eb);
            }
            // 3xExpr
            If(ea, eb, ec) => {
                self.walk_expr(ea);
                self.walk_expr(eb);
                self.walk_expr(ec);
            }
        }
        self.visitor.exit_expr(expr);
    }

    /// like [Stmt::walk], but as a method on LValue. does the *exact*
    /// same thing
    fn walk_lval(&mut self, lval: &mut LValue) {
        use LValue::*;
        match lval {
            Id(_) => (),
            Dot(e, ..) => self.walk_expr(e),
            Bracket(ea, eb) => {
                self.walk_expr(ea);
                self.walk_expr(eb);
            }
        }
    }


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
        let mut vs = VisitorState::new(v);
        vs.walk_stmt(self);
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
        let mut vs = VisitorState::new(v);
        vs.walk_expr(self);
    }
    /// replace this statement with `undefined` and return its old
    /// value. this is used to gain ownership of a mutable reference,
    /// especially in [Expr::walk]
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Expr::Lit(Lit::Undefined))
    }
}
