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
    fn enter_expr(&mut self, _expr: &mut Expr, _loc: &Loc) {}
    /// called after recursing on a statement, with the new value
    fn exit_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called after recursing on an expression, with the new value
    fn exit_expr(&mut self, _expr: &mut Expr, _loc: &Loc) {}
}

struct VisitorState<'v, V> {
    visitor: &'v mut V
}

pub struct BlockContext {
    pub index: usize,
    pub len: usize,
    patches: Vec<(usize, Stmt)>
}

impl BlockContext {
    pub fn new(index: usize, len: usize) -> Self {
        BlockContext { index, len, patches: vec![] }
    }

    /// Insert `stmt` at position `index` into the block. The position 0 is
    /// before the first element, and the last position is after the last
    /// element. E.g., if the block has statements `[s0, s1, s2]` then the
    /// indices are `[0, s0, 1, s1, 2, s2, 3]`.
    /// 
    /// Note that you can insert multiple statements at the same index.
    /// They will appear in some order.
    /// 
    /// TODO(arjun): Figure out what the order is. It should be deterministic,
    /// as long as vector sorting is stable (see `apply_patches`).
    /// 
    /// Panics if the index is invalid.
    pub fn insert(&mut self, index: usize, stmt: Stmt) {
        assert!(index <= self.len);
        self.patches.push((index, stmt));
    }

    fn apply_patches(mut self, block: &mut Vec<Stmt>) {
        // Sort in descending order by index.
        self.patches.sort_by(|(m, _), (n, _)| n.cmp(m));
        for (index, stmt) in self.patches.drain(0..) {
            // Inserting shifts all elements to the right. However, the
            // indices are in descending order.
            block.insert(index, stmt);
        }
    }
}

pub enum Context<'a> {
    // Additional contexts can go here.
    Block(&'a mut BlockContext),
}

/// A data structure that represents the context of a call to a visitor.
/// This is closely related to the `location` of a zipper:
///
/// Gérard Huet. The Zipper. Journal of Functional Programming. 7(5) 1997.
pub enum Loc<'a> {
    Top,
    Node(Context<'a>, &'a Loc<'a>)
}

impl<'v, V> VisitorState<'v, V> where V : Visitor {

    fn new(visitor: &'v mut V) -> Self {
        return VisitorState { visitor };
    }

    fn walk_stmt<'a>(&mut self, stmt: &'a mut Stmt, loc: &'a Loc<'a>) {
        use Stmt::*;
        self.visitor.enter_stmt(stmt);
        // recurse
        match stmt {
            // 0
            Empty | Break(_) | Continue(_) => (),
            // 1xStmt
            Label(.., a) | Func(.., a) => self.walk_stmt(a, loc),
            // 2xStmt
            Finally(a, b) | Catch(a, .., b) => {
                self.walk_stmt(a, loc);
                self.walk_stmt(b, loc);
            }
            // 1x[Stmt]
            Block(ss) => {
                let mut block_cxt = BlockContext::new(0, ss.len());
                for (index, s) in ss.iter_mut().enumerate() {
                    block_cxt.index = index;
                    let loc = Loc::Node(Context::Block(&mut block_cxt), loc);
                    self.walk_stmt(s, &loc);
                }
                block_cxt.apply_patches(ss);
            },
            // 1x{ .., Stmt }
            VarDecl(vds) => {
                for super::VarDecl { name: _, named } in vds {
                    self.walk_expr(named, loc);
                }
            }
            // 1xExpr
            Throw(a) | Return(a) | Expr(a) => self.walk_expr(a, loc),
            // 1xExpr, 1xStmt
            DoWhile(s, e) | ForIn(.., e, s) | While(e, s) => {
                self.walk_expr(e, loc);
                self.walk_stmt(s, loc);
            }
            // 1xExpr, 2xStmt
            If(e, sa, sb) => {
                self.walk_expr(e, loc);
                self.walk_stmt(sa, loc);
                self.walk_stmt(sb, loc);
            }
            // 1xExpr, 1xStmt, 1x[(Expr,Stmt)]
            Switch(e, es_ss, s) => {
                self.walk_expr(e, loc);
                es_ss.iter_mut().for_each(|(e, s)| {
                    self.walk_expr(e, loc);
                    self.walk_stmt(s, loc);
                });
                self.walk_stmt(s, loc);
            }
            // 2xExpr, 1xStmt
            For(_, ea, eb, s) => {
                self.walk_expr(ea, loc);
                self.walk_expr(eb, loc);
                self.walk_stmt(s, loc);
            }
        }
        self.visitor.exit_stmt(stmt);
    }

    pub fn walk_expr<'a>(&mut self, expr: &'a mut Expr, loc: &'a Loc<'a>) {
        use Expr::*;
        self.visitor.enter_expr(expr, loc);
        match expr {
            // 0
            Lit(_) | This | Id(_) => (),
            // 1xLValue
            UnaryAssign(.., lv) => self.walk_lval(lv, loc),
            // 1xStmt
            Func(.., s) => self.walk_stmt(s, loc),
            // 1x[Expr]
            Array(es) | Seq(es) => {
                for e in es {
                    self.walk_expr(e, loc);
                }
            }
            // 1x[(_, Expr)]
            Object(ks_es) => {
                for (_, e) in ks_es {
                    self.walk_expr(e, loc);
                }
            }
            // 1xExpr
            Dot(e, ..) | Unary(.., e) => self.walk_expr(e, loc),
            // 1xExpr, 1xLValue
            Assign(.., lv, e) => {
                self.walk_lval(lv, loc);
                self.walk_expr(e, loc);
            }
            // 1xExpr, 1x[Expr]
            New(e, es) | Call(e, es) => {
                self.walk_expr(e, loc);
                for e in es {
                    self.walk_expr(e, loc);
                }
            }
            // 2xExpr
            Bracket(ea, eb) | Binary(.., ea, eb) => {
                self.walk_expr(ea, loc);
                self.walk_expr(eb, loc);
            }
            // 3xExpr
            If(ea, eb, ec) => {
                self.walk_expr(ea, loc);
                self.walk_expr(eb, loc);
                self.walk_expr(ec, loc);
            }
        }
        self.visitor.exit_expr(expr, loc);
    }

    /// like [Stmt::walk], but as a method on LValue. does the *exact*
    /// same thing
    fn walk_lval<'a>(&mut self, lval: &'a mut LValue, loc: &'a Loc<'a>) {
        use LValue::*;
        match lval {
            Id(_) => (),
            Dot(e, ..) => self.walk_expr(e, loc),
            Bracket(ea, eb) => {
                self.walk_expr(ea, loc);
                self.walk_expr(eb, loc);
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
        let loc = Loc::Top;
        vs.walk_stmt(self, &loc);
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
        let loc = Loc::Top;
        vs.walk_expr(self, &loc);
    }
    /// replace this statement with `undefined` and return its old
    /// value. this is used to gain ownership of a mutable reference,
    /// especially in [Expr::walk]
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Expr::Lit(Lit::Undefined))
    }
}
