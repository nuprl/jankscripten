//! A visitor for JavaScript ASTs.

use super::syntax::*;
use std::cell::RefCell;

/// Statements, expressions, and other types of AST nodes have a `walk` method
/// that receives an implementation of this `Visitor` trait.
///
/// Each method in `Visitor` has a default implementation that does nothing, so
/// you only need to define the methods that you need.
///
/// To build a visitor, you need to define a type `T` that holds the visitor
/// state, and then write `impl Visitor for T { ... }`. Note that you need
/// to define a new type *for stateless visitors too*. For example:
/// 
/// ```
/// use libjankscripten::javascript::Visitor;
/// struct T;
/// impl Visitor for T { };
/// ```
pub trait Visitor {
    /// called before recursing on a statement
    fn enter_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called before recursing on an expression
    fn enter_expr(&mut self, _expr: &mut Expr, _loc: &Loc) {}
    /// called after recursing on a statement, with the new value
    fn exit_stmt(&mut self, _stmt: &mut Stmt, _loc: &Loc) {}
    /// called after recursing on an expression, with the new value
    fn exit_expr(&mut self, _expr: &mut Expr, _loc: &Loc) {}
}

struct VisitorState<'v, V> {
    visitor: &'v mut V,
}

#[derive(Debug)]
pub struct BlockContext {
    pub index: usize,
    pub len: usize,
    patches: RefCell<Vec<(usize, Stmt)>>,
}

impl BlockContext {
    pub fn new(index: usize, len: usize) -> Self {
        BlockContext {
            index,
            len,
            patches: RefCell::new(vec![]),
        }
    }

    /// Insert `stmt` at position `index` into the block. The position 0 is
    /// before the first element, and the last position is after the last
    /// element. E.g., if the block has statements `[s0, s1, s2]` then the
    /// indices are `[0, s0, 1, s1, 2, s2, 3]`.
    ///
    /// Note that you can insert multiple statements at the same index.
    /// They will appear in the order they were added.
    ///
    /// Panics if the index is invalid.
    pub fn insert(&self, index: usize, stmt: Stmt) {
        assert!(index <= self.len);
        self.patches.borrow_mut().push((index, stmt));
    }

    fn apply_patches(mut self, block: &mut Vec<Stmt>) {
        // but we want patches to be applied in order they were added. because
        // we add them in reverse order we want to iterate in reverse order
        // Vec::sort_by maintains diff=0 values to be in their original order
        // (stable), but we want the opposite behavior. to do this we sort in
        // ascending order and reverse the whole iterator
        let patches = self.patches.get_mut();
        patches.sort_by(|(m, _), (n, _)| m.cmp(n));
        // .rev() reverses the reverse-sorted iterator now
        for (index, stmt) in patches.drain(0..).rev() {
            // Inserting shifts all elements to the right. However, the
            // indices are in descending order.
            block.insert(index, stmt);
        }
    }
}

/// A single-level of a context.
#[derive(Debug)]
pub enum Context<'a> {
    /// Within a block statement. The `BlockContext` type has several methods
    /// that allow the visitor to add statements to the block.
    Block(&'a BlockContext),
    VarDeclRhs,
    /// Within the context of a statement of an unknown kind.
    Stmt,
    /// Within the right-hand side of an assignment expression. The context
    /// includes the assignment operator.
    AssignRhs(AssignOp),
    /// Within the context of an expression of an unknown kind.
    Expr,
    /// Within the left-hand side of an assignment expression.
    LValue,
    // Within a function body
    FunctionBody,
}

/// A data structure that represents the context of a call to a visitor.
///
/// For example, in a call to `enter_stmt(&mut self, stmt, loc)`, if `loc`
/// is `Loc::Node(Block(...), ...)`, then `stmt` is immediately within a a
/// block statement.
/// 
/// See the `Context` type for the various kinds of contexts available.
/// TODO(arjun): Now that this is immutable, we might as well use a `Vec`.
#[derive(Debug)]
pub enum Loc<'a> {
    Top,
    Node(Context<'a>, &'a Loc<'a>),
}

impl<'a> Loc<'a> {

    pub fn enclosing_block(&self) -> Option<&'a BlockContext> {
        match self {
            Loc::Top => None,
            Loc::Node(Context::Block(cxt), _) => Some(cxt),
            Loc::Node(_, rest) => rest.enclosing_block()
        }
    }

    /// Returns the `BlockContext` of the enclosing function. If the statement is at the top-level,
    /// returns the top-level `BlockContext`.
    pub fn body_of_enclosing_function_or_program(&self) -> &'a BlockContext {
        match self {
            Loc::Top => panic!("the top-level program is not a block"),
            Loc::Node(Context::Block(cxt), rest) => match *rest {
                Loc::Top => cxt,
                Loc::Node(Context::FunctionBody, _) => {
                    cxt
                },
                Loc::Node(_, rest) => rest.body_of_enclosing_function_or_program()
            }
            Loc::Node(_, rest) => rest.body_of_enclosing_function_or_program()
        }
    }

}
impl<'v, V> VisitorState<'v, V>
where
    V: Visitor,
{
    pub fn new(visitor: &'v mut V) -> Self {
        VisitorState { visitor }
    }

    pub fn walk_stmt(&mut self, stmt: &mut Stmt, loc: &Loc) {
        use Stmt::*;
        self.visitor.enter_stmt(stmt);
        // recurse
        match stmt {
            // 0
            Empty | Break(_) | Continue(_) => (),
            Label(.., a) => {
                let loc = Loc::Node(Context::Stmt, loc);
                self.walk_stmt(a, &loc);
            }
            Func(.., a) => {
                let loc = Loc::Node(Context::FunctionBody, loc);
                self.walk_stmt(a, &loc);
            }
            // 2xStmt
            Finally(a, b) | Catch(a, .., b) => {
                let loc = Loc::Node(Context::Stmt, loc);
                self.walk_stmt(a, &loc);
                self.walk_stmt(b, &loc);
            }
            // 1x[Stmt]
            Block(ss) => {
                let mut block_cxt = BlockContext::new(0, ss.len());
                for (index, s) in ss.iter_mut().enumerate() {
                    block_cxt.index = index;
                    let loc = Loc::Node(Context::Block(&block_cxt), loc);
                    self.walk_stmt(s, &loc);
                }
                block_cxt.apply_patches(ss);
            }
            // 1x{ .., Stmt }
            VarDecl(vds) => {
                for super::VarDecl { name: _, named } in vds {
                    let loc = Loc::Node(Context::VarDeclRhs, loc);
                    self.walk_expr(named, &loc);
                }
            }
            // 1xExpr
            Throw(a) | Return(a) | Expr(a) => {
                let loc = Loc::Node(Context::Stmt, loc);
                self.walk_expr(a, &loc);
            }
            // 1xExpr, 1xStmt
            DoWhile(s, e) | ForIn(.., e, s) | While(e, s) => {
                let loc = Loc::Node(Context::Stmt, loc);
                self.walk_expr(e, &loc);
                self.walk_stmt(s, &loc);
            }
            // 1xExpr, 2xStmt
            If(e, sa, sb) => {
                let loc = Loc::Node(Context::Stmt, loc);
                self.walk_expr(e, &loc);
                self.walk_stmt(sa, &loc);
                self.walk_stmt(sb, &loc);
            }
            // 1xExpr, 1xStmt, 1x[(Expr,Stmt)]
            Switch(e, es_ss, s) => {
                let loc = Loc::Node(Context::Stmt, &loc);
                self.walk_expr(e, &loc);
                es_ss.iter_mut().for_each(|(e, s)| {
                    self.walk_expr(e, &loc);
                    self.walk_stmt(s, &loc);
                });
                self.walk_stmt(s, &loc);
            }
            // 2xExpr, 1xStmt
            For(_, ea, eb, s) => {
                let loc = Loc::Node(Context::Stmt, &loc);
                self.walk_expr(ea, &loc);
                self.walk_expr(eb, &loc);
                self.walk_stmt(s, &loc);
            }
        }
        self.visitor.exit_stmt(stmt, &loc);
    }

    pub fn walk_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        use Expr::*;
        self.visitor.enter_expr(expr, loc);
        match expr {
            // 0
            Lit(_) | This | Id(_) => (),
            // 1xLValue
            UnaryAssign(.., lv) => {
                let loc = Loc::Node(Context::Expr, loc);
                self.walk_lval(lv, &loc);
            }
            Func(.., s) => {
                let loc = Loc::Node(Context::FunctionBody, loc);
                self.walk_stmt(s, &loc);
            }
            // 1x[Expr]
            Array(es) | Seq(es) => {
                let loc = Loc::Node(Context::Expr, loc);
                for e in es {
                    self.walk_expr(e, &loc);
                }
            }
            // 1x[(_, Expr)]
            Object(ks_es) => {
                let loc = Loc::Node(Context::Expr, loc);
                for (_, e) in ks_es {
                    self.walk_expr(e, &loc);
                }
            }
            // 1xExpr
            Dot(e, ..) | Unary(.., e) => {
                let loc = Loc::Node(Context::Expr, loc);
                self.walk_expr(e, &loc);
            }
            // 1xExpr, 1xLValue
            Assign(op, lv, e) => {
                let lv_loc = Loc::Node(Context::LValue, loc);
                self.walk_lval(lv, &lv_loc);
                let rv_loc = Loc::Node(Context::AssignRhs(*op), loc);
                self.walk_expr(e, &rv_loc);
            }
            // 1xExpr, 1x[Expr]
            New(e, es) | Call(e, es) => {
                let loc = Loc::Node(Context::Expr, loc);
                self.walk_expr(e, &loc);
                for e in es {
                    self.walk_expr(e, &loc);
                }
            }
            // 2xExpr
            Bracket(ea, eb) | Binary(.., ea, eb) => {
                let loc = Loc::Node(Context::Expr, loc);
                self.walk_expr(ea, &loc);
                self.walk_expr(eb, &loc);
            }
            // 3xExpr
            If(ea, eb, ec) => {
                let loc = Loc::Node(Context::Expr, loc);
                self.walk_expr(ea, &loc);
                self.walk_expr(eb, &loc);
                self.walk_expr(ec, &loc);
            }
        }
        self.visitor.exit_expr(expr, loc);
    }

    /// like [Stmt::walk], but as a method on LValue. does the *exact*
    /// same thing
    pub fn walk_lval(&mut self, lval: &mut LValue, loc: &Loc) {
        use LValue::*;
        match lval {
            Id(_) => (),
            Dot(e, ..) => {
                let loc = Loc::Node(Context::LValue, loc);
                self.walk_expr(e, &loc);
            }
            Bracket(ea, eb) => {
                let loc = Loc::Node(Context::LValue, loc);
                self.walk_expr(ea, &loc);
                self.walk_expr(eb, &loc);
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
        let mut loc = Loc::Top;
        vs.walk_stmt(self, &mut loc);
    }

    /// Replace this statement with `;` and return its old value.
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Stmt::Empty)
    }
}

impl Expr {
    /// like `Stmt::walk`, but as a method on Expr. does the *exact*
    /// same thing.
    pub fn walk(&mut self, v: &mut impl Visitor) {
        let mut vs = VisitorState::new(v);
        let mut loc = Loc::Top;
        vs.walk_expr(self, &mut loc);
    }

    /// Replace this statement with `undefined` and return its old
    /// value.
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Expr::Lit(Lit::Undefined))
    }
}
