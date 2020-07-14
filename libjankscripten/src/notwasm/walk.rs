//! walk the statements / now you've made it

use super::syntax::*;

/// a visitor is passed to [Stmt::walk] to describe what happens when walking
///
/// each method has a default implementation of doing nothing, so you
/// only have to specify what you need
pub trait Visitor {
    /// called before recursing on a statement
    fn enter_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called before recursing on an expression
    fn enter_expr(&mut self, _expr: &mut Expr, _loc: &mut Loc) {}
    /// called before recursing on an atom
    fn enter_atom(&mut self, _atom: &mut Atom, _loc: &mut Loc) {}
    /// called after recursing on a statement, with the new value
    fn exit_stmt(&mut self, _stmt: &mut Stmt) {}
    /// called after recursing on an expression, with the new value
    fn exit_expr(&mut self, _expr: &mut Expr, _loc: &mut Loc) {}
    /// called after recursing on an atom, with the new value
    fn exit_atom(&mut self, _atom: &mut Atom, _loc: &mut Loc) {}
}

pub struct VisitorState<'v, V> {
    visitor: &'v mut V,
}

#[derive(Debug)]
pub struct BlockContext {
    pub index: usize,
    pub len: usize,
    patches: Vec<(usize, Stmt)>,
}

impl BlockContext {
    pub fn new(index: usize, len: usize) -> Self {
        BlockContext {
            index,
            len,
            patches: vec![],
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
    pub fn insert(&mut self, index: usize, stmt: Stmt) {
        assert!(index <= self.len);
        self.patches.push((index, stmt));
    }

    fn apply_patches(mut self, block: &mut Vec<Stmt>) {
        // but we want patches to be applied in order they were added. because
        // we add them in reverse order we want to iterate in reverse order
        // Vec::sort_by maintains diff=0 values to be in their original order
        // (stable), but we want the opposite behavior. to do this we sort in
        // ascending order and reverse the whole iterator
        self.patches.sort_by(|(m, _), (n, _)| m.cmp(n));
        // .rev() reverses the reverse-sorted iterator now
        for (index, stmt) in self.patches.drain(0..).rev() {
            // Inserting shifts all elements to the right. However, the
            // indices are in descending order.
            block.insert(index, stmt);
        }
    }
}

#[derive(Debug)]
pub enum Context<'a> {
    // Additional contexts can go here.
    Block(&'a mut BlockContext),
}

/// A data structure that represents the context of a call to a visitor.
/// This is closely related to the `location` of a zipper:
///
/// Gérard Huet. The Zipper. Journal of Functional Programming. 7(5) 1997.
#[derive(Debug)]
pub enum Loc<'a> {
    Top,
    Node(Context<'a>, &'a Loc<'a>),
}

impl<'v, V> VisitorState<'v, V>
where
    V: Visitor,
{
    pub fn new(visitor: &'v mut V) -> Self {
        VisitorState { visitor }
    }

    pub fn walk_stmt(&mut self, stmt: &mut Stmt, loc: &mut Loc) {
        use Stmt::*;
        self.visitor.enter_stmt(stmt);
        // recurse
        match stmt {
            // 0
            Empty | Break(..) | Goto(..) | Trap => (),
            // 1xStmt
            Label(.., a) | Loop(a) => self.walk_stmt(a, loc),
            // 1x[Stmt]
            Block(ss) => {
                let mut block_cxt = BlockContext::new(0, ss.len());
                for (index, s) in ss.iter_mut().enumerate() {
                    block_cxt.index = index;
                    let mut loc = Loc::Node(Context::Block(&mut block_cxt), loc);
                    self.walk_stmt(s, &mut loc);
                }
                block_cxt.apply_patches(ss);
            }
            Var(var_stmt) => {
                self.walk_expr(&mut var_stmt.named, loc);
            }
            // 1xExpr
            Expression(a) | Assign(.., a) | Store(.., a) => self.walk_expr(a, loc),
            // 1xAtom
            Return(a) => self.walk_atom(a, loc),
            // 1xExpr, 2xStmt
            If(e, sa, sb) => {
                self.walk_atom(e, loc);
                self.walk_stmt(sa, loc);
                self.walk_stmt(sb, loc);
            }
        }
        self.visitor.exit_stmt(stmt);
    }

    pub fn walk_expr(&mut self, expr: &mut Expr, loc: &mut Loc) {
        use Expr::*;
        self.visitor.enter_expr(expr, loc);
        match expr {
            ObjectEmpty | HT | Array | Call(..) => (),
            HTSet(ea, eb, ec, ..) | ObjectSet(ea, eb, ec, ..) => {
                self.walk_atom(ea, loc);
                self.walk_atom(eb, loc);
                self.walk_atom(ec, loc);
            }
            Push(ea, eb, ..) => {
                self.walk_atom(ea, loc);
                self.walk_atom(eb, loc);
            }
            ToString(a) | NewRef(a) | Atom(a, ..) => self.walk_atom(a, loc),
        }
        self.visitor.exit_expr(expr, loc);
    }

    pub fn walk_atom(&mut self, atom: &mut Atom, loc: &mut Loc) {
        use Atom::*;
        self.visitor.enter_atom(atom, loc);
        match atom {
            // 0
            Lit(..) | Id(..) | Deref(..) => (),
            ToAny(to_any) => {
                self.walk_atom(to_any.atom.as_mut(), loc);
            }
            StringLen(ea) | ArrayLen(ea, ..) | Unary(.., ea) | FromAny(ea, ..) => {
                self.walk_atom(ea, loc);
            }
            HTGet(ea, eb, ..) | ObjectGet(ea, eb, ..) | Binary(.., ea, eb) | Index(ea, eb, ..) => {
                self.walk_atom(ea, loc);
                self.walk_atom(eb, loc);
            }
        }
        self.visitor.exit_atom(atom, loc);
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
        let mut loc = Loc::Top;
        vs.walk_expr(self, &mut loc);
    }
}

impl Atom {
    /// like [Stmt::walk], but as a method on Atom
    pub fn walk(&mut self, v: &mut impl Visitor) {
        let mut vs = VisitorState::new(v);
        let mut loc = Loc::Top;
        vs.walk_atom(self, &mut loc);
    }
    /// replace this atom with `false` and return its old
    /// value. this is used to gain ownership of a mutable reference,
    /// especially in [Atom::walk]
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Atom::Lit(Lit::Bool(false)))
    }
}

impl Program {
    /// like [Stmt::walk], but as a method on Program
    pub fn walk(&mut self, v: &mut impl Visitor) {
        for func in self.functions.values_mut() {
            func.body.walk(v);
        }
        for global in self.globals.values_mut() {
            global.atom.walk(v);
        }
    }
}
