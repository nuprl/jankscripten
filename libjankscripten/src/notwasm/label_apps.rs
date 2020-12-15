//! Label all function applications with a label variant for elim_gotos

use super::constructors::*;
use super::syntax::*;
use super::walk::*;

#[allow(unused)]
pub fn label_apps(program: &mut Program) {
    let mut vis = LabelAppsVisitor::default();
    program.walk(&mut vis);
}

#[derive(Default)]
struct LabelAppsVisitor {
    n: i32,
}
impl Visitor for LabelAppsVisitor {
    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            &mut Assign(_, Expr::Call(..), ref s) => {
                let s = s.clone();
                *stmt = label_(super::syntax::Label::App(self.n), stmt.take(), s);
                self.n += 1;
            }
            &mut Var(ref mut var_stmt, ref s) => {
                if let Expr::Call(..) = var_stmt.named {
                    let s = s.clone();
                    *stmt = label_(super::syntax::Label::App(self.n), stmt.take(), s);
                    self.n += 1;
                }
            }
            _ => (),
        }
    }
}
