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
            Assign(_, Expr::Call(..)) => {
                *stmt = label_(super::syntax::Label::App(self.n), stmt.take());
                self.n += 1;
            }
            Var(var_stmt) => {
                if let Expr::Call(..) = var_stmt.named {
                    *stmt = label_(super::syntax::Label::App(self.n), stmt.take());
                    self.n += 1;
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::parse;
    use super::*;
    #[test]
    fn labels_apps() {
        let mut program = parse(
            r#"
            function main() : i32 {
                var x = aCall();
                x = callTwo();
            }
            "#,
        );
        label_apps(&mut program);
        let expected_body = Stmt::Block(vec![
            label_(
                Label::App(0),
                Stmt::Var(VarStmt::new(id_("x"), Expr::Call(id_("aCall"), vec![]))),
            ),
            label_(
                Label::App(1),
                Stmt::Assign(id_("x"), Expr::Call(id_("callTwo"), vec![])),
            ),
        ]);
        let expected = test_program_(expected_body);
        assert_eq!(program, expected);
    }
}
