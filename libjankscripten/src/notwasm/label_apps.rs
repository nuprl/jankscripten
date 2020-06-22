//! Label all function applications with a label variant for elim_gotos
//!
//! Postconditions:
//! - running index_labels on this might(?) mess it up

use super::constructors::*;
use super::syntax::*;
use super::walk::*;

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
            Assign(_, Expr::CallDirect(..))
            | Assign(_, Expr::CallIndirect(..))
            | Var(_, Expr::CallDirect(..), _)
            | Var(_, Expr::CallIndirect(..), _) => {
                *stmt = label_(super::syntax::Label::App(self.n), stmt.take());
                self.n += 1;
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
                var x: i32 = aCall();
                x = callTwo();
            }
            "#,
        );
        label_apps(&mut program);
        let expected_body = Stmt::Block(vec![
            label_(
                Label::App(0),
                Stmt::Var(id_("x"), Expr::CallDirect(id_("aCall"), vec![]), Type::I32),
            ),
            label_(
                Label::App(1),
                Stmt::Assign(id_("x"), Expr::CallDirect(id_("callTwo"), vec![])),
            ),
        ]);
        let expected = test_program_(expected_body);
        assert_eq!(program, expected);
    }
}
