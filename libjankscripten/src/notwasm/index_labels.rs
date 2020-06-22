//! Indexes
//!

use super::syntax::*;
use super::walk::*;
use std::collections::HashMap;

type IndexEnv = HashMap<Label, u32>;

/// this turns all breaks's labels into "stack depth counts"
pub fn index_labels(program: &mut Program) {
    program.walk(&mut LabelVisitor::default());
}

#[derive(Default)]
struct LabelVisitor {
    /// index = depth - rev_index - 1
    rev_indexes: IndexEnv,
    depth: usize,
}
impl Visitor for LabelVisitor {
    fn enter_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Loop(..) | If(..) => self.depth += 1,
            Label(id, ..) => {
                self.rev_indexes.insert(id.clone(), self.depth as u32);
                self.depth += 1;
            }
            Break(id) => {
                let rev_index = self.rev_indexes.get(id).expect("break no label");
                let index = self.depth as u32 - rev_index - 1;
                *id = super::syntax::Label::Indexed(index);
            }
            _ => (),
        }
    }

    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Label(id, ..) => {
                self.depth -= 1;
                assert_eq!(self.rev_indexes.remove(id), Some(self.depth as u32));
            }
            Loop(..) | If(..) => {
                self.depth -= 1;
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::constructors::*;
    use super::super::syntax::*;
    use super::index_labels;
    #[test]
    fn test_index_labels() {
        let body = label_(
            "a",
            label_(
                "b",
                Stmt::Block(vec![Stmt::Break("a".into()), Stmt::Break("b".into())]),
            ),
        );
        let mut program = test_program_(body);
        index_labels(&mut program);
        let body = label_(
            "a",
            label_(
                "b",
                Stmt::Block(vec![
                    Stmt::Break(Label::Indexed(1)),
                    Stmt::Break(Label::Indexed(0)),
                ]),
            ),
        );
        let expected = test_program_(body);
        assert_eq!(program, expected);
    }
}
