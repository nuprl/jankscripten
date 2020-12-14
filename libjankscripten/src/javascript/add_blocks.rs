//! in control structure, one statement is likely to turn into two or more
//! as we desugar. those statements need to be contained in one block, so
//! we wrap every single-statement member of a control structure in a block

use super::syntax::*;
use super::*;

struct AddBlocks;

impl Visitor for AddBlocks {
    fn exit_stmt(&mut self, stmt: &mut Stmt, _loc: &Loc) {
        match stmt {
            Stmt::If(_, a, b, s) | Stmt::Catch(a, _, b, s) | Stmt::Finally(a, b, s) => {
                ensure_block(a, *s);
                ensure_block(b, *s);
            }
            // Switch has been desugared
            Stmt::ForIn(_, _, _, a, s) | Stmt::While(_, a, s) | Stmt::Label(_, a, s) => {
                ensure_block(a, *s);
            }
            // DoWhile, For have been desugared
            // Func has been desugared
            // Block is, well, a block
            // the others don't hold statements
            _ => (),
        }
    }
}

// in theory we would be able to match on every statement to pull out
// the span, but i simply don't want to. instead, we'll grab the span from the
// parent and call it good enough
fn ensure_block(stmt: &mut Stmt, span: Span) {
    match stmt {
        Stmt::Block(..) => (),
        _ => *stmt = Stmt::Block(vec![stmt.take()], span),
    }
}

pub fn add_blocks(program: &mut Stmt) {
    let mut v = AddBlocks;
    program.walk(&mut v);
}
