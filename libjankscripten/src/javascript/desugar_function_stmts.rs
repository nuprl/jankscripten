use super::syntax::*;
use super::walk::*;
use crate::pos::Pos;

pub struct DesugarFunctionStmts {}

/// If `stmt` is a `Stmt::Func`, replace it with empty and return the functino's name, arguments,
/// and body.
fn take_fun_stmt(stmt: &mut Stmt) -> Option<(Id, Vec<Id>, Box<Stmt>, Pos)> {
    if let Stmt::Func(..) = stmt {
        let fun_stmt = stmt.take();
        match fun_stmt {
            Stmt::Func(name, args, body, s) => {
                return Some((name, args, body, s));
            }
            _ => unreachable!(),
        }
    }
    return None;
}

impl Visitor for DesugarFunctionStmts {
    fn exit_stmt(&mut self, stmt: &mut Stmt, loc: &Loc) {
        if let Some((name, args, body, s)) = take_fun_stmt(stmt) {
            let named = Box::new(Expr::Func(None, args, body, s.clone()));
            let block_cxt = loc.body_of_enclosing_function_or_program();
            // Insert `var name = function(args ...) { body ... }` at the top of the block that
            // defines the innermost enclosing function.
            block_cxt.insert(0, Stmt::VarDecl(vec![VarDecl { name, named }], s.clone()));
        }
    }
}
