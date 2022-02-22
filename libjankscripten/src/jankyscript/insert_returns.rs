//! NOTE: Exprs CAN contain Stmts which may contain return,
//! but only within functions, in which the return is irrelevant. So the only
//! reason to collect Exprs is to collect functions to do the transformation on

use super::{
    constructors::return_,
    syntax::*,
    walk::{Loc, Visitor},
};
use crate::javascript::syntax::Lit::Undefined;

/// Returns true when the statement (which may be a block) is guaranteed to
/// return OR THROW at some point in control flow
fn returns(stmt: &Stmt) -> bool {
    use Stmt::*;
    match stmt {
        Var(..) | Empty | Break(..) | Expr(..) => false,
        Block(stmts, _) => stmts.iter().any(returns),
        If(_, true_part, false_part, _) => returns(true_part) && returns(false_part),
        // This is conservative. We could analyze the conditions and breaks etc...
        Loop(..) | ForIn(..) => false,
        // This is conservative. We could analyze if/when the body throws,
        // but instead we assume the body may or may not throw at any time, so no
        // return can be guaranteed to be reachable
        Catch(..) => false,
        // On the other hand, finally blocks always run, so this is the same as Block
        // I'm not sure what the body of Finally is, it's a try-catch right? So
        // returns(body) should always be false when we're being conservative?
        Finally(body, finally_body, _) => {
            assert!(!returns(body));
            returns(finally_body)
        }
        // We're being conservative yet again! If this label is broken to
        // before a return, then this is false, otherwise it could be true
        Label(..) => false,
        // If this throw was within a Catch, it wouldn't have been recurred
        // on. So this will be caught by a Catch outside of the current function,
        // meaning there is no need to insert a return
        Throw(..) => true,
        Return(..) => true,
    }
}

struct InsertReturns;

impl Visitor for InsertReturns {
    fn exit_fn(&mut self, func: &mut Func, _: &Loc) {
        if !returns(&func.body) {
            if let Stmt::Block(body, p) = &mut *func.body {
                body.push(return_(Expr::Lit(Undefined, p.clone()), p.clone()));
            } else {
                panic!("A function with a non-block body. That's fine, just rewrite this to not assume.");
            }
        }
    }
}

/// Inserts return undefined at the end of every function that we can't verify
/// will return or throw
pub fn insert_returns(program: &mut Stmt) {
    program.walk(&mut InsertReturns);
}
