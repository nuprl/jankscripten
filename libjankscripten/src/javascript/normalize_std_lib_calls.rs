//! it would be really weird if you made a function called parseInt, so i think
//! it's fair to say that if you call a function called parseInt you're calling the
//! library function global.parseInt. the trouble is, parseInt is one function that
//! is very commonly called with the wrong number of arguments. so, we normalize
//! its using using these assumptions here. the following normalizations are made:
//!
//! we assume that the `this` argument hasn't been inserted yet when counting
//! arguments
//!
//! parseInt => default radix insertion (10)
//! Error => default message insertion ("")
use super::constructors::*;
use super::syntax::*;
use super::*;

struct NormalizeStdLibCalls;

impl Visitor for NormalizeStdLibCalls {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        match expr {
            // why not do this after `new` desugaring? because then we lose the
            // name of the call because of how we happen to do it
            Expr::Call(f, args, s) | Expr::New(f, args, s) => {
                if let Expr::Id(Id::Named(id), _) = &**f {
                    match &id[..] {
                        "parseInt" => {
                            match args.len() {
                                // default radix of 10
                                1 => args.push(int_(10, s.clone())),
                                // perfect already
                                2 => (),
                                got => panic!("why was parseInt given {} arguments", got),
                            }
                        }
                        "Error" => {
                            match args.len() {
                                // default message of empty
                                0 => args.push(str_("", s.clone())),
                                // perfect already
                                1 => (),
                                got => panic!("why was {} given {} arguments at {}", id, got, s),
                            }
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }
    }
}

pub fn normalize_std_lib_calls(program: &mut Stmt) {
    program.walk(&mut NormalizeStdLibCalls);
}
