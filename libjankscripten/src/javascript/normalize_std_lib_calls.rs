//! it would be really weird if you made a function called parseInt, so i think
//! it's fair to say that if you call a function called parseInt you're calling the
//! library function global.parseInt. the trouble is, parseInt is one function that
//! is very commonly called with the wrong number of arguments. so, we normalize
//! its using using these assumptions hereuse super::constructors::*;
use super::constructors::*;
use super::syntax::*;
use super::*;

struct NormalizeStdLibCalls;

impl Visitor for NormalizeStdLibCalls {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Call(f, args, s) => {
                if let Expr::Id(Id::Named(id), _) = &**f {
                    if &*id == "parseInt" {
                        match args.len() {
                            // default radix of 10
                            1 => args.push(int_(10, s.clone())),
                            // perfect already
                            2 => (),
                            got => panic!("why was parseInt given {} arguments", got),
                        }
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
