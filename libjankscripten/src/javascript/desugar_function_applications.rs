use super::constructors::*;
use super::syntax::*;
use super::*;

// Naming the result of all function applications
struct NameFunctionCalls<'a> {
    ng: &'a mut NameGen,
}

impl Visitor for NameFunctionCalls<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            &mut Expr::Call(_, _, s) => {
                match loc {
                    Loc::Node(Context::VarDeclRhs, _) => {
                        // already being named, so no worries
                    }
                    Loc::Node(Context::AssignRhs(AssignOp::Equal), _) => {
                        // already being named, so no worries
                    }
                    _ => {
                        let block_ctx = loc.enclosing_block().expect("Block context expected");
                        let name = self.ng.fresh("f_call");
                        block_ctx.insert(block_ctx.index, vardecl1_(name.clone(), expr.clone(), s));
                        *expr = id_(name, s);
                    }
                }
            }
            _ => {
                //not a call, proceed as usual
            }
        }
    }
}

pub fn desugar_function_applications(program: &mut Stmt, namegen: &mut NameGen) {
    let mut v = NameFunctionCalls { ng: namegen };
    program.walk(&mut v);
}
