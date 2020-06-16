use super::syntax::*;
use super::*;
use super::constructors::*;

// Naming the result of all function applications 
struct NameFunctionCalls <'a> { ng: &'a mut NameGen }

impl Visitor for NameFunctionCalls<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Call(_fid, _args) => {
                match loc {
                    Loc::Node(Context::RValue, _) => {
                        // already being named, so no worries
                    },
                    _ => {
                        let block_ctx = loc.enclosing_block().expect("Block context expected");
                        let name = self.ng.fresh("f_call");
                        block_ctx.insert(block_ctx.index, vardecl1_(name.clone(), expr.clone()));
                        *expr = id_(name);
                    }
                }
            }
            _ => {
                //not a call, proceed as usual
            }
        }
    }
}

pub fn simpl(program: &mut Stmt, namegen: &mut NameGen) {
    let mut v = NameFunctionCalls {ng: namegen};
    program.walk(&mut v);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::*;

    #[test]
    fn name_call_fancyupdate() {
        let prog =r#"
            function f(arg) {
                return arg;
            }
            var x = 1;
            x += f(1);
        "#;
        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_dot() {
        let prog =r#"
            var obj = { x: 1 }
            function f() {
                return obj;
            }
            f().x += 1;
        "#;
        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_nested() {
        let prog = r#"
            function f(arg) {
                return arg + 1;
            }
            function g() {
                return 5;
            }
            console.log(f(g()));
        "#;

        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_decl() {
        let prog = r#"
            function f() {
                return 1;
            }
            var x = f();
            x
        "#;

        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_decl2() {
        let prog = r#"
            function f(arg) {
                return arg+1;
            }
            function g() {
                return 5;
            }
            var x = f(g());
            x
        "#;

        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_simpleupdate() {
        let prog = r#"
            function f() {
                return 1;
            }
            var x = 2;
            x = f();
        "#;

        desugar_okay(prog, simpl);
    }
}