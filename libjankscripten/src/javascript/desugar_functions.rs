use super::syntax::*;
use super::*;
use super::constructors::*;

// Naming the result of all function applications 
struct NameFunctionCalls <'a> { ng: &'a mut NameGen }

// Naming all functions using var 
struct VarFunctions <'a> { ng : &'a mut NameGen }

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
            _ => {}
        }
    }
}

impl Visitor for VarFunctions<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Func(Some(id), args, stmt) => {
                match loc {
                    Loc::Node(Context::RValue, _) => {
                        // already in a var
                        // note: this case unsettles me. not sure abt its validity 
                    },
                    _ => {
                        let block_ctx = loc.enclosing_block().expect("Block context expected");
                        block_ctx.insert(0, vardecl1_(id.clone(), expr_func_ (None::<Id>, args.clone(), stmt.take())));
                        *expr = id_(id.clone());
                    }
                }
            },
            Expr::Func(None, _args, _stmt) => {
                match loc {
                    Loc::Node(Context::RValue, _) => {
                        // already in a var
                    },
                    _ => {
                        let block_ctx = loc.enclosing_block().expect("Block context expected");
                        let id = self.ng.fresh("f");
                        block_ctx.insert(0, vardecl1_(id.clone(), expr.clone()));
                        *expr = id_(id);
                    }
                }
            },
            _ => {}
        }
    }

    fn exit_stmt(&mut self, stmt: &mut Stmt) { // tfw no loc :(
        match stmt {
            Stmt::Func(_id, _args, _f_stmt) => {
                // match loc {
                //     Loc::Node(Context::RValue, _) => {
                //         // already in a var
                //         // again, i don't think this is possible, but who knows with js tbh
                //     },
                //     _ => {
                //         let block_ctx = loc.enclosing_block().expect("Block context expected");
                //         block_ctx.insert(0, vardecl1_(id.clone(), expr_func_::<Id, T> (None, args.clone(), f_stmt.take())));
                        
                //         stmt.take(); // no need for the declaration anymore
                //     }
                // }
            },
            _ => {}
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
        //desugar_okay gets mad when no changes are made? 
        //using assert_eq! for now
        let mut prog = parse(r#"
            function f() {
                return 1;
            }
            var x = f();
        "#).unwrap();
        let result = parse(r#"
            function f() {
                return 1;
            }
            var x = f();
        "#).unwrap();

        let mut ng = NameGen::default();
        simpl(&mut prog, &mut ng);
        assert_eq!(prog, result);
    }

    #[test]
    fn name_call_decl2() {
        let mut prog = r#"
            function f(arg) {
                return arg+1;
            }
            function g() {
                return 5;
            }
            var x = f(g());
        "#;

        desugar_okay(prog, simpl);
    }

    #[test]
    fn name_call_simpleupdate() {
        let mut prog = parse(r#"
            function f() {
                return 1;
            }
            var x = 2;
            x = f();
        "#).unwrap();
        let result = parse(r#"
            function f() {
                return 1;
            }
            var x = 2;
            x = f();
        "#).unwrap();

        let mut ng = NameGen::default();
        simpl(&mut prog, &mut ng);
        assert_eq!(prog, result);
    }


}