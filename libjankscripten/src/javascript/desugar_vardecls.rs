use super::constructors::*;
use super::syntax::*;
use super::*;

// Separating variable declarations that take place in the same line
// Ex. var x = 1, y = 2;     =>    var x = 1;  var y = 2;
// note: depends on desugar_loops in case of multiple variables declared in for loop

struct SeparateVarDecls;

impl Visitor for SeparateVarDecls {
    fn exit_stmt(&mut self, stmt: &mut Stmt, loc: &Loc) {
        match stmt {
            Stmt::VarDecl(decls, s) => {
                if decls.len() > 1 {
                    let block_ctx = loc.enclosing_block().expect("Block context expected");
                    // save last decl to replace original statement
                    let last_decl = decls.remove(decls.len() - 1);

                    // insert previous decls in order above stmt
                    for decl in decls.drain(0..) {
                        block_ctx.insert(
                            block_ctx.index,
                            vardecl1_(decl.name, *decl.named, s.clone()),
                        );
                    }

                    *stmt = vardecl1_(last_decl.name, *last_decl.named, s.clone());
                }
            }
            _ => {
                //not a variable declaration, proceed as usual
            }
        }
    }
}

pub fn desugar_vardecls(program: &mut Stmt) {
    let mut v = SeparateVarDecls;
    program.walk(&mut v);
}
