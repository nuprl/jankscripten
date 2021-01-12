use super::constructors::*;
use super::syntax::*;
use super::walk::*;

pub fn lift_vars(js: &mut Stmt) {
    let mut v = LiftVars;
    js.walk(&mut v);
}

struct LiftVars;

impl Visitor for LiftVars {
    fn exit_stmt(&mut self, stmt: &mut Stmt, loc: &Loc) {
        match stmt {
            Stmt::VarDecl(decl, s) => {
                let decl1 = decl.pop().expect("no decls in vardecl");
                assert_eq!(decl.pop(), None, "vardecls not desugared");
                let new_decl = vardecl1_(decl1.name.clone(), UNDEFINED_, s.clone());
                loc.body_of_enclosing_function_or_program()
                    .insert(0, new_decl);
                *stmt = expr_(assign_(decl1.name, *decl1.named, s.clone()), s.clone());
            }
            Stmt::ForIn(is_var, bind, container, _, s) if *is_var => {
                let new_decl = vardecl1_(bind.clone(), container.take(), s.clone());
                let block_context = loc.body_of_enclosing_function_or_program();
                block_context.insert(block_context.index, new_decl);
                *is_var = false;
            }
            _ => (),
        }
    }
}
