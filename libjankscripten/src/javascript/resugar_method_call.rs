use super::syntax::*;
use super::*;

struct ResugarMethodCall;

impl Visitor for ResugarMethodCall {
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Call(f, args, _) => match &mut **f {
                Expr::Dot(obj, name, s) => {
                    let id = if let Expr::Id(obj_name, _) = &**obj {
                        obj_name.clone()
                    } else {
                        panic!("Desugar should have named method objects")
                    };
                    let name = if let Id::Named(field) = name {
                        std::mem::replace(field, Default::default())
                    } else {
                        panic!("Dot should't access special ids")
                    };
                    let args = std::mem::replace(args, vec![]);
                    *expr = Expr::MethodCall(id, name, args, s.clone());
                }
                _ => (),
            },
            _ => (),
        }
    }
}

pub fn resugar_method_call(program: &mut Stmt) {
    let mut v = ResugarMethodCall;
    program.walk(&mut v);
}
