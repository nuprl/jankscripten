//! Turn (typed) method calls into relevant typed calls

use crate::rts_function::RTSFunction;

use super::constructors::*;
use super::syntax::*;
use super::walk::*;

/// Turn (typed) method calls into relevant typed calls
/// For any, this leaves them be and they become AnyMethodCall in notwasm
/// For object, this turns them into a dot and a call
/// For all other times, this turns them into the relevant PrimCall!
pub fn select_method_call(program: &mut Stmt) {
    let mut v = MethodCallVisitor;
    program.walk(&mut v);
}

struct MethodCallVisitor;
impl Visitor for MethodCallVisitor {
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::MethodCall(obj, method, args, typ @ Type::Function(..), p) => {
                match typ.unwrap_fun().0[0] {
                    Type::Any => (),
                    Type::DynObject => {
                        *expr = Expr::Call(
                            Box::new(Expr::Coercion(
                                Coercion::Meta(Type::Any, typ.take()),
                                Box::new(dot_(obj.take(), method.as_str(), p.clone())),
                                p.clone(),
                            )),
                            std::mem::replace(args, vec![]),
                            p.clone(),
                        );
                    }
                    _ => {
                        *expr = Expr::PrimCall(
                            RTSFunction::Method(method.clone(), typ.take()),
                            std::mem::replace(args, vec![]),
                            p.clone(),
                        );
                    }
                }
            }
            _ => (),
        }
    }
}
