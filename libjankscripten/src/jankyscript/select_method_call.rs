//! Turn (typed) method calls into relevant typed calls

use super::constructors::*;
use super::methods::get_type_by_prefix;
use super::syntax::*;
use super::walk::*;
use crate::rts_function::RTSFunction;
use crate::typ;

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
            Expr::MethodCall(obj, method, args, typ, p) => match typ {
                Type::Any => (),
                Type::DynObject => {
                    *expr = Expr::Call(
                        Box::new(Expr::Coercion(
                            Coercion::Meta(Type::Any, get_type_by_prefix(method, args.len(), typ)),
                            Box::new(dot_(obj.take(), method.as_str(), p.clone())),
                            p.clone(),
                        )),
                        std::mem::replace(args, vec![]),
                        p.clone(),
                    );
                }
                _ => {
                    *expr = Expr::PrimCall(
                        RTSFunction::Method(
                            method.clone(),
                            get_type_by_prefix(method, args.len(), typ),
                        ),
                        std::mem::replace(args, vec![]),
                        p.clone(),
                    );
                }
            },
            Expr::Length(obj, typ, p) => match typ {
                Type::Any => (),
                Type::DynObject => *expr = dot_(obj.take(), "length", p.clone()),
                _ => {
                    *expr = Expr::PrimCall(
                        RTSFunction::Method(
                            "length".to_string(),
                            typ!(fun((unquote typ.take())) -> int),
                        ),
                        vec![obj.take()],
                        p.clone(),
                    );
                }
            },
            _ => (),
        }
    }
}
