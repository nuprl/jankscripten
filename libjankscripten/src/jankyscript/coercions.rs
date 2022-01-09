//! Replace Coercion::meta with concrete coercions

use super::syntax::*;
use super::walk::*;

/// Replace Coercion::meta with concrete coercions
pub fn reify_coercions(program: &mut Stmt) {
    let mut v = CoercionVisitor {};
    program.walk(&mut v);
}

struct CoercionVisitor {}
impl Visitor for CoercionVisitor {
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Coercion(coercion, _, p) => {
                if let Coercion::Meta(src, dst) = coercion {
                    *coercion = Coercion::new(src.take(), dst.take(), p.clone());
                }
            }
            _ => (),
        }
    }
}
