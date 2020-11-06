//! change obj["field_name"] to obj.field_name
//!
//! this helps with some particular inferences about whether the container is
//! an object or an array, but not all. must occur after desugar_assign

use super::constructors::*;
use super::syntax::*;
use super::*;

struct BracketToDot;

impl Visitor for BracketToDot {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        match expr {
            Expr::Bracket(container, field, s) => {
                if let Expr::Lit(Lit::String(name), _) = &**field {
                    *expr = dot_(container.take(), Id::Named(name.clone()), *s);
                }
            }
            Expr::Assign(_, lv, ..) => {
                if let LValue::Bracket(container, field) = &mut **lv {
                    if let Expr::Lit(Lit::String(name), _) = field {
                        *lv = Box::new(LValue::Dot(container.take(), Id::Named(name.clone())));
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn desugar_bracket_str(program: &mut Stmt) {
    let mut v = BracketToDot;
    program.walk(&mut v);
}
