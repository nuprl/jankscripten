//! Eliminate function-local gotos by checking an inGoto flag
//!
//! Preconditions:
//! - all apps should be labeled ([label_apps])

use super::constructors::*;
use super::syntax::{Label as Lbl, *};
use super::walk::*;

pub fn elim_gotos(program: &mut Program) {
    let mut vis = GotoVisitor::default();
    for func in program.functions.values_mut() {
        func.body.walk(&mut vis);
        // TODO(luna): fresh names??
        func.body = Stmt::Block(vec![
            // var inGoto = false;
            Stmt::Var(id_("inGoto"), atom_(FALSE_), Type::Bool),
            // var gotoTarget = 0;
            Stmt::Var(id_("gotoTarget"), atom_(i32_(0)), Type::I32),
            func.body.take(),
            // if (inGoto) { trap; }
            if_(get_id_("inGoto"), Stmt::Trap, Stmt::Empty),
        ]);
    }
}

#[derive(Default)]
struct GotoVisitor;
impl Visitor for GotoVisitor {
    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Goto(Lbl::App(l)) => {
                *stmt = if_(
                    get_id_("inGoto"),
                    Empty,
                    Block(vec![
                        Assign(id_("inGoto"), atom_(TRUE_)),
                        Assign(id_("gotoTarget"), atom_(i32_(*l as i32))),
                    ]),
                )
            }
            Label(Lbl::App(n), call) if is_call(call) => {
                *stmt = if_(
                    // i think bor/band is fine because these are all
                    // already-computed booleans (expect eq) but it could be
                    // improved if we wanted to hand-lower the or/ands
                    bor_(
                        not_(get_id_("inGoto")),
                        band_(get_id_("inGoto"), eq_(get_id_("gotoTarget"), i32_(*n))),
                    ),
                    Block(vec![Assign(id_("inGoto"), atom_(FALSE_)), stmt.take()]),
                    Empty,
                )
            }
            Var(..) | Assign(..) if !is_call(stmt) => {
                *stmt = if_(get_id_("inGoto"), Empty, stmt.take())
            }
            If(cond, cons, alt) => {
                let normal = band_(not_(get_id_("inGoto")), cond.take());
                let bounds = bounds(cons);
                let cond = if let Some((lo, hi)) = bounds {
                    let if_goto = band_(
                        gte_(get_id_("inGoto"), i32_(lo)),
                        lte_(get_id_("inGoto"), i32_(hi)),
                    );
                    let in_goto_case = band_(get_id_("inGoto"), if_goto);
                    bor_(normal, in_goto_case)
                } else {
                    normal
                };
                *stmt = if_(cond, cons.take(), alt.take())
            }
            Label(..) => (),
            _ => (),
        }
    }
}

fn is_call(stmt: &Stmt) -> bool {
    use Stmt::*;
    match stmt {
        Assign(_, Expr::CallDirect(..))
        | Assign(_, Expr::CallIndirect(..))
        | Var(_, Expr::CallDirect(..), _)
        | Var(_, Expr::CallIndirect(..), _) => true,
        _ => false,
    }
}

/// since labels are ordered, we can define n belongs to L as min <= n <= max
#[derive(Default)]
struct LabelBoundsVisitor {
    bounds: Option<(i32, i32)>,
}
impl Visitor for LabelBoundsVisitor {
    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Label(Lbl::App(n), ..) => {
                if let Some((lo, hi)) = self.bounds.as_mut() {
                    if n < lo {
                        *lo = *n;
                    }
                    if n > hi {
                        *hi = *n;
                    }
                } else {
                    self.bounds = Some((*n, *n));
                }
            }
            _ => (),
        }
    }
}
fn bounds(stmt: &mut Stmt) -> Option<(i32, i32)> {
    let mut vis = LabelBoundsVisitor::default();
    stmt.walk(&mut vis);
    vis.bounds
}
