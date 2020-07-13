//! Eliminate function-local gotos by checking an inGoto flag
//!
//! Preconditions:
//! - all apps should be labeled ([label_apps])

use super::constructors::*;
use super::syntax::{Label as Lbl, *};
use super::walk::*;

#[allow(unused)]
pub fn elim_gotos(program: &mut Program) {
    let mut vis = GotoVisitor::default();
    for func in program.functions.values_mut() {
        func.body.walk(&mut vis);
        // TODO(luna): fresh names??
        func.body = Stmt::Block(vec![
            // var inGoto = false;
            Stmt::Var(VarStmt::new(id_("inGoto"), atom_(FALSE_))),
            // var gotoTarget = 0;
            Stmt::Var(VarStmt::new(id_("gotoTarget"), atom_(i32_(0)))),
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
                    // already-computed booleans (except eq) but it could be
                    // improved if we wanted to hand-lower the or/ands
                    bor_(
                        not_(get_id_("inGoto")),
                        eq_(get_id_("gotoTarget"), i32_(*n)),
                    ),
                    Block(vec![Assign(id_("inGoto"), atom_(FALSE_)), call.take()]),
                    Empty,
                );
            }
            Var(..) | Assign(..) if !is_call(stmt) => {
                *stmt = if_(get_id_("inGoto"), Empty, stmt.take())
            }
            If(cond, cons, alt) => {
                let cons_cond = bounds_check_if(cons, cond.take());
                let alt_cond = bounds_check(alt);
                *stmt = if_(
                    cons_cond,
                    cons.take(),
                    if_(alt_cond, alt.take(), Stmt::Empty),
                )
            }
            Loop(body) => {
                let cond = bounds_check(body);
                *stmt = if_(cond, stmt.take(), Stmt::Empty)
            }
            // the rest fall into ~3 groups
            // - could not be advanced past to trigger GC (ie Return)
            // - need no special handling (ie Block)
            // - covered logically by other cases (ie Var/Assign if !is_call)
            _ => (),
        }
    }
}

fn is_call(stmt: &Stmt) -> bool {
    use Stmt::*;
    match stmt {
        Assign(_, Expr::Call(..)) => true,
        Var(var_stmt) => {
            match var_stmt.named {
                 Expr::Call(..) => true,
                 _ => false
            }
        }
        _ => false,
    }
}
fn bounds_check_maybe_if(body: &mut Stmt, alt_check: Atom) -> Atom {
    if let Some((lo, hi)) = bounds(body) {
        let if_goto = band_(
            gte_(get_id_("gotoTarget"), i32_(lo)),
            lte_(get_id_("gotoTarget"), i32_(hi)),
        );
        let in_goto_case = band_(get_id_("inGoto"), if_goto);
        bor_(alt_check, in_goto_case)
    } else {
        alt_check
    }
}
fn bounds_check_if(body: &mut Stmt, not_goto_check: Atom) -> Atom {
    bounds_check_maybe_if(body, band_(not_goto_check, not_(get_id_("inGoto"))))
}
fn bounds_check(body: &mut Stmt) -> Atom {
    bounds_check_maybe_if(body, not_(get_id_("inGoto")))
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
