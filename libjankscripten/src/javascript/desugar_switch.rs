/// Principal pattern for desugaring a switch statement is to turn this:
///
/// ```ignore
/// switch (e) {
///   case lit: s;
///   ...
///   default: s_default;
/// }
/// ```
///
/// into:
///
/// ```ignore
/// sw: {
///   let tmp = e;
///   let fallthrough = false;
///   if (fallthrough || lit === e) { s; fallthrough = true } ...
///   s_default;
/// }
/// ```
///
/// In addition, any 'break' within 's ...' and 's_default' turns into 'break sw';
use super::constructors::*;
use super::syntax::BinaryOp;
use super::syntax::LogicalOp;
use super::syntax::*;
use super::walk::*;
use super::*;

struct SwitchToIf<'a> {
    ng: &'a mut NameGen,
    name_stack: Vec<Id>,
}

impl<'a> SwitchToIf<'a> {
    fn enclosing_switch_name(&self) -> Id {
        self.name_stack.iter().last().unwrap().clone()
    }
}

impl Visitor for SwitchToIf<'_> {
    fn enter_stmt(&mut self, stmt: &mut Stmt, _loc: &Loc) {
        match stmt {
            Stmt::Switch(.., _) => {
                let name = self.ng.fresh("sw");
                self.name_stack.push(name.clone());
            }
            _ => {}
        }
    }

    fn exit_stmt(&mut self, stmt: &mut Stmt, loc: &Loc) {
        match stmt {
            Stmt::Break(None, s) => {
                if loc.in_switch_block() {
                    *stmt = Stmt::Break(Some(self.enclosing_switch_name()), *s);
                }
            }
            Stmt::Switch(expr, cases, default, s) => {
                // cases = vec<(expr, stmt)>
                let name = self.name_stack.pop().expect("no name to pop");
                let test = expr.take();
                let test_id = self.ng.fresh("test");
                let fallthrough = self.ng.fresh("fallthrough");

                let mut v = vec![
                    vardecl1_(fallthrough.clone(), FALSE_, *s),
                    vardecl1_(test_id.clone(), test, *s),
                ];

                // create if statements for cases (test === e || fallthrough)
                for (e, stmt) in cases.drain(0..) {
                    v.push(if_(
                        binary_(
                            BinOp::LogicalOp(LogicalOp::Or),
                            binary_(
                                BinOp::BinaryOp(BinaryOp::StrictEqual),
                                id_(test_id.clone(), *s),
                                e.clone(),
                                *s,
                            ),
                            id_(fallthrough.clone(), *s),
                            *s,
                        ),
                        Stmt::Block(
                            vec![stmt, expr_(assign_(fallthrough.clone(), TRUE_, *s), *s)],
                            *s,
                        ),
                        Stmt::Empty,
                        *s,
                    ))
                }

                // add default case (if applicable)
                let d = default.take();
                match d {
                    Stmt::Block(mut dv, _) => {
                        for s in dv.drain(0..) {
                            v.push(s);
                        }
                    }
                    Stmt::Empty => {
                        // no default, move along
                    }
                    _ => {
                        panic!("Block or Empty expected");
                    }
                }

                // create labeled block w if statements/default
                *stmt = label_(name, Stmt::Block(v, *s), *s)
            }
            _ => {
                // not a switch statement, proceed as usual
            }
        }
    }
}

pub fn desugar_switch(program: &mut Stmt, namegen: &mut NameGen) {
    let mut v = SwitchToIf {
        ng: namegen,
        name_stack: vec![],
    };
    program.walk(&mut v);
}
