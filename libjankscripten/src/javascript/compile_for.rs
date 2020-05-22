//! desugar loops
//!
//! - desugar for and do..while to while
//! - then label for..in and while with a break label and a continue label
//! - change continue to break from the continue label
//! - use explicit labels in all breaks

use super::constructors::*;
use super::Stmt::*;
use super::*;

pub fn compile_for(script: &mut Stmt, ng: &mut NameGen) {
    script.walk(&mut LoopsToWhile(ng));
    script.walk(&mut LabelLoops::new(ng));
}

struct LoopsToWhile<'a>(&'a mut NameGen);
impl Visitor for LoopsToWhile<'_> {
    fn enter_stmt(&mut self, node: &mut Stmt) {
        match node {
            For(init, cond, advance, body) => {
                let init = match init {
                    ForInit::Expr(e) => expr_(e.take()),
                    ForInit::Decl(ds) => Stmt::VarDecl(std::mem::replace(ds, vec![])),
                };
                *node = Block(vec![
                    init,
                    while_(cond.take(), Block(vec![body.take(), expr_(advance.take())])),
                ]);
            }
            DoWhile(body, cond) => {
                let once = self.0.fresh("once");
                *node = Block(vec![
                    vardecl1_(once.clone(), TRUE_),
                    while_(
                        or_(id_(once.clone()), cond.take()),
                        Block(vec![expr_(assign_(lval_id_(once), FALSE_)), body.take()]),
                    ),
                ])
            }
            _ => (),
        }
    }
}

/// labels all while / break as expected; labels block inside of while and
/// changes continue to break
///
/// please see Stopify/normalize-js/ts/desugarLoop.ts:WhileStatement
struct LabelLoops<'a> {
    ng: &'a mut NameGen,
    break_label: Option<Id>,
    cont_label: Option<Id>,
    /// avoids adding a label recursively forever
    is_labeled: bool,
}
impl<'a> LabelLoops<'a> {
    fn new(ng: &'a mut NameGen) -> Self {
        Self {
            ng,
            break_label: None,
            cont_label: None,
            is_labeled: false,
        }
    }
}
impl Visitor for LabelLoops<'_> {
    fn enter_stmt(&mut self, node: &mut Stmt) {
        match node {
            Break(None) => {
                self.is_labeled = false;
                *node = Break(self.break_label.clone())
            }
            Continue(None) => {
                self.is_labeled = false;
                *node = Break(self.cont_label.clone())
            }
            Label(..) => self.is_labeled = true,
            ForIn(.., body) | While(.., body) if !self.is_labeled => {
                self.is_labeled = true;
                let break_name = self.ng.fresh("break");
                let cont_name = self.ng.fresh("cont");
                self.break_label = Some(break_name.clone());
                self.cont_label = Some(cont_name.clone());
                *body = Box::new(label_(cont_name, body.take()));
                *node = label_(break_name, node.take());
            }
            _ => self.is_labeled = false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_for_to_while() {
        let mut for_loop = parse(
            "
            for (var i=0; i<10; ++i) {
                console.log(i);
            }
            do {
                console.log(i);
            } while (3 < 4)
        ",
        )
        .unwrap();
        let while_loop = parse(
            "
            {
                var i=0;
                while (i<10) {
                    {
                        console.log(i);
                    }
                    ++i
                }
            }
            {
                var $jen_once_0 = true;
                while ($jen_once_0 || 3 < 4) {
                    $jen_once_0 = false;
                    {
                        console.log(i);
                    }
                }
            }
        ",
        )
        .unwrap();
        let mut ng = NameGen::default();
        for_loop.walk(&mut LoopsToWhile(&mut ng));
        println!("input:\n{}\noutput:\n{}", for_loop, while_loop);
        // Id::Named(x) != Id::Generated(x) so this is a workaround
        assert_eq!(for_loop.to_pretty(80), while_loop.to_pretty(80));
    }
    #[test]
    fn test_labels() {
        let mut unlabeled = parse(
            "
            while (true) {
                break;
                while (true) {
                    continue;
                }
            }
        ",
        )
        .unwrap();
        let labeled = parse(
            "
            $jen_break_0: while (true) $jen_cont_0: {
                break $jen_break_0;
                $jen_break_1: while (true) $jen_cont_1: {
                    break $jen_cont_1;
                }
            }
        ",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        // Id::Named(x) != Id::Generated(x) so this is a workaround
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
}
