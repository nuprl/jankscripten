//! desugar loops
//!
//! - label for..in and while with a break label and a continue label
//! - change continue to break to the continue label
//! - use explicit labels in all breaks
//! - desugar for and do..while to while

use super::constructors::*;
use super::Stmt::*;
use super::*;
use std::collections::HashMap;

pub fn compile_for(script: &mut Stmt, ng: &mut NameGen) {
    script.walk(&mut LabelLoops::new(ng));
    script.walk(&mut LoopsToWhile(ng));
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

/// labels all loop / break as expected; labels block inside of loops and
/// changes continue to break
///
/// please see Stopify/normalize-js/ts/desugarLoop.ts:WhileStatement
struct LabelLoops<'a> {
    ng: &'a mut NameGen,
    /// stack of loop labels
    breaks_stack: Vec<Id>,
    /// maps named and generated labels to the 'continue -> break' desugared
    /// label
    breaks_for_conts: HashMap<Id, Id>,
}
impl<'a> LabelLoops<'a> {
    fn new(ng: &'a mut NameGen) -> Self {
        Self {
            ng,
            breaks_stack: vec![],
            breaks_for_conts: HashMap::new(),
        }
    }
}
impl Visitor for LabelLoops<'_> {
    /// on loops, add their break name to the stack
    fn enter_stmt(&mut self, node: &mut Stmt) {
        // if it's already there use that
        if let Label(break_name, labeled) = node {
            if if_loop_then_body(labeled).is_some() {
                // this allows continue to be desugared (see precondition
                // at that match arm)
                self.label_loop_state(break_name.clone());
                // it's easier to always add the label back, so we delete
                // it when it already exists but its in the stack now
                // this also avoids recursing infinitely
                *node = labeled.take();
            }
        } else if if_loop_then_body(node).is_some() {
            // this does the same as the Label(..) match except generates
            // a fresh break name
            let break_name = self.ng.fresh("break");
            self.label_loop_state(break_name);
        }
    }
    /// do the actual AST modification
    fn exit_stmt(&mut self, node: &mut Stmt) {
        match node {
            // break to name needs no change
            // Some(unwrap) ensures we get our label
            Break(None) => {
                // precondition we have a labeled loop
                //     $jen_break_0: while (true) {
                //         break;
                //     }
                // and the top of the stack reflects that label
                //     breaks_stack = [.., "$jen_break_0"]
                // end result:
                //     break $jen_break_0;
                *node = Break(Some(
                    self.breaks_stack.last().expect("no close break").clone(),
                ))
            }
            Continue(None) => {
                // precondition we have a labeled loop and labeled body
                //     $jen_break_0: while (true) $jen_cont_0: {
                //         continue;
                //     }
                // the top of the stack reflects the break
                //     breaks_stack = [.., $jen_break_0]
                // and the map maps that to the equivalent continue
                //     breaks_for_conts = { .., $jen_break_0 => $jen_cont_0 }
                // end result:
                //     continue $jen_cont_0;
                *node = Break(Some(
                    self.breaks_for_conts
                        .get(self.breaks_stack.last().expect("no break label"))
                        .expect("no cont map")
                        .clone(),
                ));
            }
            Continue(Some(other)) => {
                // precondition we have a labeled loop and labeled body
                //     my_loop: while (true) $jen_cont_0: {
                //         continue my_loop;
                //     }
                // the top of the stack reflects the break
                //     breaks_stack = [.., my_loop]
                // and the map maps that to the equivalent continue
                //     breaks_for_conts = { .., my_loop => $jen_cont_0 }
                // end result:
                //     continue $jen_cont_0;
                *node = Break(Some(
                    self.breaks_for_conts
                        .get(other)
                        .expect("cont has no break")
                        .clone(),
                ))
            }
            _ => (),
        }
        // this is only outside the match to let us use the helper fn
        // no special handling of already-labeled because we deleted their
        // labels
        if let Some(body) = if_loop_then_body(node) {
            let break_name = self
                .breaks_stack
                .pop()
                .expect("no generated label for loop");
            let cont_name = self
                .breaks_for_conts
                .remove(&break_name)
                .expect("no cont for break");
            *body = Box::new(label_(cont_name, body.take()));
            *node = label_(break_name, node.take());
        }
    }
}
impl LabelLoops<'_> {
    /// - generates continue name
    /// - adds break_name to stack of breaks
    /// - adds continue name to map of breaks->conts
    fn label_loop_state(&mut self, break_name: Id) {
        let cont_name = self.ng.fresh("cont");
        self.breaks_stack.push(break_name.clone());
        self.breaks_for_conts.insert(break_name, cont_name);
    }
}
fn if_loop_then_body(stmt: &mut Stmt) -> Option<&mut Box<Stmt>> {
    if let For(.., body) | DoWhile(body, ..) | ForIn(.., body) | While(.., body) = stmt {
        // some... BODY once told me
        Some(body)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::expect_same;
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
    #[test]
    fn nested_loops() {
        let mut unlabeled = parse(
            "while (true) {
                while (true) {}
                break;
            }",
        )
        .unwrap();
        let labeled = parse(
            "$jen_break_0: while(true) $jen_cont_0: {
                $jen_break_1: while(true) $jen_cont_1: {}
                break $jen_break_0;
            }",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
    #[test]
    fn continue_labeled_loop() {
        let mut unlabeled = parse(
            "program_label: while (true) {
                while (true) {
                    continue program_label;
                }
            }",
        )
        .unwrap();
        let labeled = parse(
            "program_label: while(true) $jen_cont_0: {
                $jen_break_0: while(true) $jen_cont_1: {
                    break $jen_cont_0;
                }
            }",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
    #[test]
    fn stopify_for_labels() {
        let mut unlabeled = parse(
            "let i = 0;
            l: for (let j = 0; j < 10; j++) {
                if (j % 2 === 0) {
                    i++;
                    do {
                        continue l;
                    } while (0);
                    i++;
                }
            }
            i;",
        )
        .unwrap();
        let labeled = parse(
            "let i = 0;
            l: for (let j = 0; j < 10; j++) $jen_cont_0: {
                if (j % 2 === 0) {
                    i++;
                    $jen_break_0: do $jen_cont_1: {
                        break $jen_cont_0;
                    } while (0);
                    i++;
                }
            }
            i;",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
    #[test]
    fn stopify_continue_nested() {
        let mut unlabeled = parse(
            "var i = 0;
            var j = 8;

            checkiandj: while (i < 4) {
                i += 1;

                checkj: while (j > 4) {
                    j -= 1;
                    if ((j % 2) === 0) {
                        i = 5;
                        continue checkiandj;
                    }
                }
            }
            // TODO(luna): ({i: i, j: j}) goes thru parser+pretty as {i: i,
            // j: j} (no parens) which isn't valid, not sure why
            i;",
        )
        .unwrap();
        let labeled = parse(
            "var i = 0;
            var j = 8;

            checkiandj: while (i < 4) $jen_cont_0: {
                i += 1;

                checkj: while (j > 4) $jen_cont_1: {
                    j -= 1;
                    if ((j % 2) === 0) {
                        i = 5;
                        break $jen_cont_0;
                    }
                }
            }
            i;",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        println!("desugared:\n{}\nexpected:\n{}", unlabeled, labeled);
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
        expect_same(&unlabeled, &labeled);
    }
    #[test]
    fn labeled_block_stack() {
        let mut unlabeled = parse(
            "while (true) {
                bogus: {
                    break bogus;
                }
                break;
            }",
        )
        .unwrap();
        let labeled = parse(
            "$jen_break_0: while (true) $jen_cont_0: {
                bogus: {
                    break bogus;
                }
                break $jen_break_0;
            }",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
        expect_same(&unlabeled, &labeled);
    }
    #[test]
    fn labeled_block_to_loop() {
        let mut unlabeled = parse(
            "while (true) {
                bogus: {
                    break;
                }
            }",
        )
        .unwrap();
        let labeled = parse(
            "$jen_break_0: while (true) $jen_cont_0: {
                bogus: {
                    break $jen_break_0;
                }
            }",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
}
