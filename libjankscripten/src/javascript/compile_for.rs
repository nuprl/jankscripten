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
    /// closest non-generated-continue label
    breaks_stack: Vec<Id>,
    /// avoids adding a label recursively forever
    skip_next: bool,
    /// maps named and generated labels to the 'continue -> break' desugared
    /// label
    breaks_for_conts: HashMap<Id, Id>,
}
impl<'a> LabelLoops<'a> {
    fn new(ng: &'a mut NameGen) -> Self {
        Self {
            ng,
            breaks_stack: vec![],
            skip_next: false,
            breaks_for_conts: HashMap::new(),
        }
    }
}
impl Visitor for LabelLoops<'_> {
    fn enter_stmt(&mut self, node: &mut Stmt) {
        if self.skip_next {
            self.skip_next = false;
            return;
        }
        match node {
            // break to name needs no change
            // Some(unwrap) ensures we get our label
            Break(None) => {
                *node = Break(Some(
                    self.breaks_stack.last().expect("no close break").clone(),
                ))
            }
            Continue(None) => {
                *node = Break(Some(
                    self.breaks_for_conts
                        .get(self.breaks_stack.last().expect("no break label"))
                        .expect("no cont map")
                        .clone(),
                ));
            }
            Continue(Some(other)) => {
                *node = Break(Some(
                    self.breaks_for_conts
                        .get(other)
                        .expect("cont has no break")
                        .clone(),
                ))
            }
            // a labeled non-loop block, goes on the stack but no continue fanciness
            Label(break_name, labeled) if !self.skip_next => {
                if let Some(body) = if_loop_then_body(labeled) {
                    // a labeled loop we need to capture our name for continues to it
                    self.add_continue_and_state(body, break_name.clone());
                    // now we've already desugared the loop (which is below
                    // us in the tree) so we don't want to desugar it again
                    // when we recurse
                    self.skip_next = true;
                    // we technically will hit our cont label again, but that's
                    // not an issue because we don't have a desugar for
                    // labeled block
                }
                // a labeled block needs no desugar (break name is already
                // correct, continue is illegal, no implicit break)
            }
            _ => (),
        }
        // this is only outside the match to let us use the helper fn
        if let Some(body) = if_loop_then_body(node) {
            let break_name = self.ng.fresh("break");
            self.add_continue_and_state(body, break_name.clone());
            *node = label_(break_name, node.take());
            // now that we've surrounded ourselves in a label, walk is going
            // to recurse on US again, so to avoid recursively labeling forever
            // we'll skip the very next visit
            self.skip_next = true;
        }
    }
    fn exit_stmt(&mut self, node: &mut Stmt) {
        match node {
            // the loop will never be exited because we change it to label live
            // however no matter, the label is what really matters anyway right
            Label(name, ..) => {
                if let Id::Generated("cont", _) = name {
                    // cont doesn't go on the stack
                } else {
                    let mut old = self
                        .breaks_stack
                        .pop()
                        .expect("exited more labels than entered");
                    assert_eq!(&mut old, name);
                    self.breaks_for_conts.remove(&old);
                }
            }
            _ => (),
        }
    }
}
impl LabelLoops<'_> {
    fn add_continue_and_state(&mut self, body: &mut Box<Stmt>, break_name: Id) {
        let cont_name = self.ng.fresh("cont");
        self.breaks_stack.push(break_name.clone());
        self.breaks_for_conts
            .insert(break_name.clone(), cont_name.clone());
        *body = Box::new(label_(cont_name, body.take()));
    }
}
fn if_loop_then_body(stmt: &mut Stmt) -> Option<&mut Box<Stmt>> {
    if let For(.., body) | DoWhile(body, ..) | ForIn(.., body) | While(.., body) = stmt {
        Some(body)
    } else {
        None
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
            ({i: i, j: j});",
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
            ({i: i, j: j});",
        )
        .unwrap();
        let mut ng = NameGen::default();
        unlabeled.walk(&mut LabelLoops::new(&mut ng));
        assert_eq!(unlabeled.to_pretty(80), labeled.to_pretty(80));
    }
}
