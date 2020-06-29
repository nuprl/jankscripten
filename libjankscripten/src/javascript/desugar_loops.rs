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

pub fn desugar_loops(script: &mut Stmt, ng: &mut NameGen) {
    script.walk(&mut ExplicitBreaks);
    script.walk(&mut LabelLoops::new(ng));
    script.walk(&mut ForToWhile);
}

/// changes for loops to while loops
///
/// precondition: already labeled because statements will be inserted after
/// continues
struct ForToWhile;
impl Visitor for ForToWhile {
    fn enter_stmt(&mut self, node: &mut Stmt) {
        if let For(init, cond, advance, body) = node {
            let init = match init {
                ForInit::Expr(e) => expr_(e.take()),
                ForInit::Decl(ds) => Stmt::VarDecl(std::mem::replace(ds, vec![])),
            };
            *node = Block(vec![
                init,
                while_(cond.take(), Block(vec![body.take(), expr_(advance.take())])),
            ]);
        }
    }
}

/// labels all loop / break as expected; labels block inside of loops and
/// changes continue to break
///
/// please see Stopify/normalize-js/ts/desugarLoop.ts:WhileStatement
///
/// unlike in stopify fsr, this has to happen before ForToWhile because for
/// loop inserts stuff after the continuation
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
                //     $break_0: while (true) {
                //         break;
                //     }
                // and the top of the stack reflects that label
                //     breaks_stack = [.., "$break_0"]
                // end result:
                //     break $break_0;
                *node = Break(Some(
                    self.breaks_stack.last().expect("no close break").clone(),
                ))
            }
            Continue(None) => {
                // precondition we have a labeled loop and labeled body
                //     $break_0: while (true) $cont_0: {
                //         continue;
                //     }
                // the top of the stack reflects the break
                //     breaks_stack = [.., $break_0]
                // and the map maps that to the equivalent continue
                //     breaks_for_conts = { .., $break_0 => $cont_0 }
                // end result:
                //     continue $cont_0;
                *node = Break(Some(
                    self.breaks_for_conts
                        .get(self.breaks_stack.last().expect("no break label"))
                        .expect("no cont map")
                        .clone(),
                ));
            }
            Continue(Some(other)) => {
                // precondition we have a labeled loop and labeled body
                //     my_loop: while (true) $cont_0: {
                //         continue my_loop;
                //     }
                // the top of the stack reflects the break
                //     breaks_stack = [.., my_loop]
                // and the map maps that to the equivalent continue
                //     breaks_for_conts = { .., my_loop => $cont_0 }
                // end result:
                //     continue $cont_0;
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

/// unlike what stopify does, this should happen before labeling because we
/// use an implicit break. this unfortunately means it has to happen before
/// loop unification, so we special case them
struct ExplicitBreaks;
impl Visitor for ExplicitBreaks {
    fn exit_stmt(&mut self, node: &mut Stmt) {
        match node {
            While(cond, body) | For(.., cond, _, body) => {
                // if you don't add a block, inserted statements will go above
                *body = Box::new(Block(vec![if_(cond.take(), body.take(), Break(None))]));
                *cond = Box::new(TRUE_);
            }
            DoWhile(body, cond) => {
                // if you don't add a block, inserted statements will go above
                let new_body = Block(vec![
                    body.take(),
                    if_(cond.take(), Stmt::Empty, Break(None)),
                ]);
                *node = while_(TRUE_, new_body);
            }
            // TODO(luna): for..in? no expressions are possible so im not
            // super worried rn
            _ => (),
        }
    }
}
