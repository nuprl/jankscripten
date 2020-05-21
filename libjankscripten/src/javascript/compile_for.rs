//! desugar loops
//!
//! - desugar for and do..while to while
//! - then label for..in and while with a break label and a continue label
//! - change continue to break from the continue label
//! - use explicit labels in all breaks

use super::constructors::*;
use super::Stmt::*;
use super::*;

pub fn compile_for(script: &mut Stmt) {
    script.walk(&mut LoopsToWhile::default());
    script.walk(&mut LabelLoops::default());
}

#[derive(Default)]
struct LoopsToWhile;
impl Visitor for LoopsToWhile {
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
                *node = Block(vec![
                    vardecl1_("$jen_once", TRUE_),
                    while_(
                        or_(id_("$jen_once"), cond.take()),
                        Block(vec![
                            expr_(assign_(lval_id_("$jen_once"), FALSE_)),
                            body.take(),
                        ]),
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
#[derive(Default)]
struct LabelLoops {
    label_i: usize,
    break_label: Option<String>,
    cont_label: Option<String>,
    /// avoids adding a label recursively forever
    is_labeled: bool,
}
impl Visitor for LabelLoops {
    fn enter_stmt(&mut self, node: &mut Stmt) {
        match node {
            Break(None) => {
                self.is_labeled = false;
                *node = Break(self.break_label.as_ref().map(Into::into))
            }
            Continue(None) => {
                self.is_labeled = false;
                *node = Break(self.cont_label.as_ref().map(Into::into))
            }
            Label(..) => self.is_labeled = true,
            ForIn(.., body) | While(.., body) if !self.is_labeled => {
                self.is_labeled = true;
                let break_name = format!("$break_{}", self.label_i);
                let cont_name = format!("$cont_{}", self.label_i);
                self.label_i += 1;
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
            for (let i=0; i<10; ++i) {
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
                let i=0;
                while (i<10) {
                    {
                        console.log(i);
                    }
                    ++i
                }
            }
            {
                let $jen_once = true;
                while ($jen_once || 3 < 4) {
                    $jen_once = false;
                    {
                        console.log(i);
                    }
                }
            }
        ",
        )
        .unwrap();
        for_loop.walk(&mut LoopsToWhile::default());
        println!("input:\n{}\noutput:\n{}", for_loop, while_loop);
        assert_eq!(for_loop, while_loop);
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
            $break_0: while (true) $cont_0: {
                break $break_0;
                $break_1: while (true) $cont_1: {
                    break $cont_1;
                }
            }
        ",
        )
        .unwrap();
        unlabeled.walk(&mut LabelLoops::default());
        assert_eq!(unlabeled, labeled);
    }
}
