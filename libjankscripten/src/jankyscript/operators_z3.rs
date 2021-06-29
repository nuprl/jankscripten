use z3;
use std::collections::HashMap;
use super::operators::{NotwasmOp, OverloadTable, TypeScheme};
use super::syntax::*;

pub struct Z3Operators<'a> {
    sort: z3::Sort<'a>, // the sort in Z3 for operators
    ctors: HashMap<NotwasmOp, z3::FuncDecl<'a>>,
    preds: HashMap<NotwasmOp, z3::FuncDecl<'a>>,
    ctx: &'a z3::Context,
    metavars: Vec<z3::ast::Dynamic<'a>>,
}

impl<'a> Z3Operators<'a> {

    pub fn new(table: &OverloadTable, ctx: &'a z3::Context) -> Self {
        // Turn the set into a vec to ensure stable enumeration order.
        let all_ops = table.all_ops().into_iter().collect::<Vec<_>>();
        let names: Vec<z3::Symbol> = all_ops
            .iter()
            .enumerate()
            // Awful names for operators.
            .map(|(i, _)| format!("op{}", i).into())
            .collect();
        let (sort, ctors_vec, preds_vec) = z3::Sort::enumeration(
            ctx, 
            "Operators".into(), 
            &names);
        let mut ctors = HashMap::new();
        let mut preds = HashMap::new();
        for ((op, ctor), pred) in all_ops.into_iter().zip(ctors_vec.into_iter()).zip(preds_vec.into_iter()) {
            ctors.insert(op.clone(), ctor);
            preds.insert(op, pred);
        }
        let metavars = Vec::new();
        return Z3Operators { sort, ctors, preds, ctx, metavars };
    }

    pub fn fresh_op_selector(&mut self) -> NotwasmOp {
        let ast = z3::ast::Dynamic::from_ast(&z3::ast::Datatype::fresh_const(self.ctx, "op", &self.sort));
        self.metavars.push(ast);
        return NotwasmOp::Metavar(self.metavars.len() - 1);
    }

    pub fn z(&self, op: &NotwasmOp) -> z3::ast::Dynamic<'a> {
        match op {
            NotwasmOp::Metavar(n) => self.metavars[*n].clone(),
            _ => self.ctors.get(op).expect("operator not in table").apply(&[])
        }
    }

    pub fn eval_op(&self, op: &NotwasmOp, model: &z3::Model) -> NotwasmOp {
        match op {
            NotwasmOp::Metavar(n) => {
                let x = &self.metavars[*n];
                for (notwasm_op, pred) in &self.preds {
                    if model.eval(&pred.apply(&[&x]).as_bool().unwrap()).unwrap().as_bool().unwrap() {
                        return notwasm_op.clone();
                    }
                }
                panic!("no value for metavar");
            }
            _ => panic!("not a metavar")
        }
    }
}