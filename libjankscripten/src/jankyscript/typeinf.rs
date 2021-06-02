//! Type inference for JankyScript, using the TypeWhich approach.
//!
//! See the TypeWhich paper for details for a high-level overview:
//!
//! https://khoury.northeastern.edu/~arjunguha/main/papers/2021-typewhich.html
//!
//! This module hews closely to the TypeWhich artifact:
//!
//! https://github.com/arjunguha/TypeWhich

use super::super::shared::coercions::Coercion;
use super::syntax::*;
use super::typeinf_z3::Z3Typ;
use super::walk::{Loc, Visitor};
use crate::pos::Pos;
use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;
use z3::ast::{self, Ast, Dynamic};
use z3::{Model, Optimize, SatResult};

struct Typeinf<'a> {
    vars: Vec<Dynamic<'a>>,
    z: Z3Typ<'a>,
    solver: Optimize<'a>,
}

fn typ_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Num(Num::Float(_)) => Type::Float,
        Lit::Num(Num::Int(_)) => Type::Int,
        Lit::String(_) => Type::String,
        Lit::Bool(_) => Type::Bool,
        Lit::Regex(..) => Type::Any,
        Lit::Undefined => Type::Any,
        Lit::Null => Type::Any,
    }
}

fn coerce(src: Type, dst: Type, e: Expr, p: Pos) -> Expr {
    Expr::Coercion(Coercion::Meta(src, dst), Box::new(e), p)
}

macro_rules! typ {
    (int) => (Type::Int);
    (bool) => (Type::Bool);
    (string) => (Type::String);
    (any) => (Type::Any);
    (fun($( $arg:tt ),*) -> $ret:tt) =>
        (Type::Function(vec![ $( typ!($arg) ),* ], Box::new(typ!($ret))))
}

lazy_static! {
    static ref OP_OVERLOADS: HashMap<JsOp, Vec<Type>> = {
        use super::super::javascript::BinaryOp;

        let mut binops = hashmap! {
            BinaryOp::Plus => vec![
                typ!(fun(int, int) -> int),
                typ!(fun(string, string) -> string),
                typ!(fun(any, any) -> any)
            ],
        };

        let mut m = HashMap::new();
        for (op, t) in binops.drain() {
            m.insert(JsOp::Binary(op), t);
        }
        m
    };
}

struct SubtMetavarVisitor<'a> {
    vars: &'a Vec<Type>,
}

impl<'a> Visitor for SubtMetavarVisitor<'a> {
    fn enter_typ(&mut self, t: &mut Type) {
        match t {
            Type::Metavar(n) => {
                *t = self
                    .vars
                    .get(*n)
                    .expect("unbound type metavariable")
                    .clone();
                println!("Found a metavar: {}", t);
            }
            _ => (),
        }
    }

    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        use super::super::javascript::BinaryOp;
        match expr {
            Expr::JsOp(JsOp::Binary(BinaryOp::Plus), es, ts, p) => {
                let t1 = &ts[0];
                let t2 = &ts[1];
                match (t1, t2) {
                    (Type::Any, Type::Any) => {
                        let es = std::mem::replace(es, Default::default());
                        let p = std::mem::replace(p, Default::default());
                        *expr = Expr::PrimCall(crate::rts_function::RTSFunction::Plus, es, p)
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}

impl<'a> Typeinf<'a> {
    fn t(&self, t: &Type) -> z3::ast::Dynamic<'a> {
        match t {
            Type::Int => self.z.make_int(),
            Type::Any => self.z.make_any(),
            Type::String => self.z.make_str(),
            Type::Metavar(n) => self
                .vars
                .get(*n)
                .expect("unbound type metavariable")
                .clone(),
            _ => todo!("Type: {:?}", t),
        }
    }

    fn z3_to_typ(&self, model: &'a Model, e: Dynamic) -> Type {
        if self.z.is_int(model, &e) {
            Type::Int
        } else if self.z.is_any(model, &e) {
            Type::Any
        } else {
            todo!()
        }
    }

    fn fresh_weight(&self) -> z3::ast::Bool<'a> {
        let e = z3::ast::Bool::fresh_const(self.z.cxt, "w");
        self.solver.assert_soft(&e, 1, None);
        return e;
    }

    fn fresh_metavar(&mut self, prefix: &'static str) -> (ast::Dynamic<'a>, Type) {
        let x = self.z.fresh(prefix);
        let n = self.vars.len();
        self.vars.push(x.clone());
        return (x, Type::Metavar(n));
    }

    pub fn cgen_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(e, _) => {
                let (phi, _) = self.cgen_expr(&mut *e);
                self.solver.assert(&phi);
            }
            Stmt::Block(stmts, _) => {
                for s in stmts.iter_mut() {
                    self.cgen_stmt(s);
                }
            }
            _ => todo!("{:?}", stmt),
        }
    }

    pub fn cgen_expr(&mut self, expr: &mut Expr) -> (ast::Bool<'a>, Type) {
        match expr {
            Expr::Binary(..) => panic!("cgen_expr received Expr::Binary"),
            Expr::Lit(l, p) => {
                let t = typ_lit(&l);
                let p = p.clone();
                let (alpha, alpha_t) = self.fresh_metavar("alpha");
                let w = self.fresh_weight();
                let phi = (alpha._eq(&self.t(&t)) & &w) | (alpha._eq(&self.z.make_any()) & !w);
                let mut e = expr.take();
                *expr = coerce(t, alpha_t.clone(), e, p);
                (phi, alpha_t)
            }
            Expr::JsOp(op, args, empty_args_t, _) => {
                // NOTE(arjun): We do not have any weights here. Why bother?
                // Fresh variable for the result
                let (alpha, alpha_t) = self.fresh_metavar("alpha");
                // Recur into each argument
                let (mut args_phi, args_t): (Vec<_>, Vec<_>) =
                    args.iter_mut().map(|e| self.cgen_expr(e)).unzip();
                // all overloads for op
                let sigs = OP_OVERLOADS
                    .get(op)
                    .expect(&format!("missing specification for {:?}", op));
                // In DNF, one disjunct for each overload
                let mut disjuncts = Vec::new();
                for (op_arg_t, op_ret_t) in sigs.iter().map(|t| t.unwrap_fun()) {
                    // For this overload, arguments and result must match
                    let mut conjuncts = Vec::new();
                    for (t1, t2) in args_t.iter().zip(op_arg_t) {
                        let x = self.t(t1)._eq(&self.t(t2));
                        conjuncts.push(x);
                    }
                    conjuncts.push(alpha._eq(&self.t(op_ret_t)));
                    let cases = conjuncts.iter().collect::<Vec<_>>();
                    disjuncts.push(ast::Bool::and(self.z.cxt, cases.as_slice()));
                }
                let cases =
                    ast::Bool::or(self.z.cxt, disjuncts.iter().collect::<Vec<_>>().as_slice());
                args_phi.push(cases);
                *empty_args_t = args_t;
                (
                    ast::Bool::and(self.z.cxt, args_phi.iter().collect::<Vec<_>>().as_slice()),
                    alpha_t,
                )
            }
            // }
            _ => todo!("Expression: {:?}", expr),
        }
    }

    fn solve_model(&self, model: z3::Model) -> Vec<Type> {
        let mut result = Vec::new();
        for x_ast in self.vars.iter() {
            let x_val_ast = model.eval(x_ast).expect("evaluating metavar");
            result.push(self.z3_to_typ(&model, x_val_ast));
        }
        result
    }
}

pub fn typeinf(stmt: &mut Stmt) {
    println!("Mapping: {}", &stmt);
    let z3_cfg = z3::Config::new();
    let cxt = z3::Context::new(&z3_cfg);
    let dts = Z3Typ::make_dts(&cxt);
    let z = Z3Typ::new(&cxt, &dts);
    let mut state = Typeinf {
        vars: Default::default(),
        z,
        solver: Optimize::new(&cxt),
    };
    state.cgen_stmt(stmt);
    match state.solver.check(&[]) {
        SatResult::Unknown => panic!("Got an unknown from Z3"),
        SatResult::Unsat => panic!("type error"),
        SatResult::Sat => (),
    };
    let model = state
        .solver
        .get_model()
        .expect("model not available (despite SAT result)");
    let mapping = state.solve_model(model);

    let mut subst_metavar = SubtMetavarVisitor { vars: &mapping };
    stmt.walk(&mut subst_metavar);

    println!("Mapping: {}", &stmt);
}

#[cfg(test)]
mod tests {
    use super::super::syntax::*;
    use super::typeinf;

    fn typeinf_test(s: &str) {
        let mut js = crate::javascript::parse("<text>>", s).expect("error parsing JavaScript");
        let mut ng = crate::shared::NameGen::default();
        crate::javascript::desugar(&mut js, &mut ng);
        let mut janky = crate::jankyscript::from_js::from_javascript(js);
        typeinf(&mut janky);
    }

    #[test]
    fn num() {
        typeinf_test(r#"1 + "2";"#);
    }
}
