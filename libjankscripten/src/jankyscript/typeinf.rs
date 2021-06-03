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
        Lit::Num(Num::Float(_)) => { println!("ugh float"); Type::Float },
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

struct Overload {
    overloads: Vec<Type>,
    result_on_other_args: Option<Type>
}

impl Overload {
    fn new(overloads: &[Type]) -> Self {
        Overload { 
            overloads: overloads.to_vec(),
            result_on_other_args: None
         }
    }

    fn others(mut self, t: Type) -> Self {
        self.result_on_other_args = Some(t);
        self
    }
}

lazy_static! {
    static ref OP_OVERLOADS: HashMap<JsOp, Overload> = {
        use super::super::javascript::BinaryOp::*;

        let mut binops = hashmap! {
            Plus => Overload::new(&[
                typ!(fun(int, int) -> int),
                typ!(fun(string, string) -> string),
            ]).others(typ!(any)),
            LeftShift => Overload::new(&[
                typ!(fun(int, int) -> int),
            ]).others(typ!(int)),
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
            }
            _ => (),
        }
    }

    fn exit_expr(&mut self, expr: &mut Expr, _loc: &Loc) {
        use super::super::javascript::BinaryOp;
        use super::super::notwasm::syntax::{BinaryOp as WasmBinOp};
        match expr {
            // Expr::JsOp(JsOp::Binary(BinaryOp::LeftShift), es, ts, p)
            Expr::JsOp(JsOp::Binary(BinaryOp::Plus), es, ts, p) => {
                let t1 = &ts[0];
                let t2 = &ts[1];
                match (t1, t2) {
                    (Type::Any, Type::Any) => {
                        let e2 = es.pop().unwrap();
                        let e1 = es.pop().unwrap();
                        let p = std::mem::replace(p, Default::default());
                        *expr = Expr::PrimCall(crate::rts_function::RTSFunction::Plus, vec![e1, e2], p)
                    }
                    (Type::Int, Type::Int) => {
                        let e2 = es.pop().unwrap();
                        let e1 = es.pop().unwrap();
                        let p = std::mem::replace(p, Default::default());
                        *expr = Expr::Binary(WasmBinOp::I32Add, Box::new(e1), Box::new(e2), p);
                    }
                    _ => {
                        println!("NOOP: {:?} {:?}", t1, t2);
                    }
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
        } else if self.z.is_str(model, &e) {
            Type::String
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
                let e = expr.take();
                *expr = coerce(t, alpha_t.clone(), e, p);
                (phi, alpha_t)
            }
            Expr::JsOp(op, args, empty_args_t, _) => {
                // all overloads for op
                let sigs = OP_OVERLOADS
                    .get(op)
                    .expect(&format!("missing specification for {:?}", op));
                // NOTE(arjun): We do not have any weights here. Why bother?
                // Fresh type metavariable for the result of this expression
                let (alpha, alpha_t) = self.fresh_metavar("alpha");
                // Recur into each argument and unzip Z3 constants and our type metavars
                let args_rec = args.iter_mut().map(|e| self.cgen_expr(e));
                let (mut args_phi, args_t): (Vec<_>, Vec<_>) = args_rec.unzip();
                // Fresh type metavariables for each argument
                let mut betas_t = Vec::new();
                let mut betas_phi = Vec::new();
                for (arg, arg_t) in args.iter_mut().zip(&args_t) {
                    let a = arg.take();
                    let (beta_phi, beta_t) = self.fresh_metavar("beta");
                    *arg = coerce(arg_t.clone(), beta_t.clone(), a, Default::default());
                    betas_t.push(beta_t);
                    betas_phi.push(beta_phi);
                }
                // In DNF, one disjunct for each overload
                let mut disjuncts = Vec::new();
                for (op_arg_t, op_ret_t) in sigs.overloads.iter().map(|t| t.unwrap_fun()) {
                    // For this overload, arguments and result must match
                    let mut conjuncts = Vec::new();
                    for ((t1, t2), t3) in args_t.iter().zip(op_arg_t).zip(betas_t.iter()) {
                        conjuncts.push(self.t(t1)._eq(&self.t(t2)));
                        conjuncts.push(self.t(t2)._eq(&self.t(t3)));
                    }
                    conjuncts.push(alpha._eq(&self.t(op_ret_t)));
                    let cases = conjuncts.iter().collect::<Vec<_>>();
                    disjuncts.push(ast::Bool::and(self.z.cxt, cases.as_slice()));
                }

                if let Some(result_typ) = &sigs.result_on_other_args {
                    let mut conjuncts = Vec::new();
                    for (_t1, t2) in args_t.iter().zip(betas_t.iter()) {
                        // TODO(arjun): t1 must be compatible with any
                        conjuncts.push(self.t(t2)._eq(&self.z.make_any()));
                    }
                    conjuncts.push(alpha._eq(&self.t(result_typ)));
                    let cases = conjuncts.iter().collect::<Vec<_>>();
                    disjuncts.push(ast::Bool::and(self.z.cxt, cases.as_slice()));
                }
                let cases =
                    ast::Bool::or(self.z.cxt, disjuncts.iter().collect::<Vec<_>>().as_slice());
                args_phi.push(cases);
                // Annotate the AST with the type metavariables that hold the argument types
                *empty_args_t = betas_t;
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

#[allow(unused)]
pub fn typeinf(stmt: &mut Stmt) {
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
    println!("Mapping: {:?}", &mapping);

    let mut subst_metavar = SubtMetavarVisitor { vars: &mapping };
    stmt.walk(&mut subst_metavar);

}

#[cfg(test)]
mod tests {
    use super::typeinf;
    use super::super::syntax::*;
    use super::super::walk::*;
    use super::super::type_checking::type_check;
    use super::super::super::javascript::{parse, desugar};
    use super::super::super::shared::NameGen;

    #[derive(Default)]
    struct CountAnys {
        num_anys: usize
    }

    impl Visitor for CountAnys {
        fn enter_typ(&mut self, t: &mut Type) {
            if let Type::Any = t {
                self.num_anys += 1;
            }
        }
    }

    fn typeinf_test(s: &str) -> usize {
        let mut js = parse("<text>>", s).expect("error parsing JavaScript");
        let mut ng = NameGen::default();
        desugar(&mut js, &mut ng);
        let mut janky = crate::jankyscript::from_js::from_javascript(js);
        println!("Before type inference:\n{}", &janky);
        typeinf(&mut janky);
        println!("Result of type inference:\n{}", &janky);
        let mut count_anys = CountAnys::default();
        janky.walk(&mut count_anys);
        type_check(&janky)
            .expect("result of type inference does not type check");
        return count_anys.num_anys;
    }

    #[test]
    fn janky_plus() {
        let n = typeinf_test(r#"1 + "2";"#);
        assert_eq!(n, 2);
    }

    #[test]
    fn num_plus() {
        let n = typeinf_test(r#"1 + 2;"#);
        assert_eq!(n, 0);
    }

}
