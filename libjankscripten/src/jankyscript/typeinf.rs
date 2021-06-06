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
use super::operators::OVERLOADS;
use super::syntax::*;
use super::typeinf_z3::Z3Typ;
use super::walk::{Loc, Visitor};
use crate::pos::Pos;
use z3::ast::{self, Ast, Dynamic};
use z3::{Model, Optimize, SatResult};
use super::typeinf_env::Env;

struct Typeinf<'a> {
    vars: Vec<Dynamic<'a>>,
    z: Z3Typ<'a>,
    solver: Optimize<'a>,
    cxt: &'a z3::Context,
    env: Env,
}

/// Calculates the type of a literal.
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
    super::constructors::coercion_(Coercion::meta(src, dst), e, p)
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
        match expr {
            Expr::JsOp(op, arg_es, arg_ts, p) => {
                let p = std::mem::replace(p, Default::default());
                let mut es = std::mem::replace(arg_es, Default::default());
                match OVERLOADS.target(op, arg_ts.as_slice()) {
                    Some(lower_op) => {
                        *expr = lower_op.make_app(es, p);
                    }
                    None => {
                        let (ty, lower_op) = OVERLOADS.any_target(op);
                        let (conv_arg_tys, _) = ty.unwrap_fun();
                        // TODO(arjun): This is going to end up duplicating a lot of
                        // code that appears above.
                        for (e, t) in es.iter_mut().zip(conv_arg_tys) {
                            *e = coerce(Type::Any, t.clone(), e.take(), p.clone());
                        }
                        *expr = lower_op.make_app(es, p);
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
            Type::Bool => self.z.make_bool(),
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
            Stmt::Var(x, t, e, _) => {
                assert!(e.is_undefined());   
                let (_, alpha) = self.fresh_metavar("x");
                *t = alpha.clone();
                self.env.update(x.clone(), alpha);
            }
            Stmt::Expr(e, _) => {
                let (phi, _) = self.cgen_expr(&mut *e);
                println!("{:?}", &phi);
                self.solver.assert(&phi);
            }
            Stmt::Empty => (),
            Stmt::Loop(s, _) => self.cgen_stmt(s),
            Stmt::Label(_, s, _) => self.cgen_stmt(s),
            Stmt::Block(stmts, _) => {
                for s in stmts.iter_mut() {
                    self.cgen_stmt(s);
                }
            }
            Stmt::Catch(body, exn_name, catch_body, _) => {
                self.cgen_stmt(&mut *body);
                let env = self.env.clone();
                self.env.extend(exn_name.clone(), Type::Any);
                self.cgen_stmt(catch_body);
                self.env = env;
            }
            _ => todo!("{:?}", stmt),
        }
    }

    pub fn cgen_expr(&mut self, expr: &mut Expr) -> (ast::Bool<'a>, Type) {
        match expr {
            Expr::Binary(..)
            | Expr::PrimCall(..)
            | Expr::NewRef(..)
            | Expr::Deref(..)
            | Expr::Store(..)
            | Expr::EnvGet(..)
            | Expr::Coercion(..)
            | Expr::Closure(..)
            | Expr::Unary(..) => panic!("unexpected {:?}", &expr),
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
            Expr::Array(..) => todo!(),
            Expr::Object(..) => todo!(),
            Expr::Id(x, t, _) => {
                *t = self.env.get(x);
                (ast::Bool::from_bool(self.cxt,true), t.clone())
            }
            Expr::Dot(..) => todo!(),
            Expr::Bracket(..) => todo!(),
            Expr::JsOp(op, args, empty_args_t, _) => {
                let w = self.fresh_weight();
                // all overloads for op
                let sigs = OVERLOADS.overloads(op);
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
                for (op_arg_t, op_ret_t) in sigs.map(|t| t.unwrap_fun()) {
                    // For this overload, arguments and result must match
                    let mut conjuncts = vec![w.clone()];
                    for ((t1, t2), t3) in args_t.iter().zip(op_arg_t).zip(betas_t.iter()) {
                        conjuncts.push(self.t(t1)._eq(&self.t(t2)));
                        conjuncts.push(self.t(t2)._eq(&self.t(t3)));
                    }
                    conjuncts.push(alpha._eq(&self.t(op_ret_t)));
                    let cases = conjuncts.iter().collect::<Vec<_>>();
                    disjuncts.push(ast::Bool::and(self.z.cxt, cases.as_slice()));
                }

                if let Some(any_ty) = OVERLOADS.on_any(op) {
                    let (_, result_typ) = any_ty.unwrap_fun();
                    let mut conjuncts = vec![!w];
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
            Expr::Assign(lval, e, _) => match &mut **lval {
                LValue::Id(x, x_t) => {
                    let t = self.env.get(x);
                    *x_t = t.clone();
                    let (phi_1, e_t) = self.cgen_expr(&mut *e);
                    // TODO(arjun): Not quite right. Too strict
                    let phi_2 = self.t(&t)._eq(&self.t(&e_t));
                    (phi_1 & phi_2, t)
                }
                _ => todo!()
            }
            Expr::Call(..) => todo!(),
            Expr::Func(..) => todo!(),
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
    let env = Env::new();
    let mut state = Typeinf {
        vars: Default::default(),
        z,
        cxt: &cxt,
        solver: Optimize::new(&cxt),
        env,
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
    println!("After cgen: {}", &stmt);
}

#[cfg(test)]
mod tests {
    use super::super::super::javascript::{desugar, parse};
    use super::super::super::shared::NameGen;
    use super::super::syntax::*;
    use super::super::type_checking::type_check;
    use super::super::walk::*;
    use super::typeinf;

    #[derive(Default)]
    struct CountAnys {
        num_anys: usize,
    }

    impl Visitor for CountAnys {
        fn enter_typ(&mut self, t: &mut Type) {
            if let Type::Any = t {
                self.num_anys += 1;
            }
        }
    }

    fn typeinf_test(s: &str) -> usize {
        let mut js = parse("<text>", s).expect("error parsing JavaScript");
        let mut ng = NameGen::default();
        desugar(&mut js, &mut ng);
        let mut janky = crate::jankyscript::from_js::from_javascript(js);
        typeinf(&mut janky);
        let mut count_anys = CountAnys::default();
        janky.walk(&mut count_anys);
        type_check(&janky).expect("result of type inference does not type check");
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

    #[test]
    fn simple_update() {
        let n = typeinf_test(r#"
            var x = 20;
            x = 30 + x;
        "#);
        assert_eq!(n, 0);
    }

    #[test]
    fn any_inducing_update() {
        let n = typeinf_test(r#"
            var x = 20;
            x = true;
        "#);
        assert_eq!(n, 5); // 3 occurrences + 2 conversions
    }

}
