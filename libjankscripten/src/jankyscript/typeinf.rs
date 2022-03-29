//! Type inference for JankyScript, using the TypeWhich approach.
//!
//! See the TypeWhich paper for a high-level overview:
//!
//! https://khoury.northeastern.edu/~arjunguha/main/papers/2021-typewhich.html
//!
//! This module hews closely to the TypeWhich artifact:
//!
//! https://github.com/arjunguha/TypeWhich
//!
//! However, it goes beyond the artifact and paper in a few ways:
//!
//! 1. This module deals with operator overloading in a more sophisticated way. The presentation of
//!    overloading in the TypeWhich paper hand-waved through instruction selection, which is
//!    implemented here.
//! 2. This module supports n-ary functions, which requires a second datatype in Z3 to represent a
//!    list of types.

use super::super::shared::coercions::Coercion;
use super::operators::OVERLOADS;
use super::operators_z3::Z3Operators;
use super::syntax::*;
use super::typeinf_env::Env;
use super::walk::{Loc, Visitor};
use crate::pos::Pos;
use crate::typ;
use crate::z3ez::Z3EZ;
use z3::ast::{self, Ast, Dynamic};
use z3::{Model, Optimize, SatResult};
// paste::paste and crate::z3_data_type_accessor are macros that appear during expansion of
// z3_datatype. Do we really have to import them here? There must be a better way.
use crate::{z3_datatype, z3_datatype_accessor, z3f};
use paste::paste;

z3_datatype! {
    Z3Typ
    (any)
    (int)
    (float)
    (bool)
    (str)
    (array)
    (dynobject)
    // The must_ground field is a trick we use to ground function types. When
    // `e` is a `fun and `(must_ground e)` is `true`, then the arguments are
    // return type are constrained to be `any`. This is accomplished by
    // auxilliary constraints that we define when we construct the type of
    // `Expr::Fun` in `zfun`.
    (fun (must_ground bool) (args (datatype Z3TypList)) (ret (datatype Z3Typ)))
}

z3_datatype! {
    Z3TypList
    (tnil)
    (tcons (thd (datatype Z3Typ)) (ttl (datatype Z3TypList)))
}

struct Typeinf<'a> {
    vars: Vec<Dynamic<'a>>,
    z: Z3Typ<'a>,
    zl: Z3TypList<'a>,
    ops: Z3Operators<'a>,
    solver: &'a Optimize<'a>,
    cxt: &'a z3::Context,
    env: Env,
    return_type: Type,
    trace: bool,
    z3ez: Z3EZ<'a>,
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
    match (e, &src) {
        (Expr::Coercion(Coercion::Meta(src1, Type::Any), e1, p1), Type::Any) => {
            super::constructors::coercion_(Coercion::meta(src1, dst), *e1, p1)
        }
        (e, _) => super::constructors::coercion_(Coercion::meta(src, dst), e, p),
    }
}

struct SubtMetavarVisitor<'a> {
    vars: &'a Vec<Type>,
    ops: &'a Z3Operators<'a>,
    model: &'a Model<'a>,
    z3ez: &'a Z3EZ<'a>,
}

/// Replaces type metavariables with concrete types produced by Z3.
impl<'a> Visitor for SubtMetavarVisitor<'a> {
    fn enter_typ(&mut self, t: &mut Type, _loc: &Loc) {
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
            Expr::Coercion(c, e, _) => {
                if let Coercion::Meta(t1, t2) = c {
                    if t1 == t2 {
                        *expr = e.take();
                    }
                }
            }
            Expr::JsOp(_, arg_es, ti, p) => {
                let p = std::mem::replace(p, Default::default());
                let es = std::mem::replace(arg_es, Default::default());
                let lower_op = self.ops.eval_op(&ti.op_metavar, self.model);
                *expr = lower_op.make_app(es, p);
            }
            _ => {}
        }
    }
}

impl<'a> Typeinf<'a> {
    fn t(&self, t: &Type) -> z3::ast::Dynamic<'a> {
        match t {
            Type::Int => self.z.make_int(),
            Type::Float => self.z.make_float(),
            Type::Any => self.z.make_any(),
            Type::String => self.z.make_str(),
            Type::Bool => self.z.make_bool(),
            Type::Array => self.z.make_array(),
            Type::DynObject => self.z.make_dynobject(),
            Type::Metavar(n) => self
                .vars
                .get(*n)
                .expect("unbound type metavariable")
                .clone(),
            Type::Function(args, r) => {
                // Note that this case doesn't setup the grounding constraints
                // on args and ret. For those constraints, use `zfun`.
                let mut z_args = self.zl.make_tnil();
                let must_ground = z3::ast::Bool::fresh_const(self.z.cxt, "must_ground");
                for a in args.iter().rev() {
                    z_args = self.zl.make_tcons(&self.t(a), &z_args);
                }
                self.z.make_fun(&must_ground.into(), &z_args, &self.t(r))
            }
            _ => todo!("Type: {:?}", t),
        }
    }

    fn zfun(
        &self,
        must_ground: z3::ast::Bool<'a>,
        args: &Vec<Type>,
        r: &Type,
    ) -> z3::ast::Dynamic<'a> {
        let mut z_args = self.zl.make_tnil();
        for a in args.iter().rev() {
            let z_arg = self.t(a);
            self.solver.assert(&z3f!(self,
                (or (not (id must_ground.clone()))
                    (= (id z_arg.clone()) (typ any)))));
            z_args = self.zl.make_tcons(&z_arg, &z_args);
        }
        let z_r = self.t(r);
        self.solver.assert(&z3f!(self,
            (or (not (id must_ground.clone()))
                (= (id z_r.clone()) (typ any)))));
        self.z.make_fun(&must_ground.into(), &z_args, &z_r)
    }

    fn is_ground(&mut self, t: &Type) -> z3::ast::Bool<'a> {
        let t_z = self.t(t);
        let test_fun = self.z.test_fun(&t_z).as_bool().unwrap();
        let fun_must_ground = self.z.fun_must_ground(&t_z).as_bool().unwrap();
        z3f!(self,
            (or (not (id test_fun))
                (id fun_must_ground)))
    }

    fn z3_to_typ_vec(&self, model: &'a Model, mut e: Dynamic<'a>) -> Vec<Type> {
        let mut r = Vec::<Type>::new();
        while !self.zl.is_tnil(&model, &e) {
            let hd = model.eval(&self.zl.tcons_thd(&e)).expect("no head model");
            r.push(self.z3_to_typ(model, hd));
            e = model.eval(&self.zl.tcons_ttl(&e)).unwrap();
        }
        return r;
    }

    fn z3_to_typ(&self, model: &'a Model, e: Dynamic) -> Type {
        if self.z.is_int(model, &e) {
            Type::Int
        } else if self.z.is_float(&model, &e) {
            Type::Float
        } else if self.z.is_bool(&model, &e) {
            Type::Bool
        } else if self.z.is_any(model, &e) {
            Type::Any
        } else if self.z.is_str(model, &e) {
            Type::String
        } else if self.z.is_array(model, &e) {
            Type::Array
        } else if self.z.is_dynobject(&model, &e) {
            Type::DynObject
        } else if self.z.is_fun(&model, &e) {
            let args = model
                .eval(&self.z.fun_args(&e))
                .expect("model for fun_args");
            let ret = model.eval(&self.z.fun_ret(&e)).expect("model for fun_args");
            Type::Function(
                self.z3_to_typ_vec(&model, args),
                Box::new(self.z3_to_typ(&model, ret)),
            )
        } else {
            panic!("{:?}", model.eval(&e));
        }
    }

    fn fresh_weight(&self) -> z3::ast::Bool<'a> {
        let e = z3::ast::Bool::fresh_const(self.z.cxt, "w");
        self.solver.assert_soft(&e, 1, None);
        return e;
    }

    fn fresh_metavar(&mut self, prefix: &'static str) -> Type {
        let x = self.z.fresh(prefix);
        let n = self.vars.len();
        self.vars.push(x.clone());
        return Type::Metavar(n);
    }

    pub fn cgen_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Var(x, t, e, _) => {
                let alpha = self.fresh_metavar("x");
                *t = alpha.clone();
                self.env.update(x.clone(), alpha.clone());
                // TODO(arjun): This is a little hacky, but necessary to deal with function results
                // getting named.
                if !e.is_undefined() {
                    let (phi, t) = self.cgen_expr(e);
                    self.solver.assert(&phi);
                    self.solver.assert(&z3f!(self,
                         (= (tid t) (tid alpha))));
                }
            }
            Stmt::Expr(e, _) => {
                let (phi, _) = self.cgen_expr(&mut *e);
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
            Stmt::Return(e, p) => {
                let (phi, t) = self.cgen_expr(e);
                let w = self.fresh_weight();
                self.solver.assert(&phi);
                let t_r = self.return_type.clone();
                let t_is_ground = self.is_ground(&t);
                self.solver.assert(&z3f!(self,
                    (or (and (id w.clone()) (= (tid t_r.clone()) (tid t.clone())))
                        (and (not (id w))
                             (id t_is_ground)
                             (= (tid t_r.clone()) (typ any))))
                ));
                **e = coerce(t, t_r, e.take(), p.clone());
            }
            Stmt::If(test, then_branch, else_branch, p) => {
                let w = self.fresh_weight();
                let (phi_1, t) = self.cgen_expr(test);
                let phi_2 = z3f!(self,
                    (or (and (id w.clone())
                             (= (tid t.clone()) (typ bool)))
                        (and (not (id w))
                             (= (tid t.clone()) (typ any)))));
                self.solver.assert(&phi_1);
                self.solver.assert(&phi_2);
                **test = coerce(t, typ!(bool), test.take(), p.clone());
                self.cgen_stmt(then_branch);
                self.cgen_stmt(else_branch);
            }
            Stmt::ForIn(x, iter, body, p) => {
                let (phi, t) = self.cgen_expr(iter);
                self.solver.assert(&phi);
                **iter = coerce(t, typ!(string), iter.take(), p.clone());
                let outer_env = self.env.clone();
                self.env.update(x.clone(), typ!(string));
                self.cgen_stmt(body);
                self.env = outer_env;
            }
            Stmt::Break(..) => {
                // Nothing to do
            }
            Stmt::Throw(e, _) => {
                let (phi, t) = self.cgen_expr(e);
                self.solver.assert(&phi);
                self.solver.assert(&z3f!(self, (= (tid t) (typ any))));
            }
            Stmt::Finally(main_block, finally_block, _) => {
                self.cgen_stmt(main_block);
                self.cgen_stmt(finally_block);
            }
        }
    }

    fn cgen_exprs<'b>(
        &mut self,
        exprs: impl Iterator<Item = &'b mut Expr>,
    ) -> (Vec<ast::Bool<'a>>, Vec<Type>) {
        let mut phis = Vec::new();
        let mut ts = Vec::new();
        for e in exprs {
            let (phi, t) = self.cgen_expr(e);
            phis.push(phi);
            ts.push(t);
        }
        (phis, ts)
    }

    // The type of Dot is always any because jankyscript containers hold any
    // -
    // NOTE(luna): Dot is actually not an elimination form in general.
    // We sometimes don't know for sure if we
    // refer to the prototype of a janky-object (Array, Date, ...) or a
    // real DynObject. We hope we can resolve the type from other
    // constraints; if not we have to use a runtime dot function. I
    // want to see how far we can get *without* having to write that (please).
    // -
    // Janky assumption:
    // (No platypus) Array/Date/Strings don't have string fields (besides
    // their prototype); DynObjects don't have numeric fields. My
    // memory is our jankyp run found this mostly possible with some
    // regexes
    // -
    // We'll never type a DynObject with unfortunate-named fields as Array:
    // Consider the unfortunately named field "length" on a DynObject o
    // Then o.length is at some point written, so o is DynObject or any
    fn cgen_dot(&mut self, obj_e: &mut Expr, x: &mut Id, p: &mut Pos) -> ast::Bool<'a> {
        let w = self.fresh_weight();
        let (phi_1, t) = self.cgen_expr(obj_e);
        let e = obj_e.take();
        *obj_e = coerce(t.clone(), Type::DynObject, e, p.clone());
        let phi_2 = z3f!(self,
                    (or
                        (and (= (tid t) (typ dynobject)) (id w.clone()))
                        (id w.clone())
                        (and (= (tid t) (typ any)) (not (id &w)))));
        phi_1 & phi_2
    }

    // The type of Bracket is always any because jankyscript containers hold any
    // NOTE(luna): We "know" from our platypus object jankyp that
    // o[f : string] has o : DynObject and o[f : int] has o : Array
    // and vice versa
    // -
    // Maybe we can say o[f] always has f : int? But that seems pretty
    // strong. It would be nice though, because then we definitely
    // wouldn't have to add an (any, any) -> any bracket operation
    fn cgen_bracket(&mut self, o: &mut Expr, f: &mut Expr, p: &mut Pos) -> ast::Bool<'a> {
        let (phi_1, ot) = self.cgen_expr(o);
        let (phi_2, ft) = self.cgen_expr(f);
        // final container type
        let otf = self.fresh_metavar("o");
        let ftf = self.fresh_metavar("f");
        let wcoerce = self.fresh_weight();
        let wrt = self.fresh_weight();
        // a : S -> b : T   or
        // a : S -> coerce(any, T) b
        // af = "a final"
        let known = |a: &Type, af: &Type, s: &Type, b: &Type, bf: &Type, t: &Type| -> ast::Bool {
            z3f!(self, (and
                        (= (tid a) (tid af) (tid s))
                        (= (tid bf) (tid t))
                        (or
                            (and (= (tid b) (tid t)) (id wcoerce.clone()))
                            (and (= (tid b) (typ any)) (not (id wcoerce.clone()))))
                        (id wrt.clone())))
        };
        // o : DynObject implies f may be coerced to string
        // (but o : DynObject doesn't imply f : string; f may be any)
        let phi_3 = known(&ot, &otf, &Type::DynObject, &ft, &ftf, &Type::String)
                    // f : string implies o may be coerced to DynObject
                    | known(&ft, &ftf, &Type::String, &ot, &otf, &Type::DynObject)
                    // o : Array implies f may be coerced to int
                    | known(&ot, &otf, &Type::Array, &ft, &ftf, &Type::Int)
                    // f : int implies o may be coerced to Array
                    | known(&ft, &ftf, &Type::Int, &ot, &otf, &Type::Array)
                    // o:any[f:any]. hopefully this doesn't happen in practice
                    | z3f!(self, (and
                        (= (tid ot) (tid otf) (typ any))
                        (= (tid ft) (tid ftf) (typ any))
                        (not (id wrt))));
        let cont = o.take();
        *o = coerce(ot, otf, cont, p.clone());
        let field = f.take();
        *f = coerce(ft, ftf, field, p.clone());
        phi_1 & phi_2 & phi_3
    }

    // Coerce expr: t at p to either t (preferred) or any. Returns
    // (phis & additional, t or any). Only coerces to any when t is ground
    // NOTE(luna): If we end up with z3 performance concerns, not checking for
    // ground on known types like literals would likely help
    fn wobbly<B: Into<Option<ast::Bool<'a>>>>(
        &mut self,
        p: Pos,
        expr: &mut Expr,
        phis: B,
        t: Type,
    ) -> (ast::Bool<'a>, Type) {
        let w = self.fresh_weight();
        let alpha = self.fresh_metavar("wobbly");
        let is_ground = self.is_ground(&t);
        let phi = z3f!(self,
            (or (and (id w.clone()) (= (tid alpha) (tid t)))
                (and (not (id w))
                    (= (tid alpha) (typ any))
                    (id is_ground))));
        let e = expr.take();
        *expr = coerce(t, alpha.clone(), e, p);
        match phis.into() {
            Some(phis) => (phis & phi, alpha),
            None => (phi, alpha),
        }
    }

    fn zand(&self, phis: Vec<ast::Bool<'a>>) -> ast::Bool<'a> {
        let phis = phis.iter().collect::<Vec<_>>();
        ast::Bool::and(self.z.cxt, phis.as_slice())
    }

    fn zor(&self, phis: &[&ast::Bool<'a>]) -> ast::Bool<'a> {
        ast::Bool::or(self.z.cxt, phis)
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
                self.wobbly(p.clone(), expr, None, t)
            }
            Expr::Array(es, p) => {
                let (phi_vec, ts) = self.cgen_exprs(es.iter_mut());
                let phis = self.zand(
                    phi_vec
                        .into_iter()
                        .chain(ts.iter().map(|x| z3f!(self, (= (tid x) (typ any)))))
                        .collect(),
                );
                self.wobbly(p.clone(), expr, phis, Type::Array)
            }
            Expr::Object(props, p) => {
                let p = p.clone();
                let (mut phis, ts) = self.cgen_exprs(props.iter_mut().map(|(_, e)| e));
                for t in ts {
                    // See note for array elements: same principle applies here.
                    phis.push(z3f!(self, (= (tid t) (typ any))));
                }
                self.wobbly(p.clone(), expr, self.zand(phis), Type::DynObject)
            }
            Expr::Id(x, t, p) => {
                // NOTE(luna): A brief argument for wobbly/non-rigid vars:
                // 1. The usual advantage of rigid vars is more consistent/sensical
                //    types. This makes sense in a documentation context, but not in
                //    a performance context where types will likely not be seen
                // 2. In jankscripten in particular, we desugar x.f() as x.f(x)
                //    This is for supporting `this`. When f has any -> any,
                //    this forces x : any when x is clearly dynobject. Since this
                //    happens so much, either we should allow the first argument of
                //    ground functions to be DynObject, or allow wobbly vars. The latter
                //    is easier, and both could be done eventually
                // 3. This could be worked around, but when x comes from
                //    std_lib, we don't get to decide it has type any, because we
                //    already specified it has type DynObject. This is a limitation
                //    of TypeWhich that we never addressed (partially annotated
                //    programs are not guaranteed to migrate)
                *t = self.env.get(x);
                let t = t.clone();
                self.wobbly(p.clone(), expr, z3f!(self, true), t)
            }
            Expr::Dot(obj_e, x, p) => (self.cgen_dot(obj_e, x, p), Type::Any),
            Expr::Bracket(o, f, p) => (self.cgen_bracket(o, f, p), Type::Any),
            Expr::JsOp(op, args, JsOpTypeinf { op_metavar }, p) => {
                let w = self.fresh_weight();
                // Fresh metavariable for the operator that we will select, stored in the AST for
                // the next phase.
                *op_metavar = self.ops.fresh_op_selector();
                // Fresh type metavariable for the result of this expression
                let alpha_t = self.fresh_metavar("alpha");
                // Recur into each argument and unzip Z3 constants and our type metavars
                let args_rec = args.iter_mut().map(|e| self.cgen_expr(e));
                let (mut args_phi, args_t): (Vec<_>, Vec<_>) = args_rec.unzip();
                // Fresh type metavariables for each argument
                let mut betas_t = Vec::new();
                for (arg, arg_t) in args.iter_mut().zip(&args_t) {
                    let a = arg.take();
                    let beta_t = self.fresh_metavar("beta");
                    *arg = coerce(arg_t.clone(), beta_t.clone(), a, Default::default());
                    betas_t.push(beta_t);
                }
                // In DNF, one disjunct for each overload
                let mut disjuncts = Vec::new();
                let mut one_possibility = |t: &Type, notwasm_op, allow_coerce| {
                    let (op_arg_t, op_ret_t) = t.unwrap_fun();
                    // For this overload, arguments and result must match
                    let mut conjuncts = vec![
                        z3f!(self, (= (id self.ops.z(&op_metavar)) (id self.ops.z(notwasm_op)))),
                    ];
                    for ((t1, t2), t3) in args_t.iter().zip(op_arg_t).zip(betas_t.iter()) {
                        if allow_coerce {
                            // If we allow coerce, we still prefer not to
                            // coerce (ie, so we don't coerce through any for no
                            // reason)
                            conjuncts.push(z3f!(self, (or
                                (and (not (id w.clone())) (= (tid t1) (typ any)))
                                (= (tid t1) (tid t2)))));
                        } else {
                            conjuncts.push(z3f!(self, (= (tid t1) (tid t2))));
                        }
                        conjuncts.push(z3f!(self, (= (tid t2) (tid t3))));
                    }
                    conjuncts.push(self.t(&alpha_t)._eq(&self.t(op_ret_t)));
                    disjuncts.push(self.zand(conjuncts));
                };
                // This is the case where we perform no coercions. This
                // includes the any case! The coercions are done on the
                // expressions themselves when asserted to be any
                for (t, notwasm_op) in OVERLOADS.overloads(op) {
                    one_possibility(t, notwasm_op, false);
                }
                // Special case: we allow a coercion from any on operators with
                // only one reasonable type. We assume the target type is
                // ground because we wrote it so in operators.rs
                if let Some((t, notwasm_op)) = OVERLOADS.coercible(op) {
                    one_possibility(t, notwasm_op, true);
                }
                let cases =
                    ast::Bool::or(self.z.cxt, disjuncts.iter().collect::<Vec<_>>().as_slice());
                args_phi.push(cases);
                self.wobbly(p.clone(), expr, self.zand(args_phi), alpha_t)
            }
            Expr::Assign(lval, e, p) => match &mut **lval {
                LValue::Id(x, x_t) => {
                    let t = self.env.get(x);
                    *x_t = t.clone();
                    let (phi_1, e_t) = self.cgen_expr(&mut *e);
                    // TODO(arjun): Can we get away with strict equality? If so, we must ensure that
                    // the type of any expression may be any. This is already the case for literals,
                    // function applications, and getting values from collections. However, what
                    // about builtin operators? It seems reasonable to same that e1 === e2 *always*
                    // produces a boolean and does not produce an any.
                    //
                    // However, introducing coercions to any in arbitrary places, such as here, can
                    // break object identity. It is safer to coerce to any *only* where we introduce
                    // new values, including the values produced by builtin operators.
                    let phi_2 = self.t(&t)._eq(&self.t(&e_t));
                    (phi_1 & phi_2, t)
                }
                LValue::Dot(c, f) => {
                    let (phi_1, e_t) = self.cgen_expr(&mut *e);
                    let phi_2 = self.cgen_dot(c, f, p);
                    let phi_3 = z3f!(self, (= (tid e_t) (typ any)));
                    (phi_1 & phi_2 & phi_3, Type::Any)
                }
                LValue::Bracket(o, f) => {
                    let (phi_1, e_t) = self.cgen_expr(&mut *e);
                    let phi_2 = self.cgen_bracket(o, f, p);
                    let phi_3 = z3f!(self, (= (tid e_t) (typ any)));
                    (phi_1 & phi_2 & phi_3, Type::Any)
                }
            },
            Expr::Call(f, args, p) => {
                let w_1 = self.fresh_weight();
                let w_2 = self.fresh_weight();
                let (phi_1, t_f) = self.cgen_expr(f);
                let (args_phi, args_t) = self.cgen_exprs(args.iter_mut());
                let phi_2 = self.zand(args_phi);
                // Name for the result type of f(args)
                let beta = self.fresh_metavar("beta");
                let gamma = self.fresh_metavar("gamma");

                let phi_31 = self.zand(
                    args_t
                        .iter()
                        .map(|x| z3f!(self, (= (tid x) (typ any))))
                        .collect(),
                );
                let phi_3 = z3f!(self,
                    (or
                        (and (= (tid t_f) (typ fun_vec(args_t.clone()) -> unquote(beta.clone())))
                             (id w_1.clone()))
                        (and (= (tid t_f) (typ any))
                             (= (tid beta) (typ any))
                             (id phi_31)
                             (not (id w_1)))));
                // NOTE(luna): Does this properly enforce ground constraints?
                let phi_4 = z3f!(self,
                    (or (and (= (tid beta) (tid gamma)) (id w_2.clone()))
                        (and (= (tid gamma) (typ any)) (not (id w_2)))));
                **f = coerce(
                    t_f,
                    typ!(fun_vec(args_t) -> unquote(beta.clone())),
                    f.take(),
                    p.clone(),
                );
                let p = p.clone();
                let e = expr.take();
                *expr = coerce(beta, gamma.clone(), e, p);
                (self.zand(vec![phi_1, phi_2, phi_3, phi_4]), gamma)
            }
            // TypeWhich shows us how to do unary functions. This is a significant generalization.
            Expr::Func(f, p) => {
                // Fudge stack with local state: the function body will
                // update the environment and the return type.
                let outer_env = self.env.clone();
                let outer_return_typ = self.return_type.take();
                // Fresh metavariable for the return type.
                self.return_type = self.fresh_metavar("ret");
                f.result_typ = self.return_type.clone();
                // Fresh metavariables for formal arguments.
                for (x, t) in f.args_with_typs.iter_mut() {
                    assert_eq!(t, &Type::Missing);
                    *t = self.fresh_metavar("arg");
                    self.env.update(x.clone(), t.clone());
                }
                // Recur into the body.
                self.cgen_stmt(&mut *f.body);
                // Get the return type.
                let return_typ = self.return_type.take();
                // Pop the fudged stack: restore outer environment and return type.
                self.return_type = outer_return_typ;
                self.env = outer_env;

                let args: Vec<Type> = f.args_with_typs.iter().map(|(_, t)| t.clone()).collect();

                let w = self.fresh_weight();
                let beta = self.fresh_metavar("beta");
                // When `(not w)` is true, the auxiliary grounding assertions
                // produced by `zfun` force `args` and `return_typ` to be `any`.
                let z_fun = self.zfun(z3f!(self, (not (id w.clone()))), &args, &return_typ);

                let phi = z3f!(self,
                    (or
                      (and (id w.clone()) (= (tid beta) (id z_fun.clone())))
                      (and (not (id w.clone())) (= (tid beta) (typ any)))));
                let p = p.clone();
                *expr = coerce(
                    typ!(fun_vec(args) -> unquote(return_typ)),
                    beta.clone(),
                    expr.take(),
                    p,
                );
                (phi, beta)
            }
        }
    }

    fn solve_model(&self, model: &z3::Model) -> Vec<Type> {
        let mut result = Vec::new();
        for x_ast in self.vars.iter() {
            let x_val_ast = model.eval(x_ast).expect("evaluating metavar");
            result.push(self.z3_to_typ(&model, x_val_ast));
        }
        result
    }
}

pub fn typeinf(stmt: &mut Stmt) {
    let z3_cfg = z3::Config::new();
    let cxt = z3::Context::new(&z3_cfg);
    let bool_sort = z3::Sort::bool(&cxt);
    let dts = Z3Typ::make_dts(&cxt, &bool_sort);
    let dts_list = Z3TypList::make_dts(&cxt, &bool_sort);
    let sorts = z3::datatype_builder::create_datatypes(vec![dts, dts_list]);
    let z = Z3Typ::new(&cxt, &sorts[0]);
    let zl = Z3TypList::new(&cxt, &sorts[1]);
    let ops = Z3Operators::new(&OVERLOADS, &cxt);
    let env = Env::new();
    let trace = false;
    let solver = Optimize::new(&cxt);
    let z3ez = Z3EZ::new(&cxt, &solver);
    let mut state = Typeinf {
        trace,
        vars: Default::default(),
        z,
        zl,
        z3ez,
        cxt: &cxt,
        ops,
        solver: &solver,
        // Cannot have return statement at top-level
        return_type: Type::Missing,
        env,
    };
    state.cgen_stmt(stmt);
    if state.trace {
        println!("Before subst: {}", &stmt);
        println!("{:?}", state.solver);
    }
    match state.solver.check(&[]) {
        SatResult::Unknown => panic!("Got an unknown from Z3"),
        SatResult::Unsat => {
            println!("Constraints:\n{}", state.solver);
            panic!("type inference failed (unsat)")
        }
        SatResult::Sat => (),
    };
    let model = state
        .solver
        .get_model()
        .expect("model not available (despite SAT result)");
    let mapping = state.solve_model(&model);

    let mut subst_metavar = SubtMetavarVisitor {
        z3ez: &state.z3ez,
        vars: &mapping,
        model: &model,
        ops: &state.ops,
    };
    stmt.walk(&mut subst_metavar);
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
    struct CountToAnys {
        num_anys: usize,
    }

    impl Visitor for CountToAnys {
        fn enter_typ(&mut self, t: &mut Type, loc: &Loc) {
            match (loc, t) {
                (Loc::Node(Context::MetaCoercionRight(..), _), Type::Any) => {
                    self.num_anys += 1;
                }
                _ => {}
            }
        }
    }

    fn typeinf_test(s: &str) -> usize {
        let mut js = parse("<text>", s).expect("error parsing JavaScript");
        let mut ng = NameGen::default();
        desugar(&mut js, &mut ng);
        let mut janky = crate::jankyscript::from_js::from_javascript(js);
        println!("after from_js: {}", janky);
        typeinf(&mut janky);
        let mut count_anys = CountToAnys::default();
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
        let n = typeinf_test(
            r#"
            var x = 20;
            x = 30 + x;
        "#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn any_inducing_update() {
        let n = typeinf_test(
            r#"
            var x = 20;
            x = true;
        "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn heterogenous_array() {
        let n = typeinf_test(
            r#"
            [10, "hi", true]
        "#,
        );
        assert_eq!(n, 3);
    }

    #[test]
    fn object_lit() {
        let n = typeinf_test(
            r#"
            ({ x: 10, y: 20 })
        "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn prop_read() {
        let n = typeinf_test(
            r#"
            ({x : 10}).y << 2
            "#,
        );
        assert_eq!(n, 1); // We coerce the 10 to any. The y is coerced *from* any
    }

    #[test]
    fn id_trivial_app() {
        let n = typeinf_test(
            r#"
            function F(x) {
                return x;
            }
            F(100)
            "#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn poly_id() {
        let n = typeinf_test(
            r#"
            function F(x) {
                return x;
            }
            F(100);
            F(true);
            "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn inc_fn() {
        let n = typeinf_test(
            r#"
            function F(x) {
                return x + 1; // In the generated constraint, the type of x is a metavariable
            }
            F(10);"#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn fac() {
        let n = typeinf_test(
            r#"
            function F(x) {
                if ( x === 0) {
                    return 1;
                }
                else {
                    return x * F(x - 1);
                }
            }
            F(100);
            "#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn escaping_fun() {
        let n = typeinf_test(
            r#"
            function f(x) {
                return x;
            }
            var o = { g: f };
            //f(1);
            o.g(true);
            "#,
        );
        assert_eq!(n, 3); // 1) the function, 2) the object, 3) the bool
    }

    #[test]
    fn higher_order_app_err() {
        let n = typeinf_test(
            r#"
            function f(g, x) {
                return g(x);
            }
            f(1, 2);
            "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn heterogenous_return_types_must_be_ground() {
        let n = typeinf_test(
            r#"
            function F() {
                if (true) {
                    return function(x) { return x << 2; }
                }
                else {
                    return "hi";
                
                }
            }
            "#,
        );
        // - Nested function is coerced to any
        // - x is coerced to a number (does not count, we are counting *to* any)
        // - x << 2 is coerced to any
        // - "hello" is coerced to any
        assert_eq!(n, 3);
    }

    #[test]
    fn funarg_to_operator_must_be_ground() {
        let n = typeinf_test(
            r#"
            function id(x) {
                return x;
            }
            function F(x) { // F is coerced to any
                return x * 10; // result is coerced to any
            }
            id(F)(id(200)); // 200 is coerced to any
            F(300); // 300 is coerced to any
            "#,
        );
        assert_eq!(n, 4);
    }

    #[test]
    fn bracket_produces_any() {
        let n = typeinf_test(
            r#"
              let arr = [1, 2, 3]; // 3 coercions
              arr[0] + 2; // 1 coercion
              "#,
        );
        assert_eq!(n, 4);
    }

    #[test]
    fn bracket_can_consume_any() {
        let n = typeinf_test(
            r#"
              function F(x) {
                  return x[0] * x[0];
              }
              F(""); // coercion from "" to any
              F([]); // coercion from [] to any
            "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn variable_may_be_any_typed() {
        let n = typeinf_test(
            r#"
            var x = 10;
            x = "hello";
            x = true;
            "#,
        );
        assert_eq!(n, 3);
    }

    #[test]
    fn variable_may_be_any_typed_2() {
        let n = typeinf_test(
            r#"
            var x = (2 === 3); // ===->any
            x = "hello"; // "hello"->any
            "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn array_length() {
        let n = typeinf_test("[undefined, undefined, undefined].length");
        assert_eq!(n, 0);
    }

    #[test]
    fn array_push() {
        let n = typeinf_test("[].push(undefined)");
        // the coercion is hidden in `this` desugar:
        // let obj4this = [];
        // obj4this.push([] AS ANY, undefined);
        assert_eq!(n, 1);
    }

    #[test]
    fn array_bracket() {
        let n = typeinf_test(
            r#"
            let a = [];
            let i = 2;
            (a[i]);
            "#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn obj_bracket() {
        let n = typeinf_test(
            r#"
            let a = {};
            let i = "x";
            (a[i]);
            "#,
        );
        assert_eq!(n, 0);
    }

    #[test]
    fn obj_bracket_through_any() {
        let n = typeinf_test(
            r#"
            let a = {};
            let i = 2;
            (a[i]);
            "#,
        );
        assert_eq!(n, 1);
    }

    #[test]
    fn any_obj_dot_assign() {
        let n = typeinf_test(
            r#"
            let a = undefined;
            a = {}; // {} as any
            a.x = 2; // 2 as any
            "#,
        );
        assert_eq!(n, 2);
    }

    #[test]
    fn array_bracket_assign_through_any() {
        let n = typeinf_test(
            r#"
            let a = [];
            let i = "2"; // "2" as any (i as int)
            a[i] = 10; // 10 as any
            "#,
        );
        assert_eq!(n, 2);
    }
}
