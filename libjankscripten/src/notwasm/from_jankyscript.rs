//! This module compiles JankyScript to NotWasm. This requires a transformation that is akin to
//! *A-normalization*:
//!
//! Cormac Flanagan, Amr Sabry, Bruce F. Duba, and Matthias Felleisen.
//! The Essence of Compiling with Continuations. PLDI 1993.
//!
//! However, we use a variation of what the aforementioned paper presents that produces cleaner
//! code. We'll explain this variation using a small language in A Normal Form. The language does
//! not support tail calls, but JankyScript doesn't either. We'll present the A-normalization
//! algorithm using Haskell-ish notation.
//!
//! This is our source language:
//!
//! ```ignore
//! expr ::= c | x | lambda x . expr | expr1(expr2) | expr1 + expr2 | let x = expr1 in expr2
//!
//! ```
//!
//! This is our target language, which closely corresponds to A Normal Form:
//!
//! ```ignore
//! a ::= c | x | lambda x . e
//! b ::= a | a1 + a2 | a1(a2)
//! e ::= let x = b in e | a
//! ```
//!
//! Original program: `1 + (2 * 3)`
//!
//! Compiled program: `let tmp = 2 * 3 in let tmp1 = 1 + tmp in tmp1`
//!
//!
//! The following function translates from the source to target. (Use `id` as the initial value for
//! `k`):
//!
//! ```ignore
//! anf : expr -> (a -> e) -> e
//! anf c k = k c
//! and x k = k x
//! anf (e1 e2) k = anf e1 (\x1 -> anf e2 (\x2 -> let x = x1(x2) in k x))
//! anf (lambda x . e) k = k (lambda x . anf e (\x -> x))
//! anf (e1 + e2) k =  anf e1 (\x1 -> anf e2 (\x2 -> let r = x1 + x2 in k r))
//! anf (let x = e1 in e2) k = anf e1 (\y -> let x = y in (anf e2 k))
//! ```
//!
//! To compile `expr`, run `anf expr (\a -> a)`.
//!
//! Unfortunately, the previous function introduces a lot of useless names. For example:
//!
//! ```ignore
//!   anf (let x = 1 + 2 in x) id
//! = anf (1 + 2) (\y -> let x = y in anf x id)
//! = anf (1 + 2) (\y -> let x = y in x)
//! = anf 1 (\x1 -> anf 2 (\x2 -> let r = x1 + x2 in (\y -> let x = y in x) r))
//! = anf 1 (\x1 -> anf 2 (\x2 -> let r = x1 + x2 in let x = r in x))
//! = anf 1 (\x1 -> let r = x1 + 2 in let x = r in x)
//! = let r = 1 + 2 in (let x = r in x)
//! ```
//!
//! We can address this by introducing two kinds of contexts: 1) the (a -> e) context receives an
//! a-value as shown above, and 2) a (b -> e) context that receives a b-expression.
//!
//! ```ignore
//! data Context = AContext (a -> e) | BContext (b -> e)
//!
//! anf : expr -> Context -> expr
//! anf (e1 + e2) k = anf e1 (AContext (\x1 ->
//!   anf e2 (AContext (\x2 ->
//!     case k of
//!       BContext k' -> k' (x1 + x2)
//!       AContext k' -> let r = x1 + x2 in k' r))))
//! anf (let x = e1 in e2) k = anf e1 (BContext (\b -> let x = b in e2 k))
//! ...
//! ```
//!
//! Instead of case-splitting on kind of context when we need to use it, we can define two helper
//! functions:
//!
//! ```ignore
//! recv_b : Context -> b -> e
//! recv_b (BContext k) b = k b
//! recv_b (AContext k) b = let x = b in k x -- x is fresh
//!
//! recv_a : Context -> a -> e
//! recv_a (AContext k) a = k a
//! recv_a (BContext k) a = k a -- all a-values can be injected into b-expressions
//! ```
//!
//! When we compile from JankyScript to NotWasm, the two kinds of contexts receive `Syntax::Atom` and
//! `Syntax::Expr`, and the output of A-normalization is a `Syntax::Stmt`.
//!
//! To summarize, A-normalization requires:
//! - Generating fresh names
//! - Two kinds of contexts

use super::super::jankyscript::syntax as J;
use super::super::rope::Rope;
use super::constructors::*;
use super::syntax::*;
use std::collections::HashMap;

fn compile_lit(lit: J::Lit) -> Lit {
    match lit {
        J::Lit::String(s) => Lit::String(s),
        J::Lit::Regex(_, _) => todo!("regex not supported anywhere in toolchain"),
        J::Lit::Bool(b) => Lit::Bool(b),
        J::Lit::Null => todo!("NotWasm needs a null primitive"),
        J::Lit::Undefined => Lit::Undefined,
        J::Lit::Num(J::Num::Int(n)) => Lit::I32(n),
        J::Lit::Num(J::Num::Float(x)) => Lit::F64(x),
    }
}

/// State that is needed during A-normalization
#[derive(Default)]
struct S {
    namegen: super::super::javascript::NameGen,
    functions: HashMap<Id, Function>,
}

impl S {
    fn fresh(&mut self) -> Id {
        self.namegen.fresh("anf")
    }

    fn new_function(&mut self, name: Id, f: Function) {
        // TODO(arjun): we will have to rename functions if there are two with the same name in
        // different scopes.
        self.functions.insert(name, f);
    }
}

/// The contexts for A-normalization
enum C<'a> {
    /// Context expects an `Id`. So, name the result before passing it to the context.
    Id(Box<dyn FnOnce(&'a mut S, Id) -> Rope<Stmt> + 'a>),
    /// Context expects an `Atom`. If it isn't, name the result before passing it to the context.
    Atom(Box<dyn FnOnce(&'a mut S, Atom) -> Rope<Stmt> + 'a>),
    /// Context expects an `Expr`. This is the easy case, since an `Atom` or `Id` can be injected
    /// into an `Expr`.
    Expr(Box<dyn FnOnce(&'a mut S, Expr) -> Rope<Stmt> + 'a>),
}

impl<'a> C<'a> {
    /// Constructs a `C::Atom`, taking care of boxing the function.
    fn a(f: impl FnOnce(&'a mut S, Atom) -> Rope<Stmt> + 'a) -> C<'a> {
        C::Atom(Box::new(f))
    }

    /// Constructs a `C::Expr`, taking care of boxing the function.
    fn e(f: impl FnOnce(&'a mut S, Expr) -> Rope<Stmt> + 'a) -> C<'a> {
        C::Expr(Box::new(f))
    }

    /// Constructs a `C::Id`, taking care of boxing the function.
    fn id(f: impl FnOnce(&'a mut S, Id) -> Rope<Stmt> + 'a) -> C<'a> {
        C::Id(Box::new(f))
    }

    fn recv_a(self, s: &'a mut S, a: Atom) -> Rope<Stmt> {
        match self {
            C::Atom(f) => f(s, a),
            C::Id(f) => match a {
                Atom::Id(x) => f(s, x),
                _ => {
                    let x = s.fresh();
                    Rope::singleton(Stmt::Var(VarStmt::new(x.clone(), Expr::Atom(a))))
                        .append(f(s, x))
                }
            },
            C::Expr(f) => f(s, Expr::Atom(a)),
        }
    }

    fn recv_e(self, s: &'a mut S, e: Expr) -> Rope<Stmt> {
        match self {
            // The Id and Atom cases are essentially identical
            C::Id(f) => {
                let x = s.fresh();
                Rope::singleton(Stmt::Var(VarStmt::new(x.clone(), e))).append(f(s, x))
            }
            C::Atom(f) => {
                let x = s.fresh();
                Rope::singleton(Stmt::Var(VarStmt::new(x.clone(), e))).append(f(s, Atom::Id(x)))
            }
            C::Expr(f) => f(s, e),
        }
    }
}

/// Compile a vector of expressions, name them, and send their names (in a vector) to a context.
fn compile_exprs<'a>(
    s: &'a mut S,
    exprs: Vec<J::Expr>,
    cxt: impl FnOnce(&'a mut S, Vec<Id>) -> Rope<Stmt>,
) -> Rope<Stmt> {
    let mut ids = Vec::<Id>::new();
    let mut stmts = Rope::new();
    for e in exprs.into_iter() {
        stmts = stmts.append(compile_expr(
            s,
            e,
            C::id(|s, x| {
                ids.push(x);
                Rope::Nil
            }),
        ));
    }
    return stmts.append(cxt(s, ids));
}

pub fn compile_ty(janky_typ: J::Type) -> Type {
    match janky_typ {
        J::Type::Any => Type::Any,
        J::Type::Bool => Type::Bool,
        J::Type::Int => Type::I32,
        J::Type::Float => Type::F64,
        J::Type::DynObject => Type::DynObject,
        J::Type::Function(params, ret) => fn_ty_(
            params.into_iter().map(|jt| compile_ty(jt)).collect(),
            Some(compile_ty(*ret)),
        ),
        J::Type::Array => Type::Array,
        // TODO(luna): this isn't entirely correct
        J::Type::String => Type::StrRef,
        _ => todo!("compile_ty {:?}", janky_typ),
    }
}

fn coercion_to_expr(c: J::Coercion, a: Atom) -> Atom {
    use J::Coercion::*;
    match c {
        FloatToInt => Atom::FloatToInt(Box::new(a)),
        IntToFloat => Atom::IntToFloat(Box::new(a)),
        Tag(..) => to_any_(a),
        Untag(ty) => from_any_(a, compile_ty(ty)),
        Fun(..) => todo!(), // TODO(michael) needs to call something that proxies the function
        Id(..) => a,
        Seq(c1, c2) => coercion_to_expr(*c2, coercion_to_expr(*c1, a)),
    }
}

fn compile_expr<'a>(s: &'a mut S, expr: J::Expr, cxt: C<'a>) -> Rope<Stmt> {
    match expr {
        J::Expr::Lit(lit) => cxt.recv_a(s, Atom::Lit(compile_lit(lit))),
        J::Expr::Array(members) => compile_exprs(s, members, move |s, member_ids| {
            let array_name = s.fresh();
            let mut rv = Rope::singleton(Stmt::Var(VarStmt::new(array_name.clone(), Expr::Array)));
            for member_id in member_ids {
                rv = rv.append(Rope::singleton(Stmt::Expression(Expr::Push(
                    Atom::Id(array_name.clone()),
                    Atom::Id(member_id),
                ))))
            }
            rv.append(cxt.recv_a(s, Atom::Id(array_name)))
        }),
        J::Expr::Object(keys_exprs) => {
            let (keys, exprs): (Vec<_>, Vec<_>) = keys_exprs.into_iter().unzip();
            compile_exprs(s, exprs, move |s, ids| {
                // TODO: semi-static classes when objects are defined like this
                let obj_name = s.fresh();
                let mut rv =
                    Rope::singleton(Stmt::Var(VarStmt::new(obj_name.clone(), Expr::ObjectEmpty)));
                for (key, id) in keys.into_iter().zip(ids) {
                    let key_str = match key {
                        J::Key::Str(s) => s,
                        J::Key::Int(_) => todo!(),
                    };
                    rv = rv.append(Rope::singleton(Stmt::Expression(Expr::ObjectSet(
                        Atom::Id(obj_name.clone()),
                        str_(key_str),
                        Atom::Id(id),
                    ))))
                }
                rv.append(cxt.recv_a(s, Atom::Id(obj_name)))
            })
        }
        J::Expr::This => todo!("we need to think more deeply about this"),
        J::Expr::Dot(obj, field) => compile_expr(
            s,
            *obj,
            C::a(move |s, obj| cxt.recv_a(s, object_get_(obj, str_(field.into_name())))),
        ),
        J::Expr::Unary(op, expr) => {
            compile_expr(s, *expr, C::a(move |s, a| cxt.recv_a(s, unary_(op, a))))
        }
        J::Expr::New(_, _) => todo!("new -- need deep thought"),
        // TODO(luna): i think JankyScript bracket supports like
        // object/hashtable fetch by name, so we have to descriminate based
        // on type or something(?)
        J::Expr::Bracket(arr, index) => compile_expr(
            s,
            *arr,
            C::a(move |s, arr| {
                compile_expr(
                    s,
                    *index,
                    C::a(move |s, index| cxt.recv_a(s, index_(arr, index))),
                )
            }),
        ),
        J::Expr::Coercion(coercion, e) => compile_expr(
            s,
            *e,
            C::a(move |s, a| cxt.recv_a(s, coercion_to_expr(coercion, a))),
        ),
        J::Expr::Id(x) => cxt.recv_a(s, Atom::Id(x)),
        J::Expr::Func(ret_ty, args_tys, body) => {
            let (param_names, param_tys): (Vec<_>, Vec<_>) = args_tys.into_iter().unzip();
            let function = Function {
                body: Stmt::Block(compile_stmt(s, *body).into_iter().collect()),
                params: param_names,
                fn_type: FnType {
                    args: param_tys.into_iter().map(|t| compile_ty(t)).collect(),
                    result: Some(Box::new(compile_ty(ret_ty))),
                },
            };
            let f = s.fresh();
            s.new_function(f.clone(), function);
            cxt.recv_a(s, Atom::Id(f))
        }
        J::Expr::Binary(op, e1, e2) => compile_expr(
            s,
            *e1,
            C::a(move |s, a1| {
                compile_expr(
                    s,
                    *e2,
                    C::a(|s, a2| cxt.recv_a(s, Atom::Binary(op, Box::new(a1), Box::new(a2)))),
                )
            }),
        ),
        J::Expr::Assign(lv, e) => compile_expr(
            s,
            *e,
            // TODO(luna): if we change Assign to an expression, we can make
            // this C::e and drop the clone which will generate less useless
            // locals; but it will mean sometimes dropping values. we
            // could also change Assign to an atom, which would mean
            // introducing new locals for assignment expressions
            // but differently. see this discussion on slack:
            // https://plasma.slack.com/archives/C013E3BK7QA/p1596656877066800
            C::a(|s, a| match *lv {
                J::LValue::Id(id) => {
                    Rope::singleton(Stmt::Assign(id, atom_(a.clone()))).append(cxt.recv_a(s, a))
                }
                J::LValue::Dot(container, field) => {
                    // TODO(luna): don't assume bracket is array
                    compile_expr(
                        s,
                        container,
                        // TODO(luna): support array set in notwasm, i can't
                        // believe we don't yet
                        C::a(move |s, cont| {
                            cxt.recv_e(
                                s,
                                Expr::ObjectSet(
                                    cont,
                                    Atom::Lit(Lit::String(field.to_pretty(80))),
                                    a,
                                ),
                            )
                        }),
                    )
                }
                J::LValue::Bracket(container, field) => {
                    // TODO(luna): don't assume bracket is array
                    compile_expr(
                        s,
                        container,
                        C::a(move |s, cont| {
                            compile_expr(
                                s,
                                field,
                                C::a(move |s, f| cxt.recv_e(s, Expr::ArraySet(cont, f, a))),
                            )
                        }),
                    )
                }
            }),
        ),
        J::Expr::PrimCall(prim_name, args) => compile_exprs(s, args, move |s, arg_ids| {
            cxt.recv_e(
                s,
                Expr::PrimCall(
                    prim_name,
                    arg_ids.into_iter().map(|x| Atom::Id(x)).collect(),
                ),
            )
        }),
        J::Expr::Call(fun, args) => compile_expr(
            s,
            *fun,
            C::id(move |s, fun_id| {
                compile_exprs(s, args, move |s, arg_ids| {
                    cxt.recv_e(s, Expr::Call(fun_id, arg_ids))
                })
            }),
        ),
    }
}

// TODO(luna): remove this when we think we've completed this
#[allow(unused)]
fn compile_stmt<'a>(s: &'a mut S, stmt: J::Stmt) -> Rope<Stmt> {
    use J::Stmt as S;
    match stmt {
        // In JankyScript:
        //
        // var r = f() + 1;
        //
        // In NotWasm:
        //
        // var tmp = f();
        // var r = tmp + 1;
        S::Var(x, t, e) => compile_expr(
            s,
            *e,
            C::e(|s, e_notwasm| Rope::singleton(Stmt::Var(VarStmt::new(x, e_notwasm)))),
        ),
        S::Block(stmts) => Rope::singleton(Stmt::Block(
            stmts
                .into_iter()
                .map(|stmt| compile_stmt(s, stmt))
                .flatten()
                .collect(),
        )),
        S::Empty => Rope::singleton(Stmt::Empty),
        S::Expr(e) => compile_expr(
            s,
            *e,
            // We could use a C::e context. However, the C::a context will make generated code
            // easier to understand in trivial examples. A C::e context would discard useless
            // binary operations.
            C::a(|_s, _a_notwasm| Rope::Nil),
        ),
        S::If(cond, then_branch, else_branch) => compile_expr(
            s,
            *cond,
            C::a(|s, a| {
                Rope::singleton(if_(
                    a,
                    compile_stmt_block(s, *then_branch),
                    compile_stmt_block(s, *else_branch),
                ))
            }),
        ),
        S::Loop(body) => Rope::singleton(loop_(Stmt::Block(
            compile_stmt(s, *body).into_iter().collect(),
        ))),
        S::Label(x, body) => Rope::singleton(label_(
            Label::Named(x.to_pretty(80)),
            Stmt::Block(compile_stmt(s, *body).into_iter().collect()),
        )),
        S::Break(x) => Rope::singleton(Stmt::Break(Label::Named(x.to_pretty(80)))),
        S::Catch(_, _, _) => todo!("NotWasm needs to support exceptions"),
        S::Finally(_, _) => todo!("NotWasm needs to support exceptions"),
        S::Throw(_) => todo!("NotWasm needs to support exceptions"),
        S::Return(e) => compile_expr(s, *e, C::a(|s, a| Rope::singleton(Stmt::Return(a)))),
    }
}

fn compile_stmt_block(s: &mut S, stmt: J::Stmt) -> Stmt {
    rope_to_block(compile_stmt(s, stmt))
}
fn rope_to_block(rope: Rope<Stmt>) -> Stmt {
    Stmt::Block(rope.into_iter().collect())
}

pub fn from_jankyscript(janky_program: J::Stmt) -> Program {
    let mut state: S = Default::default();
    let main_body = Stmt::Block(
        compile_stmt(&mut state, janky_program)
            .into_iter()
            .collect(),
    );
    state.new_function(
        Id::from("main"),
        Function {
            body: main_body,
            params: Vec::new(),
            fn_type: FnType {
                args: Vec::new(),
                result: None,
            },
        },
    );
    Program {
        functions: state.functions,
        globals: HashMap::new(),
        data: Vec::new(),
    }
}

#[cfg(test)]
mod test {
    use super::super::test_wasm::expect_notwasm;
    use super::from_jankyscript;
    use crate::jankyscript::constructors::*;
    use crate::jankyscript::syntax::*;
    use crate::rts_function::RTSFunction;
    #[test]
    fn unary() {
        let program = Stmt::Block(vec![
            var_(
                "a".into(),
                Type::Float,
                Expr::Lit(Lit::Num(Num::Float(25.))),
            ),
            var_(
                "b".into(),
                Type::Float,
                unary_(crate::notwasm::syntax::UnaryOp::Sqrt, Expr::Id("a".into())),
            ),
            expr_(Expr::PrimCall(
                RTSFunction::LogAny,
                vec![coercion_(Coercion::Tag(Type::Float), Expr::Id("b".into()))],
            )),
        ]);
        expect_notwasm("F64(5)".to_string(), from_jankyscript(program));
    }
}
