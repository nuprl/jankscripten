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
use super::syntax::*;
use std::collections::HashMap;

fn compile_lit(lit: J::Lit) -> Lit {
    match lit {
        J::Lit::String(s) => Lit::String(s),
        J::Lit::Regex(_, _) => todo!("regex not supported anywhere in toolchain"),
        J::Lit::Bool(b) => Lit::Bool(b),
        J::Lit::Null => todo!("NotWasm needs a null primitive"),
        J::Lit::Undefined => todo!("NotWasm needs an undefined"),
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

fn compile_ty(janky_typ: J::Type) -> Type {
    match janky_typ {
        J::Type::Any => Type::Any,
        _ => todo!("compile_ty"),
    }
}

fn compile_expr<'a>(s: &'a mut S, expr: J::Expr, cxt: C<'a>) -> Rope<Stmt> {
    match expr {
        J::Expr::Lit(lit) => cxt.recv_a(s, Atom::Lit(compile_lit(lit))),
        J::Expr::Array(_) => todo!("arrays need a variant of compile_exprs"),
        J::Expr::Object(_) => todo!("should be easy"),
        J::Expr::This => todo!("we need to think more deeply about this"),
        J::Expr::Dot(_, _) => todo!("o.x"),
        J::Expr::Unary(_, _) => todo!("unary op"),
        J::Expr::New(_, _, _) => todo!("new -- need deep thought"),
        J::Expr::Bracket(_, _) => todo!("arr[x]"),
        J::Expr::Coercion(_, _) => todo!("coercion"),
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
                    C::a(|s, a2| {
                        // TODO(arjun): compile binops correctly
                        cxt.recv_a(
                            s,
                            Atom::Binary(BinaryOp::I32Add, Box::new(a1), Box::new(a2)),
                        )
                    }),
                )
            }),
        ),
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
        S::Assign(lv, e) => todo!(),
        S::If(cond, then_branch, else_branch) => todo!(),
        S::While(cond, body) => todo!(),
        S::Label(x, body) => todo!(),
        S::Break(x) => todo!(),
        S::Catch(_, _, _) => todo!("NotWasm needs to support exceptions"),
        S::Finally(_, _) => todo!("NotWasm needs to support exceptions"),
        S::Throw(_) => todo!("NotWasm needs to support exceptions"),
        S::Return(_) => todo!(),
    }
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
