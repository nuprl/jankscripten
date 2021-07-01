//! Checks the types of the Jankyscript AST.
//! This occurs after type inference to ensure that type inference succeeded
//! correctly.

use super::syntax::*;
use crate::pos::Pos;
use crate::shared::std_lib::get_global_object;
use im_rc::HashMap;
use thiserror::Error;
type Env = HashMap<Id, Type>;

#[derive(Debug, Error)]
pub enum TypeCheckingError {
    #[error("`{0}` expected type `{1}` but received `{2}` at `{3}`")]
    TypeMismatch(String, Type, Type, Pos),
    #[error("`{0}` expected indexable type, but received `{1}` at `{2}`")]
    ExpectedIndexable(String, Type, Pos),
    #[error("`{0}` expected indexer, but received `{1}` at `{2}`")]
    ExpectedIndexer(String, Type, Pos),
    #[error("`{0}` expected an expression to have a function type, but received `{1}` at `{2}`")]
    ExpectedFunction(String, Type, Pos),
    #[error("tried to tag a value of type `{0}`, but received `{1}` at `{2}`")]
    TagTypeMismatch(Type, Type, Pos),
    #[error("`{0}` expected a ground type but got `{1}` at `{2}`")]
    ExpectedGround(String, Type, Pos),
    #[error("unexpected return of type `{0}` at `{1}`")]
    UnexpectedReturn(Type, Pos),
    #[error("a variable named `{0}` was referenced that doesn't exist at `{1}`")]
    NoSuchVariable(Id, Pos),
    #[error("Expected a box at `{0}`, but got `{1}`")]
    ExpectedBox(Type, Pos),
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

// ensure we got a specific type
fn ensure(msg: &str, expected: Type, got: Type, s: &Pos) -> TypeCheckingResult<Type> {
    if expected == got {
        Ok(got)
    } else {
        Err(TypeCheckingError::TypeMismatch(
            String::from(msg),
            expected,
            got,
            s.clone(),
        ))
    }
}

// TODO(mark): refactor ensure_indexable and ensure_indexer, they have almost
//             identical logic. i'm just not sure how to do it well in rust

// ensure the given type is indexable; that is, able to be indexed using
// braces.
fn ensure_indexable(msg: &str, got: Type, s: Pos) -> TypeCheckingResult<Type> {
    let types = [Type::DynObject, Type::Array, Type::String, Type::Any];
    for expected_type in &types {
        let result = ensure(msg, expected_type.clone(), got.clone(), &s);
        match result {
            Ok(_) => {
                return result;
            }
            Err(_) => {
                continue;
            }
        }
    }

    Err(TypeCheckingError::ExpectedIndexable(
        String::from(msg),
        got,
        s,
    ))
}

// ensure the given type is an indexer (e.g. the `x` in arr[x])
fn ensure_indexer(msg: &str, got: Type, s: Pos) -> TypeCheckingResult<Type> {
    let types = [Type::String, Type::Int, Type::Any];
    for expected_type in &types {
        let result = ensure(msg, expected_type.clone(), got.clone(), &s);
        match result {
            Ok(_) => {
                return result;
            }
            Err(_) => {
                continue;
            }
        }
    }

    Err(TypeCheckingError::ExpectedIndexable(
        String::from(msg),
        got,
        s,
    ))
}

fn ensure_function(msg: &str, got: Type, s: Pos) -> TypeCheckingResult<(Vec<Type>, Box<Type>)> {
    match got {
        Type::Function(args_types, return_type) => Ok((args_types, return_type)),
        _ => Err(TypeCheckingError::ExpectedFunction(
            String::from(msg),
            got,
            s,
        )),
    }
}

fn _ensure_ground(msg: &str, got: Type, s: Pos) -> TypeCheckingResult<Type> {
    if got.is_ground() {
        Ok(got)
    } else {
        Err(TypeCheckingError::ExpectedGround(String::from(msg), got, s))
    }
}

fn lookup(env: &Env, id: &Id, s: &Pos) -> TypeCheckingResult<Type> {
    if let Some(ty) = env.get(id) {
        Ok(ty.clone())
    } else {
        Err(TypeCheckingError::NoSuchVariable(id.clone(), s.clone()))
    }
}

// type check an entire program.
pub fn type_check(stmt: &Stmt) -> TypeCheckingResult<()> {
    match type_check_stmt(
        stmt,
        get_global_object()
            .into_iter()
            .map(|(k, v)| (Id::Named(k), v))
            .collect(),
        &None,
    ) {
        Ok(_) => Ok(()),
        Err(error) => Err(error),
    }
}

fn type_check_stmt(stmt: &Stmt, env: Env, ret_ty: &Option<Type>) -> TypeCheckingResult<Env> {
    match stmt {
        Stmt::Empty => Ok(env),
        Stmt::Expr(e, _) => {
            type_check_expr(&e, env.clone())?;
            Ok(env)
        }
        Stmt::Finally(try_body, finally_body, _) => {
            // try_body should be well-typed
            type_check_stmt(&try_body, env.clone(), ret_ty)?;

            // finally_body should be well-typed
            type_check_stmt(&finally_body, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Catch(try_body, ex_name, catch_body, _) => {
            // try_body should be well-typed
            type_check_stmt(&try_body, env.clone(), ret_ty)?;

            // catch_body should be well-typed, when ex_name is bound to Any?
            let catch_body_env = env.clone().update(ex_name.clone(), Type::Any);
            type_check_stmt(&catch_body, catch_body_env, ret_ty)?;

            Ok(env)
        }
        Stmt::Loop(body, _) => {
            // type check body in a new scope
            type_check_stmt(&body, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::ForIn(bind, container, body, s) => {
            ensure_indexable(
                "for..in",
                type_check_expr(container, env.clone())?,
                s.clone(),
            )?;
            // type check body in a new scope
            // TODO(luna): like in jankyscript::fv, this is a guess, and ForIn
            // should really have a type
            let for_in_env = env.clone().update(bind.clone(), Type::Any);
            type_check_stmt(&body, for_in_env, ret_ty)?;
            Ok(env)
        }
        Stmt::Throw(e, _) => {
            // expression we're throwing should be well-typed
            type_check_expr(&e, env.clone())?;

            Ok(env)
        }
        Stmt::Break(_id, _) => {
            // TODO: label checking
            Ok(env)
        }
        Stmt::Label(_id, stmt, _) => {
            // TODO: label checking

            type_check_stmt(&stmt, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Var(x, t, e, s) => {
            match &**e {
                Expr::Lit(Lit::Undefined, _) => {
                    // TODO(arjun): This is a little hacky, but necessary to deal with function
                    // results getting named.
                }
                _ => {
                    ensure(
                        "variable declaration matches given type",
                        t.clone(),
                        type_check_expr(e, env.clone())?,
                        &s,
                    )?;
                }
            }

            Ok(env.update(x.clone(), t.clone()))
        }
        Stmt::If(c, t, e, s) => {
            ensure(
                "if condition",
                Type::Bool,
                type_check_expr(c, env.clone())?,
                &s,
            )?;

            type_check_stmt(&t, env.clone(), ret_ty)?;
            type_check_stmt(&e, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Block(stmts, _) => {
            type_check_stmts(stmts, env.clone(), ret_ty)?;
            Ok(env)
        }
        Stmt::Return(e, s) => {
            let e_type = type_check_expr(e, env.clone())?;

            match ret_ty {
                None => Err(TypeCheckingError::UnexpectedReturn(e_type, s.clone())),
                Some(ty) => {
                    ensure("return", ty.clone(), e_type, &s)?;
                    Ok(env)
                }
            }
        }
    }
}

fn type_check_stmts(stmts: &Vec<Stmt>, env: Env, ret_ty: &Option<Type>) -> TypeCheckingResult<Env> {
    let mut env = env;
    for s in stmts {
        env = type_check_stmt(s, env, ret_ty)?;
    }
    Ok(env)
}

// type checks a function call. this will fail if the given type is not a
// function. this function returns the type of the function call result.
fn type_check_fun_call(
    fun_type: Type,
    actual_args: &Vec<Expr>,
    env: Env,
    s: Pos,
) -> TypeCheckingResult<Type> {
    // ensure that `fun_type` is a function type.
    // get its expected argument types.
    let (expected_arg_types, return_type) =
        ensure_function("expected function for function call", fun_type, s.clone())?;

    // derive types for the actual arguments.
    let actual_arg_types: Vec<TypeCheckingResult<Type>> = actual_args
        .iter()
        .map(|e| type_check_expr(e, env.clone()))
        .collect();

    // now we want to iterate over the expected arg types and
    // actual arg types. we'll create a zipped iterator to
    // simultaneously iterate over both.
    let actual_and_expected_arg_types = actual_arg_types
        .into_iter()
        .zip(expected_arg_types.into_iter());

    for (actual, expected) in actual_and_expected_arg_types {
        ensure(
            "function argument must match declared type",
            expected,
            actual?,
            &s,
        )?;
    }

    Ok(*return_type)
}

fn type_check_func(f: &Func, env: Env) -> TypeCheckingResult<Type> {
    let mut function_env = env.clone();
    function_env.extend(f.args_with_typs.clone().into_iter());

    type_check_stmt(&f.body, function_env, &Some(f.result_typ.clone()))?;

    Ok(Type::Function(
        f.arg_typs().cloned().collect(),
        Box::new(f.result_typ.clone()),
    ))
}

fn type_check_expr(expr: &Expr, env: Env) -> TypeCheckingResult<Type> {
    match expr {
        Expr::JsOp(..) => panic!("cannot type-check JsOp"),
        Expr::Func(f, _) => type_check_func(f, env),
        Expr::Assign(lval, rval, s) => {
            // rval should be well typed
            let rval_ty = type_check_expr(&rval, env.clone())?;

            // whether or not rval can go in lval depends on what lval is
            match &**lval {
                LValue::Id(id, ty) => {
                    // what type is id?
                    let id_ty = lookup(&env, &id, &s)?;
                    ensure("rvalue assignment", ty.clone(), rval_ty.clone(), &s)?;
                    ensure("lvalue assignment", ty.clone(), id_ty, &s)?;
                    Ok(rval_ty)
                }
                LValue::Dot(e, _id) => {
                    // e should be well typed
                    type_check_expr(e, env.clone())?;

                    // TODO: can we do any further typechecking than this? do
                    //       our object types include info about their props?
                    // depending on how much type checking is supposed to
                    // duplicate with coercion insertion(?) we can ensure that
                    // rval is Any

                    Ok(rval_ty)
                }
                LValue::Bracket(e, computed_prop) => {
                    // e should be well typed
                    type_check_expr(e, env.clone())?;

                    // computed_prop should be well typed
                    type_check_expr(computed_prop, env.clone())?;

                    // TODO: can we do any further typechecking than this? do
                    //       our object types include info about their props?

                    Ok(rval_ty)
                }
            }
        }
        Expr::Call(fun, args, s) => {
            // get the type of the given function
            let fun_type = type_check_expr(fun, env.clone())?;

            // type check this call
            type_check_fun_call(fun_type, args, env, s.clone())
        }
        Expr::Coercion(coercion, e, s) => {
            // type the expression. regardless of the coercion, the expression
            // needs to be well-typed.
            let actual_type = type_check_expr(e, env)?;

            // find the types the coercion is going from and to
            let (from, to) = type_check_coercion(coercion, s.clone())?;

            // ensure we can feed the given expr into the given coercion
            ensure(
                "expression matches coercion source type",
                from,
                actual_type,
                &s,
            )?;

            // we can, so we will have the output type of the coercion
            Ok(to)
        }
        Expr::Lit(l, _) => Ok(type_check_lit(&l)),
        Expr::Id(id, ty, s) => ensure("id get", ty.clone(), lookup(&env, &id, &s)?, &s),
        Expr::Object(props, _) => {
            // type check each property
            for (_key, val) in props {
                type_check_expr(val, env.clone())?;
            }

            Ok(Type::DynObject)
        }
        Expr::Array(vals, _) => {
            // type check each element
            for val in vals {
                type_check_expr(val, env.clone())?;
            }

            Ok(Type::Array)
        }
        Expr::Dot(obj, _prop, s) => {
            let obj_type = type_check_expr(obj, env)?;

            ensure(
                "property lookup done on real object",
                Type::DynObject,
                obj_type,
                &s,
            )?;

            // we don't know anything about the type we're returning.
            // even if this property doesn't exist on the given object,
            // it'll return undefined (in non-strict mode).

            Ok(Type::Any)
        }
        Expr::Bracket(obj, dyn_prop, s) => {
            let obj_type = type_check_expr(obj, env.clone())?;

            ensure_indexable("brackets object", obj_type, s.clone())?;

            let dyn_prop_type = type_check_expr(dyn_prop, env)?;

            ensure_indexer("brackets index", dyn_prop_type, s.clone())?;

            // see Expr::Dot case for why we're returning Any
            Ok(Type::Any)
        }
        Expr::PrimCall(prim, args, s) => {
            // get the type of the primitive function we're calling
            let prim_type = prim.janky_typ();

            // type check this function call
            type_check_fun_call(prim_type, args, env, s.clone())
        }
        Expr::Unary(op, e, s) => {
            // ensure expr has expected input type
            let (ty_in, ty_out) = op.janky_typ();
            let got = type_check_expr(e, env)?;
            ensure("unary op", ty_in, got, &s)?;

            // whole operation has output type
            Ok(ty_out)
        }
        Expr::Binary(op, e_l, e_r, s) => {
            // ensure exprs have expected input type
            let (ty_in, ty_out) = op.janky_typ();
            let got_l = type_check_expr(e_l, env.clone())?;
            let got_r = type_check_expr(e_r, env.clone())?;
            ensure("binary op lhs", ty_in.clone(), got_l, &s)?;
            ensure("binary op rhs", ty_in, got_r, &s)?;

            // whole operation has output type
            Ok(ty_out)
        }
        Expr::NewRef(boxed_e, boxed_t, p) => {
            ensure(
                "expression in new box",
                boxed_t.clone(),
                type_check_expr(&boxed_e, env)?,
                p,
            )?;
            Ok(Type::Ref(Box::new(boxed_t.clone())))
        }
        Expr::Deref(e, t_annot, p) => match type_check_expr(e, env)? {
            Type::Ref(t) => {
                ensure("incorrect annotation", *t.clone(), t_annot.clone(), p)?;
                Ok(*t.clone())
            }
            t_unexpected => Err(TypeCheckingError::ExpectedBox(
                t_unexpected.clone(),
                p.clone(),
            )),
        },
        Expr::Store(e1, e2, t_annot, p) => match type_check_expr(e1, env.clone())? {
            Type::Ref(t1) => {
                ensure("incorrect annotation", *t1.clone(), t_annot.clone(), p)?;
                ensure(
                    "ref cell contents",
                    *t1.clone(),
                    type_check_expr(e2, env)?,
                    p,
                )?;
                Ok(*t1.clone())
            }
            t_unexpected => Err(TypeCheckingError::ExpectedBox(
                t_unexpected.clone(),
                p.clone(),
            )),
        },
        Expr::EnvGet(_, t, _) => Ok(t.clone()),
        Expr::Closure(f, f_env, p) => {
            for (e, t) in f_env.iter() {
                ensure(
                    "value captured in closure",
                    t.clone(),
                    type_check_expr(e, env.clone())?,
                    p,
                )?;
            }
            type_check_func(f, env)
        }
    }
}

// ensures the given coercion is well-formed, and returns its input and output
// types.
fn type_check_coercion(c: &Coercion, s: Pos) -> TypeCheckingResult<(Type, Type)> {
    match c {
        Coercion::Meta(t1, t2) => Ok((t1.clone(), t2.clone())),
        Coercion::FloatToInt => Ok((Type::Float, Type::Int)),
        Coercion::IntToFloat => Ok((Type::Int, Type::Float)),
        Coercion::Tag(from_type) => Ok((from_type.clone(), Type::Any)),
        Coercion::Untag(to_type) => Ok((Type::Any, to_type.clone())),
        Coercion::Fun(args_to_type, ret_to_type) => {
            // type check the arguments' coercions

            // the original types of the arguments
            let mut args_from = Vec::<Type>::new();

            // the new types of the arguments
            let mut args_to = Vec::<Type>::new();

            for arg_coercion in args_to_type {
                let (from, to) = type_check_coercion(arg_coercion, s.clone())?;
                args_from.push(from);
                args_to.push(to);
            }

            // get the original and new return type
            let (ret_from, ret_to) = type_check_coercion(ret_to_type, s)?;

            // construct "from" function type
            let from = Type::Function(args_from, Box::new(ret_from));

            // construct "to" function type
            let to = Type::Function(args_to, Box::new(ret_to));

            // put them together
            Ok((from, to))
        }
        Coercion::Id(to_type) => Ok((to_type.clone(), to_type.clone())),
        Coercion::Seq(t1, t2) => {
            let (t2_from, t2_to) = type_check_coercion(t2, s.clone())?;
            let (t1_from, t1_to) = type_check_coercion(t1, s.clone())?;

            ensure("sequence composition", t1_to, t2_from, &s)?;

            Ok((t1_from, t2_to))
        }
    }
}

fn type_check_lit(l: &Lit) -> Type {
    match l {
        Lit::String(_) => Type::String,
        Lit::Bool(_) => Type::Bool,
        Lit::Null => Type::Any,
        Lit::Num(Num::Float(_)) => Type::Float,
        Lit::Num(Num::Int(_)) => Type::Int,
        Lit::Undefined => Type::Any,
        Lit::Regex(_, _) => Type::Any,
    }
}
