//! Checks the types of the Jankyscript AST.
//! This occurs after type inference to ensure that type inference succeeded
//! correctly.

use super::syntax::*;
use crate::pos::Pos;
use crate::shared::std_lib::get_global_object;
use crate::shared::Report;
use im_rc::HashMap;

type Env = HashMap<Id, Type>;

#[derive(Debug)]
pub enum TypeCheckingError {
    // Expected expression to have the first type, but it had the second
    TypeMismatch(String, Type, Type, Pos),
    // Expected expression to be indexable (either DynObject or Arrray),
    // but it had the given type
    ExpectedIndexable(String, Type, Pos),
    // Expected an indexer (e.g. the `x` in arr[x]), but it had the given type
    ExpectedIndexer(String, Type, Pos),
    // Expected an expression to have a function type
    ExpectedFunction(String, Type, Pos),
    // Tried to tag a value of the first type, but it had the second
    TagTypeMismatch(Type, Type, Pos),
    // Expected a ground type
    ExpectedGround(String, Type, Pos),
    // A return statement was used outside of a function
    UnexpectedReturn(Type, Pos),
    // A variable was referenced that does not exist
    NoSuchVariable(Id, Pos),
}

impl Report for TypeCheckingError {
    fn report(&self) -> String {
        use TypeCheckingError::*;
        match self {
            TypeMismatch(a, b, c, s) => {
                format!("{} expected type {} but received {} at {}", a, b, c, s)
            }
            ExpectedIndexable(a, b, s) => {
                format!("{} expected indexable type, but received {} at {}", a, b, s)
            }
            ExpectedIndexer(a, b, s) => {
                format!("{} expected idexer, but received {} at {}", a, b, s)
            }
            ExpectedFunction(a, b, s) => format!(
                "{} expected an expression to have a function type, but received {} at {}",
                a, b, s
            ),
            TagTypeMismatch(a, b, s) => format!(
                "tried to tag a value of type {}, but received {} at {}",
                a, b, s
            ),
            ExpectedGround(a, b, s) => {
                format!("{} expected a ground type but got {} at {}", a, b, s)
            }
            UnexpectedReturn(a, s) => format!("unexpected return of type {} at {}", a, s),
            NoSuchVariable(a, s) => format!(
                "a variable named {} was referenced that doesn't exist at {}",
                a, s
            ),
        }
    }
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

fn ensure_ground(msg: &str, got: Type, s: Pos) -> TypeCheckingResult<Type> {
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
pub fn type_check(stmt: &mut Stmt) -> TypeCheckingResult<()> {
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
            ensure(
                "variable declaration matches given type",
                t.clone(),
                type_check_expr(e, env.clone())?,
                &s,
            )?;

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

fn type_check_expr(expr: &Expr, env: Env) -> TypeCheckingResult<Type> {
    match expr {
        Expr::Func(f, _) => {
            // type check body under assumption that args have the specified
            // types
            let mut function_env = env.clone();
            function_env.extend(f.args_with_typs.clone().into_iter());

            type_check_stmt(&f.body, function_env, &Some(f.result_typ.clone()))?;

            Ok(Type::Function(
                f.arg_typs().cloned().collect(),
                Box::new(f.result_typ.clone()),
            ))
        }
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
        Expr::NewRef(..) | Expr::Deref(..) | Expr::Store(..) => {
            todo!("optionally, typechecking could occur after boxing")
        }
        Expr::EnvGet(..) | Expr::Closure(..) => {
            panic!("typechecking should occur before closure conversion")
        }
    }
}

// ensures the given coercion is well-formed, and returns its input and output
// types.
fn type_check_coercion(c: &Coercion, s: Pos) -> TypeCheckingResult<(Type, Type)> {
    match c {
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
        _ => todo!("regex"),
        // Regex(String, String), // TODO(arjun): The Regex is not properly parsed
    }
}
