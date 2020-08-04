//! Checks the types of the Jankyscript AST.
//! This occurs after type inference to ensure that type inference succeeded
//! correctly.

use super::syntax::*;

use im_rc::HashMap;

type Env = HashMap<Id, Type>;

#[derive(Debug)]
pub enum TypeCheckingError {
    // Expected expression to have the first type, but it had the second
    TypeMismatch(String, Type, Type),

    // Expected an expression to have a function type
    ExpectedFunction(String, Type),

    // Tried to tag a value of the first type, but it had the second
    TagTypeMismatch(Type, Type),

    // Expected a ground type
    ExpectedGround(String, Type),

    // A return statement was used outside of a function
    UnexpectedReturn(Type),

    // A variable was referenced that does not exist
    NoSuchVariable(Id),
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

// ensure we got a specific type
fn ensure(msg: &str, expected: Type, got: Type) -> TypeCheckingResult<Type> {
    if expected == got {
        Ok(got)
    } else {
        Err(TypeCheckingError::TypeMismatch(
            String::from(msg),
            expected,
            got,
        ))
    }
}

fn ensure_function(msg: &str, got: Type) -> TypeCheckingResult<(Vec<Type>, Box<Type>)> {
    match got {
        Type::Function(args_types, return_type) => Ok((args_types, return_type)),
        _ => Err(TypeCheckingError::ExpectedFunction(String::from(msg), got)),
    }
}

fn ensure_ground(msg: &str, got: Type) -> TypeCheckingResult<Type> {
    if got.is_ground() {
        Ok(got)
    } else {
        Err(TypeCheckingError::ExpectedGround(String::from(msg), got))
    }
}

fn lookup(env: &Env, id: &Id) -> TypeCheckingResult<Type> {
    if let Some(ty) = env.get(id) {
        Ok(ty.clone())
    } else {
        Err(TypeCheckingError::NoSuchVariable(id.clone()))
    }
}

// type check an entire program.
pub fn type_check(stmt: Stmt) -> TypeCheckingResult<()> {
    match type_check_stmt(stmt, HashMap::new(), &None) {
        Ok(_) => Ok(()),
        Err(error) => Err(error),
    }
}

fn type_check_stmt(stmt: Stmt, env: Env, ret_ty: &Option<Type>) -> TypeCheckingResult<Env> {
    match stmt {
        Stmt::Empty => Ok(env),
        Stmt::Expr(e) => {
            type_check_expr(*e, env.clone())?;
            Ok(env)
        }
        Stmt::Finally(try_body, finally_body) => {
            // try_body should be well-typed
            type_check_stmt(*try_body, env.clone(), ret_ty)?;

            // finally_body should be well-typed
            type_check_stmt(*finally_body, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Catch(try_body, ex_name, catch_body) => {
            // try_body should be well-typed
            type_check_stmt(*try_body, env.clone(), ret_ty)?;

            // catch_body should be well-typed, when ex_name is bound to Any?
            let catch_body_env = env.clone().update(ex_name, Type::Any);
            type_check_stmt(*catch_body, catch_body_env, ret_ty)?;

            Ok(env)
        }
        Stmt::Assign(lval, rval) => {
            // rval should be well typed
            let rval_ty = type_check_expr(*rval, env.clone())?;

            // whether or not rval can go in lval depends on what lval is
            match *lval {
                LValue::Id(id) => {
                    // what type is id?
                    let id_ty = lookup(&env, &id)?;
                    ensure("variable assignment", id_ty, rval_ty)?;
                    Ok(env)
                }
                LValue::Dot(e, _id) => {
                    // e should be well typed
                    type_check_expr(e, env.clone())?;

                    // TODO: can we do any further typechecking than this? do
                    //       our object types include info about their props?

                    Ok(env)
                }
                LValue::Bracket(e, computed_prop) => {
                    // e should be well typed
                    type_check_expr(e, env.clone())?;

                    // computed_prop should be well typed
                    type_check_expr(computed_prop, env.clone())?;

                    // TODO: can we do any further typechecking than this? do
                    //       our object types include info about their props?

                    Ok(env)
                }
            }
        }
        Stmt::While(cond, body) => {
            // make sure cond is a bool
            let cond_ty = type_check_expr(*cond, env.clone())?;
            ensure("while loop conditional", Type::Bool, cond_ty)?;

            // type check body in a new scope
            type_check_stmt(*body, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Throw(e) => {
            // expression we're throwing should be well-typed
            type_check_expr(*e, env.clone())?;

            Ok(env)
        }
        Stmt::Break(_id) => {
            // TODO: label checking
            Ok(env)
        }
        Stmt::Label(_id, stmt) => {
            // TODO: label checking

            type_check_stmt(*stmt, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Var(x, t, e) => {
            ensure(
                "variable declaration matches given type",
                t.clone(),
                type_check_expr(*e, env.clone())?,
            )?;

            Ok(env.update(x, t))
        }
        Stmt::If(c, t, e) => {
            ensure(
                "if condition",
                Type::Bool,
                type_check_expr(*c, env.clone())?,
            )?;

            type_check_stmt(*t, env.clone(), ret_ty)?;
            type_check_stmt(*e, env.clone(), ret_ty)?;

            Ok(env)
        }
        Stmt::Block(stmts) => {
            type_check_stmts(stmts, env.clone(), ret_ty)?;
            Ok(env)
        }
        Stmt::Return(e) => {
            let e_type = type_check_expr(*e, env.clone())?;

            match ret_ty {
                None => Err(TypeCheckingError::UnexpectedReturn(e_type)),
                Some(ty) => {
                    ensure("return", ty.clone(), e_type)?;
                    Ok(env)
                }
            }
        }
    }
}

fn type_check_stmts(stmts: Vec<Stmt>, env: Env, ret_ty: &Option<Type>) -> TypeCheckingResult<Env> {
    let mut env = env;
    for s in stmts {
        env = type_check_stmt(s, env, ret_ty)?;
    }
    Ok(env)
}

fn type_check_expr(expr: Expr, env: Env) -> TypeCheckingResult<Type> {
    match expr {
        Expr::Func(return_type, args, body) => {
            // type check body under assumption that args have the specified
            // types
            let mut function_env = env.clone();
            function_env.extend(args.clone().into_iter());

            type_check_stmt(*body, function_env, &Some(return_type.clone()))?;

            Ok(Type::Function(
                args.into_iter().map(|(_, ty)| ty).collect(),
                Box::new(return_type),
            ))
        }
        Expr::Call(fun, args) => {
            // ensure that `fun` is a function.
            // get its expected argument types.
            let (expected_arg_types, return_type) = ensure_function(
                "expected function for function call",
                type_check_expr(*fun, env.clone())?,
            )?;

            // derive types for the actual arguments.
            let actual_arg_types: Vec<TypeCheckingResult<Type>> = args
                .into_iter()
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
                )?;
            }

            Ok(*return_type)
        }
        Expr::Coercion(coercion, e) => {
            // type the expression. regardless of the coercion, the expression
            // needs to be well-typed.
            let actual_type = type_check_expr(*e, env)?;

            // find the types the coercion is going from and to
            let (from, to) = type_check_coercion(coercion)?;

            // ensure we can feed the given expr into the given coercion
            ensure("expression matches coercion source type", from, actual_type)?;

            // we can, so we will have the output type of the coercion
            Ok(to)
        }
        _ => unimplemented!(),
    }
}

// ensures the given coercion is well-formed, and returns its input and output
// types.
fn type_check_coercion(c: Coercion) -> TypeCheckingResult<(Type, Type)> {
    match c {
        Coercion::Tag(from_type) => Ok((from_type, Type::Any)),
        Coercion::Untag(to_type) => Ok((Type::Any, to_type)),
        Coercion::Fun(args_to_type, ret_to_type) => {
            // type check the arguments' coercions

            // the original types of the arguments
            let mut args_from = Vec::<Type>::new();

            // the new types of the arguments
            let mut args_to = Vec::<Type>::new();

            for arg_coercion in args_to_type {
                let (from, to) = type_check_coercion(arg_coercion)?;
                args_from.push(from);
                args_to.push(to);
            }

            // get the original and new return type
            let (ret_from, ret_to) = type_check_coercion(*ret_to_type)?;

            // construct "from" function type
            let from = Type::Function(args_from, Box::new(ret_from));

            // construct "to" function type
            let to = Type::Function(args_to, Box::new(ret_to));

            // put them together
            Ok((from, to))
        }
        Coercion::Id(to_type) => Ok((to_type.clone(), to_type)),
        Coercion::Seq(t1, t2) => {
            let (t2_from, t2_to) = type_check_coercion(*t2)?;
            let (t1_from, t1_to) = type_check_coercion(*t1)?;

            ensure("sequence composition", t1_to, t2_from)?;

            Ok((t1_from, t2_to))
        }
    }
}
