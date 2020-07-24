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

    // Tried to tag a value of the given type, but the type wasn't ground
    TagNonGroundFunction(Type),
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
        Type::Function(args_types, return_type) => {
            Ok((args_types, return_type))
        },
        _ => Err(TypeCheckingError::ExpectedFunction(String::from(msg), got))
    }
}

// type check an entire program.
pub fn type_check(stmt: Stmt) -> TypeCheckingResult<()> {
    
    match type_check_stmt(stmt, HashMap::new()) {
        Ok(_) => Ok(()),
        Err(error) => Err(error)
    }
}

fn type_check_stmt(stmt: Stmt, env: Env) -> TypeCheckingResult<Env> {
    match stmt {
        Stmt::Var(x, t, e) => {
            ensure("variable declaration matches given type",
                t.clone(),
                type_check_expr(*e, env.clone())?)?;

            Ok(env.update(x, t))
        },
        Stmt::If(c, t, e) => {
            ensure("if condition", 
                Type::Bool, 
                type_check_expr(*c, env.clone())?)?;

            type_check_stmt(*t, env.clone())?;
            type_check_stmt(*e, env.clone())?;

            Ok(env)
        },
        Stmt::Block(stmts) => {
            type_check_stmts(stmts, env.clone())?;
            Ok(env)
        },
        Stmt::Return(_) => {
            todo!("type_check_stmt should take optional ret_ty");
        }
        _ => unimplemented!()
    }
}

fn type_check_stmts(stmts: Vec<Stmt>, env: Env) -> TypeCheckingResult<Env> {
    let mut env = env;
    for s in stmts {
        env = type_check_stmt(s, env)?;
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

            match *body {
                Stmt::Block(stmts) => {
                    // TODO: this is probably *NOT* where this should happen.
                    // we're not modifying the program here, just type-checking it.
                    // this should probably happen in the desugaring step, since
                    // it already has the mechanisms to walk and modify the AST.
                    todo!("gather all variables defined in this function without crossing a function definition boundary, lift their definitions to the top");
                },
                _ => panic!()
            }

            type_check_stmt(*body, function_env)?;

            Ok(Type::Function(
                args.into_iter().map(|(_, ty)| ty).collect(), 
                Box::new(return_type)))
        },
        Expr::Call(fun, args) => {
            // ensure that `fun` is a function.
            // get its expected argument types.
            let (expected_arg_types, return_type) = 
                ensure_function("expected function for function call",
                    type_check_expr(*fun, env.clone())?)?;

            // derive types for the actual arguments.
            let actual_arg_types: Vec<TypeCheckingResult<Type>> = 
                args.into_iter().map(|e| type_check_expr(e, env.clone())).collect();

            // now we want to iterate over the expected arg types and 
            // actual arg types. we'll create a zipped iterator to
            // simultaneously iterate over both.
            let actual_and_expected_arg_types = actual_arg_types.into_iter()
                .zip(expected_arg_types.into_iter());

            for (actual, expected) in actual_and_expected_arg_types {
                ensure("function argument must match declared type", 
                    expected,
                    actual?)?;
            }

            Ok(*return_type)
        },
        Expr::Coercion(coercion, e) => {
            // type the expression. regardless of the coercion, the expression
            // needs to be well-typed.
            let actual_type = type_check_expr(*e, env)?;

            match coercion {
                Coercion::Tag(from_type) => {
                    // Tags are only valid when
                    // 1. they correctly specify the type they want to tag
                    // 2. that type is ground
                    match from_type {
                        // tagging at the correct type
                        actual_type if actual_type.is_ground() => 
                            Ok(actual_type),

                        // tagging at a non-ground type
                        actual_type => 
                            Err(TypeCheckingError::TagNonGroundFunction(
                                actual_type)),

                        // tagging at the wrong type
                        _ => Err(TypeCheckingError::TagTypeMismatch(from_type, 
                                actual_type))
                    }
                },
                Coercion::Untag(to_type) => {
                    // untagging cannot fail at compile time, only runtime
                    Ok(to_type)
                },
                Coercion::Fun(args_to_type, ret_to_type) {

                },
                Coercion::Id(to_type) {
                    match to_type {
                        // id used with the correct type
                        actual_type => Ok(actual_type),

                        // id used with the wrong type
                        _ => Err(TypeCheckingError::TagTypeMismatch(to_type,
                            actual_type))
                    }
                },
                // t1 : S -> U
                // t2 : U -> T
                // Seq(t2, t1) : S -> T
                Coercion::Seq(t2, t1) { 
                    /**
                     * e : S    t1 : S -> U    t2 : U -> T
                     * -----------------------------------
                     *     Coerce(Seq(t2, t1), e) : T
                     */
                    unimplemented!();
                },
            }
        }
        _ => unimplemented!()
    }
}
