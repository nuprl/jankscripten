//! Checks the types of the Jankyscript AST.
//! This occurs after type inference to ensure that type inference succeeded
//! correctly.

use super::syntax::*;

use im_rc::HashMap;

type Env = HashMap<Id, Type>;

#[derive(Debug)]
pub enum TypeCheckingError {
    TypeMismatch(String, Type, Type),
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
        Stmt::Block(stmts) => type_check_stmts(stmts, env),
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

            type_check_stmt(*body, function_env)?;

            Ok(Type::Function(
                args.into_iter().map(|(name, ty)| ty).collect(), 
                Box::new(return_type)))
        },
        Expr::Call(fun, args) => {
            // let fun_type = 
            unimplemented!();
        },
        _ => unimplemented!()
    }
}
