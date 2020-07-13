//! Checks the types of the Jankyscript AST.
//! This occurs after type inference to ensure that type inference succeeded
//! correctly.

use super::syntax::*;
use super::types::Type;

use im_rc::HashMap;

type Env = HashMap<Id, Type>;

#[derive(Debug)]
pub enum TypeCheckingError {
    MismatchedTypes(String),
}
   
pub type TypeCheckingResult = Result<Env, TypeCheckingError>;

pub fn type_check(stmt: Stmt) -> TypeCheckingResult {
    type_check_stmt(stmt, HashMap::new())
}

fn type_check_stmt(stmt: Stmt, env: Env) -> TypeCheckingResult {
    match stmt {
        Stmt::Var(x, t, e) => {
            let mut env = env;
            env.insert(x, t);
            Ok(env)
        }
        Stmt::Block(stmts) => type_check_stmts(stmts, env),
        _ => unimplemented!()
    }
}

fn type_check_stmts(stmts: Vec<Stmt>, env: Env) -> TypeCheckingResult {
    let mut env = env;
    for s in stmts {
        env = type_check_stmt(s, env)?;
    }
    Ok(env)
}

fn type_check_expr(expr: Expr, env: Env) -> TypeCheckingResult {
    unimplemented!()
}