use super::syntax::*;

use im_rc::HashMap;

type Env = HashMap<Id, Type>;

pub enum TypeCheckingError {
    NoSuchVariable(Id),
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

fn lookup(env: &Env, id: &Id) -> TypeCheckingResult<Type> {
    match env.get(id) {
        Some(ty) => Ok(ty.clone()),
        None => Err(TypeCheckingError::NoSuchVariable(id.clone())),
    }
}

pub fn type_check(p: &Program) -> TypeCheckingResult<()> {
    unimplemented!();
}

pub fn type_check_function(p: &Program, f: &Function) -> TypeCheckingResult<()> {
    unimplemented!();
}

pub fn type_check_stmt(p: &Program, env: Env, s: &Stmt) -> TypeCheckingResult<Env> {
    unimplemented!();
}

pub fn type_check_expr(p: &Program, env: Env, e: &Expr) -> TypeCheckingResult<Type> {
    unimplemented!();
}

pub fn type_check_atom(p: &Program, env: Env, a: &Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Lit(l) => Ok(type_check_lit(l)),
        Atom::Id(id) => lookup(&env, id),
        Atom::StringLen(a) => unimplemented!(),
        _ => unimplemented!(),
    }
}

pub fn type_check_lit(l: &Lit) -> Type {
    match l {
        Lit::Bool(_) => Type::Bool,
        Lit::I32(_) => Type::I32,
        Lit::F64(_) => Type::F64,
        Lit::String(_) => Type::String,
        Lit::Interned(_) => Type::StrRef, // ??? MMG I think this is right...
    }
}
