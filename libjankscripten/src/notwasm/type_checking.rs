use super::syntax::*;

use im_rc::HashMap;

type Env = HashMap<Id, Type>;

pub enum TypeCheckingError {
    NoSuchVariable(Id),
    TypeMismatch(String, Type, Type), // context, expected, got
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

fn lookup(env: &Env, id: &Id) -> TypeCheckingResult<Type> {
    match env.get(id) {
        Some(ty) => Ok(ty.clone()),
        None => Err(TypeCheckingError::NoSuchVariable(id.clone())),
    }
}

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

pub fn type_check(p: &Program) -> TypeCheckingResult<()> {
    unimplemented!();
}

pub fn type_check_function(p: &Program, f: &Function) -> TypeCheckingResult<()> {
    unimplemented!();
}

pub fn type_check_stmt(p: &Program, env: &Env, s: &Stmt) -> TypeCheckingResult<Env> {
    unimplemented!();
}

pub fn type_check_expr(p: &Program, env: &Env, e: &Expr) -> TypeCheckingResult<Type> {
    unimplemented!();
}

pub fn type_check_atom(p: &Program, env: &Env, a: &Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Lit(l) => Ok(type_check_lit(l)),
        Atom::Id(id) => lookup(&env, id),
        Atom::StringLen(a) => {
            let ty = type_check_atom(p, env, a)?;
            ensure("string len", Type::String, ty)
        }
        Atom::ArrayLen(a, ty) => {
            let got = type_check_atom(p, env, a)?;
            ensure("array len", Type::Array(Box::new(ty.clone())), got)
        }
        Atom::Index(a_arr, a_idx, ty) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_idx = type_check_atom(p, env, a_idx)?;
            let _ = ensure("arrayi ndex (index)", Type::I32, got_idx)?;
            ensure(
                "array index (array)",
                Type::Array(Box::new(ty.clone())),
                got_arr,
            )
        }
        Atom::ObjectGet(a_obj, a_field, ty) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;
            // ??? MMG should we be doing anything around classes here?
            let _ = ensure("object get field", Type::String, got_field)?;
            let _ = ensure("object field", Type::AnyClass, got_obj)?;
            Ok(ty.clone())
        }
        Atom::HTGet(a_ht, a_field, ty) => {
            let got_ht = type_check_atom(p, env, a_ht)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let _ = ensure("ht get field", Type::Any, got_field)?;
            // ??? MMG what is the type field in HT for?
            let _ = ensure("ht field", Type::HT(Box::new(Type::Any)), got_ht)?;
            Ok(ty.clone())
        }
        Atom::Unary(op, a) => {
            let (ty_in, ty_out) = type_check_unary(op);
            let got = type_check_atom(p, env, a)?;
            let _ = ensure(&format!("unary ({:?})", op), ty_in, got)?;
            Ok(ty_out)
        }
        Atom::Binary(op, a_l, a_r) => {
            let (ty_in, ty_out) = type_check_binary(op);
            let got_l = type_check_atom(p, env, a_l)?;
            let got_r = type_check_atom(p, env, a_r)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in.clone(), got_l)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in, got_r)?;
            Ok(ty_out)
        }
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

pub fn type_check_binary(op: &BinaryOp) -> (Type, Type) {
    match op {
        BinaryOp::I32Eq | BinaryOp::I32GT | BinaryOp::I32LT | BinaryOp::I32Ge | BinaryOp::I32Le =>
          (Type::I32, Type::Bool),
        BinaryOp::I32Add | BinaryOp::I32Sub | BinaryOp::I32Mul | BinaryOp::I32And | BinaryOp::I32Or =>
          (Type::I32, Type::I32),
        BinaryOp::F64Add | BinaryOp::F64Sub | BinaryOp::F64Mul | BinaryOp::F64Div =>
          (Type::F64, Type::F64),
    }
}

pub fn type_check_unary(op: &UnaryOp) -> (Type, Type) {
    match op {
        UnaryOp::Sqrt => (Type::F64, Type::F64),
    }
}
