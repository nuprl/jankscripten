use super::syntax::*;

use im_rc::HashMap;

use std::iter::FromIterator;

type Env = HashMap<Id, Type>;

pub enum TypeCheckingError {
    NoSuchVariable(Id),
    TypeMismatch(String, Type, Type), // context, expected, got
    ExpectedFunction(Id, Type),
    UnexpectedReturn(Type),
    ArityMismatch(Id, usize), // difference in arity
    UndefinedBranch(Env),
    MultiplyDefined(Id),
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

fn lookup(p: &Program, env: &Env, id: &Id) -> TypeCheckingResult<Type> {
    if let Some(ty) = env.get(id) {
        Ok(ty.clone())
    } else if let Some(global) = p.globals.get(id) {
        Ok(global.ty.clone())
    } else if let Some(function) = p.functions.get(id) {
        Ok(function.fn_type.as_type())
    } else {
        Err(TypeCheckingError::NoSuchVariable(id.clone()))
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
    for (id, f) in p.functions.iter() {
        type_check_function(p, id, f)?;
    }

    Ok(())
}

pub fn type_check_function(p: &Program, id: &Id, f: &Function) -> TypeCheckingResult<()> {
    if f.fn_type.args.len() != f.params.len() {
        return Err(TypeCheckingError::ArityMismatch(
            id.clone(),
            f.params.len() - f.fn_type.args.len(),
        ));
    }

    let env: Env = HashMap::from_iter(
        f.params
            .iter().map(|arg| arg.clone())
            .zip(f.fn_type.args.iter().map(|ty| ty.clone()))
    );

    let _ = type_check_stmt(p, env, &f.body, &f.fn_type.result)?;

    Ok(())
}

pub fn type_check_stmt(
    p: &Program,
    env: Env,
    s: &Stmt,
    ret_ty: &Option<Type>,
) -> TypeCheckingResult<Env> {
    match s {
        Stmt::Empty => Ok(env),
        Stmt::Var(id, e, ty) => {
            if lookup(p, &env, id).is_ok() {
                return Err(TypeCheckingError::MultiplyDefined(id.clone()));
            }

            let got = type_check_expr(p, &env, e)?;
            let _ = ensure("var", ty.clone(), got)?;

            Ok(env.update(id.clone(), ty.clone()))
        }
        Stmt::Assign(id, e) => {
            let got_id = lookup(p, &env, id)?;
            let got_expr = type_check_expr(p, &env, e)?;
            let _ = ensure("assign", got_id, got_expr)?;

            Ok(env)
        }
        Stmt::If(a_cond, s_then, s_else) => {
            let got = type_check_atom(p, &env, a_cond)?;
            let _ = ensure("if (conditional)", Type::Bool, got)?;

            let env_then = type_check_stmt(p, env.clone(), s, ret_ty)?;
            let env_else = type_check_stmt(p, env, s, ret_ty)?;

            let env_new = env_then.symmetric_difference(env_else.clone());

            // ??? MMG is this the behavior we want?
            if !env_new.is_empty() {
                Err(TypeCheckingError::UndefinedBranch(env_new))
            } else {
                Ok(env_else) // arbitrarily
            }
        }
        Stmt::Loop(s) => type_check_stmt(p, env, s, ret_ty),
        Stmt::Label(_lbl, s) => type_check_stmt(p, env, s, ret_ty), // ??? MMG do I need be tracking that labels are correct, or is that handled elsewhere?
        Stmt::Break(_lbl) => Ok(env),
        Stmt::Return(a) => {
            let got = type_check_atom(p, &env, a)?;

            match ret_ty {
                None => Err(TypeCheckingError::UnexpectedReturn(got)),
                Some(ret_ty) => {
                    let _ = ensure("return", ret_ty.clone(), got)?;

                    Ok(env)        
                }
            }
        }
        Stmt::Block(ss) => {
            let mut env = env;

            for s in ss.iter() {
                env = type_check_stmt(p, env, s, ret_ty)?;
            }

            Ok(env)
        }
        Stmt::Trap => Ok(env),
        Stmt::Goto(_lbl) => Ok(env),
    }
}

pub fn type_check_expr(p: &Program, env: &Env, e: &Expr) -> TypeCheckingResult<Type> {
    match e {
        Expr::HT(ty) => Ok(Type::HT(Box::new(ty.clone()))),
        Expr::Array(ty) => Ok(Type::Array(Box::new(ty.clone()))),
        Expr::ObjectEmpty => Ok(Type::AnyClass), // ??? MMG right?
        Expr::Push(a_arr, a_elt, ty) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_elt = type_check_atom(p, env, a_elt)?;

            // ??? MMG do we need to be checking array types, or is this implicitly doing coercion?
            let _ = ensure(
                "array push (array)",
                Type::Array(Box::new(ty.clone())),
                got_arr,
            )?;
            let _ = ensure("array push (element)", ty.clone(), got_elt)?;

            Ok(Type::I32) // returns length
        }
        Expr::HTSet(a_ht, a_field, a_val, ty) => {
            let got_ht = type_check_atom(p, env, a_ht)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let got_val = type_check_atom(p, env, a_val)?;

            let _ = ensure("ht set (ht)", Type::HT(Box::new(Type::Any)), got_ht)?;
            let _ = ensure("ht set (field)", Type::String, got_field)?;
            let _ = ensure("ht set (val)", ty.clone(), got_val)?;

            Ok(ty.clone()) // returns value set
        }
        Expr::ObjectSet(a_obj, a_field, a_val, ty) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let got_val = type_check_atom(p, env, a_val)?;

            let _ = ensure("object set (ht)", Type::AnyClass, got_obj)?;
            let _ = ensure("object set (field)", Type::Any, got_field)?;
            let _ = ensure("object set (val)", ty.clone(), got_val)?;

            Ok(ty.clone()) // returns value set
        }
        Expr::ToString(a) => {
            let got = type_check_atom(p, env, a)?;
            let _ = ensure("tostring", Type::StrRef, got);
            Ok(Type::String)
        }
        Expr::ToAny(a, ty) => {
            let got = type_check_atom(p, env, a)?;
            let _ = ensure("toany", ty.clone(), got)?;
            // ??? MMG are there any types this DOESN'T work for?
            // the Any enum leaves out strings, but Any::Any will wrap it?
            Ok(Type::Any)
        }
        Expr::Call(id_f, actuals) => {
            let got_f = lookup(p, env, id_f)?;
            if let Type::Fn(formals, ret) = got_f {
                // arity check
                if actuals.len() != formals.len() {
                    return Err(TypeCheckingError::ArityMismatch(
                        id_f.clone(),
                        actuals.len() - formals.len(),
                    ));
                }

                // match formals and actuals
                let mut nth = 0;
                for (actual, formal) in actuals.iter().zip(formals.iter()) {
                    let got = lookup(p, env, actual)?;
                    let _ = ensure(
                        &format!("call {:?} (argument #{})", id_f, nth),
                        formal.clone(),
                        got,
                    )?;
                    nth += 1;
                }

                // return type or any
                // ??? MMG do we need a void/unit type?
                Ok(ret.unwrap_or(Type::Any))
            } else {
                Err(TypeCheckingError::ExpectedFunction(id_f.clone(), got_f))
            }
        }
        Expr::Atom(a) => type_check_atom(p, env, a),
    }
}

pub fn type_check_atom(p: &Program, env: &Env, a: &Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Lit(l) => Ok(type_check_lit(l)),
        Atom::Id(id) => lookup(p, env, id),
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
        BinaryOp::I32Eq | BinaryOp::I32GT | BinaryOp::I32LT | BinaryOp::I32Ge | BinaryOp::I32Le => {
            (Type::I32, Type::Bool)
        }
        BinaryOp::I32Add
        | BinaryOp::I32Sub
        | BinaryOp::I32Mul
        | BinaryOp::I32And
        | BinaryOp::I32Or => (Type::I32, Type::I32),
        BinaryOp::F64Add | BinaryOp::F64Sub | BinaryOp::F64Mul | BinaryOp::F64Div => {
            (Type::F64, Type::F64)
        }
    }
}

pub fn type_check_unary(op: &UnaryOp) -> (Type, Type) {
    match op {
        UnaryOp::Sqrt => (Type::F64, Type::F64),
    }
}
