use super::syntax::*;

use im_rc::HashMap;

use std::iter::FromIterator;

type Env = HashMap<Id, Type>;

#[derive(Debug, Clone)]
pub enum TypeCheckingError {
    NoSuchVariable(Id),
    TypeMismatch(String, Type, Type), // context, expected, got
    ExpectedFunction(Id, Type),
    ExpectedHT(String, Type),
    ExpectedArray(String, Type),
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
        Ok(function.fn_type.clone().to_type())
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
            .iter()
            .map(|arg| arg.clone()) // ow
            .zip(f.fn_type.args.iter().map(|ty| ty.clone())), // why can't i clone the pairs at the end?
    );

    let _ = type_check_stmt(p, env, &f.body, &f.fn_type.result.clone().map(|b| *b))?;

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
            let got = type_check_expr(p, &env, e)?;
            let _ = ensure("var", ty.clone(), got)?;

            // ??? MMG what do we want here? i assume we don't actually want to allow strong update...
            if id.clone().into_name().starts_with("_") {
                Ok(env)
            } else if lookup(p, &env, id).is_ok() {
                Err(TypeCheckingError::MultiplyDefined(id.clone()))
            } else {
                Ok(env.update(id.clone(), ty.clone()))
            }
        }
        Stmt::Expression(e) => {
            let _ = type_check_expr(p, &env, e)?;

            Ok(env)
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

            // then/else branches are new blocks/scopes
            let _ = type_check_stmt(p, env.clone(), s_then, ret_ty)?;
            let _ = type_check_stmt(p, env.clone(), s_else, ret_ty)?;

            Ok(env)
        }
        Stmt::Loop(s_body) => {
            let _ = type_check_stmt(p, env.clone(), s_body, ret_ty);
            Ok(env)
        }
        Stmt::Label(_lbl, s_body) => {
            // LATER label checking
            let _ = type_check_stmt(p, env.clone(), s_body, ret_ty);
            Ok(env)
        }
        Stmt::Break(_lbl) => Ok(env),
        Stmt::Return(a) => {
            let got = type_check_atom(p, &env, a)?;

            // ??? MMG if ret_ty = None, can one return early?
            match ret_ty {
                None => Err(TypeCheckingError::UnexpectedReturn(got)),
                Some(ret_ty) => {
                    let _ = ensure("return", ret_ty.clone(), got)?;

                    Ok(env)
                }
            }
        }
        Stmt::Block(ss) => {
            let mut env_inner = env.clone();

            for s in ss.iter() {
                env_inner = type_check_stmt(p, env_inner, s, ret_ty)?;
            }

            Ok(env)
        }
        Stmt::Trap => Ok(env),
        Stmt::Goto(_lbl) => unimplemented!(),
    }
}

pub fn type_check_expr(p: &Program, env: &Env, e: &Expr) -> TypeCheckingResult<Type> {
    match e {
        Expr::HT => Ok(Type::HT),
        Expr::Array => Ok(Type::Array),
        Expr::ObjectEmpty => Ok(Type::AnyClass),
        Expr::Push(a_arr, a_elt) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_elt = type_check_atom(p, env, a_elt)?;

            let _ = ensure("array push (array)", Type::Array, got_arr)?;
            let _ = ensure("array push (element)", Type::Any, got_elt)?;

            Ok(Type::I32) // returns length
        }
        Expr::HTSet(a_ht, a_field, a_val) => {
            let got_ht = type_check_atom(p, env, a_ht)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let got_val = type_check_atom(p, env, a_val)?;

            ensure("ht set (ht)", Type::HT, got_ht)?;
            let _ = ensure("ht set (field)", Type::String, got_field)?;
            let _ = ensure("ht set (val)", Type::Any, got_val)?;

            Ok(Type::Any) // returns value set
        }
        Expr::ObjectSet(a_obj, a_field, a_val) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let got_val = type_check_atom(p, env, a_val)?;

            let _ = ensure("object set (obj)", Type::AnyClass, got_obj)?;
            let _ = ensure("object set (field)", Type::String, got_field)?;
            let _ = ensure("object set (val)", Type::Any, got_val)?;

            Ok(Type::Any) // returns value set
        }
        Expr::ToString(a) => {
            let got = type_check_atom(p, env, a)?;
            let _ = ensure("tostring", Type::StrRef, got);
            Ok(Type::String)
        }
        Expr::Call(id_f, actuals) => {
            let got_f = lookup(p, env, id_f)?;
            if let Type::Fn(fn_ty) = got_f {
                // arity check
                if actuals.len() != fn_ty.args.len() {
                    return Err(TypeCheckingError::ArityMismatch(
                        id_f.clone(),
                        actuals.len() - fn_ty.args.len(),
                    ));
                }

                // match formals and actuals
                let mut nth = 0;
                for (actual, formal) in actuals.iter().zip(fn_ty.args.iter()) {
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
                Ok(fn_ty.result.map(|b| *b).unwrap_or(Type::Any))
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
        Atom::ToAny(a, ty) => {
            let got = type_check_atom(p, env, a)?;
            let _ = ensure("toany", ty.clone(), got)?;
            Ok(Type::Any)
        }
        Atom::FromAny(a, ty) => {
            let got = type_check_atom(p, env, a)?;
            ensure("from_any", Type::Any, got)?;
            Ok(ty.clone())
        }
        Atom::Id(id) => lookup(p, env, id),
        Atom::StringLen(a) => {
            let ty = type_check_atom(p, env, a)?;
            let _ = ensure("string len", Type::String, ty);

            Ok(Type::I32)
        }
        Atom::ArrayLen(a) => {
            let got = type_check_atom(p, env, a)?;
            ensure("array len", Type::Array, got)
        }
        Atom::Index(a_arr, a_idx) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_idx = type_check_atom(p, env, a_idx)?;
            let _ = ensure("arrayi ndex (index)", Type::I32, got_idx)?;
            let _ = ensure("array index (array)", Type::Array, got_arr);
            Ok(Type::Any)
        }
        Atom::ObjectGet(a_obj, a_field) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;

            let _ = ensure("object get field", Type::String, got_field)?;
            let _ = ensure("object field", Type::AnyClass, got_obj)?;
            Ok(Type::Any)
        }
        Atom::HTGet(a_ht, a_field) => {
            let got_ht = type_check_atom(p, env, a_ht)?;
            let got_field = type_check_atom(p, env, a_field)?;

            ensure("ht get", Type::HT, got_ht)?;
            let _ = ensure("ht get (field)", Type::String, got_field)?;

            Ok(Type::Any)
        }
        Atom::Unary(op, a) => {
            let (ty_in, ty_out) = type_check_unary(op);
            let got = type_check_atom(p, env, a)?;
            let _ = ensure(&format!("unary ({:?})", op), ty_in, got)?;
            Ok(ty_out)
        }
        Atom::Binary(BinaryOp::PtrEq, a_l, a_r) => {
            let got_l = type_check_atom(p, env, a_l)?;
            let got_r = type_check_atom(p, env, a_r)?;
            let _ = ensure("binary (===) lhs", got_l.clone(), got_r.clone())?;
            Ok(Type::Bool)
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
        Lit::Interned(_) => Type::StrRef,
    }
}

pub fn type_check_binary(op: &BinaryOp) -> (Type, Type) {
    match op {
        BinaryOp::PtrEq => (Type::Any, Type::Bool), // for completeness; should be special-cased
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
