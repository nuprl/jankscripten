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
    ExpectedRef(String, Type),
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

fn ensure_ht(msg: &str, got: Type) -> TypeCheckingResult<Type> {
    match got {
        Type::HT(ty) => Ok(*ty),
        _ => Err(TypeCheckingError::ExpectedHT(String::from(msg), got)),        
    }
}

fn ensure_ref(msg: &str, got: Type) -> TypeCheckingResult<Type> {
    match got {
        Type::Ref(ty) => Ok(*ty),
        _ => Err(TypeCheckingError::ExpectedRef(String::from(msg), got)),
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
        Stmt::Store(id, e) => {
            let got_id = lookup(p, &env, id)?;
            let got_expr = type_check_expr(p, &env, e)?;

            let type_pointed_to = ensure_ref("ref type", got_id)?;

            let _ = ensure("ref store", type_pointed_to, got_expr)?;

            Ok(env)
        },
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
        Expr::HT(ty) => Ok(Type::HT(Box::new(ty.clone()))),
        Expr::Array(ty) => Ok(Type::Array(Box::new(ty.clone()))),
        Expr::ObjectEmpty => Ok(Type::AnyClass),
        Expr::Push(a_arr, a_elt, ty) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_elt = type_check_atom(p, env, a_elt)?;

            // RUNTIME ASSUMPTION: any array coercion will happen elsewhere---we want an exact match
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

            let got_ht_inner = ensure_ht("ht set (ht)", got_ht)?;

            let _ = ensure("ht set (inner)", ty.clone(), got_ht_inner)?;

            let _ = ensure("ht set (field)", Type::String, got_field)?;
            let _ = ensure("ht set (val)", ty.clone(), got_val)?;

            Ok(ty.clone()) // returns value set
        }
        Expr::ObjectSet(a_obj, a_field, a_val, ty) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;
            let got_val = type_check_atom(p, env, a_val)?;

            let _ = ensure("object set (ht)", Type::AnyClass, got_obj)?;
             
            let _ = ensure("object set (field)", Type::String, got_field)?;
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
        Expr::NewRef(a) => Ok(Type::Ref(Box::new(type_check_atom(p, env, a)?))),
        Expr::Atom(a) => type_check_atom(p, env, a),
    }
}

pub fn type_check_atom(p: &Program, env: &Env, a: &Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Deref(id) => ensure_ref("dereference", lookup(p, env, id)?),
        Atom::Lit(l) => Ok(type_check_lit(l)),
        Atom::Id(id) => lookup(p, env, id),
        Atom::StringLen(a) => {
            let ty = type_check_atom(p, env, a)?;
            let _ = ensure("string len", Type::String, ty);

            Ok(Type::I32)
        }
        Atom::ArrayLen(a, ty) => {
            let got = type_check_atom(p, env, a)?;
            ensure("array len", Type::Array(Box::new(ty.clone())), got)
        }
        Atom::Index(a_arr, a_idx, ty) => {
            let got_arr = type_check_atom(p, env, a_arr)?;
            let got_idx = type_check_atom(p, env, a_idx)?;
            let _ = ensure("arrayi ndex (index)", Type::I32, got_idx)?;
            let _ = ensure(
                "array index (array)",
                Type::Array(Box::new(ty.clone())),
                got_arr,
            );
            Ok(ty.clone())
        }
        Atom::ObjectGet(a_obj, a_field, ty) => {
            let got_obj = type_check_atom(p, env, a_obj)?;
            let got_field = type_check_atom(p, env, a_field)?;

            let _ = ensure("object get field", Type::String, got_field)?;
            let _ = ensure("object field", Type::AnyClass, got_obj)?;
            Ok(ty.clone())
        }
        Atom::HTGet(a_ht, a_field, ty) => {
            let got_ht = type_check_atom(p, env, a_ht)?;
            let got_field = type_check_atom(p, env, a_field)?;

            let got_ht_inner = ensure_ht("ht get", got_ht)?;

            let _ = ensure("ht get (inner)", ty.clone(), got_ht_inner)?;
            let _ = ensure("ht get (field)", Type::String, got_field)?;

            Ok(ty.clone())
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
