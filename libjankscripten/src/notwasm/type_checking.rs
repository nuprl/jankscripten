use super::syntax::*;
use im_rc::HashMap;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Env {
    env: HashMap<Id, Type>,
}

impl Env {
    pub fn new() -> Env {
        let env = super::rt_bindings::get_rt_bindings()
            .into_iter()
            .map(|(k, v)| (Id::Named(k), v))
            .collect();
        Env { env }
    }

    pub fn get(&self, id: &Id) -> Option<&Type> {
        self.env.get(id)
    }

    #[deprecated(note = "Refactor to use .update instead")]
    pub fn insert(&mut self, id: Id, ty: Type) -> Option<Type> {
        self.env.insert(id, ty)
    }

    pub fn update(&self, id: Id, ty: Type) -> Self {
        Env {
            env: self.env.update(id, ty),
        }
    }
}

// TODO(arjun): I don't think we get much out of enumerating all kinds of errors. Refactor this
// into just TypeCheckingError::Other.
#[derive(Debug, Clone, Error)]
pub enum TypeCheckingError {
    #[error("undefined variable ")]
    NoSuchVariable(Id),
    #[error("{0} expected type {1}, but received {2}")]
    TypeMismatch(String, Type, Type),
    #[error("expected function, but got {1}")]
    ExpectedFunction(Id, Type),
    #[error("{0} expected hash table, but got {1}")]
    ExpectedHT(String, Type),
    #[error("{0} expected array, but got {1}")]
    ExpectedArray(String, Type),
    #[error("{0} expected ref, but got {1}")]
    ExpectedRef(String, Type),
    #[error("unexpected return type {0}")]
    UnexpectedReturn(Type),
    #[error("arity mismatch")]
    ArityMismatch(Id, usize), // difference in arity
    #[error("Undefined branch")]
    UndefinedBranch(Env),
    #[error("identifier is multiply defined")]
    MultiplyDefined(Id),
    #[error("In context {0}, unexpected type {1}")]
    InvalidInContext(String, Type),
    #[error("Error type-checking NotWasm: {0}")]
    Other(String),
}

pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

macro_rules! error {
    ($($t:tt)*) => (
        Err(TypeCheckingError::Other(format!($($t)*)))
    )
}

fn invalid_in_context<T>(message: impl Into<String>, ty: &Type) -> TypeCheckingResult<T> {
    return Err(TypeCheckingError::InvalidInContext(
        message.into(),
        ty.clone(),
    ));
}

fn lookup(env: &Env, id: &Id) -> TypeCheckingResult<Type> {
    if let Some(ty) = env.get(id) {
        Ok(ty.clone())
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

/// Type-check a program, returning `Ok(())` if successful. This function also
/// mutates the program, adding additional type annotations that are needed for
/// code-generation. We could require these annotations in the input program,
/// but they are trivial to calculate.
pub fn type_check(p: &mut Program) -> TypeCheckingResult<()> {
    // Top-level type environment, including checks for duplicate identifiers.
    let mut env: Env = Env::new();
    for (id, f) in p.functions.iter() {
        if env
            .insert(id.clone(), f.fn_type.clone().to_type())
            .is_some()
        {
            return Err(TypeCheckingError::MultiplyDefined(id.clone()));
        }
    }
    for (id, g) in p.globals.iter() {
        if env.insert(id.clone(), g.ty.clone()).is_some() {
            return Err(TypeCheckingError::MultiplyDefined(id.clone()));
        }
    }

    for (id, f) in p.functions.iter_mut() {
        type_check_function(env.clone(), id, f)?;
    }

    return Ok(());
}

fn ensure_ref(msg: &str, got: Type) -> TypeCheckingResult<Type> {
    match got {
        Type::Ref(ty) => Ok(*ty),
        _ => Err(TypeCheckingError::ExpectedRef(String::from(msg), got)),
    }
}

fn type_check_function(mut env: Env, id: &Id, f: &mut Function) -> TypeCheckingResult<()> {
    // TODO(arjun): We should probably check for multiply-defined argument
    // names here.
    if f.fn_type.args.len() != f.params.len() {
        return Err(TypeCheckingError::ArityMismatch(
            id.clone(),
            f.params.len() - f.fn_type.args.len(),
        ));
    }

    for (id, ty) in f.params.iter().zip(f.fn_type.args.iter()) {
        env.insert(id.clone(), ty.clone());
    }

    let _ = type_check_stmt(env, &mut f.body, &f.fn_type.result.clone().map(|b| *b))?;

    Ok(())
}

fn type_check_stmt(env: Env, s: &mut Stmt, ret_ty: &Option<Type>) -> TypeCheckingResult<Env> {
    match s {
        Stmt::Empty => Ok(env),
        Stmt::Var(var_stmt) => {
            let ty = type_check_expr(&env, &mut var_stmt.named)?;
            var_stmt.set_ty(ty.clone());
            let id = &var_stmt.id;

            // ??? MMG what do we want here? i assume we don't actually want to allow strong update...
            if let Id::Named(name) = id {
                if name.starts_with("_") {
                    return Ok(env);
                }
            }

            if lookup(&env, id).is_ok() {
                Err(TypeCheckingError::MultiplyDefined(id.clone()))
            } else {
                Ok(env.update(id.clone(), ty.clone()))
            }
        }
        Stmt::Expression(e) => {
            let _ = type_check_expr(&env, e)?;

            Ok(env)
        }
        Stmt::Store(id, e) => {
            let got_id = lookup(&env, id)?;
            let got_expr = type_check_expr(&env, e)?;

            let type_pointed_to = ensure_ref("ref type", got_id)?;

            let _ = ensure("ref store", type_pointed_to, got_expr)?;

            Ok(env)
        }
        Stmt::Assign(id, e) => {
            let got_id = lookup(&env, id)?;
            let got_expr = type_check_expr(&env, e)?;
            ensure("assign", got_id, got_expr)?;

            Ok(env)
        }
        Stmt::If(a_cond, s_then, s_else) => {
            let got = type_check_atom(&env, a_cond)?;
            let _ = ensure("if (conditional)", Type::Bool, got)?;

            // then/else branches are new blocks/scopes
            let _ = type_check_stmt(env.clone(), s_then, ret_ty)?;
            let _ = type_check_stmt(env.clone(), s_else, ret_ty)?;

            Ok(env)
        }
        Stmt::Loop(s_body) => {
            type_check_stmt(env.clone(), s_body, ret_ty)?;
            Ok(env)
        }
        Stmt::Label(_lbl, s_body) => {
            // LATER label checking
            type_check_stmt(env.clone(), s_body, ret_ty)?;
            Ok(env)
        }
        Stmt::Break(_lbl) => Ok(env),
        Stmt::Return(a) => {
            let got = type_check_atom(&env, a)?;

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

            for s in ss.iter_mut() {
                env_inner = type_check_stmt(env_inner, s, ret_ty)?;
            }

            Ok(env)
        }
        Stmt::Trap => Ok(env),
        Stmt::Goto(_lbl) => unimplemented!(),
    }
}

fn type_check_expr(env: &Env, e: &mut Expr) -> TypeCheckingResult<Type> {
    match e {
        Expr::HT => Ok(Type::HT),
        Expr::Array => Ok(Type::Array),
        Expr::ObjectEmpty => Ok(Type::DynObject),
        Expr::Push(a_arr, a_elt) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_elt = type_check_atom(env, a_elt)?;

            let _ = ensure("array push (array)", Type::Array, got_arr)?;
            let _ = ensure("array push (element)", Type::Any, got_elt)?;

            Ok(Type::I32) // returns length
        }
        Expr::ArraySet(a_arr, a_idx, a_val) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_idx = type_check_atom(env, a_idx)?;
            let got_val = type_check_atom(env, a_val)?;
            let _ = ensure("array set (index)", Type::I32, got_idx)?;
            let _ = ensure("array set (array)", Type::Array, got_arr);
            let _ = ensure("array set (value)", Type::Any, got_val);
            Ok(Type::Any)
        }
        Expr::HTSet(a_ht, a_field, a_val) => {
            let got_ht = type_check_atom(env, a_ht)?;
            let got_field = type_check_atom(env, a_field)?;
            let got_val = type_check_atom(env, a_val)?;

            ensure("ht set (ht)", Type::HT, got_ht)?;
            let _ = ensure("ht set (field)", Type::String, got_field)?;
            let _ = ensure("ht set (val)", Type::Any, got_val)?;

            Ok(Type::Any) // returns value set
        }
        Expr::ObjectSet(a_obj, a_field, a_val) => {
            let got_obj = type_check_atom(env, a_obj)?;
            let got_field = type_check_atom(env, a_field)?;
            let got_val = type_check_atom(env, a_val)?;

            let _ = ensure("object set (obj)", Type::DynObject, got_obj)?;
            let _ = ensure("object set (field)", Type::String, got_field)?;
            let _ = ensure("object set (val)", Type::Any, got_val)?;

            Ok(Type::Any) // returns value set
        }
        Expr::PrimCall(prim, args) => {
            match prim.janky_typ().notwasm_typ() {
                Type::Fn(fn_ty) => {
                    let arg_tys = args
                        .into_iter()
                        .map(|a| type_check_atom(env, a))
                        .collect::<Result<Vec<_>, _>>()?;
                    if arg_tys.len() != fn_ty.args.len() {
                        error!(
                            "primitive {:?} expected {} arguments, but received {}",
                            prim,
                            fn_ty.args.len(),
                            arg_tys.len()
                        )
                    } else if arg_tys
                        .iter()
                        .zip(fn_ty.args.iter())
                        .any(|(t1, t2)| t1 != t2)
                    {
                        error!("primitive {:?} applied to wrong argument type", prim)
                    } else {
                        // ??? MMG do we need a void/unit type?
                        Ok(match &fn_ty.result {
                            None => Type::Any,
                            Some(t) => *t.clone(),
                        })
                    }
                }
                _ => error!("primitive is not a function ({:?})", prim),
            }
        }
        Expr::Call(id_f, actuals) => {
            let got_f = lookup(env, id_f)?;
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
                    let got = lookup(env, actual)?;
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
        Expr::NewRef(a) => Ok(Type::Ref(Box::new(type_check_atom(env, a)?))),
        Expr::Atom(a) => type_check_atom(env, a),
    }
}

fn assert_variant_of_any(ty: &Type) -> TypeCheckingResult<()> {
    match ty {
        Type::Any => invalid_in_context("cannot be stored in an Any", &ty),
        Type::I32 => Ok(()),
        Type::F64 => Ok(()),
        Type::Bool => Ok(()),
        Type::String => Ok(()),
        // We need to think this through. We cannot store arbitrary functions
        // inside an Any.
        Type::Fn(_) => Ok(()), // TODO(luna): see above
        // The following turn into pointers, and an Any can store a pointer
        Type::HT => Ok(()),
        Type::Array => Ok(()),
        Type::DynObject => Ok(()),
        Type::Ref(..) => todo!(),
    }
}

fn type_check_atom(env: &Env, a: &mut Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Deref(a) => ensure_ref("dereference", type_check_atom(env, a)?),
        Atom::Lit(l) => Ok(l.notwasm_typ()),
        Atom::ToAny(to_any) => {
            let ty = type_check_atom(env, &mut to_any.atom)?;
            assert_variant_of_any(&ty)?;
            to_any.set_ty(ty);
            Ok(Type::Any)
        }
        Atom::FromAny(a, ty) => {
            let got = type_check_atom(env, a)?;
            ensure("from_any", Type::Any, got)?;
            Ok(ty.clone())
        }
        Atom::FloatToInt(a) => {
            let got = type_check_atom(env, a)?;
            ensure("float to int", Type::F64, got)?;
            Ok(Type::I32)
        }
        Atom::IntToFloat(a) => {
            let got = type_check_atom(env, a)?;
            ensure("int to float", Type::I32, got)?;
            Ok(Type::F64)
        }
        Atom::Id(id) => lookup(env, id),
        Atom::GetPrimFunc(id) => lookup(env, id),
        Atom::StringLen(a) => {
            let ty = type_check_atom(env, a)?;
            let _ = ensure("string len", Type::String, ty)?;

            Ok(Type::I32)
        }
        Atom::ArrayLen(a) => {
            let got = type_check_atom(env, a)?;
            ensure("array len", Type::Array, got)?;
            Ok(Type::I32)
        }
        Atom::Index(a_arr, a_idx) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_idx = type_check_atom(env, a_idx)?;
            let _ = ensure("arrayi ndex (index)", Type::I32, got_idx)?;
            let _ = ensure("array index (array)", Type::Array, got_arr);
            Ok(Type::Any)
        }
        Atom::ObjectGet(a_obj, a_field) => {
            let got_obj = type_check_atom(env, a_obj)?;
            let got_field = type_check_atom(env, a_field)?;

            let _ = ensure("object get field", Type::String, got_field)?;
            let _ = ensure("object field", Type::DynObject, got_obj)?;
            Ok(Type::Any)
        }
        Atom::HTGet(a_ht, a_field) => {
            let got_ht = type_check_atom(env, a_ht)?;
            let got_field = type_check_atom(env, a_field)?;

            ensure("ht get", Type::HT, got_ht)?;
            let _ = ensure("ht get (field)", Type::String, got_field)?;

            Ok(Type::Any)
        }
        Atom::Unary(op, a) => {
            let (ty_in, ty_out) = op.notwasm_typ();
            let got = type_check_atom(env, a)?;
            let _ = ensure(&format!("unary ({:?})", op), ty_in, got)?;
            Ok(ty_out)
        }
        Atom::Binary(BinaryOp::PtrEq, a_l, a_r) => {
            let got_l = type_check_atom(env, a_l)?;
            let got_r = type_check_atom(env, a_r)?;
            let _ = ensure("binary (===) lhs", got_l.clone(), got_r.clone())?;
            Ok(Type::Bool)
        }
        Atom::Binary(op, a_l, a_r) => {
            let (ty_in, ty_out) = op.notwasm_typ();
            let got_l = type_check_atom(env, a_l)?;
            let got_r = type_check_atom(env, a_r)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in.clone(), got_l)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in, got_r)?;
            Ok(ty_out)
        }
    }
}
