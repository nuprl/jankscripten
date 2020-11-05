use super::constructors::*;
use super::syntax::*;
use im_rc::HashMap;
use swc_common::SourceMap;

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
#[derive(Debug, Clone)]
pub enum TypeCheckingError {
    NoSuchVariable(Id, Span),
    TypeMismatch(String, Type, Type, Span),
    ExpectedFunction(Id, Type, Span),
    ExpectedHT(String, Type, Span),
    ExpectedArray(String, Type, Span),
    ExpectedRef(String, Type, Span),
    UnexpectedReturn(Type, Span),
    ArityMismatch(Id, usize, usize, Span), // params, then args
    MultiplyDefined(Id, Span),
    InvalidInContext(String, Type, Span),
    Other(String, Span),
}
impl crate::shared::Report for TypeCheckingError {
    fn report(&self, sm: &SourceMap) -> String {
        use TypeCheckingError::*;
        match self {
            NoSuchVariable(a, s) => {
                format!("undefined variable {} at {}", a, sm.span_to_string(*s))
            }
            TypeMismatch(a, b, c, s) => format!(
                "{} expected type {} but received {} at {}",
                a,
                b,
                c,
                sm.span_to_string(*s)
            ),
            ExpectedFunction(a, b, s) => format!(
                "expected function ({}), but got {} at {}",
                a,
                b,
                sm.span_to_string(*s)
            ),
            ExpectedHT(a, b, s) => format!(
                "{} expected hash table, but got {} at {}",
                a,
                b,
                sm.span_to_string(*s)
            ),
            ExpectedArray(a, b, s) => format!(
                "{} expected array, but got {} at {}",
                a,
                b,
                sm.span_to_string(*s)
            ),
            ExpectedRef(a, b, s) => format!(
                "{} expected ref, but got {} at {}",
                a,
                b,
                sm.span_to_string(*s)
            ),
            UnexpectedReturn(a, s) => {
                format!("unexpected return type {} at {}", a, sm.span_to_string(*s))
            }
            ArityMismatch(a, b, c, s) => format!(
                "arity mismatch at {}, expected {} parameters but received {} arguments at {}",
                a,
                b,
                c,
                sm.span_to_string(*s)
            ),
            MultiplyDefined(a, s) => format!(
                "identifier {} is multiply defined at {}",
                a,
                sm.span_to_string(*s)
            ),
            InvalidInContext(a, b, s) => format!(
                "In context {}, unexpected type {} at {}",
                a,
                b,
                sm.span_to_string(*s)
            ),
            Other(a, s) => format!(
                "Error type-checking NotWasm: {} at {}",
                a,
                sm.span_to_string(*s)
            ),
        }
    }
}
pub type TypeCheckingResult<T> = Result<T, TypeCheckingError>;

macro_rules! error {
    ($s:expr, $($t:tt)*) => (
        Err(TypeCheckingError::Other(format!($($t)*), $s))
    )
}

fn invalid_in_context<T>(message: impl Into<String>, ty: &Type, s: Span) -> TypeCheckingResult<T> {
    return Err(TypeCheckingError::InvalidInContext(
        message.into(),
        ty.clone(),
        s,
    ));
}

fn lookup(env: &Env, id: &Id, s: Span) -> TypeCheckingResult<Type> {
    if let Some(ty) = env.get(id) {
        Ok(ty.clone())
    } else {
        Err(TypeCheckingError::NoSuchVariable(id.clone(), s))
    }
}

fn ensure(msg: &str, expected: Type, got: Type, s: Span) -> TypeCheckingResult<Type> {
    if expected == got {
        Ok(got)
    } else {
        Err(TypeCheckingError::TypeMismatch(
            String::from(msg),
            expected,
            got,
            s,
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
            return Err(TypeCheckingError::MultiplyDefined(id.clone(), f.span));
        }
    }
    for (id, g) in p.globals.iter_mut() {
        // if the global is initialized
        if let Some(atom) = &mut g.atom {
            // type check it
            let got = type_check_atom(&env, atom)?;
            ensure("global var type", g.ty.clone(), got, DUMMY_SP)?;
        }

        // Insert the global into the environment
        if env.insert(id.clone(), g.ty.clone()).is_some() {
            return Err(TypeCheckingError::MultiplyDefined(id.clone(), DUMMY_SP));
        }
    }

    for (id, f) in p.functions.iter_mut() {
        type_check_function(env.clone(), id, f)?;
    }

    return Ok(());
}

fn ensure_ref(msg: &str, got: Type, s: Span) -> TypeCheckingResult<Type> {
    match got {
        Type::Ref(ty) => Ok(*ty),
        _ => Err(TypeCheckingError::ExpectedRef(String::from(msg), got, s)),
    }
}

fn type_check_function(mut env: Env, id: &Id, f: &mut Function) -> TypeCheckingResult<()> {
    // TODO(arjun): We should probably check for multiply-defined argument
    // names here.
    if f.fn_type.args.len() != f.params.len() {
        return Err(TypeCheckingError::ArityMismatch(
            id.clone(),
            f.params.len(),
            f.fn_type.args.len(),
            f.span,
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
        Stmt::Var(var_stmt, s) => {
            let ty = type_check_expr(&env, &mut var_stmt.named)?;
            var_stmt.set_ty(ty.clone());
            let id = &var_stmt.id;

            // ??? MMG what do we want here? i assume we don't actually want to allow strong update...
            if let Id::Named(name) = id {
                if name.starts_with("_") {
                    return Ok(env);
                }
            }

            if lookup(&env, id, *s).is_ok() {
                Err(TypeCheckingError::MultiplyDefined(id.clone(), *s))
            } else {
                Ok(env.update(id.clone(), ty.clone()))
            }
        }
        Stmt::Expression(e, _) => {
            let _ = type_check_expr(&env, e)?;

            Ok(env)
        }
        Stmt::Store(id, e, s) => {
            let got_id = lookup(&env, id, *s)?;
            let got_expr = type_check_expr(&env, e)?;

            let type_pointed_to = ensure_ref("ref type", got_id, *s)?;

            ensure("ref store", type_pointed_to, got_expr, *s)?;

            Ok(env)
        }
        Stmt::Assign(id, e, s) => {
            let got_id = lookup(&env, id, *s)?;
            let got_expr = type_check_expr(&env, e)?;
            ensure("assign", got_id, got_expr, *s)?;

            Ok(env)
        }
        Stmt::If(a_cond, s_then, s_else, s) => {
            let got = type_check_atom(&env, a_cond)?;
            let _ = ensure("if (conditional)", Type::Bool, got, *s)?;

            // then/else branches are new blocks/scopes
            let _ = type_check_stmt(env.clone(), s_then, ret_ty)?;
            let _ = type_check_stmt(env.clone(), s_else, ret_ty)?;

            Ok(env)
        }
        Stmt::Loop(s_body, _) => {
            type_check_stmt(env.clone(), s_body, ret_ty)?;
            Ok(env)
        }
        Stmt::Label(_lbl, s_body, _) => {
            // LATER label checking
            type_check_stmt(env.clone(), s_body, ret_ty)?;
            Ok(env)
        }
        Stmt::Break(_lbl, _) => Ok(env),
        Stmt::Return(a, s) => {
            let got = type_check_atom(&env, a)?;

            // ??? MMG if ret_ty = None, can one return early?
            match ret_ty {
                None => Err(TypeCheckingError::UnexpectedReturn(got, *s)),
                Some(ret_ty) => {
                    let _ = ensure("return", ret_ty.clone(), got, *s)?;

                    Ok(env)
                }
            }
        }
        Stmt::Block(ss, _) => {
            let mut env_inner = env.clone();

            for s in ss.iter_mut() {
                env_inner = type_check_stmt(env_inner, s, ret_ty)?;
            }

            Ok(env)
        }
        Stmt::Trap => Ok(env),
        Stmt::Goto(_lbl, _) => unimplemented!(),
    }
}

fn type_check_expr(env: &Env, e: &mut Expr) -> TypeCheckingResult<Type> {
    match e {
        Expr::HT => Ok(Type::HT),
        Expr::Array => Ok(Type::Array),
        Expr::ObjectEmpty => Ok(Type::DynObject),
        Expr::Push(a_arr, a_elt, s) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_elt = type_check_atom(env, a_elt)?;

            let _ = ensure("array push (array)", Type::Array, got_arr, *s)?;
            let _ = ensure("array push (element)", Type::Any, got_elt, *s)?;

            Ok(Type::I32) // returns length
        }
        Expr::ArraySet(a_arr, a_idx, a_val, s) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_idx = type_check_atom(env, a_idx)?;
            let got_val = type_check_atom(env, a_val)?;
            let _ = ensure("array set (index)", Type::I32, got_idx, *s)?;
            let _ = ensure("array set (array)", Type::Array, got_arr, *s);
            let _ = ensure("array set (value)", Type::Any, got_val, *s);
            Ok(Type::Any)
        }
        Expr::HTSet(a_ht, a_field, a_val, s) => {
            let got_ht = type_check_atom(env, a_ht)?;
            let got_field = type_check_atom(env, a_field)?;
            let got_val = type_check_atom(env, a_val)?;

            ensure("ht set (ht)", Type::HT, got_ht, *s)?;
            let _ = ensure("ht set (field)", Type::String, got_field, *s)?;
            let _ = ensure("ht set (val)", Type::Any, got_val, *s)?;

            Ok(Type::Any) // returns value set
        }
        Expr::ObjectSet(a_obj, a_field, a_val, s) => {
            let got_obj = type_check_atom(env, a_obj)?;
            let got_field = type_check_atom(env, a_field)?;
            let got_val = type_check_atom(env, a_val)?;

            let _ = ensure("object set (obj)", Type::DynObject, got_obj, *s)?;
            let _ = ensure("object set (field)", Type::String, got_field, *s)?;
            let _ = ensure("object set (val)", Type::Any, got_val, *s)?;

            Ok(Type::Any) // returns value set
        }
        Expr::PrimCall(prim, args, s) => {
            match prim.janky_typ().notwasm_typ() {
                Type::Fn(fn_ty) => {
                    let arg_tys = args
                        .into_iter()
                        .map(|a| type_check_atom(env, a))
                        .collect::<Result<Vec<_>, _>>()?;
                    if arg_tys.len() != fn_ty.args.len() {
                        error!(
                            *s,
                            "primitive {:?} expected {} arguments, but received {}",
                            prim,
                            fn_ty.args.len(),
                            arg_tys.len(),
                        )
                    } else if arg_tys
                        .iter()
                        .zip(fn_ty.args.iter())
                        .any(|(t1, t2)| t1 != t2)
                    {
                        error!(*s, "primitive {:?} applied to wrong argument type", prim)
                    } else {
                        // ??? MMG do we need a void/unit type?
                        Ok(match &fn_ty.result {
                            None => Type::Any,
                            Some(t) => *t.clone(),
                        })
                    }
                }
                _ => error!(*s, "primitive is not a function ({:?})", prim),
            }
        }
        Expr::Call(id_f, actuals, s) => {
            let got_f = lookup(env, id_f, *s)?;
            if let Type::Fn(fn_ty) = got_f {
                type_check_call(env, id_f, actuals, fn_ty, false, *s)
            } else {
                Err(TypeCheckingError::ExpectedFunction(id_f.clone(), got_f, *s))
            }
        }
        Expr::ClosureCall(id_f, actuals, s) => {
            let got_f = lookup(env, id_f, *s)?;
            if let Type::Closure(fn_ty) = got_f {
                type_check_call(env, id_f, actuals, fn_ty, true, *s)
            } else {
                Err(TypeCheckingError::ExpectedFunction(id_f.clone(), got_f, *s))
            }
        }
        Expr::NewRef(a, ty, s) => {
            let actual = type_check_atom(env, a)?;
            ensure("new ref", ty.clone(), actual, *s)?;
            Ok(ref_ty_(ty.clone()))
        }
        Expr::Atom(a, _) => type_check_atom(env, a),
        // this is really an existential type but for now i'm gonna try to
        // get away with pretending Type::Closure((i32) -> i32; [i32]) ==
        // Type::Closure((i32 -> i32; [])
        Expr::Closure(id, _, s) => match lookup(env, id, *s) {
            Ok(Type::Fn(fn_ty)) => Ok(Type::Closure(fn_ty)),
            Ok(got) => Err(TypeCheckingError::ExpectedFunction(id.clone(), got, *s)),
            Err(e) => Err(e),
        },
    }
}

/// implicit_arg is true in a closure; typechecks as if fn_ty started with
/// an Env
fn type_check_call(
    env: &Env,
    id_f: &Id,
    actuals: &[Id],
    fn_ty: FnType,
    implicit_arg: bool,
    s: Span,
) -> TypeCheckingResult<Type> {
    // arity check

    let actuals_len = actuals.len() + if implicit_arg { 1 } else { 0 };
    let expected_len = fn_ty.args.len();
    if actuals_len != expected_len {
        return Err(TypeCheckingError::ArityMismatch(
            id_f.clone(),
            actuals_len,
            fn_ty.args.len(),
            s,
        ));
    }

    // match formals and actuals
    let mut nth = 0;
    let mut args_iter = fn_ty.args.iter();
    if implicit_arg {
        match args_iter.next() {
            Some(Type::Env) => (),
            Some(got) => {
                return Err(TypeCheckingError::TypeMismatch(
                    String::from("closure must accept environment"),
                    Type::Env,
                    got.clone(),
                    s,
                ))
            }
            None => unreachable!(),
        }
    }
    for (actual, formal) in actuals.iter().zip(args_iter) {
        let got = lookup(env, actual, s)?;
        let _ = ensure(
            &format!("call {:?} (argument #{})", id_f, nth),
            formal.clone(),
            got,
            s,
        )?;
        nth += 1;
    }

    // return type or any
    // ??? MMG do we need a void/unit type?
    Ok(fn_ty.result.map(|b| *b).unwrap_or(Type::Any))
}

fn assert_variant_of_any(ty: &Type, s: Span) -> TypeCheckingResult<()> {
    match ty {
        // an any can be stored in an any right? but, i can see why you
        // wouldn't want to generate code that does so
        Type::Any => invalid_in_context("cannot be stored in an Any", &ty, s),
        Type::I32 => Ok(()),
        Type::F64 => Ok(()),
        Type::Bool => Ok(()),
        Type::String => Ok(()),
        // We need to think this through. We cannot store arbitrary functions
        // inside an Any.
        Type::Fn(ty) => {
            if Some(&Type::Env) == ty.args.get(0) {
                Ok(())
            } else {
                error!(s, "function must accept dummy environment to be any-ified")
            }
        }
        Type::Closure(_) => Ok(()),
        // The following turn into pointers, and an Any can store a pointer
        Type::HT => Ok(()),
        Type::Array => Ok(()),
        Type::DynObject => Ok(()),
        Type::Ref(..) => invalid_in_context("ref should not be stored in Any", &ty, DUMMY_SP),
        Type::Env => invalid_in_context("environments are not values", &ty, DUMMY_SP),
    }
}

fn type_check_atom(env: &Env, a: &mut Atom) -> TypeCheckingResult<Type> {
    match a {
        Atom::Deref(a, ty, s) => ensure(
            "dereference",
            ty.clone(),
            ensure_ref("deref atom", type_check_atom(env, a)?, *s)?,
            *s,
        ),
        Atom::Lit(l, _) => Ok(l.notwasm_typ()),
        Atom::ToAny(to_any, s) => {
            let ty = type_check_atom(env, &mut to_any.atom)?;
            assert_variant_of_any(&ty, *s)?;
            to_any.set_ty(ty);
            Ok(Type::Any)
        }
        Atom::FromAny(a, ty, s) => {
            let got = type_check_atom(env, a)?;
            ensure("from_any", Type::Any, got, *s)?;
            Ok(ty.clone())
        }
        Atom::FloatToInt(a, s) => {
            let got = type_check_atom(env, a)?;
            ensure("float to int", Type::F64, got, *s)?;
            Ok(Type::I32)
        }
        Atom::IntToFloat(a, s) => {
            let got = type_check_atom(env, a)?;
            ensure("int to float", Type::I32, got, *s)?;
            Ok(Type::F64)
        }
        Atom::Id(id, s) => lookup(env, id, *s),
        Atom::GetPrimFunc(id, s) => lookup(env, id, *s),
        Atom::StringLen(a, s) => {
            let ty = type_check_atom(env, a)?;
            let _ = ensure("string len", Type::String, ty, *s)?;

            Ok(Type::I32)
        }
        Atom::ArrayLen(a, s) => {
            let got = type_check_atom(env, a)?;
            ensure("array len", Type::Array, got, *s)?;
            Ok(Type::I32)
        }
        Atom::Index(a_arr, a_idx, s) => {
            let got_arr = type_check_atom(env, a_arr)?;
            let got_idx = type_check_atom(env, a_idx)?;
            let _ = ensure("arrayi ndex (index)", Type::I32, got_idx, *s)?;
            let _ = ensure("array index (array)", Type::Array, got_arr, *s);
            Ok(Type::Any)
        }
        Atom::ObjectGet(a_obj, a_field, s) => {
            let got_obj = type_check_atom(env, a_obj)?;
            let got_field = type_check_atom(env, a_field)?;

            let _ = ensure("object get field", Type::String, got_field, *s)?;
            let _ = ensure("object field", Type::DynObject, got_obj, *s)?;
            Ok(Type::Any)
        }
        Atom::HTGet(a_ht, a_field, s) => {
            let got_ht = type_check_atom(env, a_ht)?;
            let got_field = type_check_atom(env, a_field)?;

            ensure("ht get", Type::HT, got_ht, *s)?;
            let _ = ensure("ht get (field)", Type::String, got_field, *s)?;

            Ok(Type::Any)
        }
        Atom::Unary(op, a, s) => {
            let (ty_in, ty_out) = op.notwasm_typ();
            let got = type_check_atom(env, a)?;
            let _ = ensure(&format!("unary ({:?})", op), ty_in, got, *s)?;
            Ok(ty_out)
        }
        Atom::Binary(BinaryOp::PtrEq, a_l, a_r, s) => {
            let got_l = type_check_atom(env, a_l)?;
            let got_r = type_check_atom(env, a_r)?;
            let _ = ensure("binary (===) lhs", got_l.clone(), got_r.clone(), *s)?;
            Ok(Type::Bool)
        }
        Atom::Binary(op, a_l, a_r, s) => {
            let (ty_in, ty_out) = op.notwasm_typ();
            let got_l = type_check_atom(env, a_l)?;
            let got_r = type_check_atom(env, a_r)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in.clone(), got_l, *s)?;
            let _ = ensure(&format!("binary ({:?}) lhs", op), ty_in, got_r, *s)?;
            Ok(ty_out)
        }
        Atom::EnvGet(_, ty, _) => Ok(ty.clone()),
    }
}
