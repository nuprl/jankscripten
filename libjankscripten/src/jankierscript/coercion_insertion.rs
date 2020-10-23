use super::super::notwasm::syntax::BinaryOp;
use super::syntax::*;
use crate::jankyscript::constructors as Janky_;
use crate::jankyscript::syntax as Janky;
use crate::notwasm::syntax as NotWasm;
use crate::rts_function::RTSFunction;
use crate::shared::coercions::*;
use crate::shared::std_lib::get_global_object;
use crate::shared::Type;
use im_rc::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypingError {
    #[error("{0}")]
    Wrong(String),
}

type CoercionResult<T> = Result<T, TypingError>;

macro_rules! error {
    ($($t:tt)*) => (
        Err(TypingError::Wrong(format!($($t)*)))
    )
}

#[derive(Debug, Clone)]
enum EnvItem {
    JsId(Type),
    Prim(RTSFunction),
}

#[derive(Debug, Clone)]
struct Env {
    env: HashMap<Id, EnvItem>,
}

impl Env {
    pub fn new() -> Env {
        let mut env: HashMap<Id, EnvItem> = HashMap::new();
        env.insert(
            Id::Named("log_any".to_string()),
            EnvItem::Prim(RTSFunction::LogAny),
        );
        for (k, v) in get_global_object().into_iter() {
            env.insert(Id::Named(k), EnvItem::JsId(v));
        }
        Env { env }
    }

    pub fn get(&self, id: &Id) -> Option<&Type> {
        match self.env.get(id) {
            Some(EnvItem::JsId(t)) => Some(t),
            _ => None,
        }
    }

    /// If `expr` is an identifier that isbound to a primitive, returns its name and type.
    pub fn get_prim_ty(&self, expr: &Expr) -> Option<RTSFunction> {
        match expr {
            Expr::Id(id, _) => match self.env.get(id) {
                Some(EnvItem::Prim(rts_func)) => Some(*rts_func),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn extend(&self, id: Id, ty: Type) -> Self {
        let mut env = self.clone();
        env.env.insert(id, EnvItem::JsId(ty));
        env
    }

    pub fn update(&mut self, id: Id, ty: Type) {
        self.env.insert(id, EnvItem::JsId(ty));
    }
}

#[derive(Default)]
struct InsertCoercions {}

enum Overload {
    Prim(BinaryOp),
    RTS(RTSFunction),
}

type TypeOverload = (Overload, Type, Type, Type);
fn prim(op: BinaryOp, a: Type, b: Type, c: Type) -> TypeOverload {
    (Overload::Prim(op), a, b, c)
}
fn prim_same(op: BinaryOp, homogenous: Type) -> TypeOverload {
    prim(op, homogenous.clone(), homogenous.clone(), homogenous)
}
fn rts(rts_func: RTSFunction, ret: Type) -> TypeOverload {
    (Overload::RTS(rts_func), Type::Any, Type::Any, ret)
}
// Given a JavaScript binary operator and the types of its operands, returns
// either (1) a Wasm operator and its type, and (2) a function in the runtime
// system and its type.
fn binop_overload(op: &BinOp, lhs_ty: &Type, rhs_ty: &Type) -> TypeOverload {
    use Type::*;
    let anyish = lhs_ty == &Type::Any || rhs_ty == &Type::Any;
    match (op, lhs_ty, rhs_ty) {
        // TODO(arjun): This is not accurate. Adding two 32-bit integers in JS
        // can produce a float.
        (BinOp::Plus, Int, Int) => prim_same(BinaryOp::I32Add, Int),
        (BinOp::Plus, Float, Float) => prim_same(BinaryOp::F64Add, Float),
        (BinOp::Plus, Float, Int) => prim_same(BinaryOp::F64Add, Float),
        (BinOp::Plus, Int, Float) => prim_same(BinaryOp::F64Add, Float),
        (BinOp::Plus, _, _) if anyish => rts(RTSFunction::Plus, Any),
        (BinOp::Minus, Int, Int) => prim_same(BinaryOp::I32Sub, Int),
        (BinOp::Minus, _, _) if anyish => rts(RTSFunction::Minus, Any),
        (BinOp::Minus, _, _) => prim_same(BinaryOp::F64Sub, Float),
        // TODO(luna): string ordering is a thing, but is it used?
        (BinOp::LessThan, Int, Int) => prim(BinaryOp::I32LT, Int, Int, Bool),
        (BinOp::LessThan, _, _) => prim(BinaryOp::F64LT, Float, Float, Bool),
        (BinOp::LessThanEqual, Int, Int) => prim(BinaryOp::I32Le, Int, Int, Bool),
        (BinOp::LessThanEqual, _, _) => prim(BinaryOp::F64Le, Float, Float, Bool),
        (BinOp::GreaterThan, Int, Int) => prim(BinaryOp::I32GT, Int, Int, Bool),
        (BinOp::GreaterThan, _, _) => prim(BinaryOp::F64GT, Float, Float, Bool),
        (BinOp::GreaterThanEqual, Int, Int) => prim(BinaryOp::I32Ge, Int, Int, Bool),
        (BinOp::GreaterThanEqual, _, _) => prim(BinaryOp::F64Ge, Float, Float, Bool),
        (BinOp::And, _, _) => prim(BinaryOp::I32And, Int, Int, Int),
        (BinOp::Times, Int, Int) => prim_same(BinaryOp::I32Mul, Int),
        (BinOp::Times, _, _) if anyish => rts(RTSFunction::Times, Any),
        (BinOp::Times, _, _) => prim_same(BinaryOp::F64Mul, Float),
        (BinOp::Over, _, _) if anyish => rts(RTSFunction::Over, Float),
        (BinOp::Over, _, _) => prim_same(BinaryOp::F64Div, Float),
        (BinOp::Mod, Int, Int) => prim_same(BinaryOp::I32Rem, Int),
        (BinOp::Mod, _, _) if anyish => rts(RTSFunction::Mod, Any),
        (BinOp::Mod, _, _) => rts(RTSFunction::ModF64, Float),
        (BinOp::Equal, Float, Float) | (BinOp::StrictEqual, Float, Float) => {
            prim(BinaryOp::F64Eq, Float, Float, Bool)
        }
        (BinOp::Equal, Int, Int) | (BinOp::StrictEqual, Int, Int) => {
            prim(BinaryOp::I32Eq, Int, Int, Bool)
        }
        (BinOp::Equal, _, _) => rts(RTSFunction::Equal, Bool),
        (BinOp::StrictEqual, _, _) => rts(RTSFunction::StrictEqual, Bool),
        (BinOp::StrictNotEqual, Float, Float) | (BinOp::NotEqual, Float, Float) => {
            prim(BinaryOp::F64Ne, Float, Float, Bool)
        }
        (BinOp::StrictNotEqual, Int, Int) | (BinOp::NotEqual, Int, Int) => {
            prim(BinaryOp::I32Ne, Int, Int, Bool)
        }
        (BinOp::NotEqual, _, _) => rts(RTSFunction::NotEqual, Bool),
        (BinOp::StrictNotEqual, _, _) => rts(RTSFunction::StrictNotEqual, Bool),
        (BinOp::Or, _, _) => prim_same(BinaryOp::I32Or, Int),
        (BinOp::LeftShift, _, _) => prim_same(BinaryOp::I32Shl, Int),
        (BinOp::RightShift, _, _) => prim_same(BinaryOp::I32Shr, Int),
        (_, _, _) => (
            Overload::RTS(RTSFunction::Todo(Box::leak(Box::new(format!(
                "unimplemented binop {:?} {:?} {:?}",
                op, lhs_ty, rhs_ty
            ))))),
            Any,
            Any,
            Any,
        ),
    }
}

impl InsertCoercions {
    fn stmt(&self, stmt: Stmt, env: &mut Env, ret_ty: &Type) -> CoercionResult<Janky::Stmt> {
        match stmt {
            Stmt::Var(x, t, e, s) => {
                let (e, t) = self.expr_and_type(*e, env)?;
                env.env.insert(x.clone(), EnvItem::JsId(t.clone()));
                Ok(Janky_::var_(x, t, e, s))
            }
            Stmt::Block(stmts, s) => self.stmts(stmts, &mut env.clone(), ret_ty, s),
            Stmt::If(c, t, e, s) => {
                // coerce the conditional expression into type Bool
                let c = self.expr(*c, Type::Bool, env, s)?;

                // coerce the two branches and put it all together
                Ok(Janky_::if_(
                    c,
                    // new scopes
                    self.stmt(*t, &mut env.clone(), ret_ty)?,
                    self.stmt(*e, &mut env.clone(), ret_ty)?,
                    s,
                ))
            }
            Stmt::Loop(body, s) => {
                // new scope
                let body = self.stmt(*body, &mut env.clone(), ret_ty)?;
                Ok(Janky_::loop_(body, s))
            }
            Stmt::Empty => Ok(Janky_::empty_()),
            Stmt::Expr(e, s) => {
                // One of the few cases where the type does not matter
                let (janky_e, _) = self.expr_and_type(*e, env)?;
                Ok(Janky::Stmt::Expr(Box::new(janky_e), s))
            }
            Stmt::Label(label, body, s) => {
                let coerced_body = self.stmt(*body, env, ret_ty)?;
                Ok(Janky::Stmt::Label(label, Box::new(coerced_body), s))
            }
            Stmt::Break(id, s) => Ok(Janky::Stmt::Break(id, s)),
            Stmt::Catch(body, exn_name, catch_body, s) => {
                let coerced_body = self.stmt(*body, &mut env.clone(), ret_ty)?;
                let coerced_catch_body = self.stmt(
                    *catch_body,
                    &mut env.extend(exn_name.clone(), Type::Any),
                    ret_ty,
                )?;
                Ok(Janky::Stmt::Catch(
                    Box::new(coerced_body),
                    exn_name,
                    Box::new(coerced_catch_body),
                    s,
                ))
            }
            Stmt::Finally(body, finally_body, s) => {
                let coerced_body = self.stmt(*body, &mut env.clone(), ret_ty)?;
                let coerced_finally_body = self.stmt(*finally_body, &mut env.clone(), ret_ty)?;
                Ok(Janky::Stmt::Finally(
                    Box::new(coerced_body),
                    Box::new(coerced_finally_body),
                    s,
                ))
            }
            Stmt::Throw(expr, s) => {
                let coerced_expr = self.expr(*expr, Type::Any, &mut env.clone(), s)?;
                Ok(Janky::Stmt::Throw(Box::new(coerced_expr), s))
            }
            Stmt::Return(expr, s) => {
                let coerced_expr = self.expr(*expr, ret_ty.clone(), &mut env.clone(), s)?;

                Ok(Janky::Stmt::Return(Box::new(coerced_expr), s))
            }
        }
    }

    fn stmts(
        &self,
        stmts: Vec<Stmt>,
        env: &mut Env,
        ret_ty: &Type,
        s: Span,
    ) -> CoercionResult<Janky::Stmt> {
        let mut ret = vec![];
        for stmt in stmts.into_iter() {
            ret.push(self.stmt(stmt, env, ret_ty)?);
        }
        Ok(Janky_::block_(ret, s))
    }

    fn exprs(
        &self,
        env: &mut Env,
        exprs: Vec<Expr>,
        expected_tys: Vec<Type>,
    ) -> CoercionResult<Vec<Janky::Expr>> {
        exprs
            .into_iter()
            .zip(expected_tys.into_iter())
            .map(|(e, t)| self.expr(e, t, env, DUMMY_SP))
            .collect::<Result<Vec<_>, _>>()
    }

    fn expr_and_type(&self, expr: Expr, env: &mut Env) -> CoercionResult<(Janky::Expr, Type)> {
        match expr {
            Expr::Lit(l, s) => {
                let (l, t) = self.lit(l)?;
                Ok((Janky_::lit_(l, s), t))
            }
            Expr::Array(es, s) => Ok((
                Janky::Expr::Array(
                    es.into_iter()
                        .map(|e| self.expr(e, Type::Any, env, s))
                        .collect::<Result<_, _>>()?,
                    s,
                ),
                Type::Array,
            )),
            Expr::Object(kvs, s) => Ok((
                Janky::Expr::Object(
                    kvs.into_iter()
                        .map(|(k, v)| self.expr(v, Type::Any, env, s).and_then(|v| Ok((k, v))))
                        .collect::<Result<_, _>>()?,
                    s,
                ),
                Type::DynObject,
            )),
            Expr::Id(id, s) => {
                if let Some(EnvItem::JsId(ty)) = env.env.get(&id) {
                    Ok((Janky::Expr::Id(id, ty.clone(), s), ty.clone()))
                } else {
                    todo!("Identifier: {:?}", id)
                }
            }
            Expr::Bracket(container, field, s) => {
                // container is either array or object. for now, i'm going
                // to assume it's an array because introducing OR into here
                // seems pretty messy (TODO(luna))

                // michael: probably best to have different bracket operations for
                // different types. there could be four:
                //
                // 1. arrays
                // 2. objects
                // 3. strings
                // 4. Any
                //
                // with maybe...
                //
                // 5. instances of classes
                let cont = self.expr(*container, Type::Array, env, s)?;
                let f = self.expr(*field, Type::Int, env, s)?;
                // all containers yield Any
                Ok((Janky_::bracket_(cont, f, s), Type::Any))
            }
            Expr::Dot(container, field, s) => {
                let cont = self.expr(*container, Type::DynObject, env, s)?;
                // all containers yield Any
                Ok((Janky_::dot_(cont, field, s), Type::Any))
            }
            Expr::Binary(op, e1, e2, s) => {
                let (e1, t1) = self.expr_and_type(*e1, env)?;
                let (e2, t2) = self.expr_and_type(*e2, env)?;
                let (overload, expected_t1, expected_t2, result_ty) = binop_overload(&op, &t1, &t2);
                let coerced_e1 = self.coerce(e1, t1, expected_t1, s);
                let coerced_e2 = self.coerce(e2, t2, expected_t2, s);
                let coerced_expr = match overload {
                    Overload::Prim(op) => Janky_::binary_(op, coerced_e1, coerced_e2, s),
                    Overload::RTS(name) => {
                        Janky::Expr::PrimCall(name, vec![coerced_e1, coerced_e2], s)
                    }
                };
                Ok((coerced_expr, result_ty))
            }
            Expr::Assign(lv, e, s) => match *lv {
                LValue::Id(id) => {
                    // TODO(michael) if we're going to allow strong update, some change has to happen here
                    let (_, into_ty) = self.expr_and_type(Expr::Id(id.clone(), s), env)?;
                    Ok((
                        Janky_::assign_(
                            Janky::LValue::Id(id, into_ty.clone()),
                            self.expr(*e, into_ty.clone(), env, s)?,
                            s,
                        ),
                        into_ty,
                    ))
                }
                LValue::Dot(container, field) => {
                    let cont = self.expr(container, Type::DynObject, env, s)?;
                    let expr = self.expr(*e, Type::Any, env, s)?;
                    // all containers yield Any
                    Ok((
                        Janky_::assign_(Janky::LValue::Dot(cont, field), expr, s),
                        Type::Any,
                    ))
                }
                LValue::Bracket(container, field) => {
                    // TODO(luna): don't assume bracket is an array (could
                    // be dynamic field with object)
                    let cont = self.expr(container, Type::Array, env, s)?;
                    let f = self.expr(field, Type::Int, env, s)?;
                    let expr = self.expr(*e, Type::Any, env, s)?;
                    // array assign yields the rvalue
                    Ok((
                        Janky_::assign_(Janky::LValue::Bracket(cont, f), expr, s),
                        Type::Any,
                    ))
                }
            },
            Expr::Call(f, args, s) => {
                // Special case for a primitive function call. JavaScript, and thus JankierScript
                // do not distinguish primitive calls from calls to user-defined functions. However,
                // we make the distinction explicit right here.
                if let Some(rts_func) = env.get_prim_ty(&f) {
                    if let Type::Function(arg_typs, result_ty) = rts_func.janky_typ() {
                        let coerced_args = self.exprs(&mut env.clone(), args, arg_typs)?;
                        let coerced_e = Janky::Expr::PrimCall(rts_func, coerced_args, s);
                        return Ok((coerced_e, *result_ty.clone()));
                    }
                    return error!("primitive is not a function {:?}", f);
                }

                // TODO(arjun): Handle Expr::Call in expr too, for bidirectionality?
                // michael: eh, no need. if we use inference, it should handle it for us.
                let args_with_typs = args
                    .into_iter()
                    .map(|e| self.expr_and_type(e, &mut env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;

                let (coerced_f, f_ty) = self.expr_and_type(*f, &mut env.clone())?;
                let (coerced_f, f_args, f_ret) = match f_ty {
                    Type::Function(args, ret) => {
                        if args.len() != args_with_typs.len() {
                            return error!(
                                "bad arity applying function {:?}, got {} args",
                                coerced_f,
                                args_with_typs.len()
                            );
                        }

                        (coerced_f, args, *ret)
                    }
                    f_ty => {
                        let n = args_with_typs.len();

                        (
                            self.coerce(coerced_f, f_ty, Type::ground_function(n), s),
                            vec![Type::Any; n],
                            Type::Any,
                        )
                    }
                };

                let coerced_args = args_with_typs
                    .into_iter()
                    .zip(f_args.into_iter())
                    .map(|((e, actual_ty), formal_ty)| self.coerce(e, actual_ty, formal_ty, s))
                    .collect();

                Ok((
                    Janky::Expr::Call(Box::new(coerced_f), coerced_args, s),
                    f_ret,
                ))
            }
            Expr::Unary(op, e, s) => {
                use super::super::javascript::UnaryOp;
                let (coerced_e, e_ty) = self.expr_and_type(*e, &mut env.clone())?;
                match (&op, &e_ty) {
                    // Bitwise not; needed for one particular dart benchmark
                    (UnaryOp::Tilde, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Todo("bitwise not"),
                            vec![self.coerce(coerced_e, e_ty, Type::Int, s)],
                            s,
                        ),
                        Type::Int,
                    )),
                    (UnaryOp::Plus, Type::Float) => Ok((coerced_e, e_ty)),
                    (UnaryOp::Plus, Type::Int) => Ok((coerced_e, e_ty)),
                    (UnaryOp::Plus, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Plus,
                            vec![
                                Janky::Expr::Lit(Janky::Lit::Num(Janky::Num::Int(0)), s),
                                self.coerce(coerced_e, e_ty, Type::Any, s),
                            ],
                            s,
                        ),
                        Type::Any,
                    )),
                    (UnaryOp::Minus, Type::Float) => Ok((
                        Janky::Expr::Unary(NotWasm::UnaryOp::Neg, Box::new(coerced_e), s),
                        e_ty,
                    )),
                    (UnaryOp::Minus, Type::Int) => Ok((
                        Janky::Expr::Binary(
                            NotWasm::BinaryOp::I32Sub,
                            Box::new(Janky::Expr::Lit(
                                Janky::Lit::Num(crate::javascript::Num::Int(0)),
                                s,
                            )),
                            Box::new(coerced_e),
                            s,
                        ),
                        e_ty,
                    )),
                    (UnaryOp::Minus, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Minus,
                            vec![
                                Janky::Expr::Lit(Janky::Lit::Num(Janky::Num::Int(0)), s),
                                self.coerce(coerced_e, e_ty, Type::Any, s),
                            ],
                            s,
                        ),
                        Type::Any,
                    )),
                    (UnaryOp::Not, _) => Ok((
                        Janky::Expr::Unary(
                            NotWasm::UnaryOp::Eqz,
                            Box::new(self.coerce(coerced_e, e_ty, Type::Bool, s)),
                            s,
                        ),
                        Type::Bool,
                    )),
                    (UnaryOp::TypeOf, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Typeof,
                            vec![self.coerce(coerced_e, e_ty, Type::Any, s)],
                            s,
                        ),
                        Type::String,
                    )),
                    (UnaryOp::Void, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Void,
                            vec![self.coerce(coerced_e, e_ty, Type::Any, s)],
                            s,
                        ),
                        Type::Any,
                    )),
                    (UnaryOp::Delete, _) => Ok((
                        Janky::Expr::PrimCall(
                            RTSFunction::Todo("delete"),
                            vec![self.coerce(coerced_e, e_ty, Type::Any, s)],
                            s,
                        ),
                        Type::Any,
                    )),
                }
            }
            Expr::Func(opt_ret_ty, args_with_opt_tys, body, s) => {
                let mut body_env = env.clone();

                let mut arg_tys = Vec::with_capacity(args_with_opt_tys.len());
                let mut args_with_tys = Vec::with_capacity(args_with_opt_tys.len());
                for (arg_id, opt_ty) in args_with_opt_tys {
                    let ty = opt_ty.unwrap_or(Type::Any);
                    arg_tys.push(ty.clone());
                    args_with_tys.push((arg_id.clone(), ty.clone()));
                    body_env.update(arg_id, ty);
                }

                let ret_ty = opt_ret_ty.unwrap_or(Type::Any);
                let coerced_body = self.stmt(*body, &mut body_env, &ret_ty)?;
                // TODO(luna): i still don't know how we're dealing with
                // typed undefineds but this is what i'm doing for now.
                // this is needed for giving a default return (undefined in JS)
                let default_for_ty = match ret_ty {
                    Type::Any => self.coerce(
                        Janky_::lit_(Janky_::num_(Janky::Num::Int(0), s), s),
                        Type::Int,
                        Type::Any,
                        s,
                    ),
                    _ => todo!(),
                };
                let coerced_body =
                    Janky::Stmt::Block(vec![coerced_body, Janky_::return_(default_for_ty, s)], s);
                let fn_ty = Type::Function(arg_tys, Box::new(ret_ty.clone()));

                Ok((Janky_::func(args_with_tys, ret_ty, coerced_body, s), fn_ty))
            }
        }
    }

    fn typeof_lit(&self, lit: &Janky::Lit) -> Type {
        use Janky::Lit;
        match lit {
            Lit::Num(Janky::Num::Float(_)) => Type::Float,
            Lit::Num(Janky::Num::Int(_)) => Type::Int,
            Lit::String(_) => Type::String,
            Lit::Bool(_) => Type::Bool,
            Lit::Regex(..) => Type::Any,
            Lit::Undefined => Type::Any,
            Lit::Null => Type::Any,
        }
    }

    fn lit(&self, lit: Janky::Lit) -> CoercionResult<(Janky::Lit, Type)> {
        let ty = self.typeof_lit(&lit);
        Ok((lit, ty))
    }

    /// Inserts coercions into an expr AND ensures the expr will have the given
    /// type.
    fn expr(
        &self,
        e: Expr,
        desired_type: Type,
        env: &mut Env,
        s: Span,
    ) -> CoercionResult<Janky::Expr> {
        // insert coercions into the expression
        let (e, e_type) = self.expr_and_type(e, env)?;

        // if the expression is already the desired type, then self.coerce will
        // return `Coercion::Id` and `Janky_::coercion_` won't bother inserting
        // anything.
        Ok(self.coerce(e, e_type, desired_type, s))
    }

    fn coerce(&self, e: Janky::Expr, t1: Type, t2: Type, s: Span) -> Janky::Expr {
        Janky_::coercion_(self.coercion(t1, t2), e, s)
    }

    fn coercion(&self, t1: Type, t2: Type) -> Coercion {
        if t1 == t2 {
            Coercion::Id(t1)
        } else {
            match (t1, t2) {
                (Type::Any, t2) if t2.is_ground() => Coercion::Untag(t2),
                (t1, Type::Any) if t1.is_ground() => Coercion::Tag(t1),
                (Type::Any, Type::Function(args, ret)) => {
                    let gf = Type::ground_function(args.len());
                    Coercion::seq(
                        self.coercion(Type::Any, gf.clone()),
                        self.coercion(gf, Type::Function(args, ret)),
                    )
                }
                (Type::Any, Type::Function(args, ret)) => {
                    let gf = Type::ground_function(args.len());
                    Coercion::seq(
                        self.coercion(Type::Function(args, ret), gf.clone()),
                        self.coercion(gf, Type::Any),
                    )
                }
                (Type::Function(args1, ret1), Type::Function(args2, ret2)) => {
                    if args1.len() != args2.len() {
                        panic!("Coercing between arities: {:?} to {:?}", args1, args2);
                    }

                    Coercion::fun(
                        args1
                            .into_iter()
                            .zip(args2.into_iter())
                            .map(|(arg1, arg2)| self.coercion(arg2, arg1))
                            .collect(),
                        self.coercion(*ret1, *ret2),
                    )
                }
                (Type::Int, Type::Float) => Coercion::IntToFloat,
                (Type::Float, Type::Int) => Coercion::FloatToInt,
                (t1, t2) => {
                    eprintln!("doing coerce({:?}, {:?}) through Any", t1, t2);
                    Coercion::seq(self.coercion(t1, Type::Any), self.coercion(Type::Any, t2))
                }
            }
        }
    }
}

pub fn insert_coercions(jankier_prog: Stmt) -> CoercionResult<Janky::Stmt> {
    let coercions = InsertCoercions::default();
    let janky_prog = coercions.stmt(jankier_prog, &mut Env::new(), &Type::Any)?;
    Ok(janky_prog)
}
