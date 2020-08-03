use super::syntax::*;
use crate::shared::types::Type;
use crate::shared::coercions::*;
use crate::jankyscript::constructors as Janky_;
use crate::jankyscript::syntax as Janky;
use super::super::notwasm::syntax::BinaryOp;
use im_rc::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypingError {
    #[error("{0}")]
    Wrong(String)
}

type CoercionResult<T> = Result<T, TypingError>;

macro_rules! error {
    ($($t:tt)*) => (
        Err(TypingError::Wrong(format!($($t)*)))
    )
}

#[derive(Clone)]
enum EnvItem {
    JsId(Type),
    Prim(Type)
}

#[derive(Clone)]
struct Env {
    env: HashMap<String, EnvItem>
}

impl Env {

    pub fn new() -> Env {
        let mut env: HashMap<String, EnvItem> = HashMap::new();
        env.insert(
            "log_any".to_string(), 
            EnvItem::Prim(Type::Function(vec![Type::Any], Box::new(Type::Any))));
        Env { env }
    }

    /// If `expr` is an identifier that isbound to a primitive, returns its name and type.
    pub fn get_prim_ty(&self, expr: &Expr) -> Option<(String, &Type)> {
        match expr {
            Expr::Id(Id::Named(name)) => match self.env.get(name) {
                Some(EnvItem::Prim(ty)) => Some((name.to_owned(), ty)),
                _ => None
            },
            _ => None
        }
    }
}

#[derive(Default)]
struct InsertCoercions {

}

impl InsertCoercions {
    
    fn stmt(&self, stmt: Stmt, env: Env) -> CoercionResult<Janky::Stmt> {
        match stmt {
            Stmt::Var(x, t, e) => {
                match t {
                    Some(Type::Any) => {
                        let (e, t) = self.expr_and_type(*e, env)?;
                        Ok(Janky_::var_(x, t, e))
                    },
                    _ => unimplemented!()
                }
            },
            Stmt::Block(stmts) => self.stmts(stmts, env),
            Stmt::If(c, t, e) => {
                // coerce the conditional expression into type Bool
                let c = self.expr(*c, Type::Bool, env.clone())?;

                // coerce the two branches and put it all together
                Ok(Janky_::if_(c,
                    self.stmt(*t, env.clone())?,
                    self.stmt(*e, env.clone())?
                ))
            }
            Stmt::While(cond, body) => {
                // coerce the loop conditional into type Bool
                let cond = self.expr(*cond, Type::Bool, env.clone())?;
                let body = self.stmt(*body, env.clone())?;
                Ok(Janky_::while_(cond, body))
            }
            Stmt::Empty => Ok(Janky_::empty_()),
            Stmt::Expr(e) => {
                // One of the few cases where the type does not matter
                let (janky_e, _) = self.expr_and_type(*e, env.clone())?;
                Ok(Janky::Stmt::Expr(Box::new(janky_e)))
            }
            _ => unimplemented!()
        }
    }

    fn stmts(&self, stmts: Vec<Stmt>, env: Env) -> CoercionResult<Janky::Stmt> {
        let mut ret = vec!();
        for stmt in stmts.into_iter() {
            ret.push(self.stmt(stmt, env.clone())?);
        }
        Ok(Janky_::block_(ret))
    }

    fn expr_and_type(&self, expr: Expr, env: Env) -> CoercionResult<(Janky::Expr, Type)> {
        match expr {
            Expr::Lit(l) => {
                let (l, t) = self.lit(l)?;
                Ok((Janky_::lit_(l), t))
            },
            Expr::Binary(op, e1, e2) => {
                let (e1, t1) = self.expr_and_type(*e1, env.clone())?;
                let (e2, t2) = self.expr_and_type(*e2, env.clone())?;
                match (t1, t2) {
                    (Type::Float, Type::Float) => Ok((Janky_::binary_(BinaryOp::F64Add, e1, e2), Type::Float)),
                    (Type::Any, Type::Any) => {
                        let e1 = Janky_::coercion_(self.coerce(Type::Any, Type::Float), e1);
                        let e2 = Janky_::coercion_(self.coerce(Type::Any, Type::Float), e2);
                        todo!("any addition")
                    },
                    _ => unimplemented!()
                }
            },
            Expr::Call(f, args) => {
                // Special case for a primitive function call. JavaScript, and thus JankierScript
                // do not distinguish primitive calls from calls to user-defined functions. However,
                // we make the distinction explicit right here.
                if let Some((prim_name, prim_ty)) = env.get_prim_ty(&f) {
                    if let Type::Function(arg_typs, result_ty) = prim_ty {
                        let coerced_args = args.into_iter().zip(arg_typs.iter())
                            .map(|(e, t)| self.expr(e, t.clone(), env.clone()))
                            .collect::<Result<Vec<_>, _>>()?;
                        let coerced_e = Janky::Expr::PrimCall(prim_name, coerced_args);
                        return Ok((coerced_e, *result_ty.clone()));
                    }
                    return error!("primitive is not a function {:?}", f);
                }
                let (f, t) = self.expr_and_type(*f, env)?;
                unimplemented!()
            }
            _ => todo!("{:?}", expr)
        }
    }

    /// Inserts coercions into an expr AND ensures the expr will have the given 
    /// type.
    fn expr(&self, e: Expr, desired_type: Type, env: Env) -> CoercionResult<Janky::Expr> {
        // insert coercions into the expression
        let (e, e_type) = self.expr_and_type(e, env)?;

        // if the expression is already the desired type, return that
        // if the conditional expression isn't the desired type, coerce it to
        // the desired type.
        if e_type == desired_type {
            Ok(e)
        }
        else {
            Ok(Janky_::coercion_(self.coerce(e_type, desired_type), e))
        }
    }

    fn lit(&self, lit: Janky::Lit) -> CoercionResult<(Janky::Lit, Type)> {
        match lit {
            Janky::Lit::Num(n) => Ok((Janky_::num_(n), Type::Float)),
            _ => unimplemented!()
        }
    }

    fn coerce(&self, t1: Type, t2: Type) -> Coercion {
        if t1 == t2 {
            Coercion::Id(t1)
        } else {
            match (t1.is_ground(), t2.is_ground()) {
                (true, true) => self.coerce_ground_types(t1, t2),
                (true, false) => self.coerce_ground_and_t(t1, t2),
                (false, true) => self.coerce_t_and_ground(t1, t2),
                _ => unimplemented!()
            }
        }
    }

    fn coerce_ground_types(&self, g1: Type, g2: Type) -> Coercion {
        match (g1, g2) {
            (Type::Any, g2) => Coercion::Untag(g2),
            (g1, Type::Any) => Coercion::Tag(g1),
            _ => unimplemented!()
        }
    }

    fn coerce_ground_and_t(&self, g: Type, t: Type) -> Coercion {
        match (g, t) {
            (Type::Any, Type::Function(args, res)) => {
                let tmp1 = Type::ground_function(args.len());
                let tmp2 = Type::Function(args.clone(), res.clone());
                cseq_(self.coerce(Type::Any, tmp1.clone()), self.coerce(tmp1, tmp2))
            },
            _ => unimplemented!()
        }
    }

    fn coerce_t_and_ground(&self, t: Type, g: Type) -> Coercion {
        match (t, g) {
            (Type::Function(args, res), Type::Any) => {
                let tmp = Type::ground_function(args.len());
                cseq_(self.coerce(Type::Function(args, res), tmp.clone()), self.coerce(tmp, Type::Any))
            },
            _ => unimplemented!()
        }
    }

}

pub fn insert_coercions(jankier_prog: Stmt) -> CoercionResult<Janky::Stmt> {
    let coercions = InsertCoercions::default();
    let janky_prog = coercions.stmt(jankier_prog, Env::new())?;
    Ok(janky_prog)
}