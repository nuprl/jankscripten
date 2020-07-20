use super::syntax::*;
use crate::shared::types::Type;
use crate::shared::coercions::*;
use crate::shared::ops::*;
use crate::jankyscript::constructors as Janky_;
use crate::jankyscript::syntax as Janky;

use im_rc::HashMap;

#[derive(Debug)]
pub enum TypingError {
    Wrong(String)
}

pub type TypingResult<T> = Result<T, TypingError>;

pub type Env = HashMap<String, Type>;

pub struct Typing {

}

impl Typing {
    fn insert_coercions(&self, stmt: Stmt) -> TypingResult<Janky::Stmt> {
        self.insert_coercions_stmt(stmt, HashMap::new())
    }
    
    fn insert_coercions_stmt(&self, stmt: Stmt, env: Env) -> TypingResult<Janky::Stmt> {
        match stmt {
            Stmt::Var(x, t, e) => {
                match t {
                    Some(Type::Any) => {
                        let (e, t) = self.insert_coercions_expr(e, env)?;
                        Ok(Janky_::var_(&x, t, e))
                    },
                    _ => unimplemented!()
                }
            },
            Stmt::Block(stmts) => self.insert_coercions_stmts(stmts, env),
            Stmt::If(c, t, e) => {
                // coerce the conditional expression into type Bool
                let c = self.coerce_expr(*c, Type::Bool, env)?;

                // coerce the two branches and put it all together
                Ok(Janky_::if_(c,
                    self.insert_coercions_stmt(*t, env)?,
                    self.insert_coercions_stmt(*e, env)?
                ))
            }
            Stmt::While(cond, body) => {
                // coerce the loop conditional into type Bool
                let cond = self.coerce_expr(*cond, Type::Bool, env)?;
                let body = self.insert_coercions_stmt(*body, env)?;
                Ok(Janky_::while_(cond, body))
            }
            Stmt::Empty => Ok(Janky_::empty_()),
            _ => unimplemented!()
        }
    }

    fn insert_coercions_stmts(&self, stmts: Vec<Stmt>, env: Env) -> TypingResult<Janky::Stmt> {
        let mut ret = vec!();
        for stmt in stmts.into_iter() {
            ret.push(self.insert_coercions_stmt(stmt, env.clone())?);
        }
        Ok(Janky_::block_(ret))
    }

    fn insert_coercions_expr(&self, expr: Expr, env: Env) -> TypingResult<(Janky::Expr, Type)> {
        match expr {
            Expr::Lit(l) => {
                let (l, t) = self.insert_coercions_lit(l)?;
                Ok((Janky_::lit_(l), t))
            },
            Expr::Binary(op, e1, e2) => {
                let (e1, t1) = self.insert_coercions_expr(*e1, env.clone())?;
                let (e2, t2) = self.insert_coercions_expr(*e2, env.clone())?;
                match (t1, t2) {
                    (Type::Float, Type::Float) => Ok((Janky_::binary_(BinOp::PlusFloatFloat, e1, e2), Type::Float)),
                    (Type::Any, Type::Any) => {
                        let e1 = Janky_::coercion_(self.coerce(Type::Any, Type::Float), e1);
                        let e2 = Janky_::coercion_(self.coerce(Type::Any, Type::Float), e2);
                        Ok((Janky_::binary_(BinOp::Plus, e1, e2), Type::Any))
                    },
                    _ => unimplemented!()
                }
            },
            Expr::Call(f, args) => {
                let (f, t) = self.insert_coercions_expr(*f, env)?;
                unimplemented!()
            }
            _ => unimplemented!()
        }
    }

    /// Inserts coercions into an expr AND ensures the expr will have the given 
    /// type.
    fn coerce_expr(&self, e: Expr, desired_type: Type, env: Env) -> TypingResult<Janky::Expr> {
        // insert coercions into the expression
        let (e, e_type) = self.insert_coercions_expr(e, env)?;

        // if the expression is already the desired type, return that
        // if the conditional expression isn't the desired type, coerce it to
        // the desired type.
        match e_type {
            desired_type => Ok(e),
            _ => Ok(Janky_::coercion_(self.coerce(e_type, desired_type), e)),
        }
    }

    fn insert_coercions_lit(&self, lit: Lit) -> TypingResult<(Janky::Lit, Type)> {
        match lit {
            Lit::Num(n) => Ok((Janky_::num_(&n), Type::Float)),
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