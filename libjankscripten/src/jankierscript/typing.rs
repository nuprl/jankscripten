use super::syntax::*;
use crate::shared::types::Type;
use crate::shared::coercions::*;

#[derive(Debug)]
pub enum TypingError {
    Wrong(String)
}

pub type TypingResult<T> = Result<T, TypingError>;

pub struct Typing {

}

impl Typing {
    fn coerce(&self, t1: Type, t2: Type) -> Coercion {
        if t1 == t2 {
            unimplemented!();
            //vec!(Coercion::Id(t1))
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
    
    fn typing_stmt(&self, stmt: Stmt) -> TypingResult<Stmt> {
        unimplemented!()
    }

    fn typing_expr(&self, expr: Expr) -> TypingResult<(Expr, Type)> {
        match expr {
            Expr::Lit(l) => {
                let (l, t) = self.typing_lit(l)?;
                Ok((Expr::Lit(l), t))
            },
            Expr::Binary(op, e1, e2) => {
                let (e1, t1) = self.typing_expr(*e1)?;
                let (e2, t2) = self.typing_expr(*e2)?;
                unimplemented!()
            },
            _ => unimplemented!()
        }
    }

    fn typing_lit(&self, lit: Lit) -> TypingResult<(Lit, Type)> {
        match lit {
            Lit::Num(n) => Ok((Lit::Num(n), Type::Float)),
            _ => unimplemented!()
        }
    }

}