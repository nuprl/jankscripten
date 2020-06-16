use super::syntax::*;
use super::types::*;

/*
use im_rc::HashMap;

#[derive(Debug)]
pub enum TypingError {
    Bad(String)
}

pub type TypingResult<T> = Result<T, TypingError>;

type Env = HashMap<Id, Type>;
*/

enum Coercion {
    Tag(Type),
    Untag(Type),
    Id(Type),
    Seq(Box<Coercion>, Box<Coercion>)
    // TODO: fun_n
}

pub struct Typing {

}

impl Typing {
    fn coerce(&self, t1: Type, t2: Type) {
        if t1 == t2 {
            self.id(t1)
        } else {
            match (t1, t2) {
                (Type::Ground(g1), Type::Ground(g2)) => self.coerce_ground_types(g1, g2),
                (Type::Ground(GroundType::Any), Type::Function(args, res)) => {
                    let tmp1 = Type::Ground(GroundType::Function(args.len()));
                    let tmp2 = Type::Function(args.clone(), res.clone());
                    self.coerce(Type::Ground(GroundType::Any), tmp1.clone());
                    self.coerce(tmp1, tmp2);
                },
                (Type::Function(args, res), Type::Ground(GroundType::Any)) => {
                    let tmp = Type::Ground(GroundType::Function(args.len()));
                    self.coerce(Type::Function(args, res), tmp.clone());
                    self.coerce(tmp, Type::Ground(GroundType::Any));
                },
                _ => unimplemented!()
            }
        }
    }

    fn coerce_ground_types(&self, g1: GroundType, g2: GroundType) {
        match (g1, g2) {
            (GroundType::Any, g2) => self.untag(g2),
            (g1, GroundType::Any) => self.tag(g1),
            _ => unimplemented!()
        }
    }

    fn tag(&self, g: GroundType) {
        unimplemented!()
    }

    fn untag(&self, g: GroundType) {
        unimplemented!()
    }

    fn id(&self, t: Type) {
        unimplemented!()
    }

    fn seq(&self, c1: Coercion, c2: Coercion) {
        unimplemented!()
    }

}