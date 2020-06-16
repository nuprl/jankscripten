use super::syntax::*;
use super::types::*;

use im_rc::HashMap;

#[derive(Debug)]
pub enum TypingError {
    Incorrect(String)
}

pub type TypingResult<T> = Result<T, TypingError>;

type Env = HashMap<Id, Type>;

enum Coercion {
    Tag(Type),
    Untag(Type)
}

pub struct Typing {
    next_var: usize,
}

impl Typing {
    fn coerce(t1: Type, t2: Type) {
        match (t1, t2) {
            (Type::Ground(GroundType::Any), Type::Ground(g)) => {
                unimplemented!()
            },
            _ => unimplemented!()
        }
    }

    fn tag() {
        unimplemented!()
    }

    fn untag(g: GroundType) {
        unimplemented!()
    }
}