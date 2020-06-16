use super::syntax::*;
use super::types::Type;

use im_rc::HashMap;

#[derive(Debug)]
pub enum TypingError {
    Incorrect(String)
}

pub type TypingResult = Result<Env, TypingError>;

type Env = HashMap<Id, Type>;

type Coercion = (Type, Type);

pub struct Typing {
    coercions: Vec<Coercion>,
    next_var: usize,
}

impl Typing {
    fn fresh_metavar(&mut self) -> Type {
        let x = self.next_var;
        self.next_var = self.next_var + 1;
        return Type::Metavar(x);
    }
}