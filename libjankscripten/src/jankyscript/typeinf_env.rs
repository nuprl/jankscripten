use super::syntax::*;
use crate::shared::std_lib::*;
use crate::shared::Type;
use im_rc::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    env: HashMap<Id, Type>,
}

impl Env {
    pub fn new() -> Env {
        let mut env = HashMap::new();
        for (k, t) in get_global_object() {
            env.insert(Id::Named(k), t.clone());
        }
        Env { env }
    }
    pub fn extend(&self, id: Id, ty: Type) -> Self {
        let mut env = self.clone();
        env.env.insert(id, ty);
        env
    }

    pub fn update(&mut self, id: Id, ty: Type) {
        self.env.insert(id, ty);
    }

    pub fn get(&self, id: &Id) -> Type {
        self.env.get(id).unwrap().clone()
    }
}
