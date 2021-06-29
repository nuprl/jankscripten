//! Some convenient abstractions for working with Z3.
//!

pub struct Z3EZ<'a> {
    bool_vars: Vec<z3::ast::Bool<'a>>,
    ctx: &'a z3::Context,
    solver: &'a z3::Optimize<'a>
}


/// The types of the high-level Z3 API are all parameterized by the lifetime of the instantiated
/// Z3 solver. For example, the type of Z3 booleans is `z3::ast::Bool<'a>`. That lifetime parameter
/// prevents us from storing Z3 booleans in other types, such as the type of JankyScript
/// expressions. This structure provides a workaround
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Bool {
    index: usize
}

impl<'a> Z3EZ<'a> {
    pub fn new(ctx: &'a z3::Context, solver: &'a z3::Optimize<'a>) -> Self {
        let bool_vars = Vec::new();
        Z3EZ { bool_vars, ctx, solver }
    }

    pub fn fresh_bool_const(&mut self) -> Bool {
        let index = self.bool_vars.len();
        self.bool_vars.push(z3::ast::Bool::fresh_const(self.ctx, "b"));
        Bool { index }
    }

  
    pub fn eval_bool_const(&self, model: &'a z3::Model<'a>, b: &Bool) -> bool {
        let z3b = self.bool_vars.get(b.index).expect("Z3 boolean not found");
        model.eval(z3b)
            .expect("no result from model")
            .as_bool()
            .expect("result in model is not a bool")
    }
}

impl Bool {

    pub fn z<'a, 'b>(&self, z3ez: &'b Z3EZ<'a>) -> z3::ast::Bool<'a> {
        z3ez.bool_vars.get(self.index).expect("Z3 boolean not found").clone()
    }

}