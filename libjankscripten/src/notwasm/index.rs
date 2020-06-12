//! give de bruijn indexes to all variables

use super::syntax::*;
use super::walk::*;
use std::collections::HashMap;

type IndexEnv = HashMap<Id, u32>;

/// this turns all identifiers to indexes and all closure references to Int
/// expressions that refer to the closure index
pub fn index(program: &mut Program) {
    // this clone is inevitable because we pass &mut func and func_names
    let func_names = program
        .functions
        .keys()
        .enumerate()
        .map(|(i, id)| (id.clone(), i as u32))
        .collect();
    for (_, mut func) in &mut program.functions {
        index_func(&mut func, &func_names);
    }
}

fn index_func(func: &mut Function, func_names: &IndexEnv) {
    let mut vis = IndexVisitor::new(func_names, func.params_tys.clone());
    func.body.walk(&mut vis);
    func.locals = vis.types;
}

struct IndexVisitor<'a> {
    /// ultimately holds a mapping of indexes to types
    types: Vec<Type>,
    /// progressively holds names to indexes, which can then be discarded
    names: IndexEnv,
    /// should be given the map of function names to indexes
    func_names: &'a IndexEnv,
}
impl Visitor for IndexVisitor<'_> {
    fn enter_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Var(id, _, ty) => self.update_env(id, ty.clone()),
            Assign(id, ..) => self.update_id(id),
            // TODO(luna): index label/break
            _ => (),
        }
    }

    fn exit_expr(&mut self, expr: &mut Expr, _loc: &mut Loc) {
        use Expr::*;
        match expr {
            Call(b, cs, ..) => {
                self.update_id(b);
                for c in cs {
                    self.update_id(c);
                }
            }
            _ => (),
        }
    }

    fn exit_atom(&mut self, atom: &mut Atom, _loc: &mut Loc) {
        use Atom::*;
        match atom {
            Id(id, ..) => self.update_id(id),
            _ => (),
        }
    }
}
/// from wasm-experiments
impl<'a> IndexVisitor<'a> {
    fn new(func_names: &'a IndexEnv, params_tys: Vec<(Id, Type)>) -> Self {
        let mut names = IndexEnv::new();
        let mut types = Vec::new();
        for (id, ty) in params_tys {
            names.insert(id, names.len() as u32);
            types.push(ty);
        }
        Self {
            types,
            names,
            func_names,
        }
    }
    /// become aware of a new id, and replace it with a new index
    fn update_env(&mut self, id: &mut Id, ty: Type) {
        self.names.insert(id.clone(), self.types.len() as u32);
        self.types.push(ty);
        self.update_id(id);
    }
    /// replace an id with the index we already know about
    fn update_id(&self, id: &mut Id) {
        match id {
            Id::Named(_) => {
                // We assume that local variables shadow functions, which are
                // declared in the module scope. Thus, we can first look for the
                // index of a local, and then look for the index of a function
                // if no no local exists.
                if let Some(idx) = self.names.get(id) {
                    *id = Id::Index(*idx);
                } else if let Some(idx) = self.func_names.get(id) {
                    *id = Id::Func(*idx);
                } else {
                    panic!("free var {:?}", id)
                }
            }
            Id::Index(_) => panic!("already indexed???"),
            Id::Func(_) => panic!("no closures should be indexed yet {:?}", id),
            Id::Label(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::constructors::*;
    use super::super::syntax::*;
    use super::index;
    use std::collections::HashMap;
    #[test]
    fn index_params() {
        let func = Function {
            locals: Vec::new(),
            body: Stmt::Return(get_id_("the_param")),
            params_tys: vec![(id_("the_param"), Type::I32)],
            ret_ty: Type::I32,
        };
        let mut program = program1_(func);
        index(&mut program);
        let indexed_func = Function {
            locals: vec![Type::I32],
            body: Stmt::Return(Atom::Id(Id::Index(0))),
            params_tys: vec![(id_("the_param"), Type::I32)],
            ret_ty: Type::I32,
        };
        let expected = program1_(indexed_func);
        assert_eq!(program, expected);
    }
}
