//! give de bruijn indexes to all variables

use super::syntax::*;
use super::walk::*;
use std::collections::HashMap;

type IndexEnv = HashMap<Id, u32>;

/// this turns all identifiers to indexes and all closure references to Int
/// expressions that refer to the closure index
fn index(program: &mut Program) {
    // this clone is inevitable because we pass &mut func and func_names
    let func_names = program
        .functions
        .keys()
        .enumerate()
        .map(|(i, id)| (id.clone(), i as u32))
        .collect();
    for (_, func) in program.functions {
        index_func(&mut func, &func_names);
    }
}

fn index_func(func: &mut Function, funcs: &IndexEnv) {
    let mut vis = IndexVisitor::new(funcs);
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
            Var(id, expr) => self.update_env(id, expr.get_type()),
            Assign(id, expr) => self.update_id(id),
            Call(a, b, cs, ty) | New(a, b, cs, ty) => {
                self.update_env(a, ty.clone());
                self.update_id(b);
                for c in cs {
                    self.update_id(c);
                }
            }
            // TODO(luna): index label/break
            //HT(pairs) => {
            //    for (id, val) in pairs {
            //        update_id(func_names, names, id);
            //        index_func(types, func_names, names, val);
            //    }
            //}
            _ => (),
        }
    }

    fn exit_expr(&mut self, expr: &mut Expr, _loc: &mut Loc) {
        use Expr::*;
        match expr {
            Id(id, ..) => self.update_id(id),
            //HT(pairs) => {
            //    for (id, val) in pairs {
            //        update_id(func_names, names, id);
            //        index_func(types, func_names, names, val);
            //    }
            //}
            _ => (),
        }
    }
}
/// from wasm-experiments
impl<'a> IndexVisitor<'a> {
    fn new(func_names: &'a IndexEnv) -> Self {
        Self {
            types: Vec::new(),
            names: IndexEnv::new(),
            func_names,
        }
    }
    fn update_env(&mut self, id: &mut Id, ty: Type) {
        self.names.insert(id.clone(), self.types.len() as u32);
        self.types.push(ty);
        self.update_id(id);
    }

    fn update_id(&self, id: &mut Id) {
        match id {
            Id::Named(_) => {
                // technically all lambdas have more outer scope than locals so
                // this hackneyed scope trick works
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
        }
    }
}

/*pub fn _index(mut exp: &mut Exp) -> Vec<Vec<Type>> {
    let mut locals_vec = vec![];
    let mut func_names = HashMap::new();
    // first we gather the lifted functions and find their locals arrays
                        update_id(&func_names, &mut HashMap::new(), name);
                        // we have to skip parameters because they're not
                        // included in locals, but the indexes still start
                        // with parameters
                        locals_vec.push(locals.into_iter().skip(params_tys.len()).collect());
                    }
                    _ => break,
                }
            }
            _ => break,
        }
}*/
