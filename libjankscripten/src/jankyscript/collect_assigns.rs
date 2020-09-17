//! determine variables that need to be boxed
//!
//! we box a variable completely if it is captured by a function at any
//! point in its scope AND it is assigned to at any point in its scope
//!
//! if it isn't boxed, assignments after the environment creation will not
//! update the closure environment; also, assignments into the closure
//! environment will not update the surrounding environment

use super::syntax::*;
use super::walk::*;
use im_rc::HashSet as ImmHashSet;

/// returns variables in the global scope that should be boxed
pub fn collect_assigns(program: &mut Stmt) -> ImmHashSet<Id> {
    let mut v = CollectAssigns::new();
    program.walk(&mut v);
    assert_eq!(v.free_children.len(), 1);
    assert_eq!(v.assigned_vars.len(), 1);
    let free_children = v.free_children.pop().unwrap();
    let assigned_vars = v.assigned_vars.pop().unwrap();
    free_children.intersection(assigned_vars.clone())
}

struct CollectAssigns {
    /// this represents the free variables of all functions created in this
    /// function ("children")
    free_children: Vec<ImmHashSet<Id>>,
    assigned_vars: Vec<ImmHashSet<Id>>,
}
impl Visitor for CollectAssigns {
    fn enter_fn(&mut self, _func: &mut Func, _: &Loc) {
        self.free_children.push(ImmHashSet::new());
        self.assigned_vars.push(ImmHashSet::new());
    }
    fn exit_fn(&mut self, func: &mut Func, _: &Loc) {
        let free_children = self.free_children.pop().unwrap();
        let assigned_vars = self.assigned_vars.pop().unwrap();
        func.assigned_free_children = free_children.intersection(assigned_vars.clone());
        // add these free variables to the free children of our parent so
        // it'll eventually have all the proper free children
        let parent_free_children = self.free_children.last_mut().unwrap();
        *parent_free_children = parent_free_children.clone().union(func.free_vars.clone());
        // this might look similar but it's very different: we care about
        // the assigned vars of ourselves and our children but not our parents,
        // so propagate up children's data
        let parent_assigned_vars = self.assigned_vars.last_mut().unwrap();
        *parent_assigned_vars = parent_assigned_vars.clone().union(assigned_vars);
    }
    fn exit_expr(&mut self, expr: &mut Expr, _: &Loc) {
        match expr {
            Expr::Assign(lv, to) => {
                match &**lv {
                    LValue::Id(id) => {
                        let assigned_vars = self.assigned_vars.last_mut().unwrap();
                        *assigned_vars = assigned_vars.update(id.clone());
                    }
                    // []/. => boxed already!
                    _ => (),
                }
            }
            _ => (),
        }
    }
}
impl CollectAssigns {
    fn new() -> Self {
        Self {
            // start with the top level
            free_children: vec![ImmHashSet::new()],
            assigned_vars: vec![ImmHashSet::new()],
        }
    }
}
