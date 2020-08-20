use super::constructors::*;
use super::syntax::*;
use super::*;

struct ThisParameter<'a> {
    ng: &'a mut NameGen,
    this_name: Id,
}

impl Visitor for ThisParameter<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Call(f, args) => match &mut **f {
                // only syntactically immediate bracket/dot preserves the
                // object as `this`
                Expr::Bracket(obj, _) | Expr::Dot(obj, _) => {
                    // fresh the obj so we can pass it to the method
                    let cxt = loc.enclosing_block().unwrap();
                    let obj_name = self.ng.fresh("obj4this");
                    cxt.insert(cxt.index, vardecl1_(obj_name.clone(), obj.take()));
                    *obj = Box::new(id_(obj_name.clone()));
                    args.insert(0, id_(obj_name));
                }
                // for the rest, we'll hand undefined. all our benchmarks
                // play nice with "use strict";
                _ => args.insert(0, UNDEFINED_),
            },
            Expr::New(f, args) => {
                // new MyFunc() => var fresh = Object.create({}), MyFunc(fresh), fresh
                let cxt = loc.enclosing_block().unwrap();
                let obj_name = self.ng.fresh("new");
                // Object.create({})
                let new_obj = call_(dot_(id_("Object"), "create"), vec![Expr::Object(vec![])]);
                cxt.insert(cxt.index, vardecl1_(obj_name.clone(), new_obj));
                args.insert(0, id_(obj_name.clone()));
                let new_call = call_(f.take(), std::mem::replace(args, vec![]));
                cxt.insert(cxt.index, expr_(new_call));
                *expr = id_(obj_name);
            }
            Expr::Func(_, params, _) => {
                // yes for once using a named id is correct here, because
                // it's a special name that may or may not be used by the body
                params.insert(0, self.this_name.clone());
            }
            Expr::This => {
                *expr = id_(self.this_name.clone());
            }
            _ => (),
        }
    }
}

pub fn desugar_this(program: &mut Stmt, ng: &mut NameGen) {
    let mut v = ThisParameter {
        this_name: ng.fresh("this"),
        ng,
    };
    program.walk(&mut v);
}
