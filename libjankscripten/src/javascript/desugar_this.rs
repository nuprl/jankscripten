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
                // Desugar `new` into:
                //     new MyFunc() => var $func = MyFunc; var $obj = Object.create($func.prototype), $func.call($obj, args...), $obj

                // syntax block surrounding this new expression
                let cxt = loc.enclosing_block().unwrap();

                // generate a new name for the function
                let func_name = self.ng.fresh("new_constructor");

                // let $func = func;
                cxt.insert(cxt.index, vardecl1_(func_name.clone(), f.take()));

                // generate a new name for the object
                let obj_name = self.ng.fresh("new_obj");

                // Object.create(f.prototype)
                let new_obj = call_(dot_(id_("Object"), "create"), vec![Expr::Dot(Box::new(Expr::Id(func_name.clone())), Id::Named("prototype".to_string()))]);

                // Insert into the surrounding syntax block:
                //     let $name = Object.create(f.prototype);
                cxt.insert(cxt.index, vardecl1_(obj_name.clone(), new_obj));

                // args => $obj, args...
                args.insert(0, id_(obj_name.clone()));

                // generate constructor call
                let new_call = call_(Expr::Id(func_name.clone()), std::mem::replace(args, vec![]));

                // insert that into the surrounding syntax block
                cxt.insert(cxt.index, expr_(new_call));

                // make the entire `new` expression evaluate to just the new object
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
