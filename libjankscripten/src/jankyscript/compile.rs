use super::box_assigns::box_assigns;
use super::collect_assigns::collect_assigns;
use super::fv::free_vars;
use super::syntax::*;
use super::type_checking::{type_check, TypeCheckingError};

pub fn compile(janky_ast: &mut Stmt) -> Result<(), TypeCheckingError> {
    free_vars(janky_ast);
    type_check(janky_ast)?;
    // TODO(luna): how do we box parameters????????
    let should_box_globals = collect_assigns(janky_ast);
    box_assigns(janky_ast, should_box_globals);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::jankyscript::{compile, constructors::*, fv::free_vars, syntax::*};
    /// we don't yet support closures but boxing criteria have gotten so
    /// messy that i want to test them, so we're building an AST and checking
    ///
    /// ```jankyscript
    /// function f(): i32 {
    ///     var x = 1;
    ///     function g(): i32 {
    ///         return x;
    ///     }
    ///     x = 5;
    ///     return 0;
    /// }
    /// ```
    #[test]
    fn boxes_appropriately() {
        let ret_int = Type::Function(vec![], Box::new(Type::Int));
        let mut program = var_(
            "f".into(),
            ret_int.clone(),
            func(
                vec![],
                Type::Int,
                block_(vec![
                    var_("x".into(), Type::Int, lit_(num_(Num::Int(1)))),
                    var_(
                        "g".into(),
                        ret_int.clone(),
                        func(vec![], Type::Int, return_(Expr::Id("x".into()))),
                    ),
                    expr_(assign_var_("x".into(), lit_(num_(Num::Int(5))))),
                ]),
            ),
        );
        // we didn't run FV on expected so we manually update it for == to work
        let mut expected_inner =
            Func::new(vec![], Type::Int, return_(deref_(Expr::Id("x".into()))));
        expected_inner.free_vars = expected_inner.free_vars.update("x".into());
        let mut expected_outer = Func::new(
            vec![],
            Type::Int,
            block_(vec![
                var_(
                    "x".into(),
                    Type::Ref(Box::new(Type::Int)),
                    new_ref_(lit_(num_(Num::Int(1))), Type::Int),
                ),
                var_("g".into(), ret_int.clone(), Expr::Func(expected_inner)),
                expr_(store_("x".into(), lit_(num_(Num::Int(5))))),
            ]),
        );
        expected_outer.assigned_free_children =
            expected_outer.assigned_free_children.update("x".into());
        let expected = var_("f".into(), ret_int.clone(), Expr::Func(expected_outer));
        compile(&mut program).unwrap();
        assert_eq!(program, expected);
    }
}
