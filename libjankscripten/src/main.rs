use libjankscripten::notwasm::compile;
use libjankscripten::notwasm::syntax::*;
use std::collections::HashMap;
use std::io::Write;

fn main() {
    let mut functions = HashMap::new();
    functions.insert(
        Id::Named("main".to_string()),
        Function {
            locals: vec![],
            body: Stmt::Block(vec![
                Stmt::Var(
                    Id::Named("x".to_string()),
                    Expr::Atom(Atom::Lit(Lit::I32(5), Type::I32), Type::I32),
                ),
                Stmt::Return(Atom::Id(Id::Named("x".to_string()), Type::I32)),
            ]),
            ty: Type::Fn(vec![], Box::new(Type::I32)),
        },
    );
    std::io::stdout()
        .write_all(
            &compile(Program {
                classes: HashMap::new(),
                functions,
                globals: HashMap::new(),
            })
            .unwrap(),
        )
        .unwrap();
}
