use super::compile;
use super::syntax::*;
use std::io::Write;
use std::process::{Command, Stdio};

fn test_wasm(expected: i32, program: Program) {
    let wasm = compile(program).expect("couldn't compile");
    // build runtime
    Command::new("cargo")
        .arg("build")
        .arg("--target=wasm32-unknown-unknown")
        .current_dir("../runtime/")
        .status()
        .expect("failed to build runtime");
    // output to stdout for debugging
    let wat = Command::new("wasm2wat")
        .arg("-")
        .arg("--no-check")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn();
    if let Ok(mut wat) = wat {
        {
            wat.stdin
                .as_mut()
                .expect("no stdin")
                .write_all(&wasm)
                .expect("couldn't write");
        }
        let wat_out = wat.wait_with_output().expect("no stdout");
        println!("{}", String::from_utf8_lossy(&wat_out.stdout));
    } else {
        println!("warning: no wasm2wat for debugging");
    }
    let mut run = Command::new("cargo")
        .arg("run")
        .current_dir("../tester/")
        // avoids needing tmp file which is test threading mess
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to run tester binary");
    // https://doc.rust-lang.org/std/process/struct.Stdio.html
    {
        run.stdin
            .as_mut()
            .expect("no stdin")
            .write_all(&wasm)
            .expect("couldn't write");
    }
    let out = run.wait_with_output().expect("no stdout");
    // print stderr for debugging
    println!("{}", String::from_utf8_lossy(&out.stderr));
    let out_str = String::from_utf8_lossy(&out.stdout);
    println!("{}", out_str);
    assert_eq!(
        expected,
        // exclude trailing newline
        out_str.trim_end().parse::<i32>().expect("non number"),
    );
}

use super::constructors::*;
use std::collections::HashMap;

#[test]
fn works_no_runtime() {
    let program = test_program_(Stmt::Block(vec![
        Stmt::Var(
            Id::Named("x".to_string()),
            Expr::Atom(Atom::Lit(Lit::I32(5))),
            Type::I32,
        ),
        Stmt::Return(Atom::Id(Id::Named("x".to_string()))),
    ]));
    test_wasm(5, program);
}

#[test]
#[should_panic]
fn fails_no_runtime() {
    let program = test_program_(Stmt::Return(Atom::Lit(Lit::I32(5))));
    test_wasm(7, program);
}

#[test]
fn works_with_runtime() {
    let ht_type = Type::HT(Box::new(Type::I32));
    let program = test_program_(Stmt::Block(vec![
        Stmt::Var(id_("x"), Expr::HT(Type::I32), ht_type),
        Stmt::Var(
            id_("_"),
            ht_set_(get_id_("x"), 0, i32_(10), Type::I32),
            Type::I32,
        ),
        Stmt::Return(ht_get_(get_id_("x"), 0, Type::I32)),
    ]));
    test_wasm(10, program);
}

#[test]
fn binary_ops() {
    let program = test_program_(Stmt::Return(plus_(i32_(5), i32_(7), Type::I32)));
    test_wasm(12, program);
}

#[test]
fn functions() {
    let mut funcs = HashMap::new();
    funcs.insert(
        id_("to_call"),
        Function {
            locals: Vec::new(),
            body: Stmt::Return(i32_(5)),
            params_tys: Vec::new(),
            ret_ty: Type::I32,
        },
    );
    funcs.insert(
        id_("main"),
        Function {
            locals: Vec::new(),
            body: Stmt::Block(vec![
                Stmt::Var(
                    id_("x"),
                    Expr::Call(id_("to_call"), vec![], vec![], Type::I32),
                    Type::I32,
                ),
                Stmt::Return(plus_(i32_(4), get_id_("x"), Type::I32)),
            ]),
            params_tys: Vec::new(),
            ret_ty: Type::I32,
        },
    );
    let program = program_(funcs);
    test_wasm(9, program);
}

#[test]
fn break_block() {
    let body = Stmt::Block(vec![
        Stmt::Var(id_("x"), atom_(i32_(0)), Type::I32),
        label_(
            id_("dont_do"),
            Stmt::Block(vec![
                Stmt::Break(id_("dont_do")),
                Stmt::Assign(id_("x"), atom_(i32_(1))),
            ]),
        ),
        Stmt::Return(get_id_("x")),
    ]);
    let program = test_program_(body);
    test_wasm(0, program);
}

#[test]
fn fib() {
    let body = Stmt::Block(vec![
        Stmt::Var(id_("a"), atom_(i32_(1)), Type::I32),
        Stmt::Var(id_("b"), atom_(i32_(1)), Type::I32),
        label_(
            id_("loop"),
            loop_(Stmt::Block(vec![
                if_(
                    gt_(get_id_("b"), i32_(1000), Type::I32),
                    Stmt::Break(id_("loop")),
                    Stmt::Empty,
                ),
                Stmt::Var(
                    id_("temp"),
                    atom_(plus_(get_id_("a"), get_id_("b"), Type::I32)),
                    Type::I32,
                ),
                Stmt::Assign(id_("a"), atom_(get_id_("b"))),
                Stmt::Assign(id_("b"), atom_(get_id_("temp"))),
            ])),
        ),
        Stmt::Return(get_id_("b")),
    ]);
    let program = test_program_(body);
    test_wasm(1597, program);
}
