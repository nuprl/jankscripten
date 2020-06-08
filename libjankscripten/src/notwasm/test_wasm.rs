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
        // exclude trailing newline
        out_str.trim_end().parse::<i32>().expect("non number"),
        expected
    );
}

use super::constructors::*;

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
        Stmt::Var(id_("x"), Expr::HT(ht_type.clone()), ht_type),
        Stmt::Var(
            id_("_"),
            ht_set_(get_id_("x"), 0, i32_(10), Type::I32),
            Type::I32,
        ),
        Stmt::Return(ht_get_(get_id_("x"), 0, Type::I32)),
    ]));
    test_wasm(10, program);
}
