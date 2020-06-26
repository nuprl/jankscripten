use super::compile;
use super::parser::parse;
use super::syntax::*;
use std::io::Write;
use std::process::{Command, Stdio};
use std::sync::Once;

static COMPILE_RUNTIME: Once = Once::new();

fn test_wasm(expected: i32, program: Program) {
    let wasm = compile(program).expect("couldn't compile");
    // NOTE(arjun): It is temption to make the runtime system a dev-dependency
    // in Cargo.toml. However, I do not believe that will work. I assume that
    // dev-dependencies are compiled with the same target as the primary crate.
    // However, we need the runtime system to target WebAssembly and this crate
    // to target native.
    COMPILE_RUNTIME.call_once(|| {
        // build runtime
        Command::new("cargo")
            .arg("build")
            .arg("--target=wasm32-unknown-unknown")
            .current_dir("../runtime/")
            .status()
            .expect("failed to build runtime");
    });

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
    let mut run = Command::new("../target/debug/tester")
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
fn test_string_ht() {
    let ht_type = Type::HT(Box::new(Type::I32));
    let program = test_program_(Stmt::Block(vec![
        Stmt::Var(id_("x"), Expr::HT(Type::I32), ht_type),
        Stmt::Var(id_("key1"), Expr::ToString(str_("1")), Type::String),
        Stmt::Var(id_("key1Eq"), Expr::ToString(str_("1")), Type::String),
        Stmt::Var(id_("key2"), Expr::ToString(str_("2")), Type::String),
        Stmt::Var(
            id_("_"),
            ht_set_(get_id_("x"), get_id_("key1"), i32_(1), Type::I32),
            Type::I32,
        ),
        Stmt::Var(
            id_("_"),
            ht_set_(get_id_("x"), get_id_("key2"), i32_(2), Type::I32),
            Type::I32,
        ),
        Stmt::Return(ht_get_(get_id_("x"), get_id_("key1Eq"), Type::I32)),
    ]));
    test_wasm(1, program);
}

#[test]
fn array_push_index() {
    let program = test_program_(Stmt::Block(vec![
        Stmt::Var(id_("x"), Expr::Array(Type::I32), array_ty_(Type::I32)),
        Stmt::Var(
            id_("_"),
            Expr::Push(get_id_("x"), i32_(135), Type::I32),
            Type::I32,
        ),
        Stmt::Var(
            id_("_"),
            Expr::Push(get_id_("x"), i32_(7), Type::I32),
            Type::I32,
        ),
        Stmt::Var(
            id_("_"),
            Expr::Push(get_id_("x"), i32_(98), Type::I32),
            Type::I32,
        ),
        Stmt::Return(index_(get_id_("x"), i32_(2), Type::I32)),
    ]));
    test_wasm(98, program);
}

#[test]
fn binary_ops() {
    let program = parse(
        r#"
        function main() : i32 {
            return 5 + 7;
        }
    "#,
    );
    test_wasm(12, program);
}

#[test]
fn functions() {
    let program = parse(
        r#"
        function toCall() : i32 {
            return 5;
        }

        function main() : i32 {
            var x : i32 = toCall();
            return 4 + x;
        }
    "#,
    );
    test_wasm(9, program);
}

#[test]
fn break_block() {
    let body = Stmt::Block(vec![
        Stmt::Var(id_("x"), atom_(i32_(0)), Type::I32),
        label_(
            "dont_do",
            Stmt::Block(vec![
                Stmt::Break("dont_do".into()),
                Stmt::Assign(id_("x"), atom_(i32_(1))),
            ]),
        ),
        Stmt::Return(get_id_("x")),
    ]);
    let program = test_program_(body);
    test_wasm(0, program);
}

#[test]
fn big_sum() {
    let program = parse(
        r#"
        function main() : i32 {
            var a : i32 = 1;
            var b : i32 = 1;
            loop {
                if (b > 1000) {
                    return b;
                } else { }
                var temp : i32 = a + b;
                a = b;
                b = temp;
            }
            return b;
        }
    "#,
    );
    test_wasm(1597, program);
}

#[test]
fn trivial_direct_call() {
    let program = parse(
        r#"
        function F(n : i32) : i32 {
            return n;
        }

        function main() : i32 {
            var n : i32 = 100;
            var result : i32 = F(n);
            return result;
        }
    "#,
    );

    test_wasm(100, program);
}

#[test]
fn trivial_indirect_call() {
    let program = parse(
        r#"
        function F(n : i32) : i32 {
            return n + 1;
        }

        function main() : i32 {
            var G : (i32) -> i32 = F;
            var n : i32 = 102;
            var result : i32 = G(n);
            return result;
        }
    "#,
    );

    test_wasm(103, program);
}

#[test]
fn goto_skips_stuff() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(id_("x"), atom_(i32_(5)), Type::I32),
        Stmt::Goto(Label::App(0)),
        // this is the part we wanna skip
        Stmt::Assign(id_("x"), atom_(i32_(2))),
        // goto goes here
        Stmt::Var(id_("_"), Expr::Call(id_("other"), vec![]), Type::I32),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}
#[test]
fn goto_skips_loop() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(id_("x"), atom_(i32_(5)), Type::I32),
        Stmt::Goto(Label::App(0)),
        // this is the part we wanna skip
        loop_(Stmt::Empty),
        // goto goes here
        Stmt::Var(id_("_"), Expr::Call(id_("other"), vec![]), Type::I32),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}
#[test]
fn goto_enters_if() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(id_("x"), atom_(i32_(5)), Type::I32),
        Stmt::Goto(Label::App(0)),
        if_(
            TRUE_,
            // this is the part we wanna skip
            Stmt::Assign(id_("x"), atom_(i32_(2))),
            // goto goes here
            Stmt::Var(id_("_"), Expr::Call(id_("other"), vec![]), Type::I32),
        ),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}

#[test]
fn strings() {
    let body = Stmt::Block(vec![
        Stmt::Var(id_("s"), Expr::ToString(str_("wow, thanks")), Type::String),
        Stmt::Return(len_(get_id_("s"))),
    ]);
    let program = test_program_(body);
    test_wasm(11, program);
}

#[test]
fn globals() {
    let mut program = test_program_(Stmt::Return(get_id_("MY_GLOBAL")));
    program.globals.insert(
        id_("MY_GLOBAL"),
        Global {
            is_mut: false,
            ty: Type::I32,
            atom: i32_(5),
        },
    );
    test_wasm(5, program);
}

#[test]
fn simple_prec() {
    let program = parse(
        "
        function main(): i32 {
            return 5 + 2 * 3 + 2;
        }
        ",
    );
    test_wasm(13, program);
}
