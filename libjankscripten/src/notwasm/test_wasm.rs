use super::compile;
use super::intern::intern;
use super::parser::parse;
use super::syntax::*;
use std::fmt::Debug;
use std::io::Write;
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::sync::Once;

static COMPILE_RUNTIME: Once = Once::new();

fn test_wasm<T>(expected: T, program: Program)
where
    T: Debug + FromStr + PartialEq,
    <T as FromStr>::Err: Debug,
{
    println!("{:?}", program);
    let wasm = compile(program).expect("could not compile");
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
    let mut run = Command::new("../bin/run-node")
        .arg("-")
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
        out_str
            .trim_end()
            .parse::<T>()
            .expect(&format!("number expected, got {}", &out_str)),
    );
}

use super::constructors::*;

#[test]
fn test_ht() {
    let program = parse(
        r#"
        function main(): i32 {
            var x = HT{};
            x<<one = any(1);
            x<<two = any(2);
            x<<three = any(3);
            return x<<one as i32;
        }
        "#,
    );
    test_wasm(1, program);
}

#[test]
fn objects() {
    let program = parse(
        r#"
        function main(): i32 {
            var obj = {};
            obj.x = any(3);
            obj.y = any(2);
            obj.x = any(1);
            obj.z = any(3);
            return obj.x as i32;
        }
        "#,
    );
    test_wasm(1, program);
}

#[test]
fn array_push_index() {
    let program = parse(
        "
        function main(): i32 {
            var x = [];
            var _ = arrayPush(x, any(135));
            var _ = arrayPush(x, any(7));
            var _ = arrayPush(x, any(98));
            return x[2] as i32;
        }
        ",
    );
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
            var x = toCall();
            return 4 + x;
        }
    "#,
    );
    test_wasm(9, program);
}

#[test]
fn break_block() {
    let body = Stmt::Block(vec![
        Stmt::Var(VarStmt::new(id_("x"), atom_(i32_(0)))),
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
            var a = 1;
            var b = 1;
            loop {
                if (b > 1000) {
                    return b;
                } else { }
                var temp = a + b;
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
            var n = 100;
            var result = F(n);
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
            var G = F;
            var n  = 102;
            var result = G(n);
            return result;
        }
    "#,
    );

    test_wasm(103, program);
}

#[test]
fn function_any() {
    let program = parse(
        r#"
        function callWith5s(anyFunc: any, takesTwoArgs: bool): i32 {
            var result = 0;
            var the5 = 5;
            if (takesTwoArgs) {
                var arity2 = anyFunc as (i32, i32) -> i32;
                result = arity2(the5, the5);
            } else {
                var arity1 = anyFunc as (i32) -> i32;
                result = arity1(the5);
            }
            return result;
        }
        function add2(n: i32): i32 {
            return n + 2;
        }
        function add(n: i32, m: i32): i32 {
            return n + m;
        }
        function main(): i32 {
            // 7
            var anyAdd2 = any(add2);
            var thisIsFalse = false;
            var a = callWith5s(anyAdd2, thisIsFalse);
            // 10
            var anyAdd = any(add);
            var thisIsTrue = true;
            var b = callWith5s(anyAdd, thisIsTrue);
            return a + b;
        }
    "#,
    );
    test_wasm(17, program);
}

#[test]
fn arity_mismatch() {
    let program = parse(
        r#"
        function arity1(n: i32): i32 {
            return n;
        }
        function main(): i32 {
            // 5
            var five = 5;
            var six = 6;
            var a = arity1(five, six);
            // ridiculousness because i don't know how undefined is supposed
            // to work
            var undefined = 0;
            // 0, should be undefined i guess
            var b = arity1();
            return a + b;
        }
    "#,
    );
    test_wasm(5, program);
}

#[test]
fn basic_ref() {
    let program = parse(
        r#"
        function main() : i32 {
            var r = newRef(150);
            return *r;
        }
        "#,
    );

    test_wasm(150, program);
}

#[test]
fn basic_ref_mutation() {
    let program = parse(
        r#"
        function main() : i32 {
            var r = newRef(150);
            *r = 130;
            return *r;
        }
        "#,
    );

    test_wasm(130, program);
}

#[test]
fn ref_doesnt_mutate_variables() {
    let program = parse(
        r#"
        function main() : i32 {
            var x = 100;
            // Refs are like OCaml refs, not like C pointers.
            // They are essentially boxes that hold values.
            var refX = newRef(x); // copy value of x

            // set value inside box. should not change value of x
            *refX = 130;

            // return original variable, which should be unchanged
            return x;
        }
        "#,
    );

    test_wasm(100, program);
}

#[test]
#[ignore]
fn goto_skips_stuff() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(VarStmt::new(id_("x"), atom_(i32_(5)))),
        Stmt::Goto(Label::App(0)),
        // this is the part we wanna skip
        Stmt::Assign(id_("x"), atom_(i32_(2))),
        // goto goes here
        Stmt::Expression(Expr::Call(id_("other"), vec![])),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}
#[test]
#[ignore]
fn goto_skips_loop() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(VarStmt::new(id_("x"), atom_(i32_(5)))),
        Stmt::Goto(Label::App(0)),
        // this is the part we wanna skip
        loop_(Stmt::Empty),
        // goto goes here
        Stmt::Expression(Expr::Call(id_("other"), vec![])),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}
#[test]
#[ignore]
fn goto_enters_if() {
    let skip_to_here = func_i32_(Stmt::Return(i32_(7)));
    let main_body = Stmt::Block(vec![
        // hopefully it stays 5
        Stmt::Var(VarStmt::new(id_("x"), atom_(i32_(5)))),
        Stmt::Goto(Label::App(0)),
        if_(
            TRUE_,
            // this is the part we wanna skip
            Stmt::Assign(id_("x"), atom_(i32_(2))),
            // goto goes here
            Stmt::Expression(Expr::Call(id_("other"), vec![])),
        ),
        Stmt::Return(get_id_("x")),
    ]);
    let program = program2_(func_i32_(main_body), skip_to_here);
    test_wasm(5, program);
}

#[test]
fn strings() {
    let body = Stmt::Block(vec![
        Stmt::Var(VarStmt::new(id_("s"), Expr::ToString(str_("wow, thanks")))),
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

const ITER_COUNT: usize = 1000;
const ALLOC_PROG: &'static str = "
    // NOTE(arjun): The follow comment is no longer accurate, since Anys are
    // no longer heap allocated. However, I'm not sure what the product is
    // intended to be.
    // allocates 8 Anys, and also 1, 2, ... = 28 * 96 = 2688
    var x = {};
    x.a = any(0.0f);
    x.b = any(1.0f);
    x.c = any(2.0f);
    x.d = any(3.0f);
    x.e = any(0);
    x.f = any(0);
    x.g = any(0);
    x.h = any(0);";

#[test]
fn will_gc() {
    let program = parse(&format!(
        "
        function alloc_and_drop(): i32 {{
            {}
            return 0;
        }}
        function main(): i32 {{
            var i = 0;
            while (i < {}) {{
                var _ = alloc_and_drop();
                i = i + 1;
            }}
            return 5;
        }}
        ",
        ALLOC_PROG, ITER_COUNT,
    ));
    test_wasm(5, program);
}
#[test]
#[should_panic]
fn oom_if_no_gc() {
    let program = parse(&format!(
        "
        function main(): i32 {{
            var i: i32 = 0;
            while (i < {}) {{
                {}
                i = i + 1;
            }}
            return 5;
        }}
        ",
        ITER_COUNT, ALLOC_PROG
    ));
    test_wasm(5, program);
}

#[test]
fn identical_interned_string_identity() {
    let mut program = parse(
        r#"
        function main(): bool {
            var s1 = "Calvin Coolidge";
            var s2 = "Calvin Coolidge";
            return s1 === s2; // ptr equality
        }"#,
    );
    intern(&mut program);
    test_wasm(1, program);
}

#[test]
fn float_in_any() {
    let mut program = parse(
        r#"
        function main(): bool {
            var x = any(32.3f);
            var y = x as f64;
            var z = y +. 0.2f;
            return true;
        }"#,
    );
    intern(&mut program);
    test_wasm(1, program);
}

#[test]
fn gc_float_in_any() {
    let program = parse(
        r#"
        function main(): i32 {
            var i = 0;
            var o = {};
            o.x = any(0.0f);
            while (i < 2000) {
                o.x = any((o.x as f64) +. 0.1f);
                i = i + 1;
            }
            return 5;
        }
        "#,
    );
    test_wasm(5, program);
}
