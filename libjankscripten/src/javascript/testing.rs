//! provides a few useful functions for testing using v8 and pretty-printing

use crate::javascript as js;
use std::io::Write;
use std::process::{Command, Stdio};

const WIDTH: usize = 80;

/// expect the result (last expression) to be the same when evaluating the
/// original block versus the second. only works with blocks because of the
/// nature of "result" / "value" as understood by V8
// mostly copied from rusty_v8 crate-level docs
pub fn expect_same(original: &js::Stmt, modified: &js::Stmt) {
    let original_script = original.to_pretty(WIDTH);
    let modified_script = modified.to_pretty(WIDTH);
    println!(
        "original script: {}\n desugared: {}",
        original_script, modified_script
    );
    let original_res = run_script(&original_script);
    let modified_res = run_script(&modified_script);
    assert_eq!(original_res, modified_res);
}

fn run_script(script: &str) -> String {
    let mut run = Command::new("node")
        // this prints the final expression
        .arg("-p")
        // avoids needing tmp file which is test threading mess
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run tester binary");
    // https://doc.rust-lang.org/std/process/struct.Stdio.html
    {
        run.stdin
            .as_mut()
            .expect("no stdin")
            .write_all(script.as_bytes())
            .expect("couldn't write");
    }
    let out = run.wait_with_output().expect("no stdout");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    println!("standard error: {}", stderr);
    stdout.to_string()
}

/// execute the program, then desugar it using `f` and execute it again. assert
/// that the results are equal
pub fn desugar_okay(program: &str, f: fn(&mut js::Stmt, &mut js::NameGen)) {
    let original = js::parse("anon", program.clone()).expect("unparsible original program");
    let mut ng = js::NameGen::default();
    let mut desugar = js::parse("anon", program).unwrap();
    f(&mut desugar, &mut ng);
    expect_same(&original, &desugar);
}

mod testing_tests {
    use super::{desugar_okay, expect_same, js};
    #[test]
    #[should_panic]
    fn fails_on_different() {
        expect_same(
            &js::parse("anon", "var x = 10; x").unwrap(),
            &js::parse("anon", "var x = 15; x").unwrap(),
        );
    }
    #[test]
    fn succeeds_on_same() {
        expect_same(
            &js::parse("anon", "var x = 10; x").unwrap(),
            &js::parse("anon", "var x = 10; x").unwrap(),
        );
    }

    fn fake_desugar_fn(stmt: &mut js::Stmt, _: &mut js::NameGen) {
        *stmt = js::parse("anon", "var x = 10; x").unwrap();
    }
    #[test]
    #[should_panic]
    fn bad_desugar() {
        desugar_okay("var x = 15; x", fake_desugar_fn);
    }
    #[test]
    fn good_desugar() {
        desugar_okay("var x = 10; x", fake_desugar_fn);
    }
}
