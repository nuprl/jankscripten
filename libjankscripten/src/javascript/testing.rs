//! provides a few useful functions for testing using v8 and pretty-printing

use crate::javascript as jen;
use rusty_v8::*;
use std::sync::Once;

static START: Once = Once::new();

const WIDTH: usize = 80;

/// expect the result (last expression) to be the same when evaluating the
/// original block versus the second. only works with blocks because of the
/// nature of "result" / "value" as understood by V8
// mostly copied from rusty_v8 crate-level docs
pub fn expect_same(original: &jen::Stmt, modified: &jen::Stmt) {
    START.call_once(|| {
        // initializing multiple times is not just a performance problem it causes a panic
        let platform = new_default_platform().unwrap();
        V8::initialize_platform(platform);
        V8::initialize();
    });

    let mut isolate = Isolate::new(Default::default());

    let mut handle_scope = HandleScope::new(&mut isolate);
    let handle_scope = handle_scope.enter();

    let context = Context::new(handle_scope);
    let mut context_scope = ContextScope::new(handle_scope, context);

    let entered_scope = context_scope.enter();

    // i can't figure out these types and lifetimes so closures to the
    // rescue right haha ugh
    let mut run_script = |script| {
        let script = String::new(entered_scope, script).unwrap();
        println!("javascript: {}", script.to_rust_string_lossy(entered_scope));

        let mut script = Script::compile(entered_scope, context, script, None).unwrap();
        script.run(entered_scope, context).unwrap()
    };

    let original_script = original.to_pretty(WIDTH);
    let modified_script = modified.to_pretty(WIDTH);
    let original_res = run_script(&original_script);
    let modified_res = run_script(&modified_script);
    println!(
        "original value:\n{:?}",
        original_res
            .to_string(entered_scope)
            .and_then(|x| Some(x.to_rust_string_lossy(entered_scope)))
    );
    println!(
        "modified value:\n{:?}",
        modified_res
            .to_string(entered_scope)
            .and_then(|x| Some(x.to_rust_string_lossy(entered_scope)))
    );
    assert!(original_res.strict_equals(modified_res));
}

/// execute the program, then desugar it using `f` and execute it again. assert
/// that the results are equal
fn desugar_okay(program: &str, f: fn(&mut jen::NameGen, &mut jen::Stmt)) {
    let original = jen::parse(program.clone()).expect("unparsible original program");
    let mut ng = jen::NameGen::default();
    let mut desugar = jen::parse(program).unwrap();
    f(&mut ng, &mut desugar);
    expect_same(&original, &desugar);
}

mod testing_tests {
    use super::{desugar_okay, expect_same, jen};
    #[test]
    #[should_panic]
    fn fails_on_different() {
        expect_same(
            &jen::parse("var x = 10; x").unwrap(),
            &jen::parse("var x = 15; x").unwrap(),
        );
    }
    #[test]
    fn succeeds_on_same() {
        expect_same(
            &jen::parse("var x = 10; x").unwrap(),
            &jen::parse("var x = 10; x").unwrap(),
        );
    }

    fn fake_desugar_fn(_: &mut jen::NameGen, stmt: &mut jen::Stmt) {
        *stmt = jen::parse("var x = 10; x").unwrap();
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
