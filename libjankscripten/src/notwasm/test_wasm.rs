//use super::compile;
//use super::syntax::*;
//use wasmtime::*;
//
//fn test_wasm(expected: i32, program: Program) {
//    let code = compile(program).unwrap();
//    let store = Store::default();
//    let module = Module::new(&store, &code).unwrap();
//    let instance = Instance::new(&module, &[]).unwrap();
//    let func = instance
//        .exports()
//        .next()
//        .unwrap()
//        .into_func()
//        .expect("export wasn't a function");
//    match func.call(&[]).unwrap()[0] {
//        Val::I32(i) => assert_eq!(expected, i),
//        _ => panic!("non-int wasm return"),
//    }
//}

/*use rusty_v8::*;
use std::sync::Once;

static START: Once = Once::new();

fn run_v8(script: &str) -> std::string::String {
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

    let script = String::new(entered_scope, script).unwrap();

    let mut script = Script::compile(entered_scope, context, script, None).unwrap();
    script
        .run(entered_scope, context)
        .unwrap()
        .to_string(entered_scope)
        .unwrap()
        .to_rust_string_lossy(entered_scope)
}

#[test]
fn whoo() {
    use std::fs::read_to_string;
    assert!(
        &run_v8(&read_to_string("../target/wasm32-unknown-unknown/release/runtime.wasm").unwrap())
            == "hello"
    );
}*/
