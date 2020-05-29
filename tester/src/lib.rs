//! Example of instantiating two modules which link to each other.

// You can execute this example with `cargo run --example linking`

use anyhow::Result;
use wasmtime::*;
use wasmtime_wasi::{Wasi, WasiCtx};

#[test]
fn test_add_num() -> Result<()> {
    let store = Store::default();

    // First set up our linker which is going to be linking modules together. We
    // want our linker to have wasi available, so we set that up here as well.
    let mut linker = Linker::new(&store);
    let wasi = Wasi::new(&store, WasiCtx::new(std::env::args())?);
    wasi.add_to_linker(&mut linker)?;

    // Load and compile our two modules
    let main_mod = Module::from_file(&store, "tests/test_add_num.wat")?;
    let runtime = Module::from_file(
        &store,
        "../target/wasm32-unknown-unknown/release/runtime.wasm",
    )?;

    // Instantiate our first module which only uses WASI, then register that
    // instance with the linker since the next linking will use it.
    let runtime = linker.instantiate(&runtime)?;
    linker.instance("runtime", &runtime)?;

    // And with that we can perform the final link and the execute the module.
    let main_mod = linker.instantiate(&main_mod)?;
    let run = main_mod.get_func("run").unwrap();
    let run = run.get0::<i32>()?;
    let val = run()?;
    assert_eq!(val, 10);
    Ok(())
}
