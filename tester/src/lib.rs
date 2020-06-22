//! Example of instantiating two modules which link to each other.

// You can execute this example with `cargo run --example linking`

use anyhow::Result;
use std::fs::read;
use wasmtime::*;
use wasmtime_wasi::{Wasi, WasiCtx};

pub fn run_with_runtime(wasm: &[u8]) -> Result<i32> {
    let engine = Engine::new(Config::new().debug_info(true));
    let store = Store::new(&engine);

    // First set up our linker which is going to be linking modules together. We
    // want our linker to have wasi available, so we set that up here as well.
    let mut linker = Linker::new(&store);
    let wasi = Wasi::new(&store, WasiCtx::new(std::env::args())?);
    wasi.add_to_linker(&mut linker)?;

    // Load and compile our two modules
    let main_mod = Module::new(&engine, wasm)?;
    let runtime = Module::from_file(
        &engine,
        "../target/wasm32-unknown-unknown/debug/runtime.wasm",
    )?;

    // Instantiate our first module which only uses WASI, then register that
    // instance with the linker since the next linking will use it.
    let runtime = linker.instantiate(&runtime)?;
    linker.instance("runtime", &runtime)?;

    // And with that we can perform the final link and the execute the module.
    let main_mod = linker.instantiate(&main_mod)?;
    let run = main_mod.get_func("main").unwrap();
    let run = run.get0::<i32>()?;
    Ok(run()?)
}

fn rt_and_filename(filename: &str) -> Result<i32> {
    run_with_runtime(&read(filename).expect("no file"))
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_add_num() -> Result<()> {
        assert_eq!(rt_and_filename("tests/test_add_num.wat")?, 10);
        Ok(())
    }
    #[test]
    fn test_shared_data() -> Result<()> {
        assert_eq!(rt_and_filename("tests/test_shared_data.wat")?, 4);
        Ok(())
    }
}
