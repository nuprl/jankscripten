//! the jankscripten system without IO/main

use std::collections::HashMap;
pub mod jankierscript;
pub mod jankyscript;
pub mod javascript;
pub mod notwasm;
pub mod opts;
pub mod pos;
mod pretty;
mod rope;
mod rts_function;
pub mod shared;
mod string_escaping;
mod z3ez;

pub fn javascript_to_wasm<F, G>(
    mut opts: opts::Opts,
    src_name: &str,
    js_code: &str,
    inspect_janky: F,
    inspect_notwasm: G,
) -> Result<(Vec<u8>, HashMap<String, u32>), Box<dyn std::error::Error>>
where
    F: FnOnce(&jankyscript::syntax::Stmt) -> (),
    G: FnOnce(&notwasm::syntax::Program) -> (),
{
    let mut js_ast = javascript::parse(src_name, js_code)?;
    let mut ng = shared::NameGen::default();
    javascript::desugar(&mut js_ast, &mut ng);
    let jankier_ast = jankierscript::from_javascript(js_ast);
    let mut janky_ast = jankierscript::insert_coercions(jankier_ast)?;
    jankyscript::compile(&mut janky_ast, inspect_janky).unwrap();
    let notwasm_ast = notwasm::from_jankyscript(janky_ast);
    notwasm::compile(&mut opts, notwasm_ast, inspect_notwasm)
}
