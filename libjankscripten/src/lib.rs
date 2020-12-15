//! the jankscripten system without IO/main

pub mod jankierscript;
pub mod jankyscript;
pub mod javascript;
pub mod notwasm;
mod rope;
mod rts_function;
pub mod shared;
pub mod pos;

use shared::UnwrapReport;

#[macro_use]
extern crate combine;

pub fn javascript_to_wasm<F, G>(
    js_code: &str,
    inspect_janky: F,
    inspect_notwasm: G,
) -> Result<Vec<u8>, Box<dyn std::error::Error>>
where
    F: FnOnce(&jankyscript::syntax::Stmt) -> (),
    G: FnOnce(&notwasm::syntax::Program) -> (),
{
    let maybe_js_ast = javascript::parse(js_code);
    let mut js_ast = maybe_js_ast?;
    let mut ng = shared::NameGen::default();
    javascript::desugar(&mut js_ast, &mut ng);
    let jankier_ast = jankierscript::from_javascript(js_ast);
    let mut janky_ast = jankierscript::insert_coercions(jankier_ast)?;
    jankyscript::compile(&mut janky_ast, inspect_janky).unwrap_report();
    let notwasm_ast = notwasm::from_jankyscript(janky_ast);
    let wasm_bin = notwasm::compile(notwasm_ast, inspect_notwasm).unwrap_report();
    Ok(wasm_bin)
}
