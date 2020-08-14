//! the jankscripten system without IO/main

pub mod jankierscript;
pub mod jankyscript;
pub mod javascript;
pub mod notwasm;
mod rope;
mod rts_function;
pub mod shared;

#[macro_use]
extern crate combine;

pub fn javascript_to_wasm<F, G>(
    js_code: &str,
    typecheck: bool,
    inspect_janky: F,
    inspect_notwasm: G,
) -> Result<Vec<u8>, Box<dyn std::error::Error>>
where
    F: FnOnce(&jankyscript::syntax::Stmt) -> (),
    G: FnOnce(&notwasm::syntax::Program) -> (),
{
    let mut js_ast = javascript::parse(js_code)?;
    let mut ng = javascript::NameGen::default();
    javascript::desugar(&mut js_ast, &mut ng);
    let jankier_ast = jankierscript::from_javascript(js_ast);
    let janky_ast = jankierscript::insert_coercions(jankier_ast)?;
    inspect_janky(&janky_ast);
    if typecheck {
        jankyscript::type_checking::type_check(&janky_ast)?;
    }
    let notwasm_ast = notwasm::from_jankyscript(janky_ast);
    inspect_notwasm(&notwasm_ast);
    let wasm_bin = notwasm::compile(notwasm_ast)?;
    Ok(wasm_bin)
}
