use super::syntax::Program;
use super::*;
use crate::opts::Opts;
use std::collections::HashMap;
use std::error::Error;

pub fn compile<G>(
    opts: &mut Opts,
    mut program: Program,
    inspect: G,
) -> Result<(Vec<u8>, HashMap<String, u32>), Box<dyn Error>>
where
    G: FnOnce(&Program) -> (),
{
    //label_apps(&mut program);
    //elim_gotos(&mut program);
    let notwasm_std_lib = opts.take_stdlib();
    program.merge_in(notwasm_std_lib);

    inspect(&program);
    type_checking::type_check(&mut program)?;
    let inverted_interned_strings = intern(&mut program);
    let wasm = translate(opts, program)?;
    Ok((wasm, inverted_interned_strings))
}
