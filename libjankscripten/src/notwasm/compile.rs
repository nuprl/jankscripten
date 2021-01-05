use super::syntax::Program;
use super::*;
use crate::opts::Opts;
use std::error::Error;

pub fn compile<G>(opts: &Opts, mut program: Program, inspect: G) -> Result<Vec<u8>, Box<dyn Error>>
where
    G: FnOnce(&Program) -> (),
{
    //label_apps(&mut program);
    //elim_gotos(&mut program);
    let notwasm_bare_rt = parse("bare_runtime.notwasm", include_str!("bare_runtime.notwasm"));
    program.merge_in(notwasm_bare_rt);
    if !opts.no_std {
        let notwasm_std_lib = parse("std_lib.notwasm", include_str!("std_lib.notwasm"));
        program.merge_in(notwasm_std_lib);
    }
    inspect(&program);
    type_checking::type_check(&mut program)?;
    intern(&mut program);
    Ok(translate(opts, program)?)
}
