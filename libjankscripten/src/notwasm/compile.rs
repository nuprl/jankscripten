use super::syntax::Program;
use super::*;
use crate::opts::Opts;
use std::error::Error;

pub fn compile<G>(
    opts: &mut Opts,
    mut program: Program,
    inspect: G,
) -> Result<Vec<u8>, Box<dyn Error>>
where
    G: FnOnce(&Program) -> (),
{
    //label_apps(&mut program);
    //elim_gotos(&mut program);
    if let Some(src_ref) = &mut opts.notwasm_stdlib_source_code {
        let mut src = String::new();
        std::mem::swap(&mut src, src_ref);
        let notwasm_std_lib = parse("std_lib.notwasm", src);
        program.merge_in(notwasm_std_lib);
    } else {
        eprintln!("Warning: NotWasm standard library not provided.");
    }

    inspect(&program);
    type_checking::type_check(&mut program)?;
    intern(&mut program);
    Ok(translate(opts, program)?)
}
