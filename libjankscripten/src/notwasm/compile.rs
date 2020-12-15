use super::syntax::Program;
use super::*;
use crate::shared::Report;

pub fn compile<G>(mut program: Program, inspect: G) -> Result<Vec<u8>, Box<dyn Report>>
where
    G: FnOnce(&Program) -> (),
{
    //label_apps(&mut program);
    //elim_gotos(&mut program);
    let notwasm_rt = parse("runtime.notwasm", include_str!("runtime.notwasm"));
    program.merge_in(notwasm_rt);
    inspect(&program);
    type_checking::type_check(&mut program)?;
    intern(&mut program);
    Ok(translate(program)?)
}
