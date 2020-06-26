use super::syntax::Program;
use super::*;
use parity_wasm::elements::Error;

pub fn compile(mut program: Program) -> Result<Vec<u8>, Error> {
    // label_apps(&mut program);
    // elim_gotos(&mut program);
    intern(&mut program);
    translate(program)
}
