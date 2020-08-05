use super::syntax::Program;
use super::*;
use parity_wasm::elements::Error;

pub fn compile(mut program: Program) -> Result<Vec<u8>, Error> {
    //label_apps(&mut program);
    //elim_gotos(&mut program);
    intern(&mut program);
    type_checking::type_check(&mut program).expect("type-checking failed");
    translate(program)
}
