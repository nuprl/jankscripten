use super::syntax::Program;
use super::*;
use parity_wasm::elements::Error;

pub fn compile(mut program: Program) -> Result<Vec<u8>, Error> {
    index(&mut program);
    index_labels(&mut program);
    intern(&mut program);
    translate(program)
}
