//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: ????

use super::syntax as N;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use Instruction::*;

fn translate(program: N::Program) {
    let mut module = module();
    for func in program.functions {
        func
    }
}
