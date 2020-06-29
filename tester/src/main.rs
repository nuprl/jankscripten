extern crate tester;

use std::io::Read;
use tester::run_with_runtime;

fn main() {
    let mut stdin = std::io::stdin();
    let mut wasm = vec![];
    stdin.read_to_end(&mut wasm).expect("couldn't read stdin");
    let val: i32 = run_with_runtime(&wasm).expect("running failed");
    println!("{}", val);
}
