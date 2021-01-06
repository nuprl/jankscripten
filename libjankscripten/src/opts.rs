#[derive(Debug)]
pub struct Opts {
    /// Disabling GC can help debug jankscripten-generated code.
    pub disable_gc: bool,
    /// Don't initialize the javascript standard library from notwasm
    pub no_std: bool,
}

impl Opts {
    pub fn new() -> Opts {
        Opts {
            disable_gc: false,
            no_std: false,
        }
    }
}
