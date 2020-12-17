#[derive(Debug)]
pub struct Opts {
    /// Disabling GC can help debug jankscripten-generated code.
    pub disable_gc: bool,
}

impl Opts {
    pub fn new() -> Opts {
        Opts { disable_gc: false }
    }
}
