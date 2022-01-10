#[derive(Debug)]
pub struct Opts {
    /// Disabling GC can help debug jankscripten-generated code.
    pub disable_gc: bool,
    /// The source code of the NotWasm standard library
    pub notwasm_stdlib_source_code: String,
    /// Type inference should eventually always be enabled, but is not yet
    /// fully supported
    pub typeinf: bool,
}

impl Opts {
    pub fn new() -> Opts {
        Opts {
            disable_gc: false,
            notwasm_stdlib_source_code: "".to_string(),
            typeinf: false,
        }
    }
}
