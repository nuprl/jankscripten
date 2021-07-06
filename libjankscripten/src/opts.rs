use crate::notwasm;

#[derive(Debug, Default)]
pub struct Opts {
    /// Disabling GC can help debug jankscripten-generated code.
    pub disable_gc: bool,
    /// The source code of the NotWasm standard library
    pub notwasm_stdlib_source_code: String,
    stdlib: Option<notwasm::syntax::Program>,
}

impl Opts {

    fn ensure_parsed_stdlib(&mut self) {
        if self.stdlib.is_some() {
            return;
        }
        let mut src = String::new();
        std::mem::swap(&mut src, &mut self.notwasm_stdlib_source_code);
        self.stdlib = Some(notwasm::parse("std_lib.notwasm", src));
    }

    /// Borrowed reference to the standard library. Parses it if necessary.
    pub fn borrow_stdlib(&mut self) -> &notwasm::syntax::Program {
        self.ensure_parsed_stdlib();
        self.stdlib.as_ref().unwrap()
    }

    /// Takes the standard library out, leaving nothing in place. Parses it if necessary.
    pub fn take_stdlib(&mut self) -> notwasm::syntax::Program {
        self.ensure_parsed_stdlib();
        self.stdlib.take().unwrap()
    }
}