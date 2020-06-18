//! Intern strings into the data segment and refer to them by memory position
//!
//! Important information about how strings are laid out:
//! - A static string is an I32 pointer to memory
//! - the memory is:
//!   - first, 4 bytes: a little-endian encoding of the u32 string length
//!   - the string, until the end of the length
//!   - aligned to 4 byte increments

use super::syntax::*;
use super::walk::*;

pub fn intern(program: &mut Program) {
    let mut vis = InternVisitor::default();
    program.walk(&mut vis);
    program.data = vis.data;
}

#[derive(Default)]
struct InternVisitor {
    data: Vec<u8>,
}
impl Visitor for InternVisitor {
    fn exit_atom(&mut self, atom: &mut Atom, _loc: &mut Loc) {
        match atom {
            Atom::Lit(old_lit @ Lit::String(_)) => {
                let lit = std::mem::replace(old_lit, Lit::I32(0));
                if let Lit::String(s) = lit {
                    let mut bytes = s.into_bytes();
                    let pos = self.data.len() as u32;
                    *old_lit = Lit::Interned(pos);
                    let length = bytes.len() as u32;
                    let mut length_bytes: [u8; 4] = unsafe { std::mem::transmute(length.to_le()) };
                    self.data.extend_from_slice(&length_bytes);
                    self.data.append(&mut bytes);
                    let in_memory_length = length + 4;
                    // now we want to preserve alignment
                    // 0 -> 3 -> 0; 1 -> 0 -> 3; 2 -> 1 -> 2; 3 -> 2 -> 1
                    // +3 not -1 because 0 -> -1 % 4 = -1 -> 4, should be 0
                    let needed = 3 - ((in_memory_length + 3) % 4);
                    for _ in 0..needed {
                        self.data.push(b'\0');
                    }
                } else {
                    unreachable!()
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::constructors::*;
    use super::super::parse;
    use super::super::syntax::*;
    use super::intern;
    #[test]
    fn some_strings() {
        let mut program = parse(
            r#"
            function main() : i32 {
                var a: str = "012301";
                var b: str = "012";
                return 0;
            }
            "#,
        );
        intern(&mut program);
        let indexed_func = Function {
            locals: vec![],
            body: Stmt::Block(vec![
                Stmt::Var(id_("a"), atom_(Atom::Lit(Lit::Interned(0))), Type::StrRef),
                // 4(len) + 6 -> 10 ->(align) -> 12
                Stmt::Var(id_("b"), atom_(Atom::Lit(Lit::Interned(12))), Type::StrRef),
                Stmt::Return(i32_(0)),
            ]),
            fn_type: FnType {
                args: vec![],
                result: Some(Type::I32),
            },
            params: vec![],
        };
        let mut expected = program1_(indexed_func);
        expected.data = b"\x06\0\0\0012301\0\0\x03\0\0\0012\0".to_vec();
        assert_eq!(program, expected);
    }
}
