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

/// String is specified as TypeTag 1 in the runtime to make it consistent
/// across compiles
/// [marked, String = 1, pad, pad]
const STRING_TAG: [u8; 4] = [0, 1, 0, 0];

pub fn intern(program: &mut Program) {
    let mut vis = InternVisitor::default();
    program.walk(&mut vis);
    program.data = vis.data;
}

#[derive(Default)]
struct InternVisitor {
    data: Vec<u8>,
    /// Helps avoid interning the same static string multiple times.
    already_interned: std::collections::HashMap<String, u32>,
}
impl Visitor for InternVisitor {
    fn exit_atom(&mut self, atom: &mut Atom, _loc: &mut Loc) {
        match atom {
            Atom::Lit(old_lit @ Lit::String(_), s) => {
                let lit = std::mem::replace(old_lit, Lit::I32(0));
                if let Lit::String(s) = lit {
                    if let Some(offset) = self.already_interned.get(&s) {
                        // We have seen this string before, so no need to
                        // reallocate.
                        *old_lit = Lit::Interned(*offset);
                        return;
                    }
                    let pos = self.data.len() as u32;
                    // Cache the offset, so that the interned string can be
                    // reused.
                    self.already_interned.insert(s.clone(), pos);
                    let mut bytes = s.into_bytes();
                    *old_lit = Lit::Interned(pos);
                    let length = bytes.len() as u32;
                    let length_bytes: [u8; 4] = unsafe { std::mem::transmute(length.to_le()) };
                    self.data.extend_from_slice(&STRING_TAG);
                    self.data.extend_from_slice(&length_bytes);
                    self.data.append(&mut bytes);
                    // tag(4), length(4)
                    let in_memory_length = length + 8;
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
                var a = "012301";
                var b = "012";
                return 0;
            }
            "#,
        );
        intern(&mut program);
        let s = DUMMY_SP;
        let indexed_func = Function {
            body: Stmt::Block(
                vec![
                    Stmt::Var(
                        VarStmt::new(id_("a"), atom_(Atom::Lit(Lit::Interned(0), s), s)),
                        s,
                    ),
                    // 4(tag) + 4(len) + 6 -> 14 ->(align) -> 16
                    Stmt::Var(
                        VarStmt::new(id_("b"), atom_(Atom::Lit(Lit::Interned(16), s), s)),
                        s,
                    ),
                    Stmt::Return(i32_(0, s), s),
                ],
                s,
            ),
            fn_type: FnType {
                args: vec![],
                result: Some(Box::new(Type::I32)),
            },
            params: vec![],
        };
        let mut expected = program1_(indexed_func);
        expected.data = b"\0\x01\0\0\x06\0\0\0012301\0\0\0\x01\0\0\x03\0\0\0012\0".to_vec();
        assert_eq!(
            program, expected,
            "got: {}\nexpected: {}",
            program, expected
        );
    }
}
