//! give de bruijn indexes to all variables

use super::syntax::*;
use super::walk::*;

pub const DATA_OFFSET: u32 = 16;

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
                    let pos = DATA_OFFSET + self.data.len() as u32;
                    *old_lit = Lit::Interned(pos, bytes.len() as u32);
                    self.data.append(&mut bytes);
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
    use super::super::syntax::*;
    use super::intern;
    #[test]
    fn intern_one_string() {
        let func = Function {
            locals: Vec::new(),
            body: Stmt::Return(str_("steven universe")),
            fn_type: FnType {
                args: vec![],
                result: Some(Type::String),
            },
            params: vec![],
        };
        let mut program = program1_(func);
        intern(&mut program);
        let indexed_func = Function {
            locals: vec![],
            body: Stmt::Return(Atom::Lit(Lit::Interned(16, 15))),
            fn_type: FnType {
                args: vec![],
                result: Some(Type::String),
            },
            params: vec![],
        };
        let mut expected = program1_(indexed_func);
        expected.data = b"steven universe".to_vec();
        assert_eq!(program, expected);
    }
}
