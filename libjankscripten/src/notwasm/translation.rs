//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: ????

use super::syntax as N;
use super::walk::*;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use Instruction::*;

pub fn translate(program: N::Program) -> Result<Vec<u8>, Error> {
    serialize(translate_parity(program))
}

pub fn translate_parity(mut program: N::Program) -> Module {
    let mut module = module();
    for func in program.functions.values_mut() {
        module.push_function(translate_func(func));
    }
    for global in program.globals {
        // do a global
    }
    let module = module.memory().build();
    module.build()
}

fn translate_func(func: &mut N::Function) -> FunctionDefinition {
    let (params_tys, ret_ty) = if let N::Type::Fn(p, r) = &func.ty {
        (p, r)
    } else {
        panic!("non-function function");
    };
    let out_func = function()
        .signature()
        .with_params(params_tys.iter().map(N::Type::as_wasm).collect())
        .with_return_type(Some(ret_ty.as_wasm()))
        .build();
    // generate the actual code
    let mut visitor = Translate::default();
    // TODO: this is unnecessary, we don't mutate we should change visitor
    // to use & or get ownership of func in iteration if that's reasonable
    func.body.walk(&mut visitor);
    let mut insts = visitor.out;
    insts.push(End);
    let locals: Vec<_> = func
        .locals
        .iter()
        .map(|t| Local::new(1, t.as_wasm()))
        .collect();
    out_func
        .body()
        .with_instructions(Instructions::new(insts))
        .with_locals(locals)
        .build()
        .build()
}

#[derive(Default)]
struct Translate {
    out: Vec<Instruction>,
}
impl Visitor for Translate {
    fn exit_stmt(&mut self, stmt: &mut N::Stmt) {
        match stmt {
            // presumably our atom has been placed on the stack now
            N::Stmt::Return(..) => self.out.push(Return),
            _ => todo!(),
        }
    }
    fn exit_expr(&mut self, expr: &mut N::Expr, _loc: &mut Loc) {}
    fn exit_atom(&mut self, atom: &mut N::Atom, _loc: &mut Loc) {
        match atom {
            N::Atom::Lit(lit, ty) => match lit {
                N::Lit::I32(i) => self.out.push(I32Const(*i)),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl N::Type {
    pub fn as_wasm(&self) -> ValueType {
        match self {
            // TODO: I64?
            N::Type::F64 => ValueType::F64,
            // almost everything is a pointer type
            _ => ValueType::I32,
        }
    }
}
