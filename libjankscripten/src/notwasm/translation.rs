//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: ????

use super::rt_bindings::get_rt_bindings;
use super::syntax as N;
use super::walk::*;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use std::collections::HashMap;
use Instruction::*;

pub fn translate(program: N::Program) -> Result<Vec<u8>, Error> {
    serialize(translate_parity(program))
}

pub fn translate_parity(mut program: N::Program) -> Module {
    let mut module = module();
    let rt_types = get_rt_bindings();
    let mut rt_indexes = HashMap::new();
    for (func_i, (name, ty)) in rt_types.iter().enumerate() {
        let type_i = if let N::Type::Fn(params, ret) = ty {
            module.push_signature(
                signature()
                    .with_params(params.iter().map(N::Type::as_wasm).collect())
                    .with_return_type(Some(ret.as_wasm()))
                    .build_sig(),
            )
        } else {
            panic!("non-fn fn");
        };
        rt_indexes.insert(*name, func_i as u32);
        module = module
            .import()
            .path("runtime", name)
            .with_external(External::Function(type_i))
            .build();
    }
    module = module
        .import()
        .path("runtime", "memory")
        .external()
        .memory(0, None)
        .build();
    let entry_index = rt_types.len() as u32;
    for func in program.functions.values_mut() {
        module.push_function(translate_func(func, &rt_indexes, entry_index));
    }
    for global in program.globals {
        // do a global
    }
    // export main
    // TODO: assumes main is first
    let module = module
        .export()
        .field("main")
        .internal()
        .func(entry_index)
        .build();
    module.build()
}

fn translate_func(
    func: &mut N::Function,
    rt_indexes: &HashMap<&'static str, u32>,
    entry_index: u32,
) -> FunctionDefinition {
    let out_func = function()
        .signature()
        .with_params(func.params_tys.iter().map(N::Type::as_wasm).collect())
        .with_return_type(Some(func.ret_ty.as_wasm()))
        .build();
    // generate the actual code
    let mut visitor = Translate::new(rt_indexes, entry_index);
    visitor.translate(&mut func.body);
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

/// don't just call walk on this, use [translate], because walk doesn't
/// handle statements correctly
struct Translate<'a> {
    out: Vec<Instruction>,
    rt_indexes: &'a HashMap<&'static str, u32>,
    entry_index: u32,
}
impl<'a> Translate<'a> {
    fn new(rt_indexes: &'a HashMap<&'static str, u32>, entry_index: u32) -> Self {
        Self {
            out: Vec::new(),
            rt_indexes,
            entry_index,
        }
    }
    fn translate(&mut self, stmt: &mut N::Stmt) {
        match stmt {
            N::Stmt::Empty => (),
            N::Stmt::Block(ss) => {
                for s in ss {
                    self.translate(s);
                }
            }
            N::Stmt::Assign(id, expr) | N::Stmt::Var(id, expr, _) => {
                // place value on stack
                self.translate_expr(expr);
                self.out.push(SetLocal(id.index()));
            }
            N::Stmt::If(cond, conseq, alt) => {
                self.translate_atom(cond);
                self.out.push(If(BlockType::NoResult));
                self.translate(conseq);
                self.out.push(Else);
                self.translate(alt);
                self.out.push(End);
            }
            N::Stmt::Return(atom) => {
                self.translate_atom(atom);
                self.out.push(Return);
            }
            _ => todo!("{:?}", stmt),
        }
    }
    fn translate_expr(&mut self, expr: &mut N::Expr) {
        match expr {
            N::Expr::Atom(atom) => self.translate_atom(atom),
            N::Expr::HT(ty) => self.rt_call("ht_new"),
            N::Expr::HTSet(ht, field, val, ty) => {
                self.translate_atom(ht);
                self.out.push(I32Const(*field));
                self.translate_atom(val);
                self.rt_call("ht_set");
            }
            _ => todo!(),
        }
    }
    fn translate_atom(&mut self, atom: &mut N::Atom) {
        match atom {
            N::Atom::Lit(lit) => match lit {
                N::Lit::I32(i) => self.out.push(I32Const(*i)),
                _ => todo!(),
            },
            N::Atom::Id(id) => self.out.push(GetLocal(id.index())),
            N::Atom::HTGet(ht, field, ty) => {
                self.translate_atom(ht);
                self.out.push(I32Const(*field));
                self.rt_call("ht_get");
            }
            _ => todo!(),
        }
    }
    fn rt_call(&mut self, name: &str) {
        if let Some(i) = self.rt_indexes.get(name) {
            self.out.push(Call(*i));
        } else {
            panic!("cannot find rt {}", name);
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
