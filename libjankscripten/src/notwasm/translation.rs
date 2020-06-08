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

type FuncTypeMap = HashMap<(Vec<ValueType>, ValueType), u32>;

pub fn translate(program: N::Program) -> Result<Vec<u8>, Error> {
    serialize(translate_parity(program))
}

pub fn translate_parity(mut program: N::Program) -> Module {
    let mut module = module();
    let rt_types = get_rt_bindings();
    let mut rt_indexes = HashMap::new();
    // build up indexes for mutual recursion first
    let mut type_indexes = HashMap::new();
    for (func_i, (name, ty)) in rt_types.iter().enumerate() {
        let type_i = if let N::Type::Fn(params, ret) = ty {
            let wasm_ty = (types_as_wasm(params), ret.as_wasm());
            let i_check = module.push_signature(
                signature()
                    .with_params(wasm_ty.0.clone())
                    .with_return_type(Some(wasm_ty.1.clone()))
                    .build_sig(),
            );
            assert_eq!(*type_indexes.entry(wasm_ty).or_insert(i_check), i_check);
            i_check
        } else {
            panic!("non-fn fn");
        };
        rt_indexes.insert(name.clone(), func_i as u32);
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
    for func in program.functions.values() {
        // has to be wasm types to dedup properly
        let func_ty = (
            types_as_wasm(&no_ids(func.params_tys.clone())),
            func.ret_ty.as_wasm(),
        );
        let next_index = type_indexes.len() as u32;
        type_indexes.entry(func_ty).or_insert(next_index);
    }
    for func in program.functions.values_mut() {
        module.push_function(translate_func(func, &rt_indexes, &type_indexes));
    }
    for (i, mut global) in program.globals {
        // can't use functions anyway so no need to worry
        let empty = HashMap::new();
        let empty2 = HashMap::new();
        let mut visitor = Translate::new(&empty, &empty2);
        visitor.translate_atom(&mut global);
        let mut insts = visitor.out;
        insts.push(End);
        // TODO: do a global
    }
    // fsr we need an identity table to call indirect
    let mut table_build = module.table().with_min(program.functions.len() as u32);
    let offset = rt_indexes.len() as u32;
    let mut main_index = None;
    for (notwasm_index, name) in program.functions.keys().enumerate() {
        let wasm_index = notwasm_index as u32 + offset;
        // find main
        if name == &N::Id::Named("main".to_string()) {
            main_index = Some(wasm_index)
        }
        table_build = table_build.with_element(notwasm_index as u32, vec![wasm_index]);
    }
    let module = table_build.build();
    // export main
    let module = module
        .export()
        .field("main")
        .internal()
        .func(main_index.expect("no main"))
        .build();
    module.build()
}

fn translate_func(
    func: &mut N::Function,
    rt_indexes: &HashMap<String, u32>,
    type_indexes: &FuncTypeMap,
) -> FunctionDefinition {
    let out_func = function()
        .signature()
        .with_params(types_as_wasm(&no_ids(func.params_tys.clone())))
        .with_return_type(Some(func.ret_ty.as_wasm()))
        .build();
    // generate the actual code
    let mut visitor = Translate::new(rt_indexes, type_indexes);
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

fn types_as_wasm(types: &[N::Type]) -> Vec<ValueType> {
    types.iter().map(N::Type::as_wasm).collect()
}
fn no_ids(ids_tys: Vec<(N::Id, N::Type)>) -> Vec<N::Type> {
    ids_tys.into_iter().map(|(_, ty)| ty).collect()
}

/// don't just call walk on this, use [translate], because walk doesn't
/// handle statements correctly
struct Translate<'a> {
    out: Vec<Instruction>,
    rt_indexes: &'a HashMap<String, u32>,
    type_indexes: &'a FuncTypeMap,
}
impl<'a> Translate<'a> {
    fn new(rt_indexes: &'a HashMap<String, u32>, type_indexes: &'a FuncTypeMap) -> Self {
        Self {
            out: Vec::new(),
            rt_indexes,
            type_indexes,
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
            N::Expr::HT(ty) => self.rt_call_mono("ht_new", ty),
            N::Expr::HTSet(ht, field, val, ty) => {
                self.translate_atom(ht);
                self.out.push(I32Const(*field));
                self.translate_atom(val);
                self.rt_call_mono("ht_set", ty);
            }
            N::Expr::Call(func, args, params_tys, ret_ty) => {
                for arg in args {
                    self.out.push(GetLocal(arg.index()));
                }
                self.out.push(GetLocal(func.index()));
                let index = if let Some(i) = self
                    .type_indexes
                    .get(&(types_as_wasm(params_tys), ret_ty.as_wasm()))
                {
                    *i
                } else {
                    panic!("unknown function type");
                };
                self.out.push(CallIndirect(index, 0));
            }
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
                self.rt_call_mono("ht_get", ty);
            }
            N::Atom::Binary(op, a, b, ty) => {
                self.translate_atom(a);
                self.translate_atom(b);
                self.out.push(match ty {
                    N::Type::I32 => match op {
                        N::BinaryOp::LeftShift => I32Shl,
                        N::BinaryOp::RightShift => I32ShrS,
                        N::BinaryOp::UnsignedRightShift => I32ShrU,
                        N::BinaryOp::Plus => I32Add,
                        N::BinaryOp::Minus => I32Sub,
                        N::BinaryOp::Times => I32Mul,
                        N::BinaryOp::Over => I32DivS, // TODO: signed/unsigned?
                        N::BinaryOp::Mod => I32RemS,
                        N::BinaryOp::Or => I32Or,
                        N::BinaryOp::XOr => I32Xor,
                        N::BinaryOp::And => I32And,
                    },
                    N::Type::F64 => match op {
                        N::BinaryOp::Plus => F64Add,
                        N::BinaryOp::Minus => F64Sub,
                        N::BinaryOp::Times => F64Mul,
                        N::BinaryOp::Over => F64Div,
                        _ => panic!("operation unsupported on floats"),
                    },
                    _ => panic!("binary operations only on floats and ints in NotWasm"),
                });
            }
        }
    }
    fn rt_call(&mut self, name: &str) {
        if let Some(i) = self.rt_indexes.get(name) {
            self.out.push(Call(*i));
        } else {
            panic!("cannot find rt {}", name);
        }
    }
    /// call a monomorphized function, which follows specific naming
    /// conventions: `name_type`, with type given by [N::Type::fmt]
    fn rt_call_mono(&mut self, name: &str, ty: &N::Type) {
        self.rt_call(&format!("{}_{}", name, ty));
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
