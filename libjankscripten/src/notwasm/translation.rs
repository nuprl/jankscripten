//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: [super::compile]

use super::rt_bindings::get_rt_bindings;
use super::syntax as N;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use std::collections::HashMap;
use Instruction::*;

const JEN_STRINGS_IDX: u32 = 0;

type FuncTypeMap = HashMap<(Vec<ValueType>, Option<ValueType>), u32>;

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
            let wasm_ty = (types_as_wasm(params), option_as_wasm(&**ret));
            let i_check = module.push_signature(
                signature()
                    .with_params(wasm_ty.0.clone())
                    .with_return_type(wasm_ty.1.clone())
                    .build_sig(),
            );
            assert_eq!(*type_indexes.entry(wasm_ty).or_insert(i_check), i_check);
            i_check
        } else {
            panic!("expected all elements in the runtime to have function type");
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
    // Create a WebAssembly function type for each function in NotWasm. These
    // go in the table of types (type_indexes).
    for func in program.functions.values() {
        // has to be wasm types to dedup properly
        let func_ty = (
            types_as_wasm(&func.fn_type.args.clone()),
            option_as_wasm(&func.fn_type.result),
        );
        let next_index = type_indexes.len() as u32;
        type_indexes.entry(func_ty).or_insert(next_index);
    }
    // NOTE(arjun): I believe that this loop relies on Rust traversing
    // program.functions in the same order as the call to .keys in
    // super::index::index. I think this is brittle, and we should probably
    // index functions first.
    for (name, func) in program.functions.iter_mut() {
        module.push_function(translate_func(func, &rt_indexes, &type_indexes, name));
    }
    // data segment
    // right now there's only one global, if that changes we can refactor
    module = module
        .import()
        .path("runtime", "JEN_STRINGS")
        .with_external(External::Global(GlobalType::new(ValueType::I32, false)))
        .build();
    let module = module
        .data()
        .offset(GetGlobal(JEN_STRINGS_IDX))
        .value(program.data)
        .build();
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
    name: &N::Id,
) -> FunctionDefinition {
    let out_func = function()
        .signature()
        .with_params(types_as_wasm(&func.fn_type.args.clone()))
        .with_return_type(option_as_wasm(&func.fn_type.result))
        .build();
    // generate the actual code
    let mut translator = Translate::new(rt_indexes, type_indexes);
    translator.translate(&mut func.body);
    let mut insts = translator.out;
    insts.push(End);
    // before the code, if this is main we have to call rt init()
    if name == &N::Id::Named("main".to_string()) {
        let mut new_insts = vec![Call(*rt_indexes.get("init").expect("no init"))];
        new_insts.append(&mut insts);
        insts = new_insts;
    }
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
fn option_as_wasm(ty: &Option<N::Type>) -> Option<ValueType> {
    ty.as_ref().map(N::Type::as_wasm)
}

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

    // We are not using a visitor, since we have to perform an operation on every
    // give of statement and expression. Thus, the visitor wouldn't give us much.
    fn translate(&mut self, stmt: &mut N::Stmt) {
        match stmt {
            N::Stmt::Empty => (),
            N::Stmt::Block(ss) => {
                // don't surround in an actual block, those are only useful
                // when labeled
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
            N::Stmt::Loop(body) => {
                // breaks should be handled by surrounding label already
                self.out.push(Loop(BlockType::NoResult));
                self.translate(body);
                // loop doesn't automatically continue, don't ask me why
                self.out.push(Br(0));
                self.out.push(End);
            }
            N::Stmt::Label(.., stmt) => {
                self.out.push(Block(BlockType::NoResult));
                self.translate(stmt);
                self.out.push(End);
            }
            N::Stmt::Break(id) => match id {
                N::Id::Label(i) => self.out.push(Br(*i)),
                _ => panic!("break non-label"),
            },
            N::Stmt::Return(atom) => {
                self.translate_atom(atom);
                self.out.push(Return);
            }
        }
    }

    fn translate_binop(&mut self, op: &N::BinaryOp) {
        match op {
            N::BinaryOp::I32Eq => self.out.push(I32Eq),
            N::BinaryOp::I32Add => self.out.push(I32Add),
            N::BinaryOp::I32Sub => self.out.push(I32Sub),
            N::BinaryOp::I32GT => self.out.push(I32GtS),
            N::BinaryOp::I32Mul => self.out.push(I32Mul),
        }
    }

    fn translate_expr(&mut self, expr: &mut N::Expr) {
        match expr {
            N::Expr::Atom(atom) => self.translate_atom(atom),
            N::Expr::HT(ty) => self.rt_call_mono("ht_new", ty),
            N::Expr::Array(ty) => self.rt_call_mono("array_new", ty),
            N::Expr::HTSet(ht, field, val, ty) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.translate_atom(val);
                self.rt_call_mono("ht_set", ty);
            }
            N::Expr::Push(array, val, ty) => {
                self.translate_atom(array);
                self.translate_atom(val);
                self.rt_call_mono("array_push", ty);
            }
            N::Expr::CallDirect(f, args) => {
                for arg in args {
                    self.out.push(GetLocal(arg.index()));
                }
                let f_idx = match f {
                    N::Id::Func(i) => {
                        // this one's a little weird. we index in notwasm
                        // by 0 = first user function. but wasm indexes by 0 =
                        // first rt function. se we have to offset. but only
                        // on direct calls, because our function table takes
                        // care of it on indirect calls
                        *i + self.rt_indexes.len() as u32
                    }
                    _ => panic!("expected Func ID"),
                };
                self.out.push(Call(f_idx));
            }
            N::Expr::CallIndirect(func, f_typ, args) => {
                for arg in args {
                    self.out.push(GetLocal(arg.index()));
                }
                panic!("Indirect calls");
                // match func {
                //     N::Id::Index(i) => {
                //         self.out.push(GetLocal(*i));
                //         let index = if let Some(i) = self
                //             .type_indexes
                //             .get(&(types_as_wasm(params_tys), ret_ty.as_wasm()))
                //         {
                //             *i
                //         } else {
                //             panic!("unknown function type");
                //         };
                //         self.out.push(CallIndirect(index, 0));
                //     }
                //     N::Id::Func(i) => {
                //         // this one's a little weird. we index in notwasm
                //         // by 0 = first user function. but wasm indexes by 0 =
                //         // first rt function. se we have to offset. but only
                //         // on direct calls, because our function table takes
                //         // care of it on indirect calls
                //         self.out.push(Call(*i + self.rt_indexes.len() as u32));
                //     }
                //     _ => panic!("id can't be function call"),
                // }
            }
            N::Expr::ToString(a) => {
                self.translate_atom(a);
                self.rt_call("string_from_str");
            }
        }
    }

    fn translate_atom(&mut self, atom: &mut N::Atom) {
        match atom {
            N::Atom::Lit(lit) => match lit {
                N::Lit::I32(i) => self.out.push(I32Const(*i)),
                N::Lit::Interned(addr) => {
                    self.out.push(GetGlobal(JEN_STRINGS_IDX));
                    self.out.push(I32Const(*addr as i32));
                    self.out.push(I32Add);
                }
                N::Lit::String(..) => panic!("uninterned string"),
                _ => todo!(),
            },
            N::Atom::Id(id) => match id {
                N::Id::Named(..) => panic!("unindexed id"),
                N::Id::Label(..) => panic!("label as atom"),
                N::Id::Func(id) => self.out.push(I32Const(*id as i32)),
                N::Id::Index(id) => self.out.push(GetLocal(*id)),
            },
            N::Atom::HTGet(ht, field, ty) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.rt_call_mono("ht_get", ty);
            }
            N::Atom::Index(ht, index, ty) => {
                self.translate_atom(ht);
                self.translate_atom(index);
                self.rt_call_mono("array_index", ty);
            }
            N::Atom::StringLen(string) => {
                self.translate_atom(string);
                self.rt_call("string_len");
            }
            N::Atom::Binary(op, a, b) => {
                self.translate_atom(a);
                self.translate_atom(b);
                self.translate_binop(op);
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
            // NOTE(arjun): We do not need to support I64, since JavaScript cannot
            // natively represent 64-bit integers.
            N::Type::F64 => ValueType::F64,
            // almost everything is a pointer type
            _ => ValueType::I32,
        }
    }
}
