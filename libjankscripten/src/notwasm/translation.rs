//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: [super::compile]

use super::super::rts_function::*;
use super::rt_bindings::get_rt_bindings;
use super::syntax as N;
use crate::opts::Opts;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use std::collections::HashMap;
use std::convert::TryInto;
use Instruction::*;

const JNKS_STRINGS_IDX: u32 = 0;
/// in bytes. i don't forsee this changing as we did a lot of work getting
/// it to fit in the largest wasm type
const ANY_SIZE: u32 = 8;
/// also bytes
const TAG_SIZE: u32 = 4;
const LENGTH_SIZE: u32 = 4;
const FN_OBJ_SIZE: u32 = 4;

type FuncTypeMap = HashMap<(Vec<ValueType>, Option<ValueType>), u32>;

pub fn translate(opts: &Opts, program: N::Program) -> Result<Vec<u8>, Error> {
    serialize(translate_parity(opts, program))
}

type IdEnv = im_rc::HashMap<N::Id, IdIndex>;

pub fn translate_parity(opts: &Opts, mut program: N::Program) -> Module {
    // The initial environment maps functions names to their indices.
    let mut global_env = IdEnv::default();
    for (index, (name, _)) in program.functions.iter().enumerate() {
        global_env.insert(
            name.clone(),
            IdIndex::Fun(index.try_into().expect("too many functions")),
        );
    }

    let mut module = module();
    // TODO(luna): these should eventually be enumerated separately in
    // something like rt_bindings
    // JNKS_STRINGS isn't used, it's looked up by the const JNKS_STRINGS_IDX,
    // but it should still be enumerated in the importing, so we give it a fake
    // env name
    let rt_globals = vec![("__JNKS_STRINGS", "JNKS_STRINGS", N::Type::I32)];
    for (_, rt_name, _) in &rt_globals {
        module = module
            .import()
            .path("runtime", rt_name)
            // runtime globals are never mutable because they're a mutable
            // pointer to the value which may or may not be mutable
            //
            // you'd think the type here should be the ty from the global, but
            // no, again they're all pointers so they're all I32. the actual
            // type according to notwasm is used later in IdIndex::RTGlobal
            .with_external(External::Global(GlobalType::new(ValueType::I32, false)))
            .build();
    }
    let mut index = 0;
    // borrow checker
    let rt_globals_len = rt_globals.len();
    for (name, _, ty) in rt_globals {
        global_env.insert(N::Id::Named(name.into()), IdIndex::RTGlobal(index, ty));
        index += 1;
    }
    for (name, global) in &program.globals {
        global_env.insert(name.clone(), IdIndex::Global(index, global.ty.clone()));
        index += 1;
    }

    // Map from function indices to original names
    let mut function_name_subsection: FunctionNameSubsection = Default::default();

    let rt_types = get_rt_bindings();
    let mut rt_indexes = HashMap::new();
    // build up indexes for mutual recursion first
    let mut type_indexes = HashMap::new();
    for (func_i, (name, ty)) in rt_types
        .into_iter()
        .chain(program.rts_fn_imports)
        .enumerate()
    {
        let type_i = if let N::Type::Fn(fn_ty) = ty {
            let wasm_ty = (types_as_wasm(&fn_ty.args), option_as_wasm(&fn_ty.result));
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
        function_name_subsection
            .names_mut()
            .insert(func_i as u32, name.to_string());
        rt_indexes.insert(name.clone(), func_i as u32);
        module = module
            .import()
            .path("runtime", &name)
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
    // data segment
    for global in program.globals.values_mut() {
        let mut visitor = Translate::new(
            opts,
            &rt_indexes,
            &type_indexes,
            &global_env,
            &mut program.data,
        );
        if let Some(atom) = &mut global.atom {
            visitor.translate_atom(atom);
        } else {
            // This global var is initialized lazily. it's default value will
            // be 0. We just need to figure out if wasm is expecting an i32 or
            // i64.
            let zero = match global.ty.as_wasm() {
                // 32-bit signed integer
                ValueType::I32 => I32Const(0),
                // 64-bit signed integer
                ValueType::I64 => I64Const(0),
                // 32-bit float
                ValueType::F32 => F32Const(0),
                // 64-bit float
                ValueType::F64 => F64Const(0),
            };

            visitor.out.push(zero);
        }
        let mut insts = visitor.out;
        assert_eq!(
            insts.len(),
            1,
            "
            parity_wasm unneccessarily restricts init_expr to len=1,
            so we're dealing with that for now i guess"
        );
        let restricted = insts.pop().unwrap();
        let mut partial_global = module
            .global()
            .with_type(global.ty.as_wasm())
            .init_expr(restricted);
        if global.is_mut {
            partial_global = partial_global.mutable();
        }
        module = partial_global.build();
    }
    // fsr we need an identity table to call indirect
    let num_runtime_functions = rt_indexes.len();
    let num_functions = num_runtime_functions + program.functions.keys().len();
    let mut table_build = module.table().with_min(num_functions as u32);
    for index in 0..num_functions {
        let index = index as u32;
        table_build = table_build.with_element(index, vec![index]);
    }
    let mut module = table_build.build();

    // For each function index, a map from local variable indices to original names.
    let mut local_name_subsection: LocalNameSubsection = Default::default();

    for (func_name, func) in program.functions.iter_mut() {
        let (f, local_map) = translate_func(
            opts,
            func,
            &global_env,
            &rt_indexes,
            &type_indexes,
            &mut program.data,
        );
        let loc = module.push_function(f);

        // It is surprising that we have to do this arithmetic ourselves. It looks like loc.body
        // does not account for the indices of the imported functions, which offset the indices
        // of all functions in this module.
        let offset: u32 = num_runtime_functions.try_into().expect("overflow");
        let actual_function_index = loc.body + offset;
        function_name_subsection
            .names_mut()
            .insert(actual_function_index, func_name.to_string());
        local_name_subsection
            .local_names_mut()
            .insert(actual_function_index, local_map);
    }
    insert_generated_main(
        opts,
        &program.globals,
        &global_env,
        &rt_indexes,
        rt_globals_len,
        &mut module,
    );
    let main_index = num_functions as u32;
    let module = module
        .data()
        .offset(GetGlobal(JNKS_STRINGS_IDX))
        .value(program.data)
        .build();

    let module = module.with_section(Section::Name(NameSection::new(
        None,
        Some(function_name_subsection),
        Some(local_name_subsection),
    )));

    // jnks_init calls main
    let module = module
        .export()
        .field("main")
        .internal()
        .func(main_index)
        .build();
    module.build()
}

fn translate_func(
    opts: &Opts,
    func: &mut N::Function,
    id_env: &IdEnv,
    rt_indexes: &HashMap<String, u32>,
    type_indexes: &FuncTypeMap,
    data: &mut Vec<u8>,
) -> (FunctionDefinition, IndexMap<String>) {
    let mut translator = Translate::new(opts, rt_indexes, type_indexes, id_env, data);

    // Add indices for parameters
    for (arg_name, arg_typ) in func.params.iter().zip(func.fn_type.args.iter()) {
        let index = translator.next_id;
        translator.next_id += 1;
        translator
            .id_env
            .insert(arg_name.clone(), IdIndex::Local(index, arg_typ.clone()));
    }

    let mut env = Env::default();

    // generate the actual code
    translator.translate_rec(&mut env, &mut func.body);
    let mut insts = vec![];

    if opts.disable_gc == false {
        // Eager shadow stack: The runtime system needs to create a shadow stack
        // frame that has enough slots for the local variables.
        let num_slots = translator.locals.len() + func.params.len();
        insts.push(I32Const(num_slots.try_into().unwrap()));
        insts.push(Call(*rt_indexes.get("gc_enter_fn").expect("no enter")));
    }

    insts.append(&mut translator.out);

    if opts.disable_gc == false {
        insts.push(Call(*rt_indexes.get("gc_exit_fn").expect("no exit")));
    }

    insts.push(End);
    let locals: Vec<_> = translator
        .locals
        .into_iter()
        .map(|t| Local::new(1, t))
        .collect();

    let local_map: IndexMap<String> = translator
        .id_env
        .iter()
        .filter_map(|(id, ix)| match ix {
            IdIndex::Local(n, _) => Some((*n, format!("{}", id))),
            _ => None,
        })
        .collect();
    let func = function()
        .signature()
        .with_params(types_as_wasm(&func.fn_type.args))
        .with_return_type(option_as_wasm(&func.fn_type.result))
        .build()
        .body()
        .with_instructions(Instructions::new(insts))
        .with_locals(locals)
        .build()
        .build();
    (func, local_map)
}

fn types_as_wasm(types: &[N::Type]) -> Vec<ValueType> {
    types.iter().map(N::Type::as_wasm).collect()
}
fn option_as_wasm(ty: &Option<Box<N::Type>>) -> Option<ValueType> {
    ty.as_ref().map(|t| t.as_wasm())
}

struct Translate<'a> {
    opts: &'a Opts,
    out: Vec<Instruction>,
    rt_indexes: &'a HashMap<String, u32>,
    type_indexes: &'a FuncTypeMap,
    data: &'a mut Vec<u8>,
    locals: Vec<ValueType>,
    next_id: u32,
    id_env: IdEnv,
}

#[derive(Clone, PartialEq, Default, Debug)]
struct Env {
    labels: im_rc::Vector<TranslateLabel>,
}

/// We use `TranslateLabel` to compile the named labels and breaks of NotWasm
/// to WebAssembly. In WebAssembly, blocks are not named, and break statements
/// refer to blocks using de Brujin indices (i.e., index zero for the innermost
/// block.  When recurring into a named NotWasm block, we add a
/// `TranslateLabel::Label` that holds the block's name on to the environment.
/// To compile a `break l` statement to WebAssembly, we scan the labels for
/// the index of `TranslateLabel::Label(l)`. When the compiler introduces an
/// unnamed WebAssembly block, it pushes a `TranslateLabel::Unused` onto the
/// `LabelEnv`, which ensures that indices shift correctly.
#[derive(Clone, PartialEq, Debug)]
enum TranslateLabel {
    Unused,
    Label(N::Label),
}

/// We use `IdIndex` to resolve identifiers that appear in a NotWasm program
/// while compiling to WebAssembly. Before compiling the body of a function, we
/// populate an `IdEnv` to map each function name `f` to its index `n`
/// (`IdIndex::Fun(n)`).
#[derive(Clone, PartialEq, Debug)]
enum IdIndex {
    Local(u32, N::Type),
    Global(u32, N::Type),
    /// runtime globals are handled differently because rust exports statics
    /// as the memory address of the actual value
    RTGlobal(u32, N::Type),
    Fun(u32),
}

impl<'a> Translate<'a> {
    fn new(
        opts: &'a Opts,
        rt_indexes: &'a HashMap<String, u32>,
        type_indexes: &'a FuncTypeMap,
        id_env: &IdEnv,
        data: &'a mut Vec<u8>,
    ) -> Self {
        Self {
            opts,
            out: Vec::new(),
            rt_indexes,
            type_indexes,
            next_id: 0,
            id_env: id_env.clone(),
            locals: Vec::new(),
            data,
        }
    }

    /// Pushes an instruction that passes a GC root to the runtime
    /// system. There are multiple kinds of roots that might contain pointers,
    /// thus we dispatch on the type of the GC root.
    fn set_in_current_shadow_frame_slot(&mut self, ty: &N::Type) {
        self.rt_call(shadow_frame_fn(ty))
    }

    // We are not using a visitor, since we have to perform an operation on every
    // give of statement and expression. Thus, the visitor wouldn't give us much.
    //
    // The `translate_rec` function receives a mutable reference to `Env`, which
    // allows it to introduce new variable declarations. This makes block
    // statements easier to compile, since each statement in a block can alter
    // the environment of its successor. (The alternative would be to have
    // `translate_rec` return a new environment.) However, we have to take care
    // to clone `env` when we enter a new block scope.
    pub(self) fn translate_rec(&mut self, env: &Env, stmt: &mut N::Stmt) {
        match stmt {
            N::Stmt::Store(id, expr, _) => {
                // storing into a reference translates into a raw write
                // TODO(luna): this should really have the type in the
                // AST so we don't have to do this messiness
                let ty = self
                    .get_id(id)
                    .expect("add types to globals to support global ref");
                let ty = if let N::Type::Ref(b_ty) = ty {
                    *b_ty
                } else {
                    panic!("tried to store into non-ref");
                };
                self.translate_expr(expr);
                self.store(ty, TAG_SIZE);
            }
            N::Stmt::Empty => (),
            N::Stmt::Block(ss, _) => {
                // don't surround in an actual block, those are only useful
                // when labeled
                for s in ss {
                    self.translate_rec(env, s);
                }
            }
            N::Stmt::Var(var_stmt, _) => {
                // Binds variable in env after compiling expr (prevents
                // circularity).
                self.translate_expr(&mut var_stmt.named);

                let index = self.next_id;
                self.next_id += 1;
                self.locals.push(var_stmt.ty().as_wasm());
                self.id_env.insert(
                    var_stmt.id.clone(),
                    IdIndex::Local(index, var_stmt.ty().clone()),
                );

                // Eager shadow stack:
                if self.opts.disable_gc == true || var_stmt.ty().is_gc_root() == false {
                    self.out.push(SetLocal(index));
                } else {
                    self.out.push(TeeLocal(index));
                    self.out.push(I32Const(index.try_into().unwrap()));
                    self.set_in_current_shadow_frame_slot(var_stmt.ty());
                }
            }
            N::Stmt::Expression(expr, _) => {
                self.translate_expr(expr);
                self.out.push(Drop); // side-effects only, please
            }
            N::Stmt::Assign(id, expr, _) => {
                match self
                    .id_env
                    .get(id)
                    .expect(&format!("unbound identifier {:?} in = {:?}", id, expr))
                    .clone()
                {
                    IdIndex::Local(n, ty) => {
                        self.translate_expr(expr);
                        if self.opts.disable_gc == true || ty.is_gc_root() == false {
                            self.out.push(SetLocal(n));
                        } else {
                            self.out.push(TeeLocal(n));
                            self.out.push(I32Const((n).try_into().unwrap()));
                            self.set_in_current_shadow_frame_slot(&ty);
                        }
                    }
                    IdIndex::Global(n, ty) => {
                        self.translate_expr(expr);
                        // no tee for globals
                        self.out.push(SetGlobal(n));
                        if self.opts.disable_gc == false && ty.is_gc_root() {
                            self.out.push(GetGlobal(n));
                            self.out.push(I32Const((n).try_into().unwrap()));
                            self.out
                                .push(get_set_in_globals_frame(&self.rt_indexes, &ty));
                        }
                    }
                    IdIndex::RTGlobal(n, ty) => {
                        // no need to update roots for RTGlobal because they
                        // reside in memory in the first place... since RTGlobals
                        // aren't really even real (yet at least), it's not worth
                        // reasoning through this
                        self.out.push(GetGlobal(n));
                        self.translate_expr(expr);
                        self.store(ty, 0);
                    }
                    IdIndex::Fun(..) => panic!("cannot set function"),
                }
            }
            N::Stmt::If(cond, conseq, alt, _) => {
                self.translate_atom(cond);
                self.out.push(If(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Unused);
                self.translate_rec(&env1, conseq);
                self.out.push(Else);
                self.translate_rec(&env1, alt);
                self.out.push(End);
            }
            N::Stmt::Loop(body, _) => {
                // breaks should be handled by surrounding label already
                self.out.push(Loop(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Unused);
                self.translate_rec(&env1, body);
                // loop doesn't automatically continue, don't ask me why
                self.out.push(Br(0));
                self.out.push(End);
            }
            N::Stmt::Label(x, stmt, _) => {
                if let N::Label::App(_) = x {
                    panic!("Label::App was not elimineted by elim_gotos");
                }
                self.out.push(Block(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Label(x.clone()));
                self.translate_rec(&mut env1, stmt);
                self.out.push(End);
            }
            N::Stmt::Break(label, _) => {
                let l = TranslateLabel::Label(label.clone());
                let i = env
                    .labels
                    .index_of(&l)
                    .expect(&format!("unbound label {:?}", label));
                self.out.push(Br(i as u32));
            }
            N::Stmt::Return(atom, _) => {
                if self.opts.disable_gc == false {
                    self.rt_call("gc_exit_fn");
                }
                self.translate_atom(atom);
                self.out.push(Return);
            }
            N::Stmt::Trap => {
                self.out.push(Unreachable);
            }
            N::Stmt::Goto(..) => {
                panic!(
                    "this should be NotWasm, not GotoWasm. did you run elim_gotos? did it work?"
                );
            }
        }
    }

    fn translate_binop(&mut self, op: &N::BinaryOp) {
        use N::BinaryOp as NO;
        match op {
            NO::PtrEq => self.out.push(I32Eq),
            NO::I32Eq => self.out.push(I32Eq),
            NO::I32Ne => self.out.push(I32Ne),
            NO::I32Add => self.out.push(I32Add),
            NO::I32Sub => self.out.push(I32Sub),
            NO::I32GT => self.out.push(I32GtS),
            NO::I32LT => self.out.push(I32LtS),
            NO::I32Ge => self.out.push(I32GeS),
            NO::I32Le => self.out.push(I32LeS),
            NO::I32Mul => self.out.push(I32Mul),
            NO::I32Div => self.out.push(I32DivS),
            NO::I32Rem => self.out.push(I32RemS),
            NO::I32And => self.out.push(I32And),
            NO::I32Or => self.out.push(I32Or),
            NO::I32Xor => self.out.push(I32Xor),
            NO::I32Shl => self.out.push(I32Shl),
            NO::I32Shr => self.out.push(I32ShrS),
            NO::I32ShrU => self.out.push(I32ShrU),
            NO::F64Add => self.out.push(F64Add),
            NO::F64Sub => self.out.push(F64Sub),
            NO::F64Mul => self.out.push(F64Mul),
            NO::F64Div => self.out.push(F64Div),
            NO::F64LT => self.out.push(F64Lt),
            NO::F64GT => self.out.push(F64Gt),
            NO::F64Le => self.out.push(F64Le),
            NO::F64Ge => self.out.push(F64Ge),
            NO::F64Eq => self.out.push(F64Eq),
            NO::F64Ne => self.out.push(F64Ne),
        }
    }
    fn translate_unop(&mut self, op: &N::UnaryOp) {
        match op {
            N::UnaryOp::Sqrt => self.out.push(F64Sqrt),
            N::UnaryOp::Neg => self.out.push(F64Neg),
            N::UnaryOp::Eqz => self.out.push(I32Eqz),
        }
    }

    fn translate_expr(&mut self, expr: &mut N::Expr) {
        match expr {
            N::Expr::Atom(atom, _) => self.translate_atom(atom),
            N::Expr::HT => self.rt_call("ht_new"),
            N::Expr::Array => self.rt_call("array_new"),
            N::Expr::ArraySet(arr, index, value, _) => {
                self.translate_atom(arr);
                self.translate_atom(index);
                self.translate_atom(value);
                self.rt_call("array_set");
            }
            N::Expr::HTSet(ht, field, val, _) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.translate_atom(val);
                self.rt_call("ht_set");
            }
            N::Expr::ObjectSet(obj, field, val, _) => {
                self.translate_atom(obj);
                self.translate_atom(field);
                self.translate_atom(val);
                self.data_cache();
                self.rt_call("object_set");
            }
            N::Expr::ObjectEmpty => {
                // New objects like `{}` or `new Object()` are created using
                // the runtime function `jnks_new_object`, which is located in
                // `runtime.notwasm`. We have to find the function index of
                // this runtime function and call it.
                //
                // The reason we use a runtime function to create `{}` as
                // opposed to creating a legitimately empty object is because
                // `{}` inherits from the default Object prototype, which
                // must be resolved dynamically.
                self.notwasm_rt_call("jnks_new_object");
            }
            N::Expr::Push(array, val, _) => {
                self.translate_atom(array);
                self.translate_atom(val);
                self.rt_call("array_push");
            }
            N::Expr::PrimCall(rts_func, args, _) => {
                for arg in args {
                    self.translate_atom(arg);
                }

                let name = rts_func.name();

                // Runtime functions can either be implemented in the
                // Rust runtime or the NotWasm runtime.
                match name {
                    RTSFunctionImpl::Rust(name) => {
                        self.rt_call(name);
                    }
                    RTSFunctionImpl::NotWasm(name) => {
                        self.notwasm_rt_call(name);
                    }
                }
            }
            N::Expr::Call(f, args, s) => {
                for arg in args {
                    self.get_id(arg);
                }
                match self.id_env.get(f).cloned() {
                    Some(IdIndex::Fun(i)) => {
                        // we index in notwasm by 0 = first user function. but
                        // wasm indexes by 0 = first rt function. so we have
                        // to offset
                        self.out.push(Call(i + self.rt_indexes.len() as u32));
                    }
                    Some(IdIndex::Local(i, t)) => {
                        self.out.push(GetLocal(i));
                        let (params_tys, ret_ty) = match t {
                            N::Type::Fn(fn_ty) => {
                                (types_as_wasm(&fn_ty.args), option_as_wasm(&fn_ty.result))
                            }
                            _ => panic!("identifier {:?} is not function-typed", f),
                        };
                        let ty_index = self
                            .type_indexes
                            .get(&(params_tys, ret_ty))
                            .unwrap_or_else(|| panic!("function type was not indexed {:?}", s));
                        self.out.push(CallIndirect(*ty_index, 0));
                    }
                    Some(index) => panic!(
                        "can't translate Func ID for function ({}): ({:?})",
                        f, index
                    ),
                    _ => panic!("expected Func ID ({})", f),
                };
            }
            N::Expr::ClosureCall(f, args, s) => {
                match self.id_env.get(f).cloned() {
                    Some(IdIndex::Fun(_)) => panic!("closures are always given a name"),
                    Some(which @ IdIndex::Local(..)) | Some(which @ IdIndex::Global(..)) => {
                        let t = match which {
                            IdIndex::Local(i, ref t) => {
                                self.out.push(GetLocal(i));
                                t.clone()
                            }
                            IdIndex::Global(i, ref t) => {
                                self.out.push(GetGlobal(i));
                                t.clone()
                            }
                            _ => unreachable!(),
                        };
                        self.rt_call("closure_env");
                        let (params_tys, ret_ty) = match t {
                            N::Type::Closure(fn_ty) => {
                                (types_as_wasm(&fn_ty.args), option_as_wasm(&fn_ty.result))
                            }
                            _ => panic!("identifier {:?} is not function-typed", f),
                        };
                        let ty_index = self
                            .type_indexes
                            .get(&(params_tys, ret_ty))
                            .unwrap_or_else(|| panic!("function type was not indexed {:?}", s));
                        for arg in args {
                            self.get_id(arg);
                        }
                        match which {
                            IdIndex::Local(i, _) => self.out.push(GetLocal(i)),
                            IdIndex::Global(i, _) => self.out.push(GetGlobal(i)),
                            _ => unreachable!(),
                        }
                        self.rt_call("closure_func");
                        self.out.push(CallIndirect(*ty_index, 0));
                    }
                    got => panic!("expected Func ID ({}), but got {:?}", f, got),
                };
            }
            N::Expr::NewRef(a, ty, _) => {
                self.translate_atom(a);
                match ty {
                    N::Type::I32 | N::Type::Bool | N::Type::Fn(..) => {
                        self.rt_call("ref_new_non_ptr_32")
                    }
                    N::Type::F64 => self.rt_call("ref_new_f64"),
                    N::Type::Ref(..) => panic!("while recursive refs can be made, they shouldn't"),
                    N::Type::Any => self.rt_call("ref_new_any"),
                    _ => self.rt_call("ref_new_ptr"),
                }
            }
            N::Expr::Closure(id, env, _) => {
                // one day, we may be able to restore a 0-size environment
                // optimization here involving nullptr
                self.out.push(I32Const(env.len() as i32));
                self.notwasm_rt_call("jnks_new_fn_obj");
                self.rt_call("env_alloc");
                // init all the
                for (i, (a, ty)) in env.iter_mut().enumerate() {
                    self.out.push(I32Const(i as i32));
                    self.translate_atom(a);
                    self.to_any(ty);
                    // this returns the env so we don't need locals magic
                    self.rt_call("env_init_at");
                }
                // env is left on the stack. now the function is the second
                // argument
                self.get_id(id);
                self.rt_call("closure_new");
            }
        }
    }

    fn translate_atom(&mut self, atom: &mut N::Atom) {
        match atom {
            N::Atom::Deref(a, ty, _) => {
                self.translate_atom(a);
                self.load(ty, TAG_SIZE);
            }
            N::Atom::Lit(lit, _) => match lit {
                N::Lit::I32(i) => self.out.push(I32Const(*i)),
                N::Lit::F64(f) => self.out.push(F64Const(unsafe { std::mem::transmute(*f) })),
                N::Lit::Interned(addr) => {
                    self.out.push(GetGlobal(JNKS_STRINGS_IDX));
                    self.out.push(I32Const(*addr as i32));
                    self.out.push(I32Add);
                }
                N::Lit::String(..) => panic!("uninterned string"),
                N::Lit::Bool(b) => self.out.push(I32Const(*b as i32)),
                N::Lit::Undefined => self.rt_call("get_undefined"),
                N::Lit::Null => self.rt_call("get_null"),
            },
            N::Atom::Id(id, _) => {
                self.get_id(id);
            }
            N::Atom::PrimApp(id, args, _) => {
                for a in args {
                    self.translate_atom(a);
                }
                self.rt_call(&id.clone().into_name());
            }
            N::Atom::GetPrimFunc(id, _) => {
                // TODO(luna): i honestly for the life of me can't remember
                // why we accept an &mut Atom instead of an Atom, which
                // would avoid this clone
                if let Some(i) = self.rt_indexes.get(&id.clone().into_name()) {
                    self.out.push(I32Const(*i as i32));
                } else {
                    panic!("cannot find rt {}", id);
                }
            }
            N::Atom::FromAny(a, ty, _) => {
                self.translate_atom(a);
                match ty {
                    N::Type::I32 => self.rt_call("any_to_i32"),
                    N::Type::Bool => self.rt_call("any_to_bool"),
                    N::Type::F64 => self.rt_call("any_to_f64"),
                    N::Type::Fn(..) => panic!("cannot attain function from any"),
                    N::Type::Closure(..) => self.rt_call("any_to_closure"),
                    N::Type::Any => (),
                    _ => self.rt_call("any_to_ptr"),
                }
            }
            N::Atom::FloatToInt(a, _) => {
                self.translate_atom(a);
                self.out.push(I32TruncSF64);
            }
            N::Atom::IntToFloat(a, _) => {
                self.translate_atom(a);
                self.out.push(F64ConvertSI32);
            }
            N::Atom::HTGet(ht, field, _) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.rt_call("ht_get");
            }
            N::Atom::ObjectGet(obj, field, _) => {
                self.translate_atom(obj);
                self.translate_atom(field);
                self.data_cache();
                self.rt_call("object_get");
            }
            N::Atom::Index(arr, index, _) => {
                self.translate_atom(arr);
                self.translate_atom(index);
                self.rt_call("array_index");
            }
            N::Atom::ArrayLen(array, _) => {
                self.translate_atom(array);
                self.rt_call("array_len");
            }
            N::Atom::StringLen(string, _) => {
                self.translate_atom(string);
                self.rt_call("string_len");
            }
            N::Atom::Binary(op, a, b, _) => {
                self.translate_atom(a);
                self.translate_atom(b);
                self.translate_binop(op);
            }
            N::Atom::Unary(op, a, _) => {
                self.translate_atom(a);
                self.translate_unop(op);
            }
            N::Atom::EnvGet(index, ty, _) => {
                // get the env which is always the first argument
                self.out.push(GetLocal(0));
                let offset = TAG_SIZE + LENGTH_SIZE + FN_OBJ_SIZE + *index * ANY_SIZE;
                // as an optimization, we can avoid calling the coercion
                // functions in the runtime since we know the type already
                if let N::Type::Closure(_) = ty {
                    // the closure is the only "special" type in an any: it is
                    // stored is the most significant 48 bits of the AnyValue,
                    // however, the closure is supposed to be stored in the
                    // *least* significant 48 bits. one might load with an offset
                    // of 2 and only load 48 bits, but it's not supported by
                    // wasm. or, you could load the full 64 bits and let
                    // the garbage be the padding. except, that is memory
                    // unsafe. it could be tried as an optimization, but this will
                    // work for now
                    self.out.push(I64Load(2, offset));
                    self.out.push(I64Const(16));
                    self.out.push(I64ShrU);
                } else {
                    if ty.as_wasm() == ValueType::I64 {
                        self.out.push(I64Load(2, offset));
                    } else {
                        // anything else is stored as the most significant 32 bits
                        // of the AnyValue. Note That Because Of Little Endian
                        // Byte Order This Means It's The Last Bytes
                        self.out.push(I32Load(2, offset + 4));
                    }
                }
            }
        }
    }

    /// this is useful for debugging when you want to put a log every time you
    /// generate some code. the to-any is handled and the return is dropped
    #[allow(unused)]
    fn call_log_any(&mut self, insts: Vec<Instruction>, ty: &N::Type) {
        // the env is not read, so it can be anything
        self.out.push(I32Const(0));
        // this
        self.rt_call("get_undefined");
        self.out.extend(insts);
        // our thing
        self.to_any(ty);
        self.rt_call("log_any");
        self.out.push(Drop);
    }

    fn load(&mut self, ty: &N::Type, offset: u32) {
        match ty.as_wasm() {
            ValueType::I32 => self.out.push(I32Load(2, offset)),
            ValueType::I64 => self.out.push(I64Load(2, offset)),
            ValueType::F32 => self.out.push(F32Load(2, offset)),
            ValueType::F64 => self.out.push(F64Load(2, offset)),
        }
    }
    fn store(&mut self, ty: N::Type, offset: u32) {
        match ty.as_wasm() {
            ValueType::I32 => self.out.push(I32Store(2, offset)),
            ValueType::I64 => self.out.push(I64Store(2, offset)),
            ValueType::F32 => self.out.push(F32Store(2, offset)),
            ValueType::F64 => self.out.push(F64Store(2, offset)),
        }
    }

    /// Generate instructions to call a *Rust* runtime function.
    fn rt_call(&mut self, name: &str) {
        if let Some(i) = self.rt_indexes.get(name) {
            self.out.push(Call(*i));
        } else {
            panic!("cannot find rt {}", name);
        }
    }

    /// Search for a function in the NotWasm runtime. Return the
    /// function index if its found.
    fn get_notwasm_rt_fn(&mut self, name: &str) -> Option<u32> {
        if let Some(IdIndex::Fun(func)) = self.id_env.get(&N::Id::Named(name.to_string())) {
            Some(*func + self.rt_indexes.len() as u32)
        } else {
            None
        }
    }

    /// Generate instructions to call a *NotWasm* runtime function.
    fn notwasm_rt_call(&mut self, name: &str) {
        if let Some(index) = self.get_notwasm_rt_fn(name) {
            self.out.push(Call(index))
        } else {
            panic!("cannot find notwasm runtime function {}", name);
        }
    }

    fn to_any(&mut self, ty: &N::Type) {
        match ty {
            N::Type::I32 => self.rt_call("any_from_i32"),
            N::Type::Bool => self.rt_call("any_from_bool"),
            N::Type::F64 => self.rt_call("f64_to_any"),
            N::Type::Fn(..) => self.rt_call("any_from_fn"),
            N::Type::Closure(..) => self.rt_call("any_from_closure"),
            N::Type::Any => (),
            _ => self.rt_call("any_from_ptr"),
        }
    }

    fn get_id(&mut self, id: &N::Id) -> Option<N::Type> {
        match self
            .id_env
            .get(id)
            .expect(&format!("unbound identifier {:?}", id))
        {
            IdIndex::Local(n, ty) => {
                self.out.push(GetLocal(*n));
                Some(ty.clone())
            }
            IdIndex::Global(n, _) => {
                self.out.push(GetGlobal(*n));
                None
            }
            IdIndex::RTGlobal(n, ty) => {
                self.out.push(GetGlobal(*n));
                let ty = ty.clone();
                self.load(&ty, 0);
                Some(ty)
            }
            // notwasm indexes from our functions, wasm indexes from rt
            IdIndex::Fun(n) => {
                self.out
                    .push(I32Const(*n as i32 + self.rt_indexes.len() as i32));
                None
            }
        }
    }

    /// Sets up caching for a particular object field lookup in the generated
    /// code. It does 2 things:
    /// 1. generates wasm instructions to push the cached offset onto the stack.
    /// 2. extends the inline cache to include a unique cache spot for these
    ///    generated object field lookup instructions.
    fn data_cache(&mut self) {
        // the end of the data segment is the new cache
        self.out.push(GetGlobal(JNKS_STRINGS_IDX));
        self.out.push(I32Const(self.data.len() as i32));
        self.out.push(I32Add);
        // -1 is our placeholder
        self.data
            .extend(&unsafe { std::mem::transmute::<_, [u8; 4]>((-1i32).to_le()) });
    }
}

fn insert_generated_main(
    opts: &Opts,
    globals: &HashMap<N::Id, N::Global>,
    global_env: &IdEnv,
    rt_indexes: &HashMap<String, u32>,
    rt_globals_len: usize,
    module: &mut ModuleBuilder,
) {
    // the true entry point is generated code to avoid GC instrumentation
    // messiness
    let mut insts = Vec::new();
    // rust init function
    insts.push(Call(*rt_indexes.get("init").expect("no enter")));

    if opts.disable_gc == false {
        // globals are roots! put them in the first shadow frame
        let num_slots = rt_globals_len + globals.len();
        insts.push(I32Const(num_slots.try_into().unwrap()));
        // this function doesn't really have locals. this is for the globals
        insts.push(Call(
            *rt_indexes.get("gc_enter_fn").expect("no gc_enter_fn"),
        ));
    }

    // TODO(arjun): Luna -- is the following comment stale? Could we figure out
    // what this is about?
    // these two for loops, i realize now, don't do anything in practice since
    // all the globals happen to be lazy
    for (index, global) in globals.values().enumerate() {
        if opts.disable_gc == false && global.ty.is_gc_root() && global.atom.is_some() {
            insts.push(GetGlobal((rt_globals_len + index) as u32));
            insts.push(I32Const((rt_globals_len + index) as i32));
            insts.push(get_set_in_globals_frame(rt_indexes, &global.ty));
        }
    }

    if let Some(IdIndex::Fun(func)) = global_env.get(&N::Id::Named("jnks_init".to_string())) {
        insts.push(Call(*func + rt_indexes.len() as u32))
    } else {
        panic!("cannot find notwasm runtime function jnks_init");
    }
    // this has to be in generated_main because no void. it'd be cleaner in
    // jnks_init, but in generated at least the locals of jnks_init aren't GC
    // roots i guess (tail call)
    if let Some(IdIndex::Fun(func)) = global_env.get(&N::Id::Named("main".to_string())) {
        insts.push(Call(*func + rt_indexes.len() as u32));
    } else {
        panic!("cannot find notwasm main");
    }
    if opts.disable_gc == false {
        insts.push(Call(*rt_indexes.get("gc_exit_fn").expect("no gc_exit_fn")));
    }
    insts.push(End);
    // this is just the worst hack due to lack of void type. i still
    // don't want to add it because it doesn't exist in from-jankyscript
    // notwasm, but it makes all my tests that return have an extra thing
    // on the stack. but since it's only the tests, i should be able to
    // identify tests and drop only then. it's awful. i know
    #[cfg(test)]
    let return_type = Some(N::Type::I32.as_wasm());
    #[cfg(not(test))]
    let return_type = None;
    module.push_function(
        function()
            .signature()
            .with_params(vec![])
            .with_return_type(return_type)
            .build()
            .body()
            .with_instructions(Instructions::new(insts))
            .build()
            .build(),
    );
}

impl N::Type {
    pub fn as_wasm(&self) -> ValueType {
        use N::Type::*;
        match self {
            // NOTE(arjun): We do not need to support I64, since JavaScript cannot
            // natively represent 64-bit integers.
            F64 => ValueType::F64,
            Any => ValueType::I64,
            I32 => ValueType::I32,
            Bool => ValueType::I32,
            // almost everything is a pointer type
            String => ValueType::I32,
            HT => ValueType::I32,
            Array => ValueType::I32,
            DynObject => ValueType::I32,
            Fn(..) => ValueType::I32,
            Closure(..) => ValueType::I64,
            Ref(..) => ValueType::I32,
            Env => ValueType::I32,
        }
    }
}

/// Like Translate::set_in_current_shadow_frame_slot, but give the name instead
/// of adding the call to the instructions
fn shadow_frame_fn(ty: &N::Type) -> &'static str {
    match ty {
        N::Type::Any => "set_any_in_current_shadow_frame_slot",
        N::Type::Closure(..) => "set_closure_in_current_shadow_frame_slot",
        _ => "set_in_current_shadow_frame_slot",
    }
}

/// this gets the instruction to call a set_in_globals_frame for a given
/// type. does not add it anywhere
#[must_use]
fn get_set_in_globals_frame(rt_indexes: &HashMap<String, u32>, ty: &N::Type) -> Instruction {
    let func = match ty {
        N::Type::Closure(..) => "set_closure_in_globals_frame",
        N::Type::Any => "set_any_in_globals_frame",
        _ => "set_in_globals_frame",
    };
    if let Some(i) = rt_indexes.get(func) {
        Call(*i)
    } else {
        panic!("cannot find rt {}", func);
    }
}
