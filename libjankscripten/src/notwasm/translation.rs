//! translate NotWasm to wasm, using the rust runtime whenever possible
//!
//! preconditions: [super::compile]

use super::rt_bindings::get_rt_bindings;
use super::syntax as N;
use parity_wasm::builder::*;
use parity_wasm::elements::*;
use parity_wasm::serialize;
use std::collections::HashMap;
use std::convert::TryInto;
use Instruction::*;

const JNKS_STRINGS_IDX: u32 = 0;

type FuncTypeMap = HashMap<(Vec<ValueType>, Option<ValueType>), u32>;

pub fn translate(program: N::Program) -> Result<Vec<u8>, Error> {
    serialize(translate_parity(program))
}

type IdEnv = im_rc::HashMap<N::Id, IdIndex>;

pub fn translate_parity(mut program: N::Program) -> Module {
    // The initial environment maps functions names to their indices.
    let mut global_env = IdEnv::default();
    for (index, (name, _)) in program.functions.iter().enumerate() {
        global_env.insert(
            name.clone(),
            IdIndex::Fun(index.try_into().expect("too many functions")),
        );
    }
    for (i, name) in program.globals.keys().enumerate() {
        global_env.insert(name.clone(), IdIndex::Global(i as u32));
    }

    let mut module = module();
    let rt_types = get_rt_bindings();
    let mut rt_indexes = HashMap::new();
    // build up indexes for mutual recursion first
    let mut type_indexes = HashMap::new();
    for (func_i, (name, ty)) in rt_types.iter().enumerate() {
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
    // data segment
    // right now there's only one global, if that changes we can refactor
    module = module
        .import()
        .path("runtime", "JNKS_STRINGS")
        .with_external(External::Global(GlobalType::new(ValueType::I32, false)))
        .build();
    for global in program.globals.values_mut() {
        // can't use functions anyway so no need to worry
        let empty = HashMap::new();
        let empty2 = HashMap::new();
        let mut visitor = Translate::new(&empty, &empty2, &global_env, &mut program.data);
        visitor.translate_atom(&mut global.atom);
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
    let mut module = table_build.build();
    for (name, func) in program.functions.iter_mut() {
        module.push_function(translate_func(
            func,
            &global_env,
            &rt_indexes,
            &type_indexes,
            name,
            &mut program.data,
        ));
    }
    let module = module
        .data()
        .offset(GetGlobal(JNKS_STRINGS_IDX))
        .value(program.data)
        .build();
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
    id_env: &IdEnv,
    rt_indexes: &HashMap<String, u32>,
    type_indexes: &FuncTypeMap,
    name: &N::Id,
    data: &mut Vec<u8>,
) -> FunctionDefinition {
    let mut translator = Translate::new(rt_indexes, type_indexes, id_env, data);

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
    // before the code, if this is main we have to call rt init()
    if name == &N::Id::Named("main".to_string()) {
        insts = vec![Call(*rt_indexes.get("init").expect("no init"))];
    }

    // Eager shadow stack: The runtime system needs to create a shadow stack
    // frame that has enough slots for the local variables.
    let num_slots = translator.locals.len() + func.params.len();
    insts.push(I32Const(num_slots.try_into().unwrap()));
    insts.push(Call(*rt_indexes.get("gc_enter_fn").expect("no enter")));

    insts.append(&mut translator.out);
    insts.push(Call(*rt_indexes.get("gc_exit_fn").expect("no exit")));
    insts.push(End);
    let locals: Vec<_> = translator
        .locals
        .into_iter()
        .map(|t| Local::new(1, t))
        .collect();
    function()
        .signature()
        .with_params(types_as_wasm(&func.fn_type.args))
        .with_return_type(option_as_wasm(&func.fn_type.result))
        .build()
        .body()
        .with_instructions(Instructions::new(insts))
        .with_locals(locals)
        .build()
        .build()
}

fn types_as_wasm(types: &[N::Type]) -> Vec<ValueType> {
    types.iter().map(N::Type::as_wasm).collect()
}
fn option_as_wasm(ty: &Option<Box<N::Type>>) -> Option<ValueType> {
    ty.as_ref().map(|t| t.as_wasm())
}

struct Translate<'a> {
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
    Global(u32),
    Fun(u32),
}

impl<'a> Translate<'a> {
    fn new(
        rt_indexes: &'a HashMap<String, u32>,
        type_indexes: &'a FuncTypeMap,
        id_env: &IdEnv,
        data: &'a mut Vec<u8>,
    ) -> Self {
        Self {
            out: Vec::new(),
            rt_indexes,
            type_indexes,
            next_id: 0,
            id_env: id_env.clone(),
            locals: Vec::new(),
            data,
        }
    }

    /// Produces an instruction that passes a GC root to the runtime system. There are two kinds
    /// of roots: pointers (I32) and Any-typed values (I64) that may contain pointers, thus we
    /// dispatch on the type of the GC root.
    fn set_in_current_shadow_frame_slot(&self, ty: &N::Type) -> Instruction {
        match ty {
            N::Type::Any => Call(
                *self
                    .rt_indexes
                    .get("set_any_in_current_shadow_frame_slot")
                    .unwrap(),
            ),
            _ => Call(
                *self
                    .rt_indexes
                    .get("set_in_current_shadow_frame_slot")
                    .unwrap(),
            ),
        }
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
            N::Stmt::Store(id, expr) => {
                // storing into a reference translates into a raw write
                self.get_id(id);
                self.translate_expr(expr);
                self.out.push(I32Store(0, 4)); // skip 32 bits for tag in I32Ptr
            }
            N::Stmt::Empty => (),
            N::Stmt::Block(ss) => {
                // don't surround in an actual block, those are only useful
                // when labeled
                for s in ss {
                    self.translate_rec(env, s);
                }
            }
            N::Stmt::Var(var_stmt) => {
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
                if var_stmt.ty().is_gc_root() == false {
                    self.out.push(SetLocal(index));
                } else {
                    self.out.push(TeeLocal(index));
                    self.out.push(I32Const(index.try_into().unwrap()));
                    self.out
                        .push(self.set_in_current_shadow_frame_slot(var_stmt.ty()));
                }
            }
            N::Stmt::Expression(expr) => {
                self.translate_expr(expr);
                self.out.push(Drop); // side-effects only, please
            }
            N::Stmt::Assign(id, expr) => {
                self.translate_expr(expr);
                match self
                    .id_env
                    .get(id)
                    .expect(&format!("unbound identifier {:?}", id))
                {
                    IdIndex::Local(n, ty) => {
                        if ty.is_gc_root() == false {
                            self.out.push(SetLocal(*n));
                        } else {
                            self.out.push(TeeLocal(*n));
                            self.out.push(I32Const((*n).try_into().unwrap()));
                            self.out.push(self.set_in_current_shadow_frame_slot(&ty));
                        }
                    }
                    // +1 for JNKS_STRINGS
                    IdIndex::Global(n) => self.out.push(SetGlobal(*n + 1)),
                    IdIndex::Fun(..) => panic!("cannot set function"),
                }
            }
            N::Stmt::If(cond, conseq, alt) => {
                self.translate_atom(cond);
                self.out.push(If(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Unused);
                self.translate_rec(&env1, conseq);
                self.out.push(Else);
                self.translate_rec(&env1, alt);
                self.out.push(End);
            }
            N::Stmt::Loop(body) => {
                // breaks should be handled by surrounding label already
                self.out.push(Loop(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Unused);
                self.translate_rec(&env1, body);
                // loop doesn't automatically continue, don't ask me why
                self.out.push(Br(0));
                self.out.push(End);
            }
            N::Stmt::Label(x, stmt) => {
                if let N::Label::App(_) = x {
                    panic!("Label::App was not elimineted by elim_gotos");
                }
                self.out.push(Block(BlockType::NoResult));
                let mut env1 = env.clone();
                env1.labels.push_front(TranslateLabel::Label(x.clone()));
                self.translate_rec(&mut env1, stmt);
                self.out.push(End);
            }
            N::Stmt::Break(label) => {
                let l = TranslateLabel::Label(label.clone());
                let i = env
                    .labels
                    .index_of(&l)
                    .expect(&format!("unbound label {:?}", label));
                self.out.push(Br(i as u32));
            }
            N::Stmt::Return(atom) => {
                self.rt_call("gc_exit_fn");
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
        match op {
            N::BinaryOp::PtrEq => self.out.push(I32Eq),
            N::BinaryOp::I32Eq => self.out.push(I32Eq),
            N::BinaryOp::I32Add => self.out.push(I32Add),
            N::BinaryOp::I32Sub => self.out.push(I32Sub),
            N::BinaryOp::I32GT => self.out.push(I32GtS),
            N::BinaryOp::I32LT => self.out.push(I32LtS),
            N::BinaryOp::I32Ge => self.out.push(I32GeS),
            N::BinaryOp::I32Le => self.out.push(I32LeS),
            N::BinaryOp::I32Mul => self.out.push(I32Mul),
            N::BinaryOp::I32Div => self.out.push(I32DivS),
            N::BinaryOp::I32Rem => self.out.push(I32RemS),
            N::BinaryOp::I32And => self.out.push(I32And),
            N::BinaryOp::I32Or => self.out.push(I32Or),
            N::BinaryOp::F64Add => self.out.push(F64Add),
            N::BinaryOp::F64Sub => self.out.push(F64Sub),
            N::BinaryOp::F64Mul => self.out.push(F64Mul),
            N::BinaryOp::F64Div => self.out.push(F64Div),
            N::BinaryOp::F64LT => self.out.push(F64Lt),
            N::BinaryOp::F64Eq => self.out.push(F64Eq),
        }
    }
    fn translate_unop(&mut self, op: &N::UnaryOp) {
        match op {
            N::UnaryOp::Sqrt => self.out.push(F64Sqrt),
            N::UnaryOp::Neg => self.out.push(F64Neg),
        }
    }

    fn translate_expr(&mut self, expr: &mut N::Expr) {
        match expr {
            N::Expr::Atom(atom) => self.translate_atom(atom),
            N::Expr::HT => self.rt_call("ht_new"),
            N::Expr::Array => self.rt_call("array_new"),
            N::Expr::ArraySet(arr, index, value) => {
                self.translate_atom(arr);
                self.translate_atom(index);
                self.translate_atom(value);
                self.rt_call("array_set");
            }
            N::Expr::HTSet(ht, field, val) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.translate_atom(val);
                self.rt_call("ht_set");
            }
            N::Expr::ObjectSet(obj, field, val) => {
                self.translate_atom(obj);
                self.translate_atom(field);
                self.translate_atom(val);
                self.data_cache();
                self.rt_call("object_set");
            }
            N::Expr::ObjectEmpty => {
                self.rt_call("object_empty");
            }
            N::Expr::Push(array, val) => {
                self.translate_atom(array);
                self.translate_atom(val);
                self.rt_call("array_push");
            }
            N::Expr::PrimCall(rts_func, args) => {
                for arg in args {
                    self.translate_atom(arg);
                }
                self.rt_call(rts_func.name());
            }
            N::Expr::Call(f, args) => {
                for arg in args {
                    self.get_id(arg);
                }
                match self.id_env.get(f) {
                    Some(IdIndex::Fun(i)) => {
                        // this one's a little weird. we index in notwasm
                        // by 0 = first user function. but wasm indexes by 0 =
                        // first rt function. se we have to offset. but only
                        // on direct calls, because our function table takes
                        // care of it on indirect calls
                        self.out.push(Call(i + self.rt_indexes.len() as u32));
                    }
                    Some(IdIndex::Local(i, t)) => {
                        self.out.push(GetLocal(*i));
                        let (params_tys, ret_ty) = match t {
                            N::Type::Fn(fn_ty) => {
                                (types_as_wasm(&fn_ty.args), option_as_wasm(&fn_ty.result))
                            }
                            _ => panic!("identifier {:?} is not function-typed", f),
                        };
                        let ty_index = self
                            .type_indexes
                            .get(&(params_tys, ret_ty))
                            .expect("function type was not indexed");
                        self.out.push(CallIndirect(*ty_index, 0));
                    }
                    _ => panic!("expected Func ID"),
                };
            }
            N::Expr::ToString(a) => {
                self.translate_atom(a);
                self.rt_call("string_from_ptr");
            }
            N::Expr::NewRef(a) => {
                self.translate_atom(a);
                self.rt_call("ref_new");
            }
        }
    }

    fn translate_atom(&mut self, atom: &mut N::Atom) {
        match atom {
            N::Atom::Deref(a) => {
                // dereferences are implemented as raw memory reads
                self.translate_atom(a);
                self.out.push(I32Load(0, 4)); // skip 32 bits for tag in I32Ptr
            }
            N::Atom::Lit(lit) => match lit {
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
            N::Atom::Id(id) => self.get_id(id),
            N::Atom::ToAny(to_any) => {
                self.translate_atom(&mut to_any.atom);
                match to_any.ty() {
                    N::Type::I32 => self.rt_call("any_from_i32"),
                    N::Type::Bool => self.rt_call("any_from_bool"),
                    N::Type::F64 => self.rt_call("f64_to_any"),
                    _ => self.rt_call("any_from_ptr"),
                }
            }
            N::Atom::FromAny(a, ty) => {
                self.translate_atom(a);
                match ty {
                    N::Type::I32 => self.rt_call("any_to_i32"),
                    N::Type::Bool => self.rt_call("any_to_bool"),
                    N::Type::F64 => self.rt_call("any_to_f64"),
                    _ => self.rt_call("any_to_ptr"),
                }
            }
            N::Atom::FloatToInt(a) => {
                self.translate_atom(a);
                self.out.push(I32TruncSF64);
            }
            N::Atom::IntToFloat(a) => {
                self.translate_atom(a);
                self.out.push(F64ConvertSI32);
            }
            N::Atom::HTGet(ht, field) => {
                self.translate_atom(ht);
                self.translate_atom(field);
                self.rt_call("ht_get");
            }
            N::Atom::ObjectGet(obj, field) => {
                self.translate_atom(obj);
                self.translate_atom(field);
                self.data_cache();
                self.rt_call("object_get");
            }
            N::Atom::Index(arr, index) => {
                self.translate_atom(arr);
                self.translate_atom(index);
                self.rt_call("array_index");
            }
            N::Atom::ArrayLen(array) => {
                self.translate_atom(array);
                self.rt_call("array_len");
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
            N::Atom::Unary(op, a) => {
                self.translate_atom(a);
                self.translate_unop(op);
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

    fn get_id(&mut self, id: &N::Id) {
        match self
            .id_env
            .get(id)
            .expect(&format!("unbound identifier {:?}", id))
        {
            IdIndex::Local(n, _) => self.out.push(GetLocal(*n)),
            // +1 for JNKS_STRINGS
            IdIndex::Global(n) => self.out.push(GetGlobal(*n + 1)),
            IdIndex::Fun(n) => self.out.push(I32Const(*n as i32)),
        }
    }

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
            StrRef => ValueType::I32,
            HT => ValueType::I32,
            Array => ValueType::I32,
            DynObject => ValueType::I32,
            Fn(..) => ValueType::I32,
            Ref(..) => ValueType::I32,
        }
    }
}
