//! The abstract syntax for NotWasm, which reflects the wasm representation
//! with garbage collected structures, expressions, and statements
//!
//! From the design document:
//! NotWasm is a lower-level IR than JankyScript, but higher-level than
//!   Wasm. It has the following features:
//! - First-order functions
//! - Garbage-collected data structures, including classes, arrays, and
//!   hash tables
//! - An environment data structure that can store addresses of local
//!   variables.
//! - All local variables are annotated with a flag that determines if they
//!   can be stored in an environment.
//! - Unlike WebAssembly, NotWasm does not have an operand stack. Instead,
//!   it has compound expressions. We can easily introduce the operand stack later.
//! - What is the type system?
//!   - T ::= i32 | f64 | num | C | ht | bool | DynObject | (T1, … Tn) -> T
//! A Program is:
//! - A set of classes (just fields, not methods, all public)
//! - A set of functions
//! - A set of global variables that are initialized to …whataever Wasm
//!   supports

use crate::pos::Pos;
use crate::rts_function::RTSFunction;
pub use crate::shared::Id;
use std::collections::HashMap;

/// The types of NotWasm. Every value has a unique type, thus we *do not* support
/// subtyping. The comment for each variant describes the shape of the value
/// with the associated type. Note we refer to several types defined in the
/// runtime system (e.g., `TypeTag`, `Tag`, and `AnyEnum`).
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    /// If `v : Any` then `v` is an `AnyEnum`.
    Any,
    /// If `v : I32` then `v` is an `i32`.
    I32,
    /// If `v : F64`, then `v` is an `f64`.
    F64,
    /// If `v : Bool` then `v` is an `i32` that is either `1` or `0`.
    Bool,
    /// If `v : String` then `v` is a `*const Tag` followed by a 4-byte
    /// little-endian length followed by utf-8 of that length.
    /// Even interned strings are preceded by a tag, so that interned and
    /// uninterned strings have the same representation.
    String,
    /// If `v : HT` then `v` is a `*const Tag`, where `v.type_tag == Array`.
    Array,
    /// If `v : DynObject` then `v` is a `*const Tag` where
    /// `v.type_tag == Class`.
    /// TODO(arjun): We do not have a type_tag called class. What is this
    /// really supposed to be? I think it is ObjectPtrPtr.
    DynObject,
    /// If `v : Fn(fn_type)` then `v` is an `i32`, which is an index of a
    /// function with the type `fn_type`.
    Fn(FnType),

    /// If `v : HT` then `v` is a `*const Tag`, where
    ///  `v.type_tag === TypeTag::HT`.
    HT,
    /// If `v : Closure(fn_type)` then `v` is an `i64`, which is an EnvPtr
    /// followed by a 16-bit truncation of a function pointer followed by 16
    /// garbage bits
    Closure(FnType),
    /// If `v : Ref(I32)` then `v` is a `*const Tag` and `v.type_tag == NonPtr32`.
    /// If `v : Ref(Bool)` then `v` is a `*const Tag` and `v.type_tag == NonPtr32`.
    /// If `v : Ref(F64)` then `v` is a `*const Tag` and resides in the f64 heap
    /// If `v : Ref(Any)` then `v` is a `*const Tag` and `v.type_tag == Any`.
    /// If `v : Ref(T)` and T is represented as a `*const Tag`, then `v` is a
    /// `*const Tag` `v.type_tag == Ptr`.
    Ref(Box<Type>),
    /// If `v : Env` then `v` is a `*const Tag` and `v.type_tag == Env`.
    /// Envs are not values, nor are they really types!!! An env actually has
    /// an existential type associated with each closure, but we simply say that
    /// all envs are "equal enough" for the code generation we do after closure
    /// conversion
    Env,
}

impl Type {

    pub fn unwrap_fun(&self) -> (&Vec<Type>, Option<&Type>) {
        match self {
            Type::Fn(fn_type) => (&fn_type.args, match &fn_type.result {
                None => None,
                Some(ret) => Some(& *ret)
            }),
            _ => panic!("unwrap_fun: unexpected type: {}", self),
        }
    }    
    pub fn is_gc_root(&self) -> bool {
        match self {
            Type::I32 => false,
            Type::F64 => false,
            Type::String => true,
            Type::HT => true,
            Type::Array => true,
            Type::Bool => false,
            Type::DynObject => true,
            Type::Fn(_) => false,
            Type::Closure(_) => true,
            Type::Ref(_) => true,
            Type::Any => true,
            // uhhh i don't think there's a way for there to be a live env when
            // there's not a live closure? so this could probably become false?
            Type::Env => true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnType {
    pub args: Vec<Type>,
    pub result: Option<Box<Type>>,
}

/// Binary operators that correspond to primitive WebAssembly instructions.
/// Other than `PtrEq`, none of these operators can be applied to `Any`-typed
/// values.
///
/// Note that all I32s are signed in jankyscript
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    PtrEq,
    I32Eq,
    I32Ne,
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Rem,
    I32GT,
    I32LT,
    I32Ge,
    I32Le,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32Shr,
    I32ShrU,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Eq,
    F64Ne,
    F64LT,
    F64Le,
    F64GT,
    F64Ge,
}

impl BinaryOp {
    pub fn notwasm_typ(self: &BinaryOp) -> (Type, Type) {
        // these binops are used in jankyscript too, so we can derive
        // their notwasm types from them
        let (janky_in_type, janky_out_type) = self.janky_typ();
        (janky_in_type.notwasm_typ(), janky_out_type.notwasm_typ())
    }
}

/// Unary operators that correspond to primitive WebAssembly instructions.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Sqrt,
    Neg,
    Eqz,
}

impl UnaryOp {
    pub fn notwasm_typ(self: &UnaryOp) -> (Type, Type) {
        // these unaryops are used in jankyscript too, so we can derive
        // their notwasm types from them
        let (janky_in_type, janky_out_type) = self.janky_typ();
        (janky_in_type.notwasm_typ(), janky_out_type.notwasm_typ())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Label {
    Named(String),
    /// in GotoWasm applications are labeled, but they shouldn't make it out
    App(i32),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    F64(f64),
    String(String),
    Interned(u32),
    Undefined,
    Null,
}

impl Lit {
    pub fn notwasm_typ(self: &Lit) -> Type {
        match self {
            Lit::Bool(_) => Type::Bool,
            Lit::I32(_) => Type::I32,
            Lit::F64(_) => Type::F64,
            Lit::String(_) => Type::String,
            Lit::Interned(_) => Type::String,
            Lit::Undefined => Type::Any,
            Lit::Null => Type::Any,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ToAny {
    pub atom: Box<Atom>,
    pub ty: Option<Type>,
}

impl ToAny {
    pub fn new(atom: Atom) -> ToAny {
        return ToAny {
            atom: Box::new(atom),
            ty: None,
        };
    }

    pub fn set_ty(&mut self, ty: Type) {
        assert!(self.ty.is_none(), "called set_typ twice on ToAny");
        self.ty = Some(ty);
    }

    pub fn ty(&self) -> &Type {
        self.ty.as_ref().expect("type not set for ToAny")
    }
}

/// An `Atom` is an expression that is guaranteed to not require garbage
/// collection. The code generated by an `Atom` may call a function in the
/// runtime system, as long as we can guarantee that that function will not
/// trigger garbage collection.
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Lit(Lit, Pos),
    /// A primtive applciation that does not have any non-trivial interaction with the garbage 
    /// collector.
    PrimApp(Id, Vec<Atom>, Pos),    
    ToAny(ToAny, Pos),
    /// `FromAny(atom, ty, Pos)`
    ///
    /// Concrete syntax: `<atom> as <ty>`
    FromAny(Box<Atom>, Type, Pos),
    FloatToInt(Box<Atom>, Pos), // MMG made these Atoms because they shouldn't ever allocate
    IntToFloat(Box<Atom>, Pos),
    ObjectGet(Box<Atom>, Box<Atom>, Pos),
    Id(Id, Pos),
    GetPrimFunc(Id, Pos),
    Unary(UnaryOp, Box<Atom>, Pos),
    Binary(BinaryOp, Box<Atom>, Box<Atom>, Pos),
    Deref(Box<Atom>, Type, Pos),
    /// get the given value from the environment at local 0
    EnvGet(u32, Type, Pos),
}

impl Atom {
    // Every Atom must have a position. Do not create a bogus position here.
    pub fn pos(&self) -> &Pos {
        match self {
            Atom::Lit(_, p) => p,
            Atom::PrimApp(_, _, p) => p,
            Atom::ToAny(_, p) => p,
            Atom::FromAny(_, _, p) => p,
            Atom::FloatToInt(_, p) => p,
            Atom::IntToFloat(_, p) => p,
            Atom::ObjectGet(_, _, p) => p,
            Atom::Id(_, p) => p,
            Atom::GetPrimFunc(_, p) => p,
            Atom::Unary(_, _, p) => p,
            Atom::Binary(_, _, _, p) => p,
            Atom::Deref(_, _, p) => p,
            Atom::EnvGet(_, _, p) => p,
        }
    }
}

// An `Expr` is an expression that may trigger garbage collection.
#[derive(Debug, PartialEq)]
pub enum Expr {
    HT,
    // TODO: Give Array initial capacity
    Array,
    Push(Atom, Atom, Pos),
    /// TODO(luna, Pos): we need to detect out-of-bounds and turn into a hashmap
    ArraySet(Atom, Atom, Atom, Pos),
    HTSet(Atom, Atom, Atom, Pos),
    /// right now, never constructed from jankyscript, only in tests
    Call(Id, Vec<Id>, Pos),
    ClosureCall(Id, Vec<Id>, Pos),
    PrimCall(RTSFunction, Vec<Atom>, Pos),
    ObjectEmpty,
    /// ObjectSet(obj, field_name, value, typ, Pos) is obj.field_name: typ = value;
    ObjectSet(Atom, Atom, Atom, Pos),
    NewRef(Atom, Type, Pos), // newRef(something, Pos)
    Atom(Atom, Pos),
    /// create a new environment with the given atoms and their types,
    /// in linear order. then create a closure with that environment and
    /// the index of the function named .0
    ///
    /// this has to be atom, not id, because what if we need to store {x:
    /// env.x} in a nested closure
    Closure(Id, Vec<(Atom, Type)>, Pos),
}

#[derive(Debug, PartialEq)]
pub struct VarStmt {
    pub id: Id,
    pub named: Expr,
    pub ty: Option<Type>,
}

impl VarStmt {
    pub fn new(id: Id, named: Expr) -> VarStmt {
        VarStmt {
            id,
            named,
            ty: None,
        }
    }

    pub fn set_ty(&mut self, ty: Type) {
        assert!(self.ty.is_none(), "called set_typ twice on VarStmt");
        self.ty = Some(ty);
    }

    pub fn ty(&self) -> &Type {
        self.ty.as_ref().expect("type not set for VarStmt")
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty,
    /// Concrete syntax: `var <id> = <named>;`
    /// The type-checker will fill in the type of the variable
    Var(VarStmt, Pos),
    Expression(Expr, Pos),
    // TODO(arjun, Pos): An Assign could probably be an Atom
    Assign(Id, Expr, Pos),
    Store(Id, Expr, Pos), // *ref = expr
    If(Atom, Box<Stmt>, Box<Stmt>, Pos),
    Loop(Box<Stmt>, Pos),
    Label(Label, Box<Stmt>, Pos),
    Break(Label, Pos),
    // Break value as return?
    Return(Atom, Pos),
    Block(Vec<Stmt>, Pos),
    Trap,
    /// these don't exist in NotWasm, only GotoWasm. if you try to [translate]
    /// a goto, it will panic
    Goto(Label, Pos),
}

#[derive(Debug, PartialEq)]
pub struct Global {
    pub is_mut: bool,
    pub ty: Type,
    /// restricted to a const expression.
    /// also, parity_wasm restricts it to one instruction (this is not a
    /// wasm restriction and could theoretically be fixed).
    /// The atom is optional to allow for lazily-initialized globals in the
    /// runtime. If you don't specify a value for the global, it will be 0.
    /// Write to a lazy global before ever reading from it.
    pub atom: Option<Atom>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub body: Stmt,
    pub fn_type: FnType,
    pub params: Vec<Id>,
    pub span: Pos,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub rts_fn_imports: HashMap<String, Type>,
    pub functions: HashMap<Id, Function>,
    /// Atom must be const as defined by wasm
    pub globals: HashMap<Id, Global>,
    /// no need to initialize, populated by intern
    pub data: Vec<u8>,
}

impl Program {
    pub fn merge_in(&mut self, other: Program) {
        self.rts_fn_imports.extend(other.rts_fn_imports.into_iter());
        self.functions.extend(other.functions.into_iter());
        self.globals.extend(other.globals.into_iter());
        assert_eq!(other.data.len(), 0, "can't merge data segments");
    }
}

impl FnType {
    pub fn to_type(self) -> Type {
        Type::Fn(self)
    }
}

impl<S: Into<String>> From<S> for Label {
    fn from(s: S) -> Self {
        Self::Named(s.into())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        write!(
            f,
            "{}",
            match self {
                I32 => "i32",
                F64 => "f64",
                String => "string",
                HT => "ht",
                Array => "array",
                Ref(..) => "ref",
                Bool => "bool",
                DynObject => "DynObject",
                Fn(..) => "fn",
                Closure(..) => "closure",
                Any => "any",
                Env => "env",
            }
        )
    }
}
