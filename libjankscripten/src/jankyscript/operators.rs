use super::syntax::*;
use crate::notwasm::syntax as notwasm;
use crate::rts_function::RTSFunction;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::collections::HashSet;

#[macro_export]
macro_rules! z3f {
    ($me:ident, (unquote $($t:tt)*)) => ($($t)*);
    ($me:ident, (tid $($t:tt)*)) => ($me.t(&$($t)*));
    ($me:ident, (typ $($t:tt)*)) => ($me.t(&typ!($($t)*)));
    ($me:ident, (and $(($($t1:tt)*))*)) =>
        ($me.zand(vec![ $(z3f!($me, ($($t1)*))),* ]));
    // ($me:ident, (and ($($t1:tt)*) ($($t2:tt)*))) =>
    //     (z3f!($me, ($($t1)*)) & &z3f!($me, ($($t2)*)));

    ($me:ident, (or ($($t1:tt)*) ($($t2:tt)*))) =>
        (z3f!($me, ($($t1)*)) | &z3f!($me, ($($t2)*)));

    ($me:ident, (id $($t:tt)*)) => ($($t)*);
    ($me:ident, (not ($($t1:tt)*))) =>
    (!z3f!($me, ($($t1)*)));

    ($me:ident, (= ($($t1:tt)*) ($($t2:tt)*))) =>
        (z3f!($me, ($($t1)*))._eq(&z3f!($me, ($($t2)*))));
}

#[macro_export]
macro_rules! typ {
    (int) => (Type::Int);
    (float) => (Type::Float);
    (bool) => (Type::Bool);
    (string) => (Type::String);
    (any) => (Type::Any);
    (fun($( $arg:tt ),*) -> $ret:tt) =>
        (Type::Function(vec![ $( typ!($arg) ),* ], Box::new(typ!($ret))));
    (fun_vec($($args:tt)*) -> $($ret:tt)*) =>
        (Type::Function($($args)*, Box::new(typ!($($ret)*))));
    (unquote ( $($x:tt)* ) ) => ($($x)*);
}

/// A NotWasm operator: either a primitive Wasm operator, or a call to a function in the runtime
/// system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NotwasmOp {
    BinOp(notwasm::BinaryOp),
    RTS(RTSFunction),
    Metavar(usize),
    Missing,
}

/// The type scheme (in the Standard ML sense) of an operator. We require type schemes to support
/// polymorphic operators, such as equality.
#[derive(Default, Debug)]
pub struct TypeScheme {
    pub vars: Vec<Id>,
    pub typ: Type
}

#[derive(Default, Debug)]
struct Overload {
    overloads: Vec<(TypeScheme, NotwasmOp)>,
    on_other_args: Option<(TypeScheme, NotwasmOp)>,
}

#[derive(Debug, Default)]
pub struct OverloadTable {
    table: HashMap<JsOp, Overload>,
}


impl NotwasmOp {
    pub fn make_app(&self, mut args: Vec<Expr>, p: crate::pos::Pos) -> Expr {
        match self {
            NotwasmOp::BinOp(notwasm_op) => {
                let e2 = args.pop().unwrap();
                let e1 = args.pop().unwrap();
                assert_eq!(args.len(), 0);
                Expr::Binary(notwasm_op.clone(), Box::new(e1), Box::new(e2), p)
            }
            NotwasmOp::RTS(rts_fun) => Expr::PrimCall(rts_fun.clone(), args, p),
            NotwasmOp::Missing | NotwasmOp::Metavar(..) => panic!("received {:?}", self)
        }
    }
}

impl Default for NotwasmOp {
    /// The default is `NotwasmOp::Missing`
    fn default() -> Self {
        NotwasmOp::Missing
    }
}

impl From<notwasm::BinaryOp> for NotwasmOp {
    fn from(op: notwasm::BinaryOp) -> NotwasmOp {
        NotwasmOp::BinOp(op)
    }
}

impl From<RTSFunction> for NotwasmOp {
    fn from(f: RTSFunction) -> NotwasmOp {
        NotwasmOp::RTS(f)
    }
}

impl TypeScheme {
    // A new type scheme with no type variables
    pub fn monotype(typ: Type) -> Self {
        TypeScheme { vars: vec![], typ }
    }

    pub fn instantiate<'a, 'b>(&'b self) -> Type {
        // Easy common case. Cloning is a bit silly.
        if self.vars.len() == 0 {
            return self.typ.clone();
        }
        
        return self.typ.clone();
    }
}

impl OverloadTable {

    /// The set of all operators defined in the table. Not clear if we really need this to be
    /// a set, if each overload maps to a unique NotWasmOp, which may be the case. However, no
    /// harm in building a set.
    pub fn all_ops(&self) -> HashSet<NotwasmOp> {
        let mut ops = HashSet::<NotwasmOp>::new();
        for overloads in self.table.values() {
            for (_, overload) in &overloads.overloads {
                ops.insert(overload.clone());
            }
            if let Some((_, op)) = &overloads.on_other_args {
                ops.insert(op.clone());
            }
        }
        return ops;
    }

    // fn to_z3(&self, cxt: &'a z3::Context)
    fn add(&mut self, op: impl Into<JsOp>, typ: Type, notwasm: impl Into<NotwasmOp>) {
        let overload = self.table.entry(op.into()).or_insert(Overload::default());
        overload.overloads.push((TypeScheme::monotype(typ), notwasm.into()));
    }

    fn add_on_any(&mut self, op: impl Into<JsOp>, typ: Type, notwasm: impl Into<NotwasmOp>) {
        let overload = self.table.entry(op.into()).or_insert(Overload::default());
        overload.on_other_args = Some((TypeScheme::monotype(typ), notwasm.into()));
    }

    pub fn overloads<'a, 'b>(&'a self, op: &'b JsOp) -> impl Iterator<Item=&'a(TypeScheme, NotwasmOp)> {
        self.table.get(op).expect(&format!("no overloads found for {:?}", op)).overloads.iter()
    }

    pub fn on_any<'a, 'b>(&'a self, op: &'b JsOp) -> Option<&'a(TypeScheme, NotwasmOp)> {
        self.table
            .get(op)
            .unwrap()
            .on_other_args
            .as_ref()
    }

    // /// Used to select an operator once argument types are known.
    // pub fn target(&self, op: &JsOp, arg_typs: &[Type]) -> Option<&NotwasmOp> {
    //     self.table
    //         .get(op)
    //         .unwrap()
    //         .overloads
    //         .iter()
    //         .find(|(typ, _)| {
    //             let (fun_args, _) = typ.unwrap_fun();
    //             arg_typs.eq(fun_args)
    //         })
    //         .map(|x| &x.1)
    // }

    // // When arguments do not match a known overload, used to call a generic version of the operator.
    // pub fn any_target(&self, op: &JsOp) -> &(Type, NotwasmOp) {
    //     self.table.get(op).unwrap().on_other_args.as_ref().unwrap()
    // }
}

lazy_static! {
    pub static ref OVERLOADS: OverloadTable = {
        use crate::javascript::syntax::BinaryOp::*;
        use notwasm::BinaryOp::*;
        let mut table = OverloadTable::default();
        table.add(Plus, typ!(fun(int, int) -> int), I32Add);
        // TODO(arjun): Why not string cat?
        // TODO(arjun): should this be int*int->int?
        table.add(Plus, typ!(fun(string, string) -> any), RTSFunction::Plus);
        table.add_on_any(Plus, typ!(fun(any, any) -> any), RTSFunction::Plus);


        table.add(Minus, typ!(fun(float, float) -> float), F64Sub);
        table.add(Minus, typ!(fun(int, int) -> int), I32Sub);
        table.add_on_any(Minus, typ!(fun(any, any) -> float), RTSFunction::Minus);

        table.add(LeftShift, typ!(fun(int, int) -> int), I32Shl);
        table.add(Times, typ!(fun(float, float) -> float), F64Mul); 
        table.add(Times, typ!(fun(int, int) -> int), I32Mul); 
        table.add_on_any(LeftShift, typ!(fun(int, int) -> int), I32Shl);
        table.add(StrictEqual, typ!(fun(any, any) -> bool), RTSFunction::StrictEqual);
        // ]).others(typ!(int)),

        table
    };
}
