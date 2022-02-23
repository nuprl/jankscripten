use super::syntax::*;
use crate::notwasm::syntax as notwasm;
use crate::rts_function::RTSFunction;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::collections::HashSet;

#[macro_export]
macro_rules! typ {
    (int) => (Type::Int);
    (float) => (Type::Float);
    (bool) => (Type::Bool);
    (string) => (Type::String);
    (any) => (Type::Any);
    (dynobject) => (Type::DynObject);
    (array) => (Type::Array);
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
    UnOp(notwasm::UnaryOp),
    RTS(RTSFunction),
    Metavar(usize),
    Missing,
}

#[derive(Default, Debug)]
struct Overload {
    overloads: Vec<(Type, NotwasmOp)>,
    on_other_args: Option<(Type, NotwasmOp)>,
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
            NotwasmOp::UnOp(notwasm_op) => {
                let e = args.pop().unwrap();
                assert_eq!(args.len(), 0);
                Expr::Unary(notwasm_op.clone(), Box::new(e), p)
            }
            NotwasmOp::RTS(rts_fun) => Expr::PrimCall(rts_fun.clone(), args, p),
            NotwasmOp::Missing | NotwasmOp::Metavar(..) => panic!("received {:?}", self),
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

impl From<notwasm::UnaryOp> for NotwasmOp {
    fn from(op: notwasm::UnaryOp) -> NotwasmOp {
        NotwasmOp::UnOp(op)
    }
}

impl From<RTSFunction> for NotwasmOp {
    fn from(f: RTSFunction) -> NotwasmOp {
        NotwasmOp::RTS(f)
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
        overload.overloads.push((typ, notwasm.into()));
    }

    fn add_coercible(&mut self, op: impl Into<JsOp>, typ: Type, notwasm: impl Into<NotwasmOp>) {
        let overload = self.table.entry(op.into()).or_insert(Overload::default());
        overload.on_other_args = Some((typ, notwasm.into()));
    }

    pub fn overloads<'a, 'b>(
        &'a self,
        op: &'b JsOp,
    ) -> impl Iterator<Item = &'a (Type, NotwasmOp)> {
        self.table
            .get(op)
            .expect(&format!("no overloads found for {:?}", op))
            .overloads
            .iter()
    }

    pub fn coercible<'a, 'b>(&'a self, op: &'b JsOp) -> Option<&'a (Type, NotwasmOp)> {
        self.table.get(op).unwrap().on_other_args.as_ref()
    }
}

lazy_static! {
    /// NOTE: Only ground types should be arguments to the provided type! ie,
    /// fun(int, int) -> int is allowed but fun(fun(int) -> int) -> int is NOT.
    /// This shouldn't be an issue, since operators should accept ground types
    /// anyway
    pub static ref OVERLOADS: OverloadTable = {
        use crate::javascript::syntax::BinaryOp::*;
        use crate::javascript::syntax::UnaryOp as JUO;
        use notwasm::BinaryOp::*;
        let mut table = OverloadTable::default();
        table.add(Plus, typ!(fun(int, int) -> int), I32Add);
        table.add(Plus, typ!(fun(string, string) -> string), RTSFunction::StringCat);
        table.add(Plus, typ!(fun(any, any) -> any), RTSFunction::Plus);


        table.add(Minus, typ!(fun(float, float) -> float), F64Sub);
        table.add(Minus, typ!(fun(int, int) -> int), I32Sub);
        table.add(Minus, typ!(fun(any, any) -> any), RTSFunction::Minus);

        table.add(Times, typ!(fun(float, float) -> float), F64Mul);
        table.add(Times, typ!(fun(int, int) -> int), I32Mul);
        table.add(Times, typ!(fun(any, any) -> any), RTSFunction::Times);
        table.add(Over, typ!(fun(float, float) -> float), F64Div);
        table.add(Over, typ!(fun(any, any) -> float), RTSFunction::Over);
        table.add(Mod, typ!(fun(int, int) -> int), I32Rem);
        table.add(Mod, typ!(fun(float, float) -> float), RTSFunction::ModF64);
        table.add(Mod, typ!(fun(any, any) -> any), RTSFunction::Mod);
        table.add(Equal, typ!(fun(int, int) -> bool), I32Eq);
        table.add(Equal, typ!(fun(float, float) -> bool), F64Eq);
        table.add(Equal, typ!(fun(any, any) -> bool), RTSFunction::Equal);
        table.add(NotEqual, typ!(fun(int, int) -> bool), I32Ne);
        table.add(NotEqual, typ!(fun(float, float) -> bool), F64Ne);
        table.add(NotEqual, typ!(fun(any, any) -> bool), RTSFunction::NotEqual);
        table.add(StrictEqual, typ!(fun(int, int) -> bool), I32Eq);
        table.add(StrictEqual, typ!(fun(float, float) -> bool), F64Eq);
        table.add(StrictEqual, typ!(fun(any, any) -> bool), RTSFunction::StrictEqual);
        table.add(StrictNotEqual, typ!(fun(int, int) -> bool), I32Ne);
        table.add(StrictNotEqual, typ!(fun(float, float) -> bool), F64Ne);
        table.add(StrictNotEqual, typ!(fun(any, any) -> bool), RTSFunction::StrictNotEqual);
        table.add(LessThan, typ!(fun(int, int) -> bool), I32LT);
        // It's always safe to coerce to float because < only operates on
        // numbers and all numbers can be represented as floats. TODO(luna): This
        // isn't actually true: string ordering is a thing. However, this hack
        // brings us to parity with the old coercion insertion
        table.add_coercible(LessThan, typ!(fun(float, float) -> bool), F64LT);
        table.add(LessThanEqual, typ!(fun(int, int) -> bool), I32Le);
        table.add_coercible(LessThanEqual, typ!(fun(float, float) -> bool), F64Le);
        table.add(GreaterThan, typ!(fun(int, int) -> bool), I32GT);
        table.add_coercible(GreaterThan, typ!(fun(float, float) -> bool), F64GT);
        table.add(GreaterThanEqual, typ!(fun(int, int) -> bool), I32Ge);
        table.add_coercible(GreaterThanEqual, typ!(fun(float, float) -> bool), F64Ge);
        table.add_coercible(LeftShift, typ!(fun(int, int) -> int), I32Shl);
        table.add_coercible(RightShift, typ!(fun(int, int) -> int), I32Shr);
        table.add_coercible(UnsignedRightShift, typ!(fun(int, int) -> int), I32ShrU);
        table.add_coercible(Or, typ!(fun(int, int) -> int), I32Or);
        table.add_coercible(And, typ!(fun(int, int) -> int), I32And);
        table.add_coercible(XOr, typ!(fun(int, int) -> int), I32Xor);
        // For the operations that are strictly number operations, we have int
        // and float, but there should be a better way to deal with the any-case
        // (should be float)
        table.add_coercible(InstanceOf, typ!(fun(any, any) -> bool), RTSFunction::InstanceOf);
        table.add_coercible(In, typ!(fun(any, any) -> bool), RTSFunction::In);

        table.add(JUO::Minus, typ!(fun(int) -> int), UnaryOp::I32Neg);
        table.add(JUO::Minus, typ!(fun(float) -> float), UnaryOp::F64Neg);
        table.add(JUO::Minus, typ!(fun(any) -> any), RTSFunction::Neg);
        table.add(JUO::TypeOf, typ!(fun(any) -> string), RTSFunction::Typeof);
        table.add(JUO::Void, typ!(fun(any) -> any), RTSFunction::Void);
        table.add(JUO::Delete, typ!(fun(any) -> any), RTSFunction::Delete);

        table.add_coercible(JUO::Not, typ!(fun(bool) -> bool), UnaryOp::Eqz);
        table.add_coercible(JUO::Tilde, typ!(fun(int) -> int), UnaryOp::I32Not);

        table
    };
}
