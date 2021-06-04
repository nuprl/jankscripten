use super::syntax::*;
use crate::rts_function::RTSFunction;
use crate::notwasm::{syntax as notwasm};
use std::collections::HashMap;
use lazy_static::lazy_static;

macro_rules! typ {
    (int) => (Type::Int);
    (bool) => (Type::Bool);
    (string) => (Type::String);
    (any) => (Type::Any);
    (fun($( $arg:tt ),*) -> $ret:tt) =>
        (Type::Function(vec![ $( typ!($arg) ),* ], Box::new(typ!($ret))))
}

#[derive(Default)]
pub struct OverloadTable {
    table: HashMap<JsOp, Overload>
}

impl OverloadTable {

    fn add(
        &mut self, 
        op: impl Into<JsOp>, 
        typ: Type,
        notwasm: impl Into<NotwasmOp>) {
        let overload = self.table.entry(op.into()).or_insert(Overload::default());
        overload.overloads.push((typ, notwasm.into()));
    }

    fn add_on_any(&mut self, op: impl Into<JsOp>, typ: Type,
        notwasm: impl Into<NotwasmOp>) {
        let overload = self.table.entry(op.into()).or_insert(Overload::default());
        overload.on_other_args = Some((typ, notwasm.into()));
            
    }

    pub fn overloads<'a, 'b>(&'a self, op: &'b JsOp) -> impl Iterator<Item = &'a Type> {
        self.table.get(op).unwrap().overloads.iter().map(|(t, _)| t)
    }

    pub fn on_any<'a, 'b>(&'a self, op: &'b JsOp) -> Option<&'a Type> {
        self.table.get(op).unwrap().on_other_args.as_ref().map(|(t, _)| t)
    }

    pub fn target(&self, op: &JsOp, arg_typs: &[Type]) -> Option<&NotwasmOp> {
        self.table.get(op).unwrap().overloads.iter().find(|(typ, _)| {
            let (fun_args, _) = typ.unwrap_fun();
            arg_typs.eq(fun_args)
        }).map(|x| &x.1)
    }

}

pub enum NotwasmOp {
    BinOp(notwasm::BinaryOp),
    RTS(RTSFunction),
}

impl From<notwasm::BinaryOp> for NotwasmOp {
    fn from(op: notwasm::BinaryOp) -> NotwasmOp {
        NotwasmOp::BinOp(op)
    }
}

impl From<RTSFunction> for NotwasmOp {
    fn from (f: RTSFunction) -> NotwasmOp {
        NotwasmOp::RTS(f)
    }
}

#[derive(Default)]
struct Overload {
    overloads: Vec<(Type, NotwasmOp)>,
    on_other_args: Option<(Type, NotwasmOp)>,
}

lazy_static! {
    pub static ref OVERLOADS: OverloadTable = {
        use crate::javascript::syntax::BinaryOp::*;
        use notwasm::BinaryOp::*;
        let mut table = OverloadTable::default();
        table.add(Plus, typ!(fun(int, int) -> int), I32Add);
        // TODO(arjun): Why not string cat?
        table.add(Plus, typ!(fun(string, string) -> any), RTSFunction::Plus);
        table.add_on_any(Plus, typ!(any), RTSFunction::Plus);

        table.add(LeftShift, typ!(fun(int, int) -> int), I32Shl);
        // ]).others(typ!(int)),

      table
    };
}

