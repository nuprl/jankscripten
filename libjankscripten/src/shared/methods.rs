use crate::{shared::Type, typ};
use lazy_static::lazy_static;
use std::collections::HashMap;

macro_rules! entry {
    ($name:ident, $($args:tt -> $ret:ident),+) => {
        {
            let tys = vec![$(typ!(fun $args -> $ret)),+];
            ((stringify!($name), tys[0].unwrap_fun().0.len()), tys)
        }
    };
}

fn methods_table() -> HashMap<(&'static str, usize), Vec<Type>> {
    [
        // NOTE(luna): Length isn't a method, it's very special because it
        // isn't called. We actually have our very own construct for it in
        // each(!) of our languages
        entry!(slice, (string, int, int) -> string, (array, int, int) -> array),
        //entry!(at, (string, int) -> any, (string, int) -> string),
        entry!(concat, (array, array) -> array, (string, string) -> string),
        entry!(push, (array, any) -> int),
        entry!(charAt, (string, int) -> string),
        // Source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
        // Array.prototype[@@unscopables] // ??
        // Array.prototype[@@iterator]() // ??
        // get Array[@@species] // ??
        //"copyWithin",
        //"entries",
        //"every",
        //"fill",
        //"filter",
        //"find",
        //"findIndex",
        //"flat",
        //"flatMap",
        //"forEach",
        //"from", // Array.from, not prototype
        //"includes",
        //"indexOf",
        //"isArray", // Array.isArray
        //"join",
        //"keys",
        //"lastIndexOf",
        //"map",
        //"of", // Array.of
        //"pop",
        //"reduce",
        //"reduceRight",
        //"reverse",
        //"shift",
        //"slice",
        //"some",
        //"sort",
        //"splice",
        //"toLocaleString",
        //"toSource",
        //"toString",
        //"unshift",
        //"values",
    ]
    .iter()
    .cloned()
    .collect()
}

pub fn get_type_by_prefix(method: &str, arity: usize, prefix: &Type) -> Type {
    match prefix {
        Type::DynObject | Type::Any => typ!(fun_vec(vec![Type::Any; arity]) -> any),
        _ => METHODS_TABLE
            .get(&(method, arity))
            .unwrap()
            .iter()
            .filter(|t| &t.unwrap_fun().0[0] == prefix)
            .next()
            .unwrap()
            .clone(),
    }
}

lazy_static! {
    pub static ref METHODS_TABLE: HashMap<(&'static str, usize), Vec<Type>> = methods_table();
}
