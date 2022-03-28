use crate::{shared::Type, typ};
use lazy_static::lazy_static;
use std::collections::HashMap;

macro_rules! entry {
    ($name:ident, $($args:tt -> $ret:ident),+) => {
        (stringify!($name), vec![$(typ!(fun $args -> $ret)),+])
    };
}

fn methods_table() -> HashMap<&'static str, Vec<Type>> {
    [
        entry!(slice, (string, int, int) -> string, (array, int, int) -> array),
        entry!(at, (string, int) -> any, (string, int) -> string),
        entry!(concat, (array, array) -> array, (string, string) -> string),
        // Source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
        // Array.prototype[@@unscopables] // ??
        // length is actually special and not really part of the prototype but
        // for the purpose of type inference it should be here
        //"length",
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
        //"push",
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

lazy_static! {
    pub static ref METHODS_TABLE: HashMap<&'static str, Vec<Type>> = methods_table();
}
