use lazy_static::lazy_static;
use std::collections::HashSet;

fn array_prototype() -> HashSet<&'static str> {
    [
        // Source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
        // Array.prototype[@@unscopables] // ??
        // length is actually special and not really part of the prototype but
        // for the purpose of type inference it should be here
        "length",
        // Array.prototype[@@iterator]() // ??
        // get Array[@@species] // ??
        "at",
        "concat",
        "copyWithin",
        "entries",
        "every",
        "fill",
        "filter",
        "find",
        "findIndex",
        "flat",
        "flatMap",
        "forEach",
        "from", // Array.from, not prototype
        "includes",
        "indexOf",
        "isArray", // Array.isArray
        "join",
        "keys",
        "lastIndexOf",
        "map",
        "of", // Array.of
        "pop",
        "push",
        "reduce",
        "reduceRight",
        "reverse",
        "shift",
        "slice",
        "some",
        "sort",
        "splice",
        "toLocaleString",
        "toSource",
        "toString",
        "unshift",
        "values",
    ]
    .iter()
    .cloned()
    .collect()
}

lazy_static! {
    pub static ref ARRAY_PROTOTYPE: HashSet<&'static str> = array_prototype();
}
