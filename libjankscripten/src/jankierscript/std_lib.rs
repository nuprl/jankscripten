use crate::shared::types::Type;
use std::collections::HashMap;
use Type::*;

/// these get merged into a hashmap so no reason to hash twice
type BindMap = Vec<(std::string::String, Type)>;

/// TODO(luna): we should eventually give these actual types rather than all Any
pub fn get_global_object() -> BindMap {
    let mut map = BindMap::new();
    let m = &mut map;

    // i don't know where this would be documented but i know we need it
    insert(m, "arguments", Array);
    // TODO(luna): this is even more stopgap: this is dead code that refers
    // to a nonexistant function in the runtime of every dart program. see this
    // issue https://github.com/dart-lang/sdk/issues/33081. we should
    // preprocess this away
    insert(m, "existingIsolateProperties", Any);
    // again dart nonsense: dart is doing UA testing, using typeof on globals
    // like "version" that shouldn't exist. technically, this should be
    // satisfied by giving undefined on undeclared variables as JS does, but
    // i don't like the idea of that, so if i explicitly allow just a few
    // of these hopefully we will not have to
    insert(m, "version", Any);

    // https://developer.mozilla.org/en-US/docs/Web/API/Window
    // except i deteleted a bunch that are obviously never going to be used or
    // implemented (esp DOM / Web stuff)

    // The Window Object
    // -----------------
    // this is the window object itself, which should be populated with all
    // these i'm inserting. eventually we should be generating an actual window
    // object and looking inside it for these functions, but for now this is
    // a stopgap
    insert(m, "window", Any);
    // Properties
    insert(m, "console", Any);
    insert(m, "directories", Any);
    // i happen to know this is used by dart though it shouldn't be
    insert(m, "navigator", Any);
    insert(m, "performance", Any);
    // Methods
    insert(m, "clearTimeout", Any);
    insert(m, "close", Any);
    // pyjs
    insert(m, "confirm", Any);
    insert(m, "setTimeout", Any);
    insert(m, "stop", Any);

    // Built-in objects
    // ----------------
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects
    insert(m, "Array", Any);
    // scala
    insert(m, "ArrayBuffer", Any);
    // scala
    insert(m, "BigInt", Any);
    insert(m, "Boolean", Any);
    // maybe elm?
    insert(m, "DataView", Any);
    insert(m, "Date", Any);
    insert(m, "Error", Any);
    // ocaml
    insert(m, "EvalError", Any);
    insert(m, "Float32Array", Any);
    insert(m, "Float64Array", Any);
    insert(m, "Function", Any);
    // pyjs
    insert(m, "Generator", Any);
    insert(m, "Infinity", Any);
    insert(m, "Int16Array", Any);
    insert(m, "Int32Array", Any);
    insert(m, "Int8Array", Any);
    insert(m, "JSON", Any);
    insert(m, "Math", Any);
    insert(m, "NaN", Any);
    insert(m, "Number", Any);
    insert(m, "Object", Any);
    // maybe clojurescript?
    insert(m, "Promise", Any);
    insert(m, "RangeError", Any);
    insert(m, "ReferenceError", Any);
    insert(m, "RegExp", Any);
    insert(m, "String", Any);
    // clojurescript
    insert(m, "Symbol", Any);
    // ocaml
    insert(m, "SyntaxError", Any);
    insert(m, "TypeError", Any);
    // there's a scala comment about this but it doesn't look actually used:
    // "The underlying Array is a TypedArray"
    insert(m, "TypedArray", Any);
    // ocaml
    insert(m, "URIError", Any);
    // scala
    insert(m, "Uint16Array", Any);
    // dart
    insert(m, "Uint8Array", Any);
    // scala
    insert(m, "WeakMap", Any);
    // i don't know why these functions aren't part of window
    // scheme
    insert(m, "eval", Any);
    insert(m, "isFinite", Any);
    insert(m, "isNaN", Any);
    insert(m, "parseFloat", Any);
    insert(m, "parseInt", Any);
    // constants
    insert(m, "undefined", Any);
    insert(m, "null", Any);

    map
}

fn insert(map: &mut BindMap, name: &str, ty: Type) {
    map.push((name.into(), ty));
}
