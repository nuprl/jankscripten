use super::parse;
use super::syntax::*;

pub fn get_notwasm_rt() -> Program {
    parse(
        r#"
        // because of the type annotations, since int has the same wasm type
        // as DynObject, i think i can get away with this in order to create
        // a lazy static
        var Object: DynObject = 0;
        var global: DynObject = 0;
        function jnks_init() {
            Object = {};
            Object.create = any(rt(object_create));
            global = {};
        }
        "#,
    )
}
