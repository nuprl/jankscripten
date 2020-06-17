use super::constructors::*;
use super::syntax::Type;
use std::collections::HashMap;
use Type::*;

const KEY: Type = Type::I32;

type BindMap = HashMap<std::string::String, Type>;

pub fn get_rt_bindings() -> BindMap {
    let mut map = HashMap::new();
    insert_mono(&mut map, "ht_new", vec![], ht_ty_(I32), vec![Any, I32]);
    insert_mono(
        &mut map,
        "ht_get",
        vec![ht_ty_(Any), KEY],
        ht_ty_(Any),
        vec![Any, I32],
    );
    insert_mono(
        &mut map,
        "ht_set",
        vec![ht_ty_(Any), KEY, Any],
        ht_ty_(Any),
        vec![Any, I32],
    );
    map
}

fn insert_mono(
    map: &mut BindMap,
    name: &str,
    params_tys: Vec<Type>,
    ret_ty: Type,
    provided_tys: Vec<Type>,
) {
    for replace_ty in provided_tys {
        let mono_name = format!("{}_{}", name, replace_ty);
        let params_tys = params_tys
            .iter()
            .cloned()
            .map(|ty| replace_if_any(ty, replace_ty.clone()))
            .collect();
        let ret_ty = replace_if_any(ret_ty.clone(), replace_ty);
        map.insert(mono_name, fn_ty_(params_tys, ret_ty));
    }
}

fn replace_if_any(replace_in: Type, replace_if: Type) -> Type {
    if replace_in == Type::Any {
        replace_if
    } else {
        replace_in
    }
}
