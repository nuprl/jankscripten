use super::constructors::*;
use super::syntax::Type;
use std::collections::HashMap;
use Type::*;

const KEY: Type = Type::String;

type BindMap = HashMap<std::string::String, Type>;

pub fn get_rt_bindings() -> BindMap {
    let mut map = HashMap::new();
    let m = &mut map;
    let mono = |t| t;
    insert_mono(m, "ht_new", vec![], &ht_ty_, vec![Any, I32, F64]);
    insert_mono(
        m,
        "ht_get",
        vec![&ht_ty_, &|_| KEY],
        &mono,
        vec![Any, I32, F64],
    );
    insert_mono(
        m,
        "ht_set",
        vec![&ht_ty_, &|_| KEY, &mono],
        &mono,
        vec![Any, I32, F64],
    );
    insert_mono(m, "array_new", vec![], &array_ty_, vec![Any, I32]);
    insert_mono(
        m,
        "array_push",
        vec![&array_ty_, &mono],
        &|_| I32, // new length
        vec![Any, I32],
    );
    insert_mono(
        m,
        "array_index",
        vec![&array_ty_, &|_| I32],
        &mono,
        vec![Any, I32],
    );
    insert_mono(m, "array_len", vec![&array_ty_], &|_| I32, vec![Any, I32]);
    insert_mono(m, "any", vec![&mono], &|_| Any, vec![F64]);
    insert_mono(m, "any_to", vec![&|_| Any], &mono, vec![F64]);
    insert(m, "object_empty", vec![], AnyClass);
    // I32s are caches here
    insert_mono(
        m,
        "object_set",
        vec![&|_| AnyClass, &|_| StrRef, &mono, &|_| I32],
        &mono,
        vec![Any, F64],
    );
    insert_mono(
        m,
        "object_get",
        vec![&|_| AnyClass, &|_| StrRef, &|_| I32],
        &mono,
        vec![Any, F64],
    );
    insert(m, "string_from_ptr", vec![StrRef], String);
    insert(m, "string_len", vec![String], I32);
    insert(m, "init", vec![], None);
    insert(m, "gc_enter_fn", vec![], None);
    insert(m, "gc_exit_fn", vec![], None);
    map
}

fn insert_mono<'a, X, I>(
    map: &mut BindMap,
    name: &str,
    params_tys: Vec<&dyn core::ops::Fn(Type) -> Type>,
    ret_ty: I,
    provided_tys: Vec<Type>,
) where
    X: core::ops::Fn(Type) -> Type + 'a,
    I: Into<Option<&'a X>> + Clone,
{
    for replace_ty in provided_tys {
        let mono_name = format!("{}_{}", name, replace_ty);
        let params_tys = params_tys.iter().map(|f| f(replace_ty.clone())).collect();
        let ret_ty = ret_ty.clone().into().map(|f| f(replace_ty));
        map.insert(mono_name, fn_ty_(params_tys, ret_ty));
    }
}

fn insert<I: Into<Option<Type>>>(map: &mut BindMap, name: &str, params_tys: Vec<Type>, ret_ty: I) {
    map.insert(name.into(), fn_ty_(params_tys, ret_ty.into()));
}
