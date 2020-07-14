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
    insert(m, "ht_new", vec![], HT);
    insert(m, "ht_get", vec![HT, KEY], Any);
    insert(m, "ht_set", vec![HT, KEY, Any], Any);
    insert(m, "array_new", vec![], Array);
    insert(m, "array_push", vec![Array, Any], I32); // new length
    insert(m, "array_index", vec![Type::Array, I32], Any);
    insert(m, "array_len", vec![Type::Array], I32);
    insert_mono(m, "any_from", vec![&mono], &|_| Any, vec![I32]);
    insert_mono(m, "any_to", vec![&|_| Any], &mono, vec![I32]);
    insert(m, "any_from_ptr", vec![I32], Any);
    insert(m, "any_to_ptr", vec![Any], I32);
    insert(m, "object_empty", vec![], DynObject);
    // I32s are caches here
    insert(m, "object_set", vec![DynObject, StrRef, Any, I32], Any);
    insert(m, "object_get", vec![DynObject, StrRef, I32], Any);
    insert(m, "string_from_ptr", vec![StrRef], String);
    insert(m, "string_len", vec![String], I32);
    insert(m, "ref_new", vec![I32], I32);
    insert(m, "init", vec![], None);
    insert(m, "gc_enter_fn", vec![], None);
    insert(m, "gc_exit_fn", vec![], None);
    insert(m, "any_to_f64", vec![Any], F64);
    insert(m, "f64_to_any", vec![F64], Any);
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
