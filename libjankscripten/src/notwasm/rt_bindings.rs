use super::constructors::*;
use super::syntax::Type;
use crate::rts_function::RTSFunction;
use std::collections::HashMap;
use strum::IntoEnumIterator;
use Type::*;

const KEY: Type = Type::String;

type BindMap = HashMap<std::string::String, Type>;

/// Generate a map of the runtime functions available to NotWasm.
/// Each runtime function is mapped to its NotWasm type signature.
///
/// This map is comprised of runtime functions from two different sources:
/// 1. Functions manually inserted into the map inside this function.
/// 2. Functions automatically inserted from RTSFunction. RTSFunction
///    includes the runtime functions available at all layers of the
///    compiler, including JavaScript itself. Only higher-level runtime
///    functions are in RTSFunction, like Object.create.
pub fn get_rt_bindings() -> BindMap {
    let mut map = HashMap::new();
    let m = &mut map;
    let mono = |t| t;
    let a_fn = fn_ty_(vec![], None);
    let a_clos = clos_ty_(vec![], None);

    // Step 1: Manually insert runtime functions for NotWasm.
    insert(m, "ht_new", vec![], HT);
    insert(m, "ht_get", vec![HT, KEY], Any);
    insert(m, "ht_set", vec![HT, KEY, Any], Any);
    insert(m, "array_new", vec![], Array);
    insert(m, "array_push", vec![Array, Any], I32); // new length
    insert(m, "array_index", vec![Array, I32], Any);
    insert(m, "array_set", vec![Array, I32, Any], Any);
    insert(m, "array_len", vec![Array], I32);
    insert_mono(
        m,
        "any_from",
        vec![&mono],
        &|_| Any,
        vec![I32, Bool, a_clos.clone(), a_fn.clone()],
    );
    insert_mono(
        m,
        "any_to",
        vec![&|_| Any],
        &mono,
        vec![I32, Bool, a_clos.clone()],
    );
    insert(m, "any_from_ptr", vec![I32], Any);
    insert(m, "any_to_ptr", vec![Any], I32);
    insert(m, "get_undefined", vec![], Any);
    insert(m, "get_null", vec![], Any);
    insert(m, "object_empty", vec![], DynObject);
    insert(m, "object_create", vec![Env, Any], Any);
    // I32s are caches here
    insert(m, "object_set", vec![DynObject, String, Any, I32], Any);
    insert(m, "object_get", vec![DynObject, String, I32], Any);
    insert(m, "string_len", vec![String], I32);
    // I32 is any I32-sized data
    insert(m, "ref_new_non_ptr_32", vec![I32], ref_ty_(I32));
    insert(m, "ref_new_f64", vec![F64], ref_ty_(F64));
    insert(m, "ref_new_any", vec![Any], ref_ty_(Any));
    // I32 is any POINTER, will return a pointer to THAT
    insert(m, "ref_new_ptr", vec![I32], ref_ty_(I32));
    insert(m, "init", vec![], None);
    insert(m, "gc_enter_fn", vec![I32], None);
    insert(m, "gc_exit_fn", vec![], None);
    // NOTE(arjun): The type below is not accurate. The first argument is
    // a *mut Tag, but we don't have a type for that.
    insert(m, "set_in_current_shadow_frame_slot", vec![I32, I32], None);
    insert(
        m,
        "set_any_in_current_shadow_frame_slot",
        vec![Any, I32],
        None,
    );
    insert(
        m,
        "set_closure_in_current_shadow_frame_slot",
        vec![clos_ty_(vec![], None), I32],
        None,
    );
    insert(m, "any_to_f64", vec![Any], F64);
    insert(m, "f64_to_any", vec![F64], Any);
    // length -> Env
    insert(m, "env_alloc", vec![I32], I32);
    // TODO(luna): this could be a single wasm instruction too
    // (env: Env, index, item) -> Env
    insert(m, "env_init_at", vec![I32, I32, Any], I32);
    // this could be 2 wasm instructions
    insert(m, "closure_new", vec![I32, I32], a_clos.clone());
    // i tried writing these 2 in wasm too but it got more complicated than i'd
    // like; i don't think i'd write it smarter than the rust compiler inlining
    // aside; and i hope we can inline the runtime automatically at some point
    // -> Env
    insert(m, "closure_env", vec![a_clos.clone()], I32);
    insert(m, "closure_func", vec![a_clos.clone()], a_fn.clone());
    // here's some standard library stuff!!
    // most of these take Env, Any which is _env, _this (usually ignored)
    insert(m, "parse_int", vec![Env, Any, Any], Any);
    // returns 5 for now because void messiness remains
    insert(m, "console_log", vec![Env, Any, Any], Any);
    // Step 2: automatically insert runtime functions from RTSFunction.
    for rts in RTSFunction::iter() {
        if let RTSFunction::Todo(_) = rts {
            // can't !let
        } else {
            // Automatically generate the name and notwasm type
            m.insert(rts.name().into(), rts.janky_typ().notwasm_typ());
        }
    }
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
