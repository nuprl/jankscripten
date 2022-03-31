use super::constructors::*;
use super::syntax::Type;
use crate::rts_function::{RTSFunction, RTSFunctionImpl};
use std::collections::HashMap;
use strum::IntoEnumIterator;
use Type::*;

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
    // Step 2: automatically insert runtime functions from RTSFunction.
    for rts in RTSFunction::iter() {
        match rts {
            RTSFunction::Todo(..) | RTSFunction::Import(..) | RTSFunction::Method(..) => (),
            _ => {
                if let RTSFunctionImpl::Rust(name) = rts.name() {
                    // Automatically generate the name and notwasm type
                    m.insert(name.into(), rts.janky_typ().notwasm_typ(false));
                }
            }
        }

        // NotWasm runtime functions do not need to be added because
        // NotWasm already knows about them.
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
