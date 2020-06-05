use super::constructors::*;
use super::syntax::Type;
use std::collections::HashMap;
use Type::*;

pub fn get_rt_bindings() -> HashMap<&'static str, Type> {
    let mut map = HashMap::new();
    map.insert("ht_new", fn_ty_(vec![], ht_ty_(I32)));
    map.insert("ht_get", fn_ty_(vec![ht_ty_(I32), I32], ht_ty_(I32)));
    map.insert("ht_set", fn_ty_(vec![ht_ty_(I32), I32, I32], ht_ty_(I32)));
    map
}
