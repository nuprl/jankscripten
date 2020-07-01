//! An enum that can store any type known to the runtime

use crate::heap;
use crate::heap_types::*;

/// should not be used directly, all anys will be on the heap. use AnyJSPtr
#[derive(Debug, Clone)]
pub enum Any<'a> {
    I32(i32),
    F64(f64),
    Bool(bool),
    AnyHT(HTPtr<'a, AnyJSPtr<'a>>),
    I32HT(HTPtr<'a, i32>),
}

#[no_mangle]
pub extern "C" fn any_f64<'a>(val: f64) -> AnyJSPtr<'a> {
    heap().alloc(Any::F64(val)).unwrap()
}

#[no_mangle]
pub extern "C" fn any_to_f64<'a>(val: AnyJSPtr<'a>) -> f64 {
    if let Any::F64(f) = *val {
        f
    } else {
        panic!("unwrap incorrect type f64");
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::init;
    use wasm_bindgen_test::wasm_bindgen_test;
    #[test]
    #[wasm_bindgen_test]
    fn in_and_out() {
        init();
        assert_eq!(any_to_f64(any_f64(5.)), 5.);
    }
}
