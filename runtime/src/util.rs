// Preserve this `allow(unused)`. We call log for debugging, and there may
// not be any debugging output!
#[allow(unused)]
#[cfg(all(target_arch = "wasm32", not(test)))]
pub fn log(s: &str) {
    use std::ffi::CString;
    use std::os::raw::c_char;
    extern "C" {
        // There is a global symbol log with type `(f64) -> f64` (i.e., the
        // logarithm function.
        fn jankscripten_log(string: *const c_char);
    }
    let c_str = CString::new(s).expect("could not create C string");
    unsafe {
        jankscripten_log(c_str.as_ptr());
    }
}

// Preserve this `allow(unused)`. We call log for debugging, and there may
// not be any debugging output!
#[allow(unused)]
#[cfg(not(target_arch = "wasm32"))]
pub fn log(s: &str) {
    println!("{}", s);
}

#[allow(unused)]
#[cfg(all(target_arch = "wasm32", test))]
pub fn log(s: &str) {
    use wasm_bindgen_test::console_log;
    console_log!("{}", s);
}

pub fn unwrap_log<T>(value: Option<T>, message: &'static str) -> T {
    match value {
        Some(v) => v,
        None => {
            log(message);
            panic!("{}", message); // the message does not appear
        }
    }
}

#[allow(unused)]
#[cfg(all(target_arch = "wasm32", not(test)))]
pub fn error(s: &str) {
    use std::ffi::CString;
    use std::os::raw::c_char;
    extern "C" {
        fn jankscripten_error(string: *const c_char);
    }
    let c_str = CString::new(s).expect("could not create C string");
    unsafe {
        jankscripten_error(c_str.as_ptr());
    }
}
#[allow(unused)]
#[cfg(all(target_arch = "wasm32", test))]
pub fn error(s: &str) {
    log(s);
}
