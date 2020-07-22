// Preserve this `allow(unused)`. We call log for debugging, and there may
// not be any debugging output!
#[allow(unused)]
#[cfg(target_arch = "wasm32")]
#[cfg(not(test))]
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
#[cfg(any(not(target_arch = "wasm32"), test))]
pub fn log(s: &str) {
    eprintln!("{}", s);
}

pub fn unwrap_log<T>(value: Option<T>, message: &'static str) -> T {
    match value {
        Some(v) => v,
        None => {
            log(message);
            panic!(message); // the message does not appear
        }
    }
}
