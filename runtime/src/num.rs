//! a generic float/int type for numbers, implementing add

/// either an f64 or an i32
#[derive(Debug, PartialEq)]
pub enum Num {
    I32(i32),
    F64(f64),
}
/// assume the number is an i32 and give its value
#[no_mangle]
pub extern "C" fn num_as_i32(num: &Num) -> i32 {
    if let Num::I32(i) = num {
        *i
    } else {
        // TODO: panic is better but gives us unreadable code while im still reading output wasm
        // panic = "abort" still generates a bunch of weird code not sure what i'm doing wrong
        //panic!();
        std::process::abort();
    }
}
/// assume the number is an f64 and give its value
#[no_mangle]
pub extern "C" fn num_as_f64(num: &Num) -> f64 {
    if let Num::F64(f) = num {
        *f
    } else {
        //panic!();
        std::process::abort();
    }
}

/// returns an i32 Num if both arguments are i32, otherwise returns an f64 Num
///
/// ```
/// # use runtime::*;
/// let a = num_i32(4);
/// let b = num_f64(6.5);
/// let sum = add_num(a, b);
/// assert_eq!(sum, num_f64(10.5));
/// ```
#[no_mangle]
pub extern "C" fn num_add(a: &Num, b: &Num) -> Num {
    match (a, b) {
        (Num::I32(a), Num::I32(b)) => Num::I32(*a + *b),
        // commutative
        (Num::I32(i), Num::F64(f)) | (Num::F64(f), Num::I32(i)) => Num::F64(*f + *i as f64),
        (Num::F64(a), Num::F64(b)) => Num::F64(*a + *b),
    }
}

/// constructor function for creating a Num using an i32
#[no_mangle]
pub extern "C" fn num_i32(i: i32) -> Num {
    Num::I32(i)
}

/// constructor function for creating a Num using an f64
#[no_mangle]
pub extern "C" fn num_f64(f: f64) -> Num {
    Num::F64(f)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    #[should_panic]
    fn bad_math_good_types() {
        assert_eq!(num_i32(20), num_add(&num_i32(4), &num_i32(6)));
    }
    #[test]
    #[should_panic]
    fn good_math_bad_types() {
        assert_eq!(num_i32(10), num_add(&num_f64(4.), &num_f64(6.)));
    }
    #[test]
    fn i32_plus_i32() {
        assert_eq!(num_i32(10), num_add(&num_i32(4), &num_i32(6)));
    }
    #[test]
    fn f64_plus_i32() {
        assert_eq!(num_f64(10.5), num_add(&num_f64(4.5), &num_i32(6)));
    }
    #[test]
    fn f64_plus_f64() {
        assert_eq!(num_f64(11.0), num_add(&num_f64(4.5), &num_f64(6.5)));
    }
    #[test]
    fn convert_back() {
        assert_eq!(num_as_f64(&num_f64(11.0)), 11.0);
        assert_eq!(num_as_i32(&num_i32(121)), 121);
    }
    #[test]
    fn main_wasm() {
        assert_eq!(num_as_i32(&num_add(&num_i32(6), &num_i32(4))), 10);
    }
}
