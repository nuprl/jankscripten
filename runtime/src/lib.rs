//! the runtime for programs compiled to wasm by jankscripten. it itself
//! externs public non-mangled functions intended to by compiled to wasm
//! and dynamically linked
//!
//! currently only implements Num and add_num
//!
//! things i've learned messing around:
//!
//! - tag all public structs with `#[repr(C)]` and all functions with `extern
//!   "C"` *and* `#[no_mangle]`. without it no code will be generated at all
//! - enum structs are disallowed, but enum+union+struct is a workaround
//! - you can't really use impl productively, just stick with flat procedures
//! - because of no multiple returns, it's much easier to think about the
//!   generated code if types are <=64 bits
//! - when not, generated code usually accepts a new first parameter, which
//!   is the memory address of output (fsr no allocator provided yet)
//! - build on release if you wanna read the generated code, it's 1000% better
//! - wasm_bindgen / wasm-pack is unneccessary and gets in the way since
//!   it's all about js bindings

/// this is a bit for whether a [Num] contains i32 or f64, should not need
/// to be used on its own. part of a workaround around no enum structs. has
/// to be repr(C) to avoid weirdly taking up way more space
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
enum NumType {
    I32,
    F64,
}

union NumData {
    data_i32: i32,
    data_f64: f64,
}

/// either an f64 or an i32
// bespoke enum struct because neither enum structs nor unions are supported
// by wasm_bindgen
#[no_mangle]
#[repr(C)]
pub struct Num {
    // this could be compressed by tagging I32's upper 32 bits with a float
    // NaN with a special data tag in 33-52. you could check hi == TAG_VALUE
    // => it's an I32; then it'd fit in 4 bytes. this would have further
    // reaching benefits than you'd think because:
    // - functions could return Nums without writing to linear memory
    // - rust aligns the kind bit to a whole 64-bit value
    kind: NumType,
    data: NumData,
}
/// assume the number is an i32 and give its value
#[no_mangle]
pub extern "C" fn num_as_i32(num: &Num) -> i32 {
    unsafe { num.data.data_i32 }
}
/// assume the number is an f64 and give its value
#[no_mangle]
pub extern "C" fn num_as_f64(num: &Num) -> f64 {
    unsafe { num.data.data_f64 }
}
impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        match (self.kind, other.kind) {
            (NumType::I32, NumType::I32) => num_as_i32(&self) == num_as_i32(&other),
            (NumType::I32, NumType::F64) | (NumType::F64, NumType::I32) => false,
            (NumType::F64, NumType::F64) => num_as_f64(&self) == num_as_f64(&other),
        }
    }
}
impl std::fmt::Debug for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            NumType::I32 => {
                write!(f, "I32(")?;
                write!(f, "{}", num_as_i32(&self))?;
                write!(f, ")")
            }
            NumType::F64 => {
                write!(f, "F64(")?;
                write!(f, "{}", num_as_f64(&self))?;
                write!(f, ")")
            }
        }
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
pub extern "C" fn add_num(a: Num, b: Num) -> Num {
    match (a.kind, b.kind) {
        (NumType::I32, NumType::I32) => num_i32(num_as_i32(&a) + num_as_i32(&b)),
        // commutative
        (NumType::I32, NumType::F64) | (NumType::F64, NumType::I32) => {
            num_f64(num_as_f64(&a) + num_as_i32(&b) as f64)
        }
        (NumType::F64, NumType::F64) => num_f64(num_as_f64(&a) + num_as_f64(&b)),
    }
}

/// constructor function for creating a Num using an i32
#[no_mangle]
pub extern "C" fn num_i32(i: i32) -> Num {
    Num {
        kind: NumType::I32,
        data: NumData { data_i32: i },
    }
}

/// constructor function for creating a Num using an f64
#[no_mangle]
pub extern "C" fn num_f64(f: f64) -> Num {
    Num {
        kind: NumType::F64,
        data: NumData { data_f64: f },
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    #[should_panic]
    fn bad_math_good_types() {
        assert_eq!(num_i32(20), add_num(num_i32(4), num_i32(6)));
    }
    #[test]
    #[should_panic]
    fn good_math_bad_types() {
        assert_eq!(num_i32(10), add_num(num_f64(4.), num_f64(6.)));
    }
    #[test]
    fn i32_plus_i32() {
        assert_eq!(num_i32(10), add_num(num_i32(4), num_i32(6)));
    }
    #[test]
    fn f64_plus_i32() {
        assert_eq!(num_f64(10.5), add_num(num_f64(4.5), num_i32(6)));
    }
    #[test]
    fn f64_plus_f64() {
        assert_eq!(num_f64(11.0), add_num(num_f64(4.5), num_f64(6.5)));
    }
    #[test]
    fn convert_back() {
        assert_eq!(num_as_f64(&num_f64(11.0)), 11.0);
        assert_eq!(num_as_i32(&num_i32(121)), 121);
    }
    #[test]
    fn main_wasm() {
        assert_eq!(num_as_i32(&add_num(num_i32(6), num_i32(4))), 10);
    }
}
