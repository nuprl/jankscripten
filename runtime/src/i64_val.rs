//! allow >32-bit types to be returned on the wasm value stack
//!
//! rust uses "out pointer" return style for any values larger than
//! a pointer. this makes sense for most register machines, because a
//! pointer is usually also the register size. however, in wasm, values
//! on the stack can be *bigger* than a pointer. here's that check:
//! https://github.com/rust-lang/rust/blob/2e0edc0/compiler/rustc_middle/src/ty/layout.rs#L2808-L2810
//!
//! by explicitly casting these values to an integer, i assume we make
//! `is_ignore` true since they're a wasm type; this sidesteps the overzealous
//! check:
//! https://github.com/rust-lang/rust/blob/2e0edc0/compiler/rustc_middle/src/ty/layout.rs#L2770-L2772

use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// implement this trait in order to allow into::<I64Val>() / vice-versa
/// / deref
pub trait AsI64 {}

/// a 64-bit type that can be returned from a wasm function and stored in
/// 1 local. rust is too stubborn to allow most types to do this automatically
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct I64Val<T> {
    #[cfg(target_pointer_width = "32")]
    val: u64,
    #[cfg(target_pointer_width = "64")]
    val: u128,
    _phantom: PhantomData<T>,
}
impl<T> Deref for I64Val<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let ptr = self as *const Self as *const Self::Target;
        unsafe { &*ptr }
    }
}
impl<T> DerefMut for I64Val<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let ptr = self as *mut Self as *mut Self::Target;
        unsafe { &mut *ptr }
    }
}
impl<T: Debug> Debug for I64Val<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", **self)
    }
}
impl<T: Copy> From<T> for I64Val<T> {
    fn from(val: T) -> Self {
        let pointer_crimes = &val as *const T as *const Self;
        unsafe { *pointer_crimes }
    }
}
impl<T: PartialEq> PartialEq<Self> for I64Val<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}
