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
#[derive(Clone, Copy)]
#[repr(C)]
pub union I64Val<T: Copy> {
    #[cfg(target_pointer_width = "32")]
    int: u64,
    #[cfg(target_pointer_width = "64")]
    int: u128,
    val: T,
}
impl<T: Copy> I64Val<T> {
    /// for debugging, provides the backing 64-int including garbage padding
    /// bits. these should be treated carefully, for example, not compared for
    /// equality
    #[cfg(target_pointer_width = "32")]
    pub(crate) fn raw_val(&self) -> u64 {
        unsafe { self.int }
    }
}
impl<T: Copy> Deref for I64Val<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &self.val }
    }
}
impl<T: Copy> DerefMut for I64Val<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.val }
    }
}
impl<T: Debug + Copy> Debug for I64Val<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", **self)
    }
}
impl<T: Copy> From<T> for I64Val<T> {
    fn from(val: T) -> Self {
        I64Val { val }
    }
}
impl<T: PartialEq + Copy> PartialEq<Self> for I64Val<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}
