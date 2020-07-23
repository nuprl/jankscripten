use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// implement this trait in order to allow into::<I64Val>() / deref. see
/// [I64Val]
pub trait AsI64 {}

/// a 64-bit type that can be returned from a wasm function and stored in
/// a local. rust is too stubborn to allow most types to do this automatically
///
/// use `into()` to turn `T` into `I64Val`, and `*` to turn I64Val<T> into T
///
/// ```
/// struct MakeMe64(i32, i32);
/// impl AsI64 for MakeMe64 {}
///
/// #[no_mangle]
/// pub extern "C" with_second_5(in: AsI64<MakeMe64>) -> AsI64<MakeMe64> {
///     let mut in_tuple = *in;
///     in_tuple.1 = 5;
///     in_tuple.into()
/// }
///
/// let x: AsI64<MakeMe64> = MakeMe64(5, 0).into(); // wasm type: i64
/// assert_eq!(*with_second_5(x), MakeMe64(5, 5));
/// ```
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
