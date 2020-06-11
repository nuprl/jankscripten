#![allow(unstable_name_collisions)]

//! Code from https://github.com/fitzgen/bumpalo/blob/master/src/alloc.rs#L38
use std::alloc::{Layout, LayoutErr};

fn new_layout_err() -> LayoutErr {
    Layout::from_size_align(1, 3).unwrap_err()
}

pub trait UnstableLayoutMethods {
    fn padding_needed_for(&self, align: usize) -> usize;
    fn repeat(&self, n: usize) -> Result<(Layout, usize), LayoutErr>;
    fn array<T>(n: usize) -> Result<Layout, LayoutErr>;
}

impl UnstableLayoutMethods for Layout {
    fn padding_needed_for(&self, align: usize) -> usize {
        let len = self.size();

        // Rounded up value is:
        //   len_rounded_up = (len + align - 1) & !(align - 1);
        // and then we return the padding difference: `len_rounded_up - len`.
        //
        // We use modular arithmetic throughout:
        //
        // 1. align is guaranteed to be > 0, so align - 1 is always
        //    valid.
        //
        // 2. `len + align - 1` can overflow by at most `align - 1`,
        //    so the &-mask wth `!(align - 1)` will ensure that in the
        //    case of overflow, `len_rounded_up` will itself be 0.
        //    Thus the returned padding, when added to `len`, yields 0,
        //    which trivially satisfies the alignment `align`.
        //
        // (Of course, attempts to allocate blocks of memory whose
        // size and padding overflow in the above manner should cause
        // the allocator to yield an error anyway.)

        let len_rounded_up = len.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
        len_rounded_up.wrapping_sub(len)
    }

    fn repeat(&self, n: usize) -> Result<(Layout, usize), LayoutErr> {
        let padded_size = self
            .size()
            .checked_add(self.padding_needed_for(self.align()))
            .ok_or_else(new_layout_err)?;
        let alloc_size = padded_size.checked_mul(n).ok_or_else(new_layout_err)?;

        unsafe {
            // self.align is already known to be valid and alloc_size has been
            // padded already.
            Ok((
                Layout::from_size_align_unchecked(alloc_size, self.align()),
                padded_size,
            ))
        }
    }

    fn array<T>(n: usize) -> Result<Layout, LayoutErr> {
        Layout::new::<T>().repeat(n).map(|(k, offs)| {
            debug_assert!(offs == std::mem::size_of::<T>());
            k
        })
    }
}