use crate::heap;

#[no_mangle]
pub extern "C" fn gc_enter_fn() {
    heap().push_shadow_frame(&[]);
}
#[no_mangle]
pub unsafe extern "C" fn gc_exit_fn() {
    heap().pop_shadow_frame();
}
