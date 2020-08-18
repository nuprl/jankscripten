use crate::heap_types::ObjectPtr;

#[no_mangle]
pub static mut GLOBAL: ObjectPtr<'static> = unsafe { ObjectPtr::new(std::ptr::null_mut()) };
