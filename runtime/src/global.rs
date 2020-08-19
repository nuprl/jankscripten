use crate::heap_types::ObjectPtr;
use crate::string::str_as_ptr;
use crate::{heap, object, AnyEnum};

pub fn init() {
    unsafe {
        GLOBAL = object::object_empty();
        Object = object::object_empty();
        ins_f(Object, "create", object::object_create as i32);
    }
}
fn insert(mut obj: ObjectPtr, field: &str, value: AnyEnum) {
    obj.insert(heap(), str_as_ptr(field), value.into(), &mut -1);
}
// almost everything we're gonna insert is gonna be a function
fn ins_f(obj: ObjectPtr, field: &str, ptr: i32) {
    insert(obj, field, AnyEnum::Fn(ptr as u32));
}

// TODO(luna): delete? if we end up not using the global object for nonsense,
// we might be able to avoid populating a real object with the runtime globals
#[no_mangle]
pub static mut GLOBAL: ObjectPtr<'static> = unsafe { ObjectPtr::new(std::ptr::null_mut()) };
#[no_mangle]
pub static mut Object: ObjectPtr<'static> = unsafe { ObjectPtr::new(std::ptr::null_mut()) };
