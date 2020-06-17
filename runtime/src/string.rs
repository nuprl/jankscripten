use super::heap;

#[no_mangle]
pub extern "C" fn string_from_str(from: &str) -> Box<String> {
    Box::new(from.into())
}

#[no_mangle]
pub extern "C" fn string_len(string: &String) -> i32 {
    string.len() as i32
}
