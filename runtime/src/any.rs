//! An enum that can store any type known to the runtime

use crate::Key;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Any {
    I32(i32),
    Bool(bool),
    AnyHT(HashMap<Key, Any>),
    I32HT(HashMap<Key, i32>),
}
