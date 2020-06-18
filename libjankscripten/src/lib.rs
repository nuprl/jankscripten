//! the jankscripten system without IO/main

pub mod javascript;
pub mod shared;
pub mod notjankyscript;
pub mod jankyscript;
pub mod notwasm;

#[macro_use]
extern crate combine;
