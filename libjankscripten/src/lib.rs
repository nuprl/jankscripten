//! the jankscripten system without IO/main

pub mod javascript;
pub mod shared;
pub mod jankierscript;
pub mod jankyscript;
pub mod notwasm;
mod rope;

#[macro_use]
extern crate combine;
