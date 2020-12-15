pub mod coercions;
mod id;
pub mod std_lib;
mod types;
#[macro_use]
pub mod pretty;

pub use id::{Id, NameGen};
pub use types::Type;

pub trait Report {
    fn report(&self) -> String;
}
pub trait UnwrapReport<T> {
    fn unwrap_report(self) -> T;
}
impl<S, T: Report> UnwrapReport<S> for Result<S, T> {
    fn unwrap_report(self) -> S {
        match self {
            Ok(o) => o,
            Err(e) => {
                panic!("ERROR: {}", e.report());
            }
        }
    }
}
impl<S> UnwrapReport<S> for Result<S, Box<dyn Report>> {
    fn unwrap_report(self) -> S {
        match self {
            Ok(o) => o,
            Err(e) => {
                panic!("ERROR: {}", e.report());
            }
        }
    }
}
impl<'a, T: Report + 'a> From<T> for Box<dyn Report + 'a> {
    fn from(e: T) -> Box<dyn Report + 'a> {
        Box::new(e)
    }
}
