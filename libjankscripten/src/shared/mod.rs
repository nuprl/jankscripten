pub mod coercions;
mod id;
mod name_gen;
pub mod std_lib;
mod types;
#[macro_use]
pub mod pretty;

pub use id::Id;
pub use name_gen::NameGen;
pub use swc_common::Span;
pub use types::Type;

use swc_common::SourceMap;
pub trait Report {
    fn report(&self, sm: &SourceMap) -> String;
}
pub trait UnwrapReport<T> {
    fn unwrap_report(self, sm: &SourceMap) -> T;
}
impl<S, T: Report> UnwrapReport<S> for Result<S, T> {
    fn unwrap_report(self, sm: &SourceMap) -> S {
        match self {
            Ok(o) => o,
            Err(e) => {
                panic!("ERROR: {}", e.report(sm));
            }
        }
    }
}
impl<S> UnwrapReport<S> for Result<S, Box<dyn Report>> {
    fn unwrap_report(self, sm: &SourceMap) -> S {
        match self {
            Ok(o) => o,
            Err(e) => {
                panic!("ERROR: {}", e.report(sm));
            }
        }
    }
}
impl<'a, T: Report + 'a> From<T> for Box<dyn Report + 'a> {
    fn from(e: T) -> Box<dyn Report + 'a> {
        Box::new(e)
    }
}
