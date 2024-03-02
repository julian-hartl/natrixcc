#[allow(clippy::all)]
#[allow(warnings)]
pub(crate) mod grammar;
pub mod module;
pub use module::{Module, parse};
