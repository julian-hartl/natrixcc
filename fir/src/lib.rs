#![deny(
    clippy::enum_glob_use,
)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
)]
#![forbid(unsafe_code)]
#![allow(clippy::too_many_lines)]
mod ty;
mod codegen;
mod parser;
#[macro_use]
extern crate strum;
#[cfg(test)]
mod test_utils;
pub mod middle;




