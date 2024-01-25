#![deny(
    clippy::enum_glob_use,
)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
)]
#![forbid(unsafe_code)]
#![allow(clippy::too_many_lines)]
mod cfg;
mod module;
mod instruction;
mod ty;
mod function;
mod cfg_builder;
mod optimization;
mod codegen;
mod parser;
#[macro_use]
extern crate strum;
#[cfg(test)]
mod test_utils;
mod analysis;




