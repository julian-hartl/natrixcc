#![deny(
    clippy::enum_glob_use,
)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
)]
#![forbid(unsafe_code)]
#![allow(clippy::too_many_lines)]

mod codegen;
#[macro_use]
extern crate strum;



