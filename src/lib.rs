#![allow(clippy::upper_case_acronyms)]
#![warn(rust_2018_idioms, missing_debug_implementations, missing_docs)]
#![allow(unused_extern_crates)]

//! # Beans
//!
//! Language-oriented programming and scripting language.
//! This library contains an API to parse grammars, compile them and compile source code.

mod case;
pub mod error;
pub mod lexer;
pub mod location;
pub(crate) mod parser;
pub(crate) mod regex;
pub(crate) mod stream;
mod wrapper;
// Macros
#[macro_use]
mod utilities;
#[cfg(test)]
#[macro_use]
mod test_utilities;
