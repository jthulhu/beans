#![allow(clippy::upper_case_acronyms, unstable_name_collisions)]
#![warn(rust_2018_idioms, missing_debug_implementations)]

//! # Beans
//!
//! Language-oriented programming and scripting language.
//! This library contains an API to parse grammars, compile them and compile source code.

mod case;
pub mod error;
pub mod lexer;
pub mod location;
pub mod parser;
pub mod regex;
pub mod stream;
// Macros
#[macro_use]
mod utilities;
#[cfg(test)]
#[macro_use]
mod test_utilities;
