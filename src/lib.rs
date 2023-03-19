#![allow(
    clippy::upper_case_acronyms,
    unstable_name_collisions,
    unreachable_code
)]
#![warn(rust_2018_idioms, missing_debug_implementations)]
#![feature(box_patterns, try_blocks)]

//! # Beans
//!
//! Language-oriented programming and scripting language.
//! This library contains an API to parse grammars, compile them and compile source code.

pub mod builder;
mod case;
pub mod error;
pub mod lexer;
mod list;
pub mod parser;
pub mod printer;
pub mod regex;
pub mod span;
pub mod stream;
pub mod typed;
// Macros
#[macro_use]
mod utilities;
#[cfg(test)]
#[macro_use]
mod test_utilities;
