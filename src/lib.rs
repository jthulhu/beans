#![allow(clippy::upper_case_acronyms, unstable_name_collisions)]
#![warn(missing_debug_implementations)]

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
