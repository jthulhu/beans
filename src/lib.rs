#![allow(clippy::upper_case_acronyms)]
#![warn(rust_2018_idioms, missing_debug_implementations, missing_docs)]
#![allow(unused_extern_crates)]

//! # Beans
//!
//! Language-oriented programming and scripting language.
//! This library contains an API to parse grammars, compile them and compile source code.

extern crate bincode;
extern crate fixedbitset;
extern crate hashbrown;
extern crate serde;
extern crate unbounded_interval_tree;

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
