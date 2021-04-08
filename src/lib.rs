#![allow(unused)]
extern crate fixedbitset;
extern crate hashbrown;
extern crate unbounded_interval_tree;
extern crate serde;
extern crate bincode;

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
#[macro_use]
mod test_utilities;
// pub use utilities::*;
// pub use test_utilities::*;
