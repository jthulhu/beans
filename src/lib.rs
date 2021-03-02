#![allow(unused)]
extern crate fixedbitset;
extern crate hashbrown;
extern crate unbounded_interval_tree;

pub mod error;
pub mod lexer;
pub mod location;
pub mod regex;
pub mod stream;
// pub mod parser;

use lexer::Lexer;
