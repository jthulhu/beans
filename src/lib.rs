#![allow(unused)]
extern crate hashbrown;
extern crate regex;
extern crate fixedbitset;

pub mod error;
pub mod lexer;
pub mod location;
pub mod stream;

use lexer::Lexer;
