#![allow(unused)]
extern crate fixedbitset;
extern crate hashbrown;
extern crate regex;

pub mod error;
pub mod lexer;
pub mod location;
pub mod regex2;
pub mod stream;

use lexer::Lexer;
