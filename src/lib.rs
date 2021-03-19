#![allow(unused)]
extern crate fixedbitset;
extern crate hashbrown;
extern crate unbounded_interval_tree;

pub mod error;
pub mod lexer;
pub mod location;
pub mod parser;
pub mod regex;
pub mod stream;
mod utilities;

#[derive(Debug)]
pub enum Case {
    // snakecase, snake_case
    SnakeCase,
    // PascalCase
    PascalCase,
    // camelCase,
    CamelCase,
    // Pascal_Snake_Case,
    PascalSnakeCase,
    // UPPERCASE
    UpperCase,
    // SCREAMING_SNAKE_CASE,
    ScreamingSnakeCase,
    Other
}

impl Case {
    pub fn case(string: &str) -> Self {
	if string.len() == 0 {
	    return Self::Other;
	}

	let mut contains_underscore = false;
	let mut first_is_capital = false;
	let mut contains_small = false;
	let mut contains_capital = false;
	
	let mut first_alpha = true;
	
	for c in string.chars() {
	    if c == '_' {
		contains_underscore = true;
	    } else if c.is_ascii_uppercase() {
		contains_capital = true;
		if first_alpha {
		    first_is_capital = true;
		}
		first_alpha = false;
	    } else if c.is_ascii_lowercase() {
		contains_small = true;
		first_alpha = false;
	    }
	}

	if first_alpha {
	    Self::Other
	} else if !contains_capital {
	    Self::SnakeCase
	} else if !first_is_capital {
	    Self::CamelCase
	} else if contains_small {
	    if contains_underscore {
		Self::PascalSnakeCase
	    } else {
		Self::PascalCase
	    }
	} else {
	    if contains_underscore {
		Self::ScreamingSnakeCase
	    } else {
		Self::UpperCase
	    }
	}
    }
}

use lexer::Lexer;
