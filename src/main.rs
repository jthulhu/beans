extern crate regex;
extern crate hashbrown;

mod lexer;
mod location;
mod error;
mod stream;

use lexer::{Lexer, GrammarParser};

fn main() {
    let mut a = GrammarParser::new(String::from("myfile"), String::from(""));
    println!("{}", a.read().unwrap().0.as_str());
}
