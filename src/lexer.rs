mod grammarparser;
#[allow(clippy::module_inception)]
mod lexer;

pub use lexer::{Lexer, LexerBuilder, Token};
