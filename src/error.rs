use crate::location::Location;

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    LexerGrammarSyntax(String),
    LexingError(String),
}

pub type Error = (Location, ErrorType);
