use crate::location::Location;
use std::error;

#[derive(Debug)]
pub struct ExecutionError {
    description: String,
}

impl ExecutionError {
    pub fn new(description: String) -> Self {
        Self { description }
    }
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.description)
    }
}

impl error::Error for ExecutionError {}

/// # Summary
///
/// `ErrorType` is an enum that contains all the possible errors
/// that `beans` might encounter when parsing.
///
/// # Error types
///
/// `LexerGrammarSyntax`: error that arises when there is a syntax error within the grammar file.
/// `LexingError`: error that arises when lexing.
#[derive(Debug, PartialEq)]
pub enum ErrorType {
    LexerGrammarSyntax(String),
    LexingError(String),
}

/// # Summary
///
/// `Error` is a type representing all information required
/// about a given error. It is a tuple containing a `Location`
/// and an `ErrorType`.
pub type Error = (Location, ErrorType);
