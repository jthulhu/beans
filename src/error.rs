use crate::location::Location;
use std::error;
use std::fmt;

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
    InternalError(String),
    LexerGrammarSyntax(String),
    LexingError(String),
}

/// # Summary
///
/// `Error` is a type representing all information required
/// about a given error. It is a tuple containing a `Location`
/// and an `ErrorType`.
#[derive(Debug, PartialEq)]
pub struct Error(Location, ErrorType);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let (type_, msg) = match &self.1 {
            LexerGrammarSyntax(msg) => ("Syntax error within the lexer's grammar", msg),
            LexingError(msg) => ("Error while lexing", msg),
            InternalError(msg) => ("Internal Error, this should not happend", msg),
        };
        write!(
            f,
            "{}\n @{}, from {}:{} to {}:{}\n{}",
            type_,
            self.0.file(),
            self.0.start().0,
            self.0.start().1,
            self.0.end().0,
            self.0.end().1,
            msg
        )
    }
}

impl error::Error for Error {}

impl From<(Location, ErrorType)> for Error {
    fn from((loc, err): (Location, ErrorType)) -> Self {
        Error(loc, err)
    }
}
