use crate::location::Location;
use crate::case::Case;
use std::error;
use std::fmt;
use std::io;

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
    GrammarDuplicateDefinition(String, Location),
    GrammarNonTerminalDuplicate(String),
    GrammarSyntaxError(String),
}

#[derive(Debug)]
pub enum WarningType {
    CaseConvention(String, Case, Vec<Case>)
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
            LexerGrammarSyntax(msg) => ("Syntax error within the lexer's grammar", msg.clone()),
            LexingError(msg) => ("Error while lexing", msg.clone()),
            InternalError(msg) => ("Internal Error, this should not happend", msg.clone()),
            GrammarDuplicateDefinition(name, pos) => (
                "Duplicate definition in grammar",
                format!(
                    "{} is already definded in file {}, from {}:{} to {}:{}",
                    name,
                    pos.file(),
                    pos.start().0,
                    pos.start().1,
                    pos.end().0,
                    pos.end().1
                ),
            ),
            GrammarSyntaxError(msg) => ("Syntax error within the grammar", msg.clone()),
	    GrammarNonTerminalDuplicate(msg) => ("Duplicate definition of a non terminal", msg.clone())
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
