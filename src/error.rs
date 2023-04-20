//! # Error
//!
//! This module contains error related primitives.

use crate::span::Span;
use fragile::Fragile;
use std::ffi::OsString;
use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;
use std::str::Utf8Error;
use std::string::FromUtf8Error;

/// `Result` is a shorthand for the usual `Result` type.
pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub struct Error {
    #[from]
    pub kind: Box<ErrorKind>,
}

impl Error {
    pub fn with_file<T>(error: T, file: impl Into<PathBuf>) -> Self
    where
        ErrorKind: From<(PathBuf, T)>,
    {
        Self::new((file.into(), error).into())
    }

    pub fn new(kind: ErrorKind) -> Self {
        Self {
            kind: Box::new(kind),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<T: Into<ErrorKind>> From<T> for Error {
    fn from(t: T) -> Self {
        Self::new(t.into())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
    InternalError {
        message: String,
    },
    IntegerTooBig {
        string: String,
        span: Fragile<Span>,
    },
    SerializationError {
        error: bincode::Error,
        path: PathBuf,
    },
    IllformedAst {
        error: serde_json::Error,
        path: PathBuf,
    },
    UnrecognisedExtension {
        extension: OsString,
        path: PathBuf,
    },
    NonUtf8Extension {
        path: PathBuf,
    },
    NonUtf8Content {
        path: PathBuf,
        error: Utf8Error,
    },
    GrammarNotFound {
        path: PathBuf,
    },
    LexerGrammarSyntax {
        message: String,
        span: Fragile<Span>,
    },
    LexerGrammarDuplicateDefinition {
        token: String,
        span: Fragile<Span>,
    },
    LexerGrammarUnwantedNoDescription {
        token: String,
        span: Fragile<Span>,
    },
    LexerGrammarEofString,
    /// `LexingError(message: String)`: error while transforming a string stream into a token stream.
    LexingError {
        /// The `Span` that made the error occur. It's a hint a what should
        /// be patched.
        span: Fragile<Span>,
    },
    UnwantedToken {
        span: Fragile<Span>,
        message: String,
    },
    GrammarDuplicateDefinition {
        name: String,
        span: Fragile<Span>,
        old_span: Fragile<Span>,
    },
    GrammarDuplicateProxyItem {
        span: Fragile<Span>,
        old_span: Fragile<Span>,
        name: String,
    },
    GrammarArityMismatch {
        macro_name: String,
        definition_arity: usize,
        call_arity: usize,
        definition_span: Fragile<Span>,
        call_span: Fragile<Span>,
    },
    GrammarUndefinedNonTerminal {
        name: String,
        span: Fragile<Span>,
    },
    GrammarUndefinedMacro {
        name: String,
        span: Fragile<Span>,
    },
    GrammarNonTerminalDuplicate {
        message: String,
        span: Fragile<Span>,
    },
    GrammarTerminalInvocation {
        terminal: String,
        span: Fragile<Span>,
    },
    GrammarSyntaxError {
        message: String,
        span: Fragile<Span>,
    },
    GrammarVariantKey {
        span: Fragile<Span>,
    },
    GrammarDuplicateMacroDefinition {
        span: Fragile<Span>,
        old_span: Fragile<Span>,
        name: String,
    },
    SyntaxError {
        name: String,
        alternatives: Vec<String>,
        span: Fragile<Span>,
    },
    SyntaxErrorValidPrefix {
        span: Fragile<Span>,
    },
    IOError {
        error: std::io::Error,
        path: PathBuf,
    },
    RegexError {
        span: Fragile<Span>,
        message: String,
    },
    SameOutputAndInput,
}

impl ErrorKind {
    pub fn err<T>(self) -> std::result::Result<T, Error> {
        Err(Error {
            kind: Box::new(self),
        })
    }
}

impl From<(PathBuf, std::io::Error)> for ErrorKind {
    fn from((path, error): (PathBuf, std::io::Error)) -> Self {
        Self::IOError { error, path }
    }
}

impl From<(PathBuf, Utf8Error)> for ErrorKind {
    fn from((path, error): (PathBuf, Utf8Error)) -> Self {
        Self::NonUtf8Content { path, error }
    }
}

impl From<(PathBuf, bincode::Error)> for ErrorKind {
    fn from((path, error): (PathBuf, bincode::Error)) -> Self {
        Self::SerializationError { error, path }
    }
}

impl From<(PathBuf, FromUtf8Error)> for ErrorKind {
    fn from((_path, _error): (PathBuf, FromUtf8Error)) -> Self {
        todo!()
    }
}

impl From<(PathBuf, serde_json::Error)> for ErrorKind {
    fn from((path, error): (PathBuf, serde_json::Error)) -> Self {
        Self::IllformedAst { error, path }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InternalError { message } => writeln!(f, "ICE: {message}"),
            Self::SerializationError { error, path } => writeln!(
                f,
                "ICE: while trying to serialize {}, the following error occured\n{error}",
                path.display(),
            ),
            Self::IllformedAst { error, path } => writeln!(
                f,
                "File {} contains an illformed AST.\n{error}",
                path.display(),
            ),
            Self::UnrecognisedExtension { extension, path } => {
                write!(
                    f,
                    "File {} has an extension that is not recognised by Beans",
                    path.display(),
                )?;
                if let Some(ext) = extension.to_str() {
                    writeln!(f, ": {ext}")
                } else {
                    writeln!(f)
                }
            }
            Self::NonUtf8Extension { path } => writeln!(
                f,
                "File {} has an extension that is not valid utf-8.",
                path.display(),
            ),
            Self::GrammarNotFound { path } => {
                writeln!(f, "Grammar not found at {}", path.display(),)
            }
            Self::LexerGrammarSyntax { message, span } => {
                writeln!(f, "Syntax error {span}.\n{message}")
            }
            Self::LexerGrammarDuplicateDefinition { token, span } => {
                writeln!(f, "Found duplication definition of {token} {span}.")
            }
            Self::LexerGrammarUnwantedNoDescription { token, span } => {
                writeln!(
                    f,
                    "{token} is tagged `unwanted` but has no description {span}."
                )
            }
            Self::LexerGrammarEofString => {
                writeln!(f, "Found EOF while reading a string.")
            }
            Self::LexingError { span } => {
                writeln!(f, "Could not lex anything {span}.")
            }
            Self::UnwantedToken { span, message } => {
                writeln!(f, "Lexing error {span}.\n{message}")
            }
            Self::GrammarDuplicateDefinition {
                name: message,
                span,
                old_span,
            } => {
                writeln!(
		    f,
		    "Found two definitions of the same non-terminal {span} and {old_span}.\n{message}"
		)
            }
            Self::GrammarDuplicateProxyItem {
                span,
                old_span,
                name,
            } => {
                writeln!(
		    f,
		    "The proxy item {name} {span} was already defined {old_span}."
		)
            }
            Self::GrammarDuplicateMacroDefinition {
                span,
                old_span,
                name,
            } => {
                writeln!(f, "Macro {name} {span} was already defined {old_span}.")
            }
            Self::GrammarArityMismatch {
                macro_name,
                definition_arity,
                call_arity,
                definition_span,
                call_span,
            } => {
                writeln!(f, "The macro {macro_name} is called with the wrong number of argument {call_arity} (expected {definition_arity}, {definition_span}) {call_span}.")
            }
            Self::GrammarNonTerminalDuplicate { message, span } => {
                writeln!(
                    f,
                    "Found two definitions of the same non-terminal {span}\n.{message}"
                )
            }
            Self::GrammarTerminalInvocation { terminal, span } => {
                writeln!(
                    f,
                    "{terminal} is a terminal, not a macro, it cannot be invoked, {span}."
                )
            }
            Self::GrammarSyntaxError { message, span } => {
                writeln!(f, "Syntax error in the grammar {span}.\n{message}")
            }
            Self::GrammarVariantKey { span } => {
                writeln!(f, "The `variant` key is reserved {span}.")
            }
            Self::SyntaxError {
                name,
                alternatives,
                span,
            } => {
                writeln!(
                    f,
                    "Syntax error {name} {span}. You could have tried {alternatives:?}."
                )
            }
            Self::IntegerTooBig { string, span } => {
                writeln!(
		    f,
		    "Integer {string} does not fit on a 64 bit integer, {span}.\nWhy do you even need such a number?"
		)
            }
            Self::GrammarUndefinedMacro { name, span } => {
                writeln!(f, "Macro {name} is undefined {span}.")
            }
            Self::GrammarUndefinedNonTerminal { name, span } => {
                writeln!(f, "Non-terminal {name} is undefined {span}.")
            }
            Self::SyntaxErrorValidPrefix { span } => {
                writeln!(
                    f,
                    "Syntax error {span}: reached EOF while parsing a valid file."
                )
            }
            Self::IOError { error, path } => {
                writeln!(
                    f,
                    "While opening {}, the following error occured: {error}",
                    path.display(),
                )
            }
            Self::RegexError { span, message } => {
                writeln!(f, "Regex error {span}.\n{message}",)
            }
            Self::SameOutputAndInput => {
                writeln!(f, "Beans refuses to overwrite a file it is reading.")
            }
            Self::NonUtf8Content { path, error } => {
                writeln!(
                    f,
                    "Could not decode {} as valid utf-8.\n{error}",
                    path.display()
                )
            }
        }
    }
}
