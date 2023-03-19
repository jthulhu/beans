//! # Error
//!
//! This module contains error and warning related primitives.
//! The main type is [`WResult`], a wrapper to [`Result`][std::result::Result] which includes warnings.
//! As a matter of fact, `WResult<T>` is isomorphic to `Result<(T, WarningSet), Error>` where
//! [`WarningSet`] is a set of warnings and [`Error`] is the Beans' error type.

use crate::case::Case;
use crate::span::Span;
use fragile::Fragile;
use std::collections::{linked_list, LinkedList};
use std::ffi::OsString;
use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;
use std::rc::Rc;
use std::string::FromUtf8Error;

/// [`EMPTY_WARNING_SET`] is a warning set without any warnings.
/// It is useful to use this set as an empty warning set
/// instead of creating a new one each time.
const EMPTY_WARNING_SET: WarningSet = WarningSet::Empty;

/// `Result` is a shorthand for the usual `Result` type.
/// It includes the crate's `Error` by default, and is inside `WithWarnings`
/// monad.
pub type Result<T> = std::result::Result<WithWarnings<T>, Error>;

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

    pub fn integer_too_big(string: String) -> Self {
        Self::new(ErrorKind::IntegerTooBig { string })
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
    },
    SerializationError(#[from] bincode::Error),
    IllformedAst {
        error: serde_json::Error,
        file: PathBuf,
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
        error: FromUtf8Error,
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
        /// The `Location` that made the error occur. It's a hint a what should
        /// be patched.
        span: Fragile<Span>,
    },
    UnwantedToken {
        span: Fragile<Span>,
        message: String,
    },
    GrammarDuplicateDefinition {
        message: String,
        span: Fragile<Span>,
        old_span: Fragile<Span>,
    },
    GrammarArityMismatch {
        macro_name: String,
        first_arity: usize,
        second_arity: usize,
        span: Fragile<Span>,
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

impl From<(PathBuf, FromUtf8Error)> for ErrorKind {
    fn from((path, error): (PathBuf, FromUtf8Error)) -> Self {
        Self::NonUtf8Content { path, error }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InternalError { message } => writeln!(f, "ICE: {message}"),
            Self::SerializationError(error) => writeln!(
                f,
                "ICE: while trying to serialize, the following error occured\n{error}",
            ),
            Self::IllformedAst { error, file } => writeln!(
                f,
                "File {} contains an illformed AST.\n{error}",
                file.display(),
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
                message,
                span,
                old_span,
            } => {
                writeln!(
		    f,
		    "Found two definitions of the same non-terminal {span} and {old_span}.\n{message}"
		)
            }
            Self::GrammarArityMismatch {
                macro_name,
                first_arity,
                second_arity,
                span,
            } => {
                writeln!(f, "The macro {macro_name} is called with the wrong number of argument {first_arity} (expected {second_arity}) {span}.")
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
            Self::IntegerTooBig { string } => {
                writeln!(f, "Integer {string} does not fit on a 64 bit integer. Why do you even need such a number?")
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

/// # Summary
///
/// Possible type of a warning.
#[derive(Debug, PartialEq, Eq)]
pub enum WarningType {
    /// `CaseConvention(message: String, expected_case: Case, found_case: Case)`
    /// Code does not respect case convention, which is bad.
    CaseConvention(Rc<str>, Case, Case),
    /// `UndefinedNonTerminal(definition: String, non_terminal: String)`
    ///
    /// Rule defined in `definition` refers to the undefined non-terminal `non_terminal`,
    /// which means it will never be constructed.
    /// This is not an error to allow users to add rules that don't work yes,
    /// as a WIP feature without preventing the compilation of the rest of the code.
    UndefinedNonTerminal(Rc<str>, Rc<str>),
    /// NullWarning
    /// Empty warning that is used only in example. It does not mean anything besides
    /// there is a warning. Please consider using a more excplicit warning type in real code.
    NullWarning,
}

/// # Summary
///
/// `Warning` is a non-fatal error.
#[derive(Debug, PartialEq, Eq)]
pub struct Warning {
    location: Option<Span>,
    warn_type: WarningType,
}

impl Warning {
    /// Build a new warning of type `warn_type`.
    pub fn new(warn_type: WarningType) -> Self {
        Self {
            warn_type,
            location: None,
        }
    }
    /// Build a new warning of type `warn_type` with `location`.
    pub fn with_location(warn_type: WarningType, location: Span) -> Self {
        Self {
            warn_type,
            location: Some(location),
        }
    }

    /// Get the type of the warning.
    pub fn warn_type(&self) -> &WarningType {
        &self.warn_type
    }

    /// Get the optional location of the warning.
    pub fn location(&self) -> Option<&Span> {
        self.location.as_ref()
    }
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use WarningType::*;
        let r#type;
        let message: Box<dyn AsRef<str>>;
        match &self.warn_type {
            CaseConvention(msg, _case_found, _case_expected) => {
                r#type = "Case convention warning";
                message = Box::new(msg.as_ref());
            }
            UndefinedNonTerminal(origin, non_terminal) => {
                r#type = "Undefined non-terminal warning";
                message = Box::new(format!(
                    "In definition of non-terminal `{}', `{}' has been used but not defined",
                    origin, non_terminal
                ));
            }
            NullWarning => {
                r#type = "Warning";
                message = Box::new("Empty warning");
            }
        };
        if let Some(location) = self.location.as_ref() {
            write!(
                f,
                "{}\n @{}, from {}:{} to {}:{}\n{}",
                r#type,
                location.file().display(),
                location.start().0,
                location.start().1,
                location.end().0,
                location.end().1,
                (*message).as_ref()
            )
        } else {
            write!(f, "{}\n{}", r#type, (*message).as_ref())
        }
    }
}

/// # Summary
///
/// Iterator over a warning set.
#[derive(Debug)]
pub struct WarningIterator<'iter> {
    warnings: Option<linked_list::Iter<'iter, Warning>>,
}

impl<'iter> WarningIterator<'iter> {
    fn new(warnings: Option<linked_list::Iter<'iter, Warning>>) -> Self {
        Self { warnings }
    }
}

impl<'iter> Iterator for WarningIterator<'iter> {
    type Item = &'iter Warning;
    fn next(&mut self) -> Option<Self::Item> {
        self.warnings.as_mut().and_then(|iterator| iterator.next())
    }
}

/// # Summary
///
/// [`WarningSet`] is an abstract type for a [`Warning`] collection.
/// It should optimize empty creation (because most of the code will build a warning set without using it).
/// It should also allow constant time union (or at least fast union) of two sets.
#[derive(Debug, PartialEq, Eq)]
pub enum WarningSet {
    /// A set of warnings that actually contains some warnings.
    Set(LinkedList<Warning>),
    /// An empty set of warnings.
    Empty,
}

impl WarningSet {
    /// Create an empty set.
    #[inline]
    pub fn empty() -> Self {
        EMPTY_WARNING_SET
    }

    /// Add a warning to the set.
    pub fn add(&mut self, warning: Warning) {
        match self {
            Self::Set(warnings) => warnings.push_back(warning),
            Self::Empty => {
                let mut ll = LinkedList::new();
                ll.push_back(warning);
                *self = Self::Set(ll);
            }
        }
    }

    /// Iterate over warnings in the set.
    pub fn iter(&self) -> WarningIterator<'_> {
        WarningIterator::new(match self {
            Self::Set(warnings) => Some(warnings.iter()),
            Self::Empty => None,
        })
    }

    /// Unify two warning sets. `self` gets updated, and consumes the other warning set.
    pub fn extend(&mut self, warningset: Self) {
        match self {
            Self::Set(selfwarnings) => match warningset {
                Self::Set(mut otherwarnings) => selfwarnings.append(&mut otherwarnings),
                Self::Empty => {}
            },
            Self::Empty => match warningset {
                Self::Set(otherwarnings) => *self = Self::Set(otherwarnings),
                Self::Empty => {}
            },
        }
    }

    /// Return whether the warning set is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    /// Utilitary function to ease the usage of regular error types.
    #[inline]
    pub fn unpack<T>(&mut self, WithWarnings { content, warnings }: WithWarnings<T>) -> T {
        self.extend(warnings);
        content
    }

    /// Constructor of the `WithWarnings` monad.
    #[inline]
    pub fn with<T>(self, content: T) -> WithWarnings<T> {
        WithWarnings {
            content,
            warnings: self,
        }
    }

    #[inline]
    pub fn with_ok<T, E>(self, content: T) -> std::result::Result<WithWarnings<T>, E> {
        Ok(self.with(content))
    }

    /// Useful constructor of the `WithWarnings` monad when you don't have any
    /// warnings.
    #[inline]
    pub fn empty_with<T>(content: T) -> WithWarnings<T> {
        Self::empty().with(content)
    }

    #[inline]
    pub fn empty_with_ok<T>(content: T) -> Result<T> {
        Ok(Self::empty().with(content))
    }

    /// `on` provides a shorthand by conjuging `f` with `with` and `unpack`.
    #[inline]
    pub fn on<T, F, U>(mut self, content: WithWarnings<T>, f: F) -> WithWarnings<U>
    where
        F: FnOnce(T) -> U,
    {
        let u = f(self.unpack(content));
        self.with(u)
    }
}

impl Default for WarningSet {
    fn default() -> Self {
        Self::empty()
    }
}

/// `WithWarnings` is a handy monad to carry warnings around.
/// The idea is that instead of triggering a warning when the offeding action
/// is executed, data carries with it warnings that were thrown to produce that
/// data.
/// It simplifies tracking exactly what caused certain warnings, what warnings
/// the creation and manipulation of some data caused, and interacts nicely
/// with the `Result` monad.
#[derive(Debug)]
#[must_use]
pub struct WithWarnings<T> {
    content: T,
    warnings: WarningSet,
}

impl<T> WithWarnings<T> {
    /// `unwrap` is a brutal way to get out of the monad.
    /// It means you can ensure there are no warnings.
    /// Note that this will **not** panic if there are warnings.
    #[inline]
    pub fn unwrap(self) -> T {
        self.content
    }

    /// `chain` is to be used as the composition functor
    /// of the WithWarnings monad.
    /// It can be ended either with `unpack_into`, if you want
    /// to retrieve only the value, kept as-is, or with `with` if you want
    /// to stay in the monad (useful in return statements).
    #[inline]
    pub fn chain<F, U>(self, f: F) -> WithWarnings<U>
    where
        F: FnOnce(T) -> WithWarnings<U>,
    {
        let WithWarnings {
            content,
            mut warnings,
        } = f(self.content);
        warnings.extend(self.warnings);
        WithWarnings { content, warnings }
    }

    /// `unpack_into` is the symetrical operator of `unpack` for `WarningSet`,
    /// but applied from WithWarnings. There are no differences, it's only
    /// for practical reasons.
    #[inline]
    pub fn unpack_into(self, warnings: &mut WarningSet) -> T {
        warnings.unpack(self)
    }

    /// `with` does the same job that `WarningSet`, but this one is an external
    /// operation in the monad, whereas the other one is a constructor.
    #[inline]
    pub fn with(mut self, warnings: WarningSet) -> Self {
        self.warnings.extend(warnings);
        self
    }

    /// `with_ok` is `with` wrapped in `Ok`.
    #[inline]
    pub fn with_ok<E>(self, warnings: WarningSet) -> std::result::Result<WithWarnings<T>, E> {
        Ok(self.with(warnings))
    }

    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithWarnings<U> {
        WithWarnings {
            content: f(self.content),
            warnings: self.warnings,
        }
    }
}
