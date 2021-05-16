//! # Error
//!
//! This module contains error and warning related primitives.
//! The main type is [`WResult`], a wrapper to [`Result`][std::result::Result] which includes warnings.
//! As a matter of fact, `WResult<T>` is isomorphic to `Result<(T, WarningSet), Error>` where
//! [`WarningSet`] is a set of warnings and [`Error`] is the Beans' error type.

use crate::case::Case;
use crate::location::Location;
use std::collections::{linked_list, LinkedList};
use std::error;
use std::fmt;
use std::rc::Rc;

/// [`EMPTY_WARNING_SET`] is a warning set without any warnings.
/// It is useful to use this set as an empty warning set
/// instead of creating a new one each time.
const EMPTY_WARNING_SET: WarningSet = WarningSet::Empty;

/// `ErrorType` is an enum that contains all the possible errors
/// that `beans` might encounter when parsing.
///
/// # Error types
///
/// `LexerGrammarSyntax`: error that arises when there is a syntax error within the grammar file.
/// `LexingError`: error that arises when lexing.
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    /// `InternalError(message: String)`: error in the Beans implementation. This should not happen.
    InternalError(String),
    /// `SerializationError(message: String)`: error serializing.
    SerializationError(String),
    /// `Deserializationerror(message: String)`: error deserializing. This could be caused by a corrupted stream,
    /// or a stream from an other version.
    DeserializationError(String),
    /// `LexerGrammarSyntax(message: String)`: error in the syntax of the lexer grammar.
    LexerGrammarSyntax(String),
    /// `LexingError(message: String)`: error while transforming a string stream into a token stream.
    LexingError(String),
    /// `GrammarDuplicateDefinition(message: String, location: Location)`: duplicate definition at `location`.
    GrammarDuplicateDefinition(String, Location),
    /// `GrammarNonTerminalDuplicate(message: String)`: duplicate non-terminal in the grammar.
    GrammarNonTerminalDuplicate(String),
    /// `GrammarSyntaxError(message: String)`: syntax error in the grammar.
    GrammarSyntaxError(String),
}

impl Default for ErrorType {
    fn default() -> Self {
        Self::InternalError(String::from("Default error"))
    }
}

/// # Summary
///
/// Possible type of a warning.
#[derive(Debug, PartialEq, Eq)]
pub enum WarningType {
    /// `CaseConvention(message: String, expected_case: Case, found_case: Case)`
    /// Code does not respect case convention, which is bad.
    CaseConvention(Rc<String>, Case, Case),
    /// `UndefinedNonTerminal(definition: String, non_terminal: String)`
    ///
    /// Rule defined in `definition` refers to the undefined non-terminal `non_terminal`,
    /// which means it will never be constructed.
    /// This is not an error to allow users to add rules that don't work yes,
    /// as a WIP feature without preventing the compilation of the rest of the code.
    UndefinedNonTerminal(Rc<String>, Rc<String>),
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
    location: Option<Location>,
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
    pub fn with_location(warn_type: WarningType, location: Location) -> Self {
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
    pub fn location(&self) -> Option<&Location> {
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
                location.file(),
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
    pub fn extend(&mut self, warningset: WarningSet) {
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
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

impl Default for WarningSet {
    fn default() -> Self {
        Self::empty()
    }
}

/// # Summary
///
/// `WResult<T>` is a wrapper to `Result<(T, WarningSet), Error>` that allows easy manipulation
/// of the type without bothering the warnings.
#[derive(Debug, PartialEq, Eq)]
pub enum WResult<T> {
    /// `WResult` variant for `Ok`.
    WOk(T, WarningSet),
    /// `WResult` variant for `Err`.
    WErr(Error),
}

impl<T> WResult<T> {
    /// Convert to a `Result`, ignoring any warnings.
    /// `WOk(result, warnings)` maps to `Ok(result)` and `WErr(errors)` maps to `Err(errors)`.
    pub fn flatten(self) -> Result<T, Error> {
        match self {
            Self::WOk(result, _) => Ok(result),
            Self::WErr(error) => Err(error),
        }
    }

    /// Convert to a `Result`, taking into account warnings.
    /// `WOk(result, warnings)` maps to `Ok((result, warnings))` and `WErr(errors)` maps to `Err(errors)`.
    pub fn flatten_with(self) -> Result<(T, WarningSet), Error> {
        match self {
            Self::WOk(result, warnings) => Ok((result, warnings)),
            Self::WErr(error) => Err(error),
        }
    }

    /// Result whether `self` is an error, ie. is of the form `WErr(_)`.
    pub fn is_error(&self, right: Error) -> bool {
        match self {
            Self::WErr(result) => *result == right,
            _ => false,
        }
    }
}

impl<T: PartialEq> WResult<T> {
    /// Return whether `self` is `WOk` and matches the given value.
    pub fn is_value(&self, right: T) -> bool {
        match self {
            Self::WOk(result, _) => *result == right,
            _ => false,
        }
    }
}

impl<T> WResult<T> {
    /// Return `res` if the result is [`WOk`][WResult::WOk], otherwise return the [`WErr`][WResult::WErr] value of `self`.
    ///
    /// # Examples
    /// Basic usage:
    /// ```rust
    /// # use beans::{error::{WResult::{self, WOk, WErr}, WarningSet, Error, ErrorType}, location::Location};
    /// let x: WResult<u32> = WOk(2, WarningSet::default());
    /// let y: WResult<&str> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("late error"))));
    /// let z: WResult<&str> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("late error"))));
    /// assert_eq!(x.and(y), z);
    ///
    /// let x: WResult<u32> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("early error"))));
    /// let y: WResult<&str> = WOk("foo", WarningSet::default());
    /// let z: WResult<&str> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("early error"))));
    /// assert_eq!(x.and(y), z);
    ///
    /// let x: WResult<u32> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("early error"))));
    /// let y: WResult<&str> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("late error"))));
    /// let z: WResult<&str> = WErr(Error::new(Location::default(), ErrorType::InternalError(String::from("early error"))));
    /// assert_eq!(x.and(y), z);
    ///
    /// let x: WResult<u32> = WOk(2, WarningSet::default());
    /// let y: WResult<&str> = WOk("different result type", WarningSet::default());
    /// let z: WResult<&str> = WOk("different result type", WarningSet::default());
    /// assert_eq!(x.and(y), z);
    /// ```
    pub fn and<U>(self, res: WResult<U>) -> WResult<U> {
        match self {
            Self::WOk(..) => res,
            Self::WErr(error) => WResult::WErr(error),
        }
    }

    /// Call `ok_handler` if the result is [`WOk`][WResult::WOk], otherwise return the [`WErr`][WResult::WErr] value of `self`.
    ///
    /// This function can be used for control flow based on [`WResult`] values.
    ///
    /// # Examples
    /// Basic usage:
    /// ```rust
    /// # use beans::{error::{WResult::{self, WOk, WErr}, WarningSet, Error, ErrorType}, location::Location};
    /// fn sq(x: u32) -> Result<u32, Error> { Ok(x*x) }
    /// fn err(x: u32) -> Result<u32, Error> { Err(Error::new(Location::default(), ErrorType::InternalError(x.to_string()))) }
    ///
    /// assert_eq!(WOk(2, WarningSet::default()).and_then(sq).and_then(sq), WOk(16, WarningSet::default()));
    /// assert_eq!(WOk(2, WarningSet::default()).and_then(sq).and_then(err), WErr(Error::new(Location::default(), ErrorType::InternalError(4.to_string()))));
    /// assert_eq!(WOk(2, WarningSet::default()).and_then(err).and_then(sq), WErr(Error::new(Location::default(), ErrorType::InternalError(2.to_string()))));
    /// assert_eq!(WErr(Error::new(Location::default(), ErrorType::InternalError(3.to_string()))).and_then(sq).and_then(sq), WErr(Error::new(Location::default(), ErrorType::InternalError(3.to_string()))));
    /// ```
    pub fn and_then<F, U>(self, ok_handler: F) -> WResult<U>
    where
        F: FnOnce(T) -> Result<U, Error>,
    {
        match self {
            Self::WOk(result, warnings) => match ok_handler(result) {
                Ok(res) => WResult::WOk(res, warnings),
                Err(error) => WResult::WErr(error),
            },
            Self::WErr(error) => WResult::WErr(error),
        }
    }

    /// Call `ok_handler` if the result is [`WOk`][WResult::WOk], otherwise return the [`WErr`][WResult::WErr] value of `self`.
    ///
    /// This function can be used for control flow based on [`WResult`] values.
    ///
    /// # Examples
    /// Basic usage:
    /// ```rust
    /// # use beans::{error::{WResult::{self, WOk, WErr}, WarningSet, Error, ErrorType}, location::Location};
    /// fn sq(x: u32, ws: WarningSet) -> WResult<u32> { WOk(x*x, ws) }
    /// fn err(x: u32, _: WarningSet) -> WResult<u32> { WErr(Error::new(Location::default(), ErrorType::InternalError(x.to_string()))) }
    ///
    /// assert_eq!(WOk(2, WarningSet::default()).and_then_warn(sq).and_then_warn(sq), WOk(16, WarningSet::default()));
    /// assert_eq!(WOk(2, WarningSet::default()).and_then_warn(sq).and_then_warn(err), WErr(Error::new(Location::default(), ErrorType::InternalError(4.to_string()))));
    /// assert_eq!(WOk(2, WarningSet::default()).and_then_warn(err).and_then_warn(sq), WErr(Error::new(Location::default(), ErrorType::InternalError(2.to_string()))));
    /// assert_eq!(WErr(Error::new(Location::default(), ErrorType::InternalError(3.to_string()))).and_then_warn(sq).and_then_warn(sq), WErr(Error::new(Location::default(), ErrorType::InternalError(3.to_string()))));
    /// ```
    pub fn and_then_warn<U, F>(self, ok_handler: F) -> WResult<U>
    where
        F: FnOnce(T, WarningSet) -> WResult<U>,
    {
        match self {
            Self::WOk(result, warnings) => ok_handler(result, warnings),
            Self::WErr(error) => WResult::WErr(error),
        }
    }

    /// Unwrap the result, panicking with the given message if it was an error, and returning `(result, warnings)` otherwise.
    pub fn expect(self, msg: &str) -> (T, WarningSet) {
        match self {
            Self::WOk(result, warnings) => (result, warnings),
            Self::WErr(error) => panic!("{}: {}", msg, error),
        }
    }

    /// Return whether the result is an error.
    pub fn is_err(&self) -> bool {
        matches!(self, Self::WErr(..))
    }

    /// Return whether the result is ok.
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::WOk(..))
    }

    /// Map the content of the result, ignoring the warnings and the error.
    /// ```rust
    /// # use beans::error::{WResult::{WOk, WErr}, WarningSet};
    /// assert_eq!(WOk(5, WarningSet::default()).map(|x| x+1), WOk(6, WarningSet::default()));
    /// ```
    pub fn map<U, O>(self, f: O) -> WResult<U>
    where
        O: FnOnce(T) -> U,
    {
        match self {
            Self::WOk(result, warnings) => WResult::WOk(f(result), warnings),
            Self::WErr(error) => WResult::WErr(error),
        }
    }

    /// Map the content and the warnings of the result, ignoring the error.
    /// ```rust
    /// # use beans::error::{WResult::{WOk, WErr}, WarningSet, WarningType, Warning};
    /// assert_eq!(
    ///   WOk(5, WarningSet::default())
    ///     .map_warn(
    ///       |content, mut warnings| {
    ///         warnings.add(Warning::new(WarningType::NullWarning));
    ///         (content, warnings)
    ///       }
    ///     ),
    ///   WOk(5, {
    ///       let mut warnings = WarningSet::default();
    ///       warnings.add(Warning::new(WarningType::NullWarning));
    ///       warnings
    ///     }
    ///   )
    /// );
    /// ```
    pub fn map_warn<U, O>(self, ok_handler: O) -> WResult<U>
    where
        O: FnOnce(T, WarningSet) -> (U, WarningSet),
    {
        match self {
            Self::WOk(result, warnings) => {
                let (content, warnings) = ok_handler(result, warnings);
                WResult::WOk(content, warnings)
            }
            Self::WErr(error) => WResult::WErr(error),
        }
    }

    /// Map the error of the result, ignoring the content and the warnings.
    pub fn map_err<E>(self, f: E) -> Self
    where
        E: FnOnce(Error) -> Error,
    {
        match self {
            Self::WOk(..) => self,
            Self::WErr(error) => Self::WErr(f(error)),
        }
    }

    /// Apply a function to the contained value (if [`WOk`][WResult::WOk]), or return
    /// the provided default (if [`WErr`][WResult::WErr]).
    ///
    /// Arguments passed to `map_or` are eagerly evaluated; if you are passing the result of a function call,
    /// it is recommended to use [`map_or_else`][WResult::map_or_else], which is lazily evaluated.
    pub fn map_or<U, O>(self, default: (U, WarningSet), ok_handler: O) -> (U, WarningSet)
    where
        O: FnOnce(T) -> U,
    {
        match self {
            Self::WOk(result, warnings) => (ok_handler(result), warnings),
            Self::WErr(..) => default,
        }
    }

    /// Apply a function to the contained value and the warnings (if [`WOk`][WResult::WOk]),
    /// or return the provided default (if [`WErr`][WResult::WErr]).
    ///
    /// Arguments passed to `map_or_warn` are eagerly evaluated; if you are passing the result of a function call,
    /// it is recommended to use [`map_or_else_warn`][WResult::map_or_else_warn], which is lazily evaluated.
    pub fn map_or_warn<U, O>(self, default: (U, WarningSet), ok_handler: O) -> (U, WarningSet)
    where
        O: FnOnce(T, WarningSet) -> (U, WarningSet),
    {
        match self {
            Self::WOk(result, warnings) => ok_handler(result, warnings),
            Self::WErr(..) => default,
        }
    }

    pub fn map_or_else<U, E, O>(self, err_handler: E, ok_handler: O) -> (U, WarningSet)
    where
        E: FnOnce(Error) -> (U, WarningSet),
        O: FnOnce(T) -> U,
    {
        match self {
            Self::WOk(result, warnings) => (ok_handler(result), warnings),
            Self::WErr(error) => err_handler(error),
        }
    }

    pub fn map_or_else_warn<U, E, O>(self, err_handler: E, ok_handler: O) -> (U, WarningSet)
    where
        E: FnOnce(Error) -> (U, WarningSet),
        O: FnOnce(T, WarningSet) -> (U, WarningSet),
    {
        match self {
            Self::WOk(result, warnings) => ok_handler(result, warnings),
            Self::WErr(error) => err_handler(error),
        }
    }

    pub fn or(self, res: Self) -> Self {
        match self {
            Self::WOk(..) => self,
            Self::WErr(..) => res,
        }
    }

    pub fn or_else<E>(self, err_handler: E) -> Self
    where
        E: FnOnce(Error) -> Self,
    {
        match self {
            Self::WOk(..) => self,
            Self::WErr(error) => err_handler(error),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::WOk(result, _) => result,
            Self::WErr(error) => panic!("called `WResult::unwrap` on an `WErr` value: {}", error),
        }
    }

    pub fn unwrap_warns(self) -> (T, WarningSet) {
        match self {
            Self::WOk(result, warnings) => (result, warnings),
            Self::WErr(error) => panic!(
                "called `WResult::unwrap_warns` on an `WErr` value: {}",
                error
            ),
        }
    }

    pub fn unwrap_or(self, default: (T, WarningSet)) -> (T, WarningSet) {
        match self {
            Self::WOk(result, warnings) => (result, warnings),
            Self::WErr(..) => default,
        }
    }

    pub fn unwrap_or_else<E>(self, err_handler: E) -> (T, WarningSet)
    where
        E: FnOnce(Error) -> (T, WarningSet),
    {
        match self {
            Self::WOk(result, warnings) => (result, warnings),
            Self::WErr(error) => err_handler(error),
        }
    }
}

impl<T> From<WResult<T>> for Result<(T, WarningSet), Error> {
    fn from(wresult: WResult<T>) -> Self {
        wresult.flatten_with()
    }
}

impl<T> From<Result<(T, WarningSet), Error>> for WResult<T> {
    fn from(result: Result<(T, WarningSet), Error>) -> Self {
        match result {
            Ok((res, warnings)) => Self::WOk(res, warnings),
            Err(error) => Self::WErr(error),
        }
    }
}

impl<T> From<Result<T, Error>> for WResult<T> {
    fn from(result: Result<T, Error>) -> Self {
        match result {
            Ok(res) => Self::WOk(res, WarningSet::empty()),
            Err(error) => Self::WErr(error),
        }
    }
}

/// # Summary
///
/// `Error` is a type representing all information required
/// about a given error. It is a tuple containing a `Location`
/// and an `ErrorType`.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Error {
    location: Location,
    err_type: ErrorType,
}

impl Error {
    pub fn new(location: Location, err_type: ErrorType) -> Self {
        Self { location, err_type }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let (type_, msg) = match &self.err_type {
            LexerGrammarSyntax(msg) => ("Syntax error within the lexer's grammar", msg.clone()),
            LexingError(msg) => ("Error while lexing", msg.clone()),
            InternalError(msg) => ("Internal error, this should not happend", msg.clone()),
            SerializationError(msg) => ("Internal error, failed to serialize", msg.clone()),
            DeserializationError(msg) => ("Internal error, failed to deserialize", msg.clone()),
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
            GrammarNonTerminalDuplicate(msg) => {
                ("Duplicate definition of a non terminal", msg.clone())
            }
        };
        write!(
            f,
            "{}\n @{}, from {}:{} to {}:{}\n{}",
            type_,
            self.location.file(),
            self.location.start().0,
            self.location.start().1,
            self.location.end().0,
            self.location.end().1,
            msg
        )
    }
}

impl error::Error for Error {}
