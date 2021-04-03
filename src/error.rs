use crate::case::Case;
use crate::location::Location;
use std::collections::LinkedList;
use std::error;
use std::fmt;
use std::io;

pub const EMPTY_WARNING_SET: WarningSet = WarningSet::Empty;

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
    CaseConvention(String, Case, Case),
}

#[derive(Debug)]
pub struct Warning {
    location: Option<Location>,
    warn_type: WarningType,
}

impl Warning {
    pub fn new(warn_type: WarningType) -> Self {
        Self {
            warn_type,
            location: None,
        }
    }
    pub fn with_location(warn_type: WarningType, location: Location) -> Self {
        Self {
            warn_type,
            location: Some(location),
        }
    }
    pub fn warn_type(&self) -> &WarningType {
        &self.warn_type
    }
    pub fn location(&self) -> Option<&Location> {
        self.location.as_ref()
    }
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use WarningType::*;
        let (type_, msg) = match &self.warn_type {
            CaseConvention(msg, case_found, case_expected) => ("Case convention warning", ""),
        };
        if let Some(location) = self.location.as_ref() {
            write!(
                f,
                "{}\n @{}, from {}:{} to {}:{}\n{}",
                type_,
                location.file(),
                location.start().0,
                location.start().1,
                location.end().0,
                location.end().1,
                msg
            )
        } else {
            write!(f, "{}\n{}", type_, msg)
        }
    }
}

pub enum WarningSet {
    Set(LinkedList<Warning>),
    Empty,
}

impl WarningSet {
    #[inline]
    pub fn empty() -> Self {
        EMPTY_WARNING_SET
    }
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
    pub fn iter(&self) -> Box<dyn Iterator<Item = &Warning> + '_> {
        match self {
            Self::Set(warnings) => Box::new(warnings.iter()),
            Self::Empty => Box::new(std::iter::empty()),
        }
    }
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
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            _ => false,
        }
    }
}

pub enum WResult<T> {
    WOk(T, WarningSet),
    WErr(Error),
}

impl<T> WResult<T> {
    pub fn flatten(self) -> Result<T, Error> {
        match self {
            Self::WOk(result, _) => Ok(result),
            Self::WErr(error) => Err(error),
        }
    }
    pub fn flatten_with(self) -> Result<(T, WarningSet), Error> {
        match self {
            Self::WOk(result, warnings) => Ok((result, warnings)),
            Self::WErr(error) => Err(error),
        }
    }
    pub fn is_error(self, right: Error) -> bool {
        match self {
            Self::WErr(result) => result == right,
            _ => false,
        }
    }
}

impl<T: PartialEq> WResult<T> {
    pub fn is_value(self, right: T) -> bool {
        match self {
            Self::WOk(result, _) => result == right,
            _ => false,
        }
    }
}

impl<T> WResult<T> {
    pub fn and(self, res: WResult<T>) -> WResult<T> {
        match self {
            Self::WOk(..) => res,
            Self::WErr(..) => self,
        }
    }
    pub fn and_then<O>(self, ok_handler: O) -> Self
    where
        O: FnOnce(T) -> T,
    {
        match self {
            Self::WOk(result, warnings) => Self::WOk(ok_handler(result), warnings),
            Self::WErr(..) => self,
        }
    }
    pub fn and_then_warn<O>(self, ok_handler: O) -> Self
    where
        O: FnOnce(T, WarningSet) -> Self,
    {
        match self {
            Self::WOk(result, warnings) => ok_handler(result, warnings),
            Self::WErr(..) => self,
        }
    }
    pub fn expect(self, msg: &str) -> (T, WarningSet) {
        match self {
            Self::WOk(result, warning) => (result, warning),
            Self::WErr(error) => panic!("{}: {}", msg, error),
        }
    }
    pub fn is_err(&self) -> bool {
        match self {
            Self::WErr(..) => true,
            _ => false,
        }
    }
    pub fn is_ok(&self) -> bool {
        match self {
            Self::WOk(..) => true,
            _ => false,
        }
    }
    pub fn map<U, O>(self, f: O) -> WResult<U>
    where
        O: FnOnce(T) -> U,
    {
        match self {
            Self::WOk(result, warnings) => WResult::WOk(f(result), warnings),
            Self::WErr(error) => WResult::WErr(error),
        }
    }
    pub fn map_warn<U, O>(self, ok_handler: O) -> WResult<U>
    where
        O: FnOnce(T, WarningSet) -> WResult<U>,
    {
        match self {
            Self::WOk(result, warnings) => ok_handler(result, warnings),
            Self::WErr(error) => WResult::WErr(error),
        }
    }
    pub fn map_err<E>(self, f: E) -> Self
    where
        E: FnOnce(Error) -> Error,
    {
        match self {
            Self::WOk(..) => self,
            Self::WErr(error) => Self::WErr(f(error)),
        }
    }
    pub fn map_or<U, O>(self, default: (U, WarningSet), ok_handler: O) -> (U, WarningSet)
    where
        O: FnOnce(T) -> U,
    {
        match self {
            Self::WOk(result, warnings) => (ok_handler(result), warnings),
            Self::WErr(..) => default,
        }
    }
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
    fn from(wresult: WResult<T>) -> Result<(T, WarningSet), Error> {
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
#[derive(Debug, PartialEq)]
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
