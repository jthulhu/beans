//! # Location
//!
//! Data to locate span of text, in files.
//! The main struct is [`Location`].

use std::{path::Path, rc::Rc};

use fragile::Fragile;

/// # Summary
///
/// Information about a position in a file,
/// stored as `(line, char_position)`
///
/// # Example
///
/// ```text
/// abc def
/// ghi
/// ```
///
/// Here, the `Location` of `a` is `(0, 0)`,
/// and the one of `i` is `(1, 2)`.
pub type Location = (usize, usize);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn location() {
        let input: Rc<str>= Rc::from(
            "01234
56789abcdef",
        );
        let span = Span::new(
            Path::new("a cool filename"),
            (0, 3),
            (1, 6),
            3,
            11,
            input.clone(),
        );
        assert_eq!(&*span.file(), Path::new("a cool filename"));
        assert_eq!(span.start(), (0, 3));
        assert_eq!(span.end(), (1, 6));
        assert_eq!(span.start_byte(), 3);
        assert_eq!(span.end_byte(), 11);
        let span = Span::new(Path::new(""), (0, 0), (0, 0), 0, 0, input);
        assert_eq!(&*span.file(), Path::new(""));
        assert_eq!(span.start(), (0, 0));
        assert_eq!(span.end(), (0, 0));
        assert_eq!(span.start_byte(), 0);
        assert_eq!(span.end_byte(), 0);
    }

    #[test]
    #[should_panic]
    fn wrong_location() {
        Span::new(Path::new("some file"), (1, 0), (0, 0), 1, 0, Rc::from(""));
    }
    #[test]
    #[should_panic]
    fn wrong_location2() {
        Span::new(Path::new("some file"), (1, 5), (1, 3), 8, 6, Rc::from(""));
    }
}

/// # Summary
///
/// Stores the location of any bit of information that is bound to a file.
/// Asks a start position (inclusive) and an end position (inclusive).
/// This means that if my chunk of data is one character long,
/// and starts at the beginning of the file `myfile`, the location
/// data bound to it is the one defined in example 1.
///
/// # Examples
///
/// Example 1
///
/// ```rust
/// # use beans::span::Span;
/// # use std::path::Path;
/// let span = Span::new(
///     Path::new("myfile"),
///     (0, 0),
///     (0, 0),
///     0,
///     0,
///     "a"
/// );
/// ```
///
/// Example 2 -- `afile`
///
/// ```text
/// abc def
/// ghi
/// ```
///
/// Here, the location of `c def/gh` is
///
/// ```rust
/// # use beans::span::Span;
/// # use std::path::Path;
/// Span::new(
///   Path::new("afile"),
///   (0, 4),
///   (1, 2),
///   4,
///   8,
///   "abcde\nbel",
/// )
/// # ;
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    file: Rc<Path>,
    start: Location,
    end: Location,
    start_byte: usize,
    end_byte: usize,
    text: Rc<str>,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "in file {}, ", self.file.display())?;
        if self.start.0 == self.end.0 {
            if self.start.1 + 1 == self.end.1 {
                write!(
                    f,
                    "at character {} of line {}",
                    self.start.1,
                    self.start.0 + 1,
                )
            } else {
                write!(
                    f,
                    "at characters {}-{} of line {}",
                    self.start.1,
                    self.end.1.checked_sub(1).unwrap_or_default(),
                    self.start.0 + 1,
                )
            }
        } else {
            write!(
                f,
                "from character {} of line {} to character {} of line {}",
                self.start.1,
                self.start.0 + 1,
                self.end.1.checked_sub(1).unwrap_or_default(),
                self.end.0 + 1,
            )
        }
    }
}

impl Span {
    /// Create a new `Location` object.
    /// Require three arguments,
    ///  * file: the name of the file where the data is;
    ///  * start: the location (inclusive) of the beginning of the data;
    ///  * end: the location (exclusive) of the end of the data.
    ///
    /// Panic if start > end (lexicographic order)
    pub fn new(
        file: impl Into<Rc<Path>>,
        start: Location,
        end: Location,
        start_byte: usize,
        end_byte: usize,
        text: impl Into<Rc<str>>,
    ) -> Self {
        assert!(start.0 < end.0 || (start.0 == end.0 && start.1 <= end.1)); // TODO: remove assert and add proper error handling.
        let file = file.into();
        let text = text.into();
        Self {
            file,
            start,
            end,
            start_byte,
            end_byte,
            text,
        }
    }

    pub fn sup(&self, other: &Self) -> Self {
        Self {
            file: self.file.clone(),
            start_byte: self.start_byte.min(other.start_byte),
            end_byte: self.end_byte.max(other.end_byte),
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            text: self.text.clone(),
        }
    }

    /// Returns the file from which the data is taken.
    pub fn file(&self) -> Rc<Path> {
        self.file.clone()
    }

    /// Returns the location of the beginning of the chunk of data in the file.
    pub fn start(&self) -> Location {
        self.start
    }

    /// Returns the location of the end of the chunk of data in the file
    pub fn end(&self) -> Location {
        self.end
    }

    pub fn start_byte(&self) -> usize {
        self.start_byte
    }

    pub fn end_byte(&self) -> usize {
        self.end_byte
    }

    pub fn test(&self) -> &str {
	&*self.text
    }
}

impl From<&Span> for Fragile<Span> {
    fn from(location: &Span) -> Fragile<Span> {
        Fragile::new(location.clone())
    }
}
