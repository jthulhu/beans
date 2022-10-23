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
/// Here, the `CharLocation` of `a` is `(0, 0)`,
/// and the one of `i` is `(1, 2)`.
pub type CharLocation = (usize, usize);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn location() {
        let location =
            Location::new(Path::new("a cool filename"), (0, 3), (1, 6));
        assert_eq!(&*location.file(), Path::new("a cool filename"));
        assert_eq!(location.start(), (0, 3));
        assert_eq!(location.end(), (1, 6));
        let location = Location::new(Path::new(""), (0, 0), (0, 0));
        assert_eq!(&*location.file(), Path::new(""));
        assert_eq!(location.start(), (0, 0));
        assert_eq!(location.end(), (0, 0));
    }

    #[test]
    fn location2() {
        //                                   0         1          2           3           4
        let location_builder =	//           01234567890123456789 0 1234567 8901234567 89 01
            LocationBuilder::new(Path::new("<input>"), "if true and false {\n\tifeat(\"something\")\n}");
        assert_eq!(
            location_builder.from(0, 2),
            Location::new(Path::new("<input>"), (0, 0), (0, 2))
        );
        assert_eq!(
            location_builder.from(3, 7),
            Location::new(Path::new("<input>"), (0, 3), (0, 7))
        );
        assert_eq!(
            location_builder.from(8, 11),
            Location::new(Path::new("<input>"), (0, 8), (0, 11))
        );
        assert_eq!(
            location_builder.from(12, 17),
            Location::new(Path::new("<input>"), (0, 12), (0, 17))
        );
        assert_eq!(
            location_builder.from(18, 19),
            Location::new(Path::new("<input>"), (0, 18), (0, 19))
        );
        assert_eq!(
            location_builder.from(21, 26),
            Location::new(Path::new("<input>"), (1, 1), (1, 6))
        );
        assert_eq!(
            location_builder.from(26, 27),
            Location::new(Path::new("<input>"), (1, 6), (1, 7))
        );
        assert_eq!(
            location_builder.from(27, 38),
            Location::new(Path::new("<input>"), (1, 7), (1, 18))
        );
        assert_eq!(
            location_builder.from(38, 39),
            Location::new(Path::new("<input>"), (1, 18), (1, 19))
        );
        assert_eq!(
            location_builder.from(40, 41),
            Location::new(Path::new("<input>"), (2, 0), (2, 1))
        );
    }

    #[test]
    #[should_panic]
    fn wrong_location() {
        Location::new(Path::new("some file"), (1, 0), (0, 0));
    }
    #[test]
    #[should_panic]
    fn wrong_location2() {
        Location::new(Path::new("some file"), (1, 5), (1, 3));
    }
}

/// # Summary
///
/// Stores the location of any bit of information that is bound to a file.
/// Asks a start position (inclusive) and an end position (exclusive).
/// This means that if my chunk of data is one character long,
/// and starts at the beginning of the file `myfile`, the location
/// data bound to it is the one defined in example 1.
///
/// # Examples
///
/// Example 1
///
/// ```rust
/// # use beans::location::Location;
/// # use std::path::Path;
/// let location = Location::new(Path::new("myfile"), (0, 0), (0, 1));
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
/// # use beans::location::Location;
/// # use std::path::Path;
/// Location::new(
///   Path::new("afile"),
///   (0, 4),
///   (1, 2)
/// )
/// # ;
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    file: Rc<Path>,
    start: CharLocation,
    end: CharLocation,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "in file {}, ", self.file.display())?;
	if self.start == self.end {
	    write!(
		f,
		"at character {} of line {}",
		self.start.1,
		self.start.0+1,
	    )
	} else if self.start.0 == self.end.0 {
	    write!(
		f,
		"at characters {}-{} of line {}",
		self.start.1,
		self.end.1,
		self.start.0+1,
	    )
	} else {
            write!(
		f,
		"from character {} of line {} to character {} of line {}",
		self.start.1,
		self.start.0+1,
		self.end.1,
		self.end.0+1,
            )
	}
    }
}

impl Location {
    /// Create a new `Location` object.
    /// Require three arguments,
    ///  * file: the name of the file where the data is;
    ///  * start: the location (inclusive) of the beginning of the data;
    ///  * end: the location (exclusive) of the end of the data.
    ///
    /// Panic if start > end (lexicographic order)
    pub fn new(
        file: impl Into<Rc<Path>>,
        start: CharLocation,
        end: CharLocation,
    ) -> Self {
        assert!(start.0 < end.0 || (start.0 == end.0 && start.1 <= end.1)); // TODO: remove assert and add proper error handling.
        let file = file.into();
        Self { file, start, end }
    }

    /// Generate of new `Location` object.
    /// Take the locations as index of the stream,
    /// and convert them as actual locations in the file.
    pub fn from_stream_pos(
        file: impl Into<Rc<Path>>,
        stream: impl AsRef<str>,
        start_pos: usize,
        end_pos: usize,
    ) -> Self {
        let mut current_char = 0;
        let mut current_line = 0;
        let mut current_pos = 0;
        assert!(start_pos <= end_pos); // TODO: remove assert and add proper error handling.
        let start;
        let mut chrs = stream.as_ref().chars();
        let mut chr = chrs.next();
        loop {
            if current_pos == start_pos || chr.is_none() {
                start = (current_line, current_char);
                break;
            }
            if chr == Some('\n') {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
            chr = chrs.next();
            current_pos += 1;
        }
        let end;
        loop {
            if current_pos == end_pos || chr.is_none() {
                end = (current_line, current_char);
                break;
            }

            if chr == Some('\n') {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
            chr = chrs.next();
            current_pos += 1;
        }

        Self {
            file: file.into(),
            start,
            end,
        }
    }

    /// Merge to contiguous locations, assuming left is before right.
    pub fn extend(left: Self, right: Self) -> Self {
        Self {
            file: left.file,
            start: left.start,
            end: right.end,
        }
    }

    /// Returns the file from which the data is taken.
    pub fn file(&self) -> Rc<Path> {
        self.file.clone()
    }

    /// Returns the location of the beginning of the chunk of data in the file.
    pub fn start(&self) -> CharLocation {
        self.start
    }

    /// Returns the location of the end of the chunk of data in the file
    pub fn end(&self) -> CharLocation {
        self.end
    }
}

impl From<&Location> for Fragile<Location> {
    fn from(location: &Location) -> Fragile<Location> {
	Fragile::new(location.clone())
    }
}

/// # Summary
/// `LocationBuilder` allows building locations from a source stream faster.
#[derive(Debug)]
pub struct LocationBuilder {
    file: Rc<Path>,
    newlines: Vec<usize>,
}

impl LocationBuilder {
    /// Create a new Builder for a Location.
    pub fn new(file: impl Into<Rc<Path>>, stream: impl AsRef<str>) -> Self {
        let file = file.into();
        let newlines = stream
            .as_ref()
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '\n')
            .map(|(i, _)| i)
            .collect();

        Self { file, newlines }
    }

    /// Create a new location from two indexes.
    pub fn from(&self, start: usize, end: usize) -> Location {
        fn pos_to_char_pos(pos: usize, newlines: &[usize]) -> CharLocation {
            let i = match newlines.binary_search(&pos) {
                Ok(x) | Err(x) => x,
            };
            if i == 0 {
                (i, pos)
            } else {
                (i, pos - newlines[i - 1] - 1)
            }
        }
        Location::new(
            self.file.clone(),
            pos_to_char_pos(start, &self.newlines),
            pos_to_char_pos(end, &self.newlines),
        )
    }
}
