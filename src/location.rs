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
        let location = Location::new(String::from("a cool filename"), (0, 3), (1, 6));
        assert_eq!(location.file(), "a cool filename");
        assert_eq!(location.start(), (0, 3));
        assert_eq!(location.end(), (1, 6));
        let location = Location::new(String::from(""), (0, 0), (0, 0));
        assert_eq!(location.file(), "");
        assert_eq!(location.start(), (0, 0));
        assert_eq!(location.end(), (0, 0));
    }
    #[test]
    #[should_panic]
    fn wrong_location() {
        Location::new(String::from("some file"), (1, 0), (0, 0));
    }
    #[test]
    #[should_panic]
    fn wrong_location2() {
        Location::new(String::from("some file"), (1, 5), (1, 3));
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
/// let location = Location::new(String::from("myfile"), (0, 0), (0, 1));
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
/// Location::new(
///   String::from("afile"),
///   (0, 4),
///   (1, 2)
/// )
/// # ;
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    file: String,
    start: CharLocation,
    end: CharLocation,
}

impl Location {
    /// Create a new `Location` object.
    /// Require three arguments,
    ///  * file: the name of the file where the data is;
    ///  * start: the location (inclusive) of the beginning of the data;
    ///  * end: the location (exclusive) of the end of the data.
    ///
    /// Panic if start > end (lexicographic order)
    pub fn new(file: String, start: CharLocation, end: CharLocation) -> Self {
        assert!(start.0 < end.0 || (start.0 == end.0 && start.1 <= end.1));
        Self { file, start, end }
    }

    /// Generate of new `Location` object.
    /// Take the locations as index of the stream,
    /// and convert them as actual locations in the file.
    pub fn from_stream_pos(file: String, stream: &str, start_pos: usize, end_pos: usize) -> Self {
        let mut current_char = 0;
        let mut current_line = 0;
        let mut current_pos = 0;
        assert!(start_pos <= end_pos);
        let start;
        let mut chrs = stream.chars();
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

        Self { file, start, end }
    }

    pub fn extend(left: Self, right: Self) -> Self {
        Self {
            file: left.file,
            start: left.start,
            end: right.end,
        }
    }

    /// Returns the file from which the data is taken.
    pub fn file(&self) -> &str {
        &self.file[..]
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

pub struct LocationBuilder {
    file: String,
    stream: String
}

impl LocationBuilder {
    pub fn new(file: String, stream: String) -> Self {
	Self {
	    file,
	    stream
	}
    }

    pub fn from(&self, start: usize, end: usize) -> Location {
	Location::from_stream_pos(self.file.clone(), &self.stream, start, end)
    }
}
