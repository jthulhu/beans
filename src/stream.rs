use crate::error::{Error, Result, WarningSet};
use crate::span::{Location, Span};
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::rc::Rc;
use std::result::Result as StdResult;

#[derive(Debug)]
pub struct RawStream {
    origin: Rc<Path>,
    stream: Vec<u8>,
}

impl RawStream {
    /// Creates a new `RowStream` a path, and the content of that file
    /// as bytes.
    pub fn from_bytes(path: impl Into<Rc<Path>>, bytes: Vec<u8>) -> Self {
	Self {
	    origin: path.into(),
	    stream: bytes,
	}
    }
    pub fn read_from_path(path: impl Into<Rc<Path>>) -> Result<Self> {
        let path = path.into();
        let mut file_stream = File::open(path.as_ref())
            .map_err(|error| Error::with_file(error, &*path))?;
        let mut stream_buffer = Vec::new();
        file_stream
            .read_to_end(&mut stream_buffer)
            .map_err(|error| Error::with_file(error, &*path))?;
        WarningSet::empty().with_ok(Self {
            origin: path,
            stream: stream_buffer,
        })
    }
}

impl TryFrom<RawStream> for StringStream {
    type Error = Error;

    fn try_from(value: RawStream) -> StdResult<Self, Self::Error> {
        let string = String::from_utf8(value.stream)
            .map_err(|error| Error::with_file(error, &*value.origin))?;
	Ok(StringStream::new(value.origin, string))
    }
}

/// # Summary
///
/// A character, or `EOF`.
///
/// # Variants
///
/// `Char(char)`: a character.
/// `EOF`: End Of File.
#[derive(Debug)]
pub enum Char {
    /// A character
    Char(char),
    /// End Of File
    EOF,
}

/// # Summary
///
/// A stream based on a string, considered as a file-like object.
/// Thus, a `StringStream` object requires an `origin`.
///
/// # Methods
/// `new`: build a `StringStream`.
/// `continues`: returns if the substring that starts at the current position matches the given one.
/// `borrow`: borrows (read-only) the stream as a string slice
/// `len`: the size of the stream
/// `is_empty`: whether the stream is empty
pub struct StringStream {
    origin: Rc<Path>,
    // Stores, for each character, its span and its size.
    spans: Vec<CharSpan>,
    stream: Rc<str>,
    lines: Rc<[usize]>,
    bytes_pos: usize,
    chars_pos: usize,
    length: usize,
    eof_span: Span,
}

struct CharSpan {
    location: Location,
    byte_location: usize,
    size: usize,
}

impl StringStream {
    /// Build a new `StringStream`, based on its `origin` and on a given `string`.
    pub fn new(
        origin: impl Into<Rc<Path>>,
        string: impl Into<Rc<str>>,
    ) -> Self {
        let origin = origin.into();
        let string = string.into();
        let mut current_char = 0;
        let mut current_line = 0;
        let mut spans = Vec::new();
        let mut current_byte = 0;
        let mut lines = vec![0];
        for chr in string.chars() {
            let start_pos = (current_line, current_char);
            spans.push(CharSpan {
                location: start_pos,
                byte_location: current_byte,
                size: chr.len_utf8(),
            });
            current_byte += chr.len_utf8();
            if chr == '\n' {
                current_line += 1;
                current_char = 0;
                lines.push(current_byte);
            } else {
                current_char += 1;
            }
        }
        let lines: Rc<[usize]> = Rc::from(lines);
        Self {
            origin: origin.clone(),
            length: spans.len(),
            stream: string.clone(),
            spans,
            lines: lines.clone(),
            bytes_pos: 0,
            chars_pos: 0,
            eof_span: Span::new(
                origin,
                (current_line, current_char),
                (current_line, current_char),
                current_byte,
                current_byte,
                string,
                lines,
            ),
        }
    }

    /// Create a [`StringStream`] directly from a file. This will try to read the content of the file right away.
    pub fn from_file(file: impl Into<Rc<Path>>) -> Result<Self> {
        let file = file.into();
        let mut file_stream = File::open(file.as_ref())
            .map_err(|err| Error::with_file(err, &*file))?;
        let mut stream_buffer = String::new();
        file_stream
            .read_to_string(&mut stream_buffer)
            .map_err(|err| Error::with_file(err, &*file))?;
        Ok(WarningSet::empty_with(StringStream::new(
            file,
            stream_buffer,
        )))
    }

    pub fn pos(&self) -> usize {
        self.chars_pos
    }

    /// Return a boolean corresponding to whether the substring of
    /// the `StringStream` that starts at the current position matches
    /// thecd given string.
    pub fn continues(&self, keyword: &str) -> bool {
        self.peek().starts_with(keyword)
    }

    pub fn shift(&mut self, length: usize) {
        for _ in 0..length {
            self.incr_pos();
        }
    }

    pub fn incr_pos(&mut self) {
        self.bytes_pos += self.spans[self.chars_pos].size;
        self.chars_pos += 1;
    }

    pub fn decr_pos(&mut self) {
        self.chars_pos -= 1;
        self.bytes_pos -= self.spans[self.chars_pos].size;
    }

    /// Return a string slice corresponding to the
    /// underlying string, starting at the position of the stream.
    pub fn peek(&self) -> &str {
        &self.stream[self.bytes_pos..]
    }

    pub fn text(&self) -> Rc<str> {
        self.stream.clone()
    }

    pub fn lines(&self) -> Rc<[usize]> {
        self.lines.clone()
    }

    pub fn get(&self) -> Char {
        self.peek()
            .chars()
            .next()
            .map(Char::Char)
            .unwrap_or(Char::EOF)
    }

    /// Return the origin file of the [`StringStream`].
    pub fn origin(&self) -> Rc<Path> {
        self.origin.clone()
    }

    /// Return the length of the stream.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Return is the stream is empty.
    pub fn is_empty(&self) -> bool {
        self.chars_pos == self.length
    }

    pub fn curr_span(&self) -> Span {
        if self.chars_pos == self.spans.len() {
            self.eof_span.clone()
        } else {
            let CharSpan {
                location: (line, column),
                byte_location: byte,
                ..
            } = self.spans[self.chars_pos];
            Span::new(
                self.origin.clone(),
                (line, column),
                (line, column),
                byte,
                byte,
                self.stream.clone(),
                self.lines.clone(),
            )
        }
    }

    pub fn span_between(&self, start: usize, end: usize) -> Span {
        let (start_location, start_byte) = self
            .spans
            .get(start)
            .map(
                |&CharSpan {
                     location: loc,
                     byte_location: byte_loc,
                     ..
                 }| (loc, byte_loc),
            )
            .unwrap_or_else(|| {
                (self.eof_span.start(), self.eof_span.start_byte())
            });
        let (end_location, end_byte) = self
            .spans
            .get(end)
            .map(
                |&CharSpan {
                     location: loc,
                     byte_location: byte_loc,
                     ..
                 }| (loc, byte_loc),
            )
            .unwrap_or_else(|| (self.eof_span.end(), self.eof_span.end_byte()));
        Span::new(
            self.origin.clone(),
            start_location,
            end_location,
            start_byte,
            end_byte,
            self.stream.clone(),
            self.lines.clone(),
        )
    }
}

impl std::fmt::Debug for StringStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.peek().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn string_stream() {
        let string = "What a nice content,\nall in a single stream!";
        let origin = Path::new("somewhere");
        let mut stream = StringStream::new(origin, string);
        assert_eq!(stream.peek(), &*string);
        for chr in string.chars() {
            let got_char = stream.get();
            match got_char {
                Char::Char(c) => assert_eq!(chr, c),
                Char::EOF => {
                    panic!("Found EOF in stream, while expecting {}", chr)
                }
            }
            stream.incr_pos();
        }
        assert!(matches!(stream.get(), Char::EOF));
    }

    #[test]
    fn unicode() {
        let string = "До́брый день.";
        let origin = Path::new("Russia");
        let mut stream = StringStream::new(origin, string);
        assert_eq!(stream.peek(), &*string);
        let mut curr_pos = 0;
        for chr in string.chars() {
            match stream.get() {
                Char::Char(c) => assert_eq!(chr, c),
                Char::EOF => {
                    panic!("Found EOF in stream, while expecting {}", chr)
                }
            }
            assert_eq!(&string[curr_pos..], stream.peek());
            stream.incr_pos();
            curr_pos += chr.len_utf8();
        }
    }

    #[test]
    fn spans() {
        let string = "Добрый день
defg
hij";
        let origin = Path::new("<SPANS>");
        let mut stream = StringStream::new(origin, string);
        let expected = [
            ('Д', (0, 0), 0, (0, 22)),
            ('о', (0, 1), 2, (0, 22)),
            ('б', (0, 2), 4, (0, 22)),
            ('р', (0, 3), 6, (0, 22)),
            ('ы', (0, 4), 8, (0, 22)),
            ('й', (0, 5), 10, (0, 22)),
            (' ', (0, 6), 12, (0, 22)),
            ('д', (0, 7), 13, (0, 22)),
            ('е', (0, 8), 15, (0, 22)),
            ('н', (0, 9), 17, (0, 22)),
            ('ь', (0, 10), 19, (0, 22)),
            ('\n', (0, 11), 21, (0, 22)),
            ('d', (1, 0), 22, (22, 27)),
            ('e', (1, 1), 23, (22, 27)),
            ('f', (1, 2), 24, (22, 27)),
            ('g', (1, 3), 25, (22, 27)),
            ('\n', (1, 4), 26, (22, 27)),
            ('h', (2, 0), 27, (27, 30)),
            ('i', (2, 1), 28, (27, 30)),
            ('j', (2, 2), 29, (27, 30)),
        ];
        assert_eq!(&string[0..22], "Добрый день\n");
        assert_eq!(&string[22..27], "defg\n");
        assert_eq!(&string[27..30], "hij");
        for (expected_char, expected_location, byte, line_byte) in expected {
            let Char::Char(found_char) = stream.get() else {
		panic!("Expected {expected_char:?}, found EOF")
	    };
            assert_eq!(expected_char, found_char);
            let curr_span = stream.curr_span();
            assert_eq!(curr_span.start(), curr_span.end());
            assert_eq!(expected_location, curr_span.start());
            assert_eq!(curr_span.start_byte(), curr_span.end_byte());
            assert_eq!(byte, curr_span.start_byte());
            assert_eq!(
                line_byte,
                curr_span.line_bytes_of_line(expected_location.0)
            );
            stream.incr_pos();
        }
        assert!(stream.is_empty());
    }
}
