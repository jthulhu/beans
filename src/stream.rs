use crate::error::{Result, WarningSet};
use crate::location::Span;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::rc::Rc;

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
}

/// The type of data returned by the stream.
/// Is a tuple of two elements, the element
/// at a given index and its location.
pub type StreamObject<T> = (T, Span);

/// # Summary
///
/// A stream-like object.
///
/// # Methods
///
/// `get_at`: get an object at a given index.
/// `pos`: return the current position.
/// `set_pos`: set the current position.
/// `pos_pp`: increment the current position by one.
/// `pos_inc`: increment the current position by a given (positive) ammount.
/// `get`: get the object at the current position.
pub trait Stream<'a> {
    /// The type that this is a stream of.
    type Output: 'a;
    /// Get the object at the given position.
    fn get_at(&'a self, pos: usize) -> Option<StreamObject<Self::Output>>;
    /// Return the current position.
    fn pos(&self) -> usize;
    /// Set the current position.
    fn set_pos(&mut self, pos: usize);
    /// Increase the current position by one.
    fn pos_pp(&mut self) {
        self.set_pos(self.pos() + 1);
    }
    /// Increase the current position by a given (positive) ammount;
    fn pos_inc(&mut self, off: usize) {
        self.set_pos(self.pos() + off);
    }
    /// Get the object at the current position.
    fn get(&'a self) -> Option<StreamObject<Self::Output>> {
        self.get_at(self.pos())
    }

    /// Get the location of the object at position `pos`.
    fn get_loc_of(&'a self, pos: usize) -> Option<Span> {
        self.get_at(pos).map(|(_, location)| location)
    }

    /// Advance the stream to the next position, and return the object at that position.
    fn next(&'a mut self) -> Option<StreamObject<Self::Output>> {
        let pos = self.pos();
        self.pos_pp();
        self.get_at(pos)
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
    spans: Vec<((usize, usize), usize)>,
    stream: Rc<str>,
    bytes_pos: usize,
    chars_pos: usize,
    length: usize,
    eof_span: Span,
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
        for chr in string.chars() {
            let start_pos = (current_line, current_char);
            spans.push((start_pos, chr.len_utf8()));
            if chr == '\n' {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
        }
        Self {
            origin: origin.clone(),
            length: spans.len(),
            stream: string,
            spans,
            bytes_pos: 0,
            chars_pos: 0,
            eof_span: Span::new(
                origin,
                (current_line, current_char),
                (current_line, current_char),
            ),
        }
    }

    /// Create a [`StringStream`] directly from a file. This will try to read the content of the file right away.
    pub fn from_file(file: impl Into<Rc<Path>>) -> Result<Self> {
        let file = file.into();
        let mut file_stream = File::open(file.as_ref())?;
        let mut stream_buffer = String::new();
        file_stream.read_to_string(&mut stream_buffer)?;
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
        self.bytes_pos += self.spans[self.chars_pos].1;
        self.chars_pos += 1;
    }

    pub fn decr_pos(&mut self) {
        self.chars_pos -= 1;
        self.bytes_pos -= self.spans[self.chars_pos].1;
    }

    /// Return a string slice corresponding to the
    /// underlying string, starting at the position of the stream.
    pub fn peek(&self) -> &str {
        &self.stream[self.bytes_pos..]
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
            let (line, column) = self.spans[self.chars_pos].0;
            Span::new(self.origin.clone(), (line, column), (line, column + 1))
        }
    }

    pub fn span_between(&self, start: usize, end: usize) -> Span {
        let start_location = self
            .spans
            .get(start)
            .map(|x| x.0)
            .unwrap_or_else(|| self.eof_span.start());
        let end_location = self
            .spans
            .get(end)
            .map(|x| x.0)
            .unwrap_or_else(|| self.eof_span.end());
        Span::new(self.origin.clone(), start_location, end_location)
    }
}

// impl Stream<'_> for StringStream {
//     type Output = Char;
//     fn get_at(&self, pos: usize) -> Option<StreamObject<Self::Output>> {
//         Some(match self.stream.get(pos) {
//             Some((chr, pos)) => (Char::Char(*chr), pos.clone()),
//             None => (Char::EOF, self.end_pos.clone()),
//         })
//     }
//     fn set_pos(&mut self, pos: usize) {
//         self.pos = pos;
//     }
//     fn pos(&self) -> usize {
//         self.pos
//     }
// }

impl std::fmt::Debug for StringStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.peek().fmt(f)
    }
}
