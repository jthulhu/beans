use crate::error::{Result, WarningSet};
use crate::location::{Location, LocationBuilder};
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
        let stream = StringStream::new(origin, string);
        assert_eq!(stream.borrow().as_str(), &*string);
        for &i in [0, 3, 5, 17, string.len(), string.len() + 2].iter() {
            let (chr, loc) = stream.get_at(i).unwrap();
            if i < string.len() {
                if let Char::Char(chr) = chr {
                    assert_eq!(chr, string.chars().nth(i).unwrap());
                } else {
                    panic!(
                        "There is a char at position {} of {}, and yet it was not in the stream",
                        i, &string
                    );
                }
                let location = Location::from_stream_pos(origin, string, i, i);
                assert_eq!(location, loc);
            } else {
                if let Char::Char(_) = chr {
                    panic!(
                        "There is no char at position {} of {}, and yet there is one in the stream",
                        i, &string
                    );
                }
                let location =
                    Location::from_stream_pos(origin, string, string.len(), string.len());
                assert_eq!(location, loc);
            }
        }
    }
}

/// The type of data returned by the stream.
/// Is a tuple of two elements, the element
/// at a given index and its location.
pub type StreamObject<T> = (T, Location);

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
    fn get_loc_of(&'a self, pos: usize) -> Option<Location> {
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
    stream: Vec<(char, Location)>,
    pos: usize,
    length: usize,
    end_pos: Location,
    location_builder: LocationBuilder,
}

impl StringStream {
    /// Build a new `StringStream`, based on its `origin` and on a given `string`.
    pub fn new(origin: impl Into<Rc<Path>>, string: impl Into<Rc<str>>) -> Self {
        let origin = origin.into();
        let string = string.into();
        let mut current_char = 0;
        let mut current_line = 0;
        let mut stream = Vec::new();
        for chr in string.chars() {
            let pos = (current_line, current_char);
            stream.push((chr, Location::new(origin.clone(), pos, pos)));
            if chr == '\n' {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }
        }
        Self {
            origin: origin.clone(),
            stream,
            pos: 0,
            length: string.len(),
            location_builder: LocationBuilder::new(origin.clone(), string),
            end_pos: Location::new(
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
            Rc::from(stream_buffer),
        )))
    }

    /// Return a boolean corresponding to whether the substring of
    /// the `StringStream` that starts at the current position matches
    /// the given string.
    pub fn continues(&self, keyword: &str) -> bool {
        let size = keyword.len();
        if self.pos + size > self.length {
            return false;
        }
        let mut result = true;
        let mut chars = keyword.chars();
        for i in 0..size {
            if self.stream[self.pos + i].0 != chars.next().unwrap() {
                result = false;
                break;
            }
        }
        result
    }

    /// Return a string slice corresponding to the
    /// underlying string.
    pub fn borrow(&self) -> String {
        self.stream.iter().map(|(chr, _)| chr).collect()
    }

    /// Return the origin file of the [`StringStream`].
    pub fn origin(&self) -> Rc<Path> {
        self.origin.clone()
    }

    /// Return the length of the stream.
    pub fn len(&self) -> usize {
        self.stream.len()
    }

    /// Return is the stream is empty.
    pub fn is_empty(&self) -> bool {
        self.stream.is_empty()
    }

    /// Return a [`Location`] struct that represents a span of text, specified by the beginning and ending character in the stream.
    pub fn loc_at(&self, start: usize, end: usize) -> Location {
        self.location_builder.from(start, end)
    }
}

impl Stream<'_> for StringStream {
    type Output = Char;
    fn get_at(&self, pos: usize) -> Option<StreamObject<Self::Output>> {
        Some(match self.stream.get(pos) {
            Some((chr, pos)) => (Char::Char(*chr), pos.clone()),
            None => (Char::EOF, self.end_pos.clone()),
        })
    }
    fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }
    fn pos(&self) -> usize {
        self.pos
    }
}

impl std::fmt::Debug for StringStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow().fmt(f)
    }
}
