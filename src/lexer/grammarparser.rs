use crate::error::{Error, ErrorType};
use crate::location::Location;
use crate::stream::{Char, Stream, StringStream};
use fixedbitset::FixedBitSet;
use hashbrown::HashSet;
use regex::{Regex, RegexBuilder};
use std::error;
use std::fs::File;
use std::io::prelude::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_parser_read_keyword() {
        let mut stream = StringStream::new(String::from("whatever"), String::from("ignore"));
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            LexerGrammarBuilder::read_keyword(&mut stream, "something"),
            false
        );
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            LexerGrammarBuilder::read_keyword(&mut stream, "ignore"),
            true
        );
        assert_eq!(stream.pos(), 6);
    }
    #[test]
    fn grammar_parser_read_id() {
        let mut stream = StringStream::new(String::from("whatever"), String::from("to del"));
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            LexerGrammarBuilder::read_id(&mut stream),
            Ok(String::from("to"))
        );
        assert_eq!(stream.pos(), 2);
        LexerGrammarBuilder::ignore_blank(&mut stream);
        assert_eq!(stream.pos(), 3);
        assert_eq!(
            LexerGrammarBuilder::read_id(&mut stream),
            Ok(String::from("del"))
        );
        assert_eq!(stream.pos(), 6);
        assert_eq!(
            LexerGrammarBuilder::read_id(&mut stream),
            Err((
                Location::new(String::from("whatever"), (0, 6), (0, 6)),
                ErrorType::LexerGrammarSyntax(String::from("expected id"))
            )
                .into())
        );
    }
    #[test]
    fn grammar_parser_regex() {
        assert_eq!(
            LexerGrammarBuilder::new()
                .with_stream(StringStream::new(
                    String::from("whatever"),
                    String::from("A ::= wot!")
                ))
                .build()
                .unwrap()
                .pattern()
                .as_str(),
            "(?P<A>wot!)"
        );
        assert_eq!(
            LexerGrammarBuilder::new()
                .with_stream(StringStream::new(
                    String::from("whatever"),
                    String::from("B ::= wot!  ")
                ))
                .build()
                .unwrap()
                .pattern()
                .as_str(),
            "(?P<B>wot!  )"
        );
        assert_eq!(
            LexerGrammarBuilder::new()
                .with_stream(StringStream::new(
                    String::from("whatever"),
                    String::from("A ::= wot!\n\nB ::= wheel")
                ))
                .build()
                .unwrap()
                .pattern()
                .as_str(),
            "(?P<A>wot!)|(?P<B>wheel)"
        );
        assert_eq!(
            LexerGrammarBuilder::new()
                .with_stream(StringStream::new(
                    String::from("whatever"),
                    String::from("")
                ))
                .build()
                .unwrap()
                .pattern()
                .as_str(),
            ""
        );
    }
    #[test]
    fn lexer_grammar() {
        let grammar = LexerGrammarBuilder::new()
            .with_stream(StringStream::new(
                String::from("whatever"),
                String::from("ignore A ::= [ ]\nignore B ::= bbb\nC ::= ccc"),
            ))
            .build()
            .unwrap();
        assert_eq!(grammar.name(0), "A");
        assert!(grammar.ignored(0));
        assert_eq!(grammar.name(1), "B");
        assert!(grammar.ignored(1));
        assert_eq!(grammar.name(2), "C");
        assert!(!grammar.ignored(2));
    }
    #[test]
    fn grammar_default_grammar() {
        //	let grammar = GrammarParser::new();
    }
}

/// # Summary
///
/// A builder for a `LexerGrammar` object.
///
/// # Attribute specificators
///
/// `with_file`: specify the grammar's file.
///            May fail, if it can't open the given file.
/// `with_stream`: specify a given stream.
#[derive(Debug)]
pub struct LexerGrammarBuilder {
    stream: Option<StringStream>,
}

impl LexerGrammarBuilder {
    pub fn new() -> Self {
        Self { stream: None }
    }

    pub fn with_file(mut self, file: String) -> Result<Self, Box<dyn error::Error>> {
        let mut file_stream = File::open(file.as_str())?;
        let mut stream_buffer = String::new();
        file_stream.read_to_string(&mut stream_buffer)?;
        let stream = StringStream::new(file, stream_buffer);
        self.stream = Some(stream);
        Ok(self)
    }

    pub fn with_stream(mut self, stream: StringStream) -> Self {
        self.stream = Some(stream);
        self
    }

    pub fn build(&mut self) -> Result<LexerGrammar, Error> {
        let stream = match &mut self.stream {
            Some(l) => l,
            None => {
                return Err((
                    Location::new(String::from("<Beans source code>"), (0, 0), (0, 0)),
                    ErrorType::InternalError(String::from("")),
                )
                    .into())
            }
        };
        let size = stream.len();
        let mut ignores = HashSet::<String>::new();
        let mut names = vec![];
        let mut full_pattern = String::new();
        Self::ignore_blank_lines(stream);
        while stream.pos() < size {
            let ignore = Self::read_keyword(stream, "ignore");
            Self::ignore_blank(stream);
            let name = Self::read_id(stream)?;
            Self::ignore_blank(stream);
            Self::ignore_assignment(stream)?;
            Self::ignore_blank(stream);
            let start = stream.pos();
            let pattern = Self::read_pattern(stream);
            Regex::new(pattern.as_str()).map_err(|error| {
                (
                    Location::from_stream_pos(
                        stream.origin().to_string(),
                        &stream.borrow()[..],
                        start,
                        stream.pos(),
                    ),
                    ErrorType::LexerGrammarSyntax(match error {
                        regex::Error::Syntax(msg) => msg,
                        regex::Error::CompiledTooBig(size) => {
                            format!("size too big (maximum is {})", size)
                        }
                        _ => String::from("unknown regex error"),
                    }),
                )
            })?;
            full_pattern.push_str(format!("(?P<{}>{})|", &name, pattern).as_str());
            names.push(name.clone());
            if ignore {
                ignores.insert(name);
            }
            Self::ignore_blank_lines(stream);
        }
        full_pattern.pop();
        let re = RegexBuilder::new(full_pattern.as_str())
            .multi_line(true)
            .build()
            .map_err(|error| {
                (
                    Location::from_stream_pos(
                        stream.origin().to_string(),
                        &stream.borrow()[..],
                        stream.pos(),
                        stream.pos(),
                    ),
                    ErrorType::LexerGrammarSyntax(match error {
                        regex::Error::Syntax(msg) => msg,
                        regex::Error::CompiledTooBig(size) => {
                            format!("Size too big (maximum is {})", size)
                        }
                        _ => String::from("unknown regex error"),
                    }),
                )
            })?;
        let mut ignores_bitset = FixedBitSet::with_capacity(names.len());
        for (i, name) in names.iter().enumerate() {
            if ignores.contains(name) {
                ignores_bitset.insert(i);
            }
        }
        Ok(LexerGrammar::new(re, names, ignores_bitset))
    }

    fn read_pattern(stream: &mut StringStream) -> String {
        let mut result = String::new();
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr == '\n' {
                break;
            }
            result.push(chr);
            stream.pos_pp();
        }
        result
    }

    /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
    fn read_keyword(stream: &mut StringStream, keyword: &str) -> bool {
        let size = keyword.chars().count();
        if stream.continues(keyword) {
            stream.pos_inc(size);
            true
        } else {
            false
        }
    }

    fn ignore_assignment(stream: &mut StringStream) -> Result<(), Error> {
        if Self::read_keyword(stream, "::=") {
            Ok(())
        } else {
            Err(Self::generate_error(stream, "expected assignment"))
        }
    }

    fn ignore_blank(stream: &mut StringStream) {
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr == ' ' || chr == '\t' {
                stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn ignore_blank_lines(stream: &mut StringStream) {
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr.is_whitespace() {
                stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn generate_error(stream: &StringStream, err_message: &str) -> Error {
        (
            Location::from_stream_pos(
                stream.origin().to_string(),
                &stream.borrow()[..],
                stream.pos(),
                stream.pos() + 1,
            ),
            ErrorType::LexerGrammarSyntax(String::from(err_message)),
        )
            .into()
    }

    fn read_id(stream: &mut StringStream) -> Result<String, Error> {
        let mut result = String::new();
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if !chr.is_ascii_alphabetic() {
                break;
            }
            result.push(chr);
            stream.pos_pp();
        }
        if result.is_empty() {
            Err(Self::generate_error(stream, "expected id"))
        } else {
            Ok(result)
        }
    }
}
/// # Summary
///
/// `LexerGrammar` is a grammar for a lexer. It is already setup.
/// Should be built with a `LexerGrammarBuilder`.
///
/// # Methods
///
/// `ignored`: return if the token with the given id should be ignored.
/// `name`: return the name of the token with the given id.
#[derive(Debug)]
pub struct LexerGrammar {
    pattern: Regex,
    names: Vec<String>,
    ignores: FixedBitSet,
}

impl LexerGrammar {
    pub fn new(pattern: Regex, names: Vec<String>, ignores: FixedBitSet) -> Self {
        Self {
            pattern,
            names,
            ignores,
        }
    }

    pub fn name(&self, idx: usize) -> &str {
        &self.names[idx][..]
    }

    pub fn ignored(&self, idx: usize) -> bool {
        self.ignores.contains(idx)
    }

    pub fn pattern(&self) -> &Regex {
        &self.pattern
    }
}
