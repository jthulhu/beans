use super::grammarparser::{LexerGrammar, LexerGrammarBuilder};
use crate::error::{self, ExecutionError};
use crate::location::Location;
use crate::stream::{Stream, StreamObject, StringStream};
use hashbrown::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::ops::Index;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn token() {
        let token = Token::new(
            String::from("wow"),
            HashMap::new(),
            Location::new(String::from("test_file"), (3, 0), (3, 3)),
        );

        assert_eq!(token.name(), "wow");
        assert_eq!(token.location().file(), "test_file");
    }

    #[test]
    fn lexer_builder() {
        // No options: fails
        assert!(LexerBuilder::new().build().is_err());

        // Only a grammar: fails
        assert!(LexerBuilder::new()
            .with_grammar(
                LexerGrammarBuilder::new()
                    .with_stream(StringStream::new(
                        String::from("grammar file"),
                        String::from("A ::= blue")
                    ))
                    .build()
                    .unwrap()
            )
            .build()
            .is_err());

        // Only a stream: fails
        assert!(LexerBuilder::new()
            .with_stream(StringStream::new(
                String::from("input file"),
                String::from("blu")
            ))
            .build()
            .is_err());

        // A grammar and a stream: succeeds
        LexerBuilder::new()
            .with_stream(StringStream::new(
                String::from("input file"),
                String::from("blu"),
            ))
            .with_grammar(
                LexerGrammarBuilder::new()
                    .with_stream(StringStream::new(
                        String::from("grammar file"),
                        String::from("A ::= blu"),
                    ))
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();
    }

    #[test]
    fn lex_basic() {
        let mut lexer = LexerBuilder::new()
            .with_stream(StringStream::new(
                String::from("input file"),
                String::from("blu"),
            ))
            .with_grammar(
                LexerGrammarBuilder::new()
                    .with_stream(StringStream::new(
                        String::from("a file name"),
                        String::from("A ::= blu"),
                    ))
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();
        let (token, loc) = lexer.get().unwrap();
        assert_eq!(*token.location(), loc);
        assert_eq!(loc.start(), (0, 0));
        assert_eq!(loc.end(), (0, 3));
        lexer.pos_pp();
        assert!(lexer.get().is_none());
    }

    #[test]
    fn default_lex_grammar() {
        let mut lexer = LexerBuilder::new()
            .with_grammar_file(String::from("gmrs/lexer.gmr"))
            .unwrap()
            .with_stream(StringStream::new(
                String::from("<input>"),
                String::from("one + two"),
            ))
            .build()
            .unwrap();
        let result = [
            ((0, 0), (0, 3), "ID"),
            ((0, 4), (0, 5), "PLUS"),
            ((0, 6), (0, 9), "ID"),
        ];
        let mut i = 0;
        while let Some((token, loc)) = lexer.get_at(i) {
            assert_eq!(*token.location(), loc);
            let (start, end, name) = result[i];
            assert_eq!(loc.start(), start);
            assert_eq!(loc.end(), end);
            assert_eq!(token.name(), name);
            i += 1;
        }

        assert_eq!(result.len(), i);
    }
}

/// # Summary
///
/// `Token` contains information about a token, thus it contains
///  - `name`: the identifier of the token;
///  - `attributes`: the attributes of the token;
///  - `location`: the location of the substring that generated this token.
#[derive(Debug)]
pub struct Token {
    name: String,
    attributes: HashMap<usize, String>,
    location: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.name)
    }
}

impl Index<&usize> for Token {
    type Output = String;

    fn index(&self, key: &usize) -> &Self::Output {
        &self.attributes[key]
    }
}

impl Token {
    /// Build a new token.
    pub fn new(name: String, attributes: HashMap<usize, String>, location: Location) -> Self {
        Self {
            name,
            attributes,
            location,
        }
    }

    /// Return whether the token has a given attribute.
    pub fn contains(&self, key: usize) -> bool {
        self.attributes.contains_key(&key)
    }

    /// Return the `name` of the token.
    pub fn name(&self) -> &str {
        &self.name[..]
    }

    /// Return the `location` of the token.
    pub fn location(&self) -> &Location {
        &self.location
    }
}

/// # Summary
///
/// A builder for a `Lexer` object.
///
/// # Attribute specificators
///
/// `with_stream`: specify the file the lexer will lex.
/// `with_grammar`: specify the grammar the lexer will use to lex.
/// `with_grammar_file`: specify the grammar file to build a grammar the lexer will use to lex.
#[derive(Debug)]
pub struct LexerBuilder {
    stream: Option<StringStream>,
    grammar: Option<LexerGrammar>,
}

impl LexerBuilder {
    /// Instantiate a new `LexerBuilder`.
    pub fn new() -> Self {
        Self {
            stream: None,
            grammar: None,
        }
    }

    /// Specify the lexer's stream.
    pub fn with_stream(mut self, stream: StringStream) -> Self {
        self.stream = Some(stream);
        self
    }

    /// Specify the lexer's grammar.
    pub fn with_grammar(mut self, grammar: LexerGrammar) -> Self {
        self.grammar = Some(grammar);
        self
    }

    /// Specify the lexer's grammar file.
    pub fn with_grammar_file(mut self, file: String) -> Result<Self, Box<dyn Error>> {
        let grammar = LexerGrammarBuilder::new().with_file(file)?.build()?;
        self.grammar = Some(grammar);
        Ok(self)
    }

    /// Build the lexer.
    pub fn build(self) -> Result<Lexer, Box<dyn Error>> {
        let mut lexer = Lexer::new(
            self.stream.ok_or_else(|| {
                ExecutionError::new(String::from("Trying to build a lexer without stream."))
            })?,
            self.grammar.ok_or_else(|| {
                ExecutionError::new(String::from("Trying to build a lexer without grammar."))
            })?,
        );
        lexer.lex()?;
        Ok(lexer)
    }
}

/// # Summary
///
/// `Lexer` is the main object that is used for lexing.
/// It is given a `StringStream`, and a `LexerGrammarBuilder`, and it
/// consumes the `StringStream`, producing a `Stream` of `Token`s.
///
/// It is better to access the tokens through the `Stream` interface
/// rather than directly through the methods defined by `Lexer`.
///
/// # Methods
///
/// `new`: build a new `Lexer`.
/// `lex`: consume the `StringStream` until a valid `Token` is generated
///      or raise an error.
#[derive(Debug)]
pub struct Lexer {
    stream: StringStream,
    grammar: LexerGrammar,
    tokens: Vec<StreamObject<Token>>,
    pos: usize,
}

impl Lexer {
    /// Create a new `Lexer` object.
    /// You may want to use the `LexerBuilder` instead.
    pub fn new(stream: StringStream, grammar: LexerGrammar) -> Self {
        Self {
            stream,
            grammar,
            tokens: vec![],
            pos: 0,
        }
    }

    /// Consume the input stream until everything could be converted to a token stream or an error was detected.
    /// You should use instead the methods provided by the `Stream` trait.
    pub fn lex(&mut self) -> Result<(), error::Error> {
        self.stream.set_pos(0);
        while self.stream.pos() < self.stream.len() {
            if let Some(result) = self
                .grammar
                .pattern()
                .find(&self.stream.borrow()[self.stream.pos()..])
            {
                let start = self.stream.pos();
                let end = start + result.length();
		self.stream.set_pos(end);
		if self.grammar.ignored(result.id()) {
		    continue;
		}
                let location = Location::from_stream_pos(
                    self.stream.origin().to_string(),
                    &self.stream.borrow(),
                    start,
                    end,
                );
                let name = result.name().to_string();
                let _attributes = vec![result.groups()];
                let token = Token::new(name, HashMap::new(), location.clone());
                self.tokens.push((token, location));
            } else {
                return Err((
                    self.stream.get_at(self.stream.pos()).unwrap().1,
                    error::ErrorType::LexingError(String::from("cannot recognize a token there")),
                )
                    .into());
            }
        }
        Ok(())
    }
}

impl<'a> Stream<'a> for Lexer {
    type Output = &'a Token;
    fn pos(&self) -> usize {
        self.pos
    }
    fn set_pos(&mut self, pos: usize) {
        self.pos = pos
    }
    fn get_at(&'a self, pos: usize) -> Option<StreamObject<Self::Output>> {
        match self.tokens.get(pos) {
            Some((t, l)) => Some((&t, l.clone())),
            None => None,
        }
    }
}
