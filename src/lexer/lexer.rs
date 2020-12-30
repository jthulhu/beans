use super::grammarparser::GrammarParser;
use crate::error::{self, ExecutionError};
use crate::location::Location;
use crate::stream::{Stream, StreamObject, StringStream};
use hashbrown::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::ops::Index;
// use std::env;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn token() {
        let token = Token::new(
            String::from("wow"),
            HashMap::new(),
            Location::new("test_file", (3, 0), (3, 3)),
        );

        assert_eq!(token.name(), "wow");
        assert_eq!(token.location().file(), "test_file");
    }
    #[test]
    fn lexer_builder() {
        assert!(LexerBuilder::new().build().is_err());
        LexerBuilder::new()
            .with_grammar(GrammarParser::new(
                String::from("whatever"),
                String::from("A ::= blu"),
            ))
            .build()
            .unwrap();
    }

    #[test]
    fn lex() {
	let mut lexer = LexerBuilder::new()
	    .with_grammar(
		GrammarParser::new(
		    String::from("a file name"),
		    String::from("A ::= blu"),
		))
	    .build()
	    .unwrap();
	lexer
	    .lex()
	    .unwrap();
	println!("{:?}", &lexer);
	assert!(lexer.get().is_none());
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
    attributes: HashMap<String, String>,
    location: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.name)
    }
}

impl Index<&String> for Token {
    type Output = String;

    fn index(&self, key: &String) -> &Self::Output {
        &self.attributes[key]
    }
}

impl Token {
    /// Build a new token.
    pub fn new(name: String, attributes: HashMap<String, String>, location: Location) -> Self {
        Self {
            name,
            attributes,
            location,
        }
    }

    /// Returns whether the token has a given attribute.
    pub fn contains(&self, key: &str) -> bool {
        self.attributes.contains_key(key)
    }

    /// Returns the `name` of the token.
    pub fn name(&self) -> &str {
        &self.name[..]
    }

    /// Returns the `location` of the token.
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
/// `with_file`: specify the file the lexer will lex.
/// `with_grammar`: specify the grammar the lexer will use to lex.
#[derive(Debug)]
pub struct LexerBuilder {
    file: Option<String>,
    grammar: Option<GrammarParser>,
}

impl LexerBuilder {
    /// Instantiate a new `LexerBuilder`.
    pub fn new() -> Self {
        Self {
            file: None,
            grammar: None,
        }
    }

    /// Specify the lexer's grammar file.
    pub fn with_file(mut self, file: String) -> Self {
        self.file = Some(file);
        self
    }

    /// Specify the lexer's grammar.
    pub fn with_grammar(mut self, grammar: GrammarParser) -> Self {
        self.grammar = Some(grammar);
        self
    }

    /// Build the lexer.
    pub fn build(self) -> Result<Lexer, Box<dyn Error>> {
        let file = if let Some(file) = self.file {
            file
        } else {
            String::from("gmrs/lexer.gmr")
        };
        let mut file_stream = File::open(file.as_str())?;
        let mut stream_buffer = String::new();
        file_stream.read_to_string(&mut stream_buffer)?;
        let stream = StringStream::new(file, stream_buffer);
        Ok(Lexer::new(
            stream,
            self.grammar.ok_or(ExecutionError::new(String::from(
                "Trying to build a lexer without grammar.",
            )))?,
        ))
    }
}

/// # Summary
///
/// `Lexer` is the main object that is used for lexing.
/// It is given a `StringStream`, and a `GrammarParser`, and it
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
    grammar: GrammarParser,
    tokens: Vec<StreamObject<Token>>,
    pos: usize,
}

impl Lexer {
    /// Create a new `Lexer` object.
    /// You may want to use the `LexerBuilder` instead.
    pub fn new(stream: StringStream, grammar: GrammarParser) -> Self {
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
        let (pattern, names, ignores) = self.grammar.read()?;
        while self.pos < self.stream.len() {
            let mut capture_locations = pattern.capture_locations();
            pattern.captures_read_at(&mut capture_locations, &self.stream.borrow(), self.pos);
            if capture_locations.len() == 0 {
                return Err((
                    self.stream.get_at(self.pos).unwrap().1,
                    error::ErrorType::LexingError(String::from("cannot recognize a token there")),
                ));
            }
            let mut i = 1;
            loop {
                if let Some((start, end)) = capture_locations.get(i) {
                    let location = Location::from_stream_pos(
                        self.stream
			    .origin()
			    .to_string(),
                        &self.stream
			    .borrow()[..],
                        start,
                        end,
                    );
                    self.tokens.push((
                        Token::new(names[i - 1].clone(), HashMap::new(), location.clone()),
                        location,
                    ));
                    break;
                }
                i += 1;
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
