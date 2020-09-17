use super::grammarparser::GrammarParser;
use crate::error;
use crate::location::Location;
use crate::stream::Stream;
use crate::stream::StringStream;
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
	assert!(LexerBuilder::new().build().is_err());
	let lexer = LexerBuilder::new()
	    .with_grammar(
		GrammarParser::new(String::from("whatever"), String::from("A ::= blu")))
	    .build()
	    .unwrap();
    }
	    
}

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
    pub fn new(name: String, attributes: HashMap<String, String>, location: Location) -> Self {
        Self {
            name,
            attributes,
            location,
        }
    }
    pub fn contains(&self, key: &String) -> bool {
        self.attributes.contains_key(key)
    }

    pub fn name(&self) -> &str {
        &self.name[..]
    }
    pub fn location(&self) -> &Location {
        &self.location
    }
}

pub struct LexerBuilder {
    file: Option<String>,
    grammar: Option<GrammarParser>,
}

impl LexerBuilder {
    pub fn new() -> Self {
        Self {
            file: None,
            grammar: None,
        }
    }

    pub fn with_file(mut self, file: String) -> Self {
        self.file = Some(file);
        self
    }

    pub fn with_grammar(mut self, grammar: GrammarParser) -> Self {
        self.grammar = Some(grammar);
        self
    }

    pub fn build(self) -> Result<Lexer, Box<dyn Error>> {
        let file = if let Some(file) = self.file {
            file
        } else {
            String::from("gmrs/lexer.gmr")
        };
        let mut file_stream = File::open(file.as_str())?;
        let mut stream_buffer = String::new();
        file_stream.read_to_string(&mut stream_buffer)?;
        let stream = StringStream::new(stream_buffer);
        Ok(Lexer::new(
            file,
            stream,
            self.grammar
                .expect("Trying to build a lexer without grammar."),
        ))
    }
}

pub struct Lexer {
    file: String,
    stream: StringStream,
    grammar: GrammarParser,
    tokens: Vec<Token>,
    pos: usize,
}

impl Lexer {
    pub fn new(file: String, stream: StringStream, grammar: GrammarParser) -> Self {
        Self {
            file,
            stream,
            grammar,
            tokens: vec![],
            pos: 0,
        }
    }
    pub fn lex(&mut self) -> Result<(), error::Error> {
        let (pattern, names, ignores) = self.grammar.read()?;
        while self.pos < self.stream.len() {
            let mut capture_locations = pattern.capture_locations();
            pattern.captures_read_at(&mut capture_locations, self.stream.borrow(), self.pos);
            if capture_locations.len() == 0 {
                return Err((
                    Location::from_stream_pos(
                        self.file.clone(),
                        self.stream.borrow(),
                        self.pos,
                        self.pos + 1,
                    ),
                    error::ErrorType::LexingError(
			String::from("cannot recognize a token there")
		    ),
                ));
            }
	    let mut i = 1;
	    loop {
		if let Some((start, end)) = capture_locations.get(i) {
		    self.tokens.push(
			Token::new(
			    names[i-1].clone(),
			    HashMap::new(),
			    Location::from_stream_pos(
				self.file.clone(),
				self.stream.borrow(),
				start,
				end,
			    )
			)
		    );
		    break;
		}
		i += 1;
	    }
	    
        }
        Ok(())
    }
}

impl<'a> Stream<'a> for Lexer {
    type Output = Option<&'a Token>;
    fn pos(&self) -> usize {
        self.pos
    }
    fn set_pos(&mut self, pos: usize) {
        self.pos = pos
    }
    fn get_at(&'a self, pos: usize) -> Self::Output {
        self.tokens.get(pos)
    }
}
