use crate::error::{Error, ErrorType};
use crate::location::Location;
use hashbrown::{HashMap, HashSet};
use regex::{Regex, RegexBuilder};
use std::fmt;
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
    fn grammar_parser_read_keyword() {
        let mut grammar_parser =
            GrammarParser::new(String::from("whatever"), String::from("ignore"));
        assert_eq!(grammar_parser.pos, 0);
        assert_eq!(grammar_parser.read_keyword("something"), false);
        assert_eq!(grammar_parser.pos, 0);
        assert_eq!(grammar_parser.read_keyword("ignore"), true);
        assert_eq!(grammar_parser.pos, 6);
    }
    #[test]
    fn grammar_parser_read_id() {
        let mut grammar_parser =
            GrammarParser::new(String::from("whatever"), String::from("to del"));
        assert_eq!(grammar_parser.pos, 0);
        assert_eq!(grammar_parser.read_id(), Ok(String::from("to")));
        assert_eq!(grammar_parser.pos, 2);
	grammar_parser.ignore_blank();
	assert_eq!(grammar_parser.pos, 3);
        assert_eq!(grammar_parser.read_id(), Ok(String::from("del")));
        assert_eq!(grammar_parser.pos, 6);
        assert_eq!(
            grammar_parser.read_id(),
            Err((
                Location::new(String::from("whatever"), (0, 6), (0, 6)),
                ErrorType::LexerGrammarSyntax(String::from("expected id"))
            ))
        );
    }
    #[test]
    fn grammar_parser_regex() {
        assert_eq!(
            GrammarParser::new(String::from("whatever"), String::from("A ::= wot!"))
                .read()
                .unwrap()
                .0
                .as_str(),
            "(?P<A>wot!)"
        );
	assert_eq!(
            GrammarParser::new(String::from("whatever"), String::from("B ::= wot!  "))
                .read()
                .unwrap()
                .0
                .as_str(),
            "(?P<B>wot!  )"
        );
	assert_eq!(
            GrammarParser::new(String::from("whatever"), String::from("A ::= wot!\n\nB ::= wheel"))
                .read()
                .unwrap()
                .0
                .as_str(),
            "(?P<A>wot!)|(?P<B>wheel)"
        );
	assert_eq!(
            GrammarParser::new(String::from("whatever"), String::from(""))
                .read()
                .unwrap()
                .0
                .as_str(),
            ""
        );
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

enum Char {
    Some(char),
    EOF,
}

pub struct GrammarParser {
    file: String,
    pos: usize,
    stream: String,
}

impl GrammarParser {
    pub fn new(file: String, stream: String) -> Self {
        Self {
            file,
            pos: 0,
            stream,
        }
    }

    fn get(&self) -> Char {
        if let Some(chr) = self.stream.chars().nth(self.pos) {
            Char::Some(chr)
        } else {
            Char::EOF
        }
    }

    pub fn read(&mut self) -> Result<(Regex, HashSet<String>), Error> {
        let size = self.stream.len();
        let mut ignores = HashSet::<String>::new();
        let mut full_pattern = String::new();
	self.ignore_blank_lines();
        while self.pos < size {
            let ignore = self.read_keyword("ignore");
	    self.ignore_blank();
            let name = self.read_id()?;
            self.ignore_blank();
            self.ignore_assignment()?;
	    self.ignore_blank();
            let start = self.pos;
            let pattern = self.read_pattern();
            Regex::new(pattern.as_str()).or_else(|error| {
                return Err((
                    Location::from_stream_pos(self.file.clone(), &self.stream[..], start, self.pos),
                    ErrorType::LexerGrammarSyntax(match error {
                        regex::Error::Syntax(msg) => msg,
                        regex::Error::CompiledTooBig(size) => {
                            format!("size too big (maximum is {})", size)
                        }
                        _ => String::from("unknown regex error"),
                    }),
                ));
            })?;
            full_pattern.push_str(format!("(?P<{}>{})|", &name, pattern).as_str());
            if ignore {
                ignores.insert(name);
            }
	    self.ignore_blank_lines();
        }
        full_pattern.pop();
        let re = RegexBuilder::new(full_pattern.as_str())
            .multi_line(true)
	    .build()
            .or_else(|error| {
                return Err((
                    Location::from_stream_pos(self.file.clone(), &self.stream[..], 0, self.pos),
                    ErrorType::LexerGrammarSyntax(match error {
                        regex::Error::Syntax(msg) => msg,
                        regex::Error::CompiledTooBig(size) => {
                            format!("Size too big (maximum is {})", size)
                        }
                        _ => String::from("unknown regex error"),
                    }),
                ));
            })?;
        Ok((re, ignores))
    }

    fn read_pattern(&mut self) -> String {
        let mut result = String::new();
        while let Char::Some(chr) = self.get() {
            if chr == '\n' {
                break;
            }
            result.push(chr);
            self.pos += 1;
        }
        result
    }

    /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
    fn read_keyword(&mut self, keyword: &str) -> bool {
        let size = keyword.len();
        if self.stream[self.pos..].starts_with(keyword) {
            self.pos += size;
            true
        } else {
            false
        }
    }

    fn ignore_assignment(&mut self) -> Result<(), Error> {
        if self.read_keyword("::=") {
            Ok(())
        } else {
            Err(self.generate_error("expected assignment"))
        }
    }

    fn ignore_blank(&mut self) {
	while let Char::Some(chr) = self.get() {
	    if chr == ' ' || chr == '\t' {
		self.pos += 1;
	    } else {
		break;
	    }
	}
    }
    
    fn ignore_blank_lines(&mut self) {
        while let Char::Some(chr) = self.get() {
            if chr.is_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn generate_error(&self, err_message: &str) -> Error {
        (
            Location::from_stream_pos(self.file.clone(), &self.stream[..], self.pos, self.pos + 1),
            ErrorType::LexerGrammarSyntax(String::from(err_message)),
        )
    }

    fn read_id(&mut self) -> Result<String, Error> {
        let mut result = String::new();
        while let Char::Some(chr) = self.get() {
            if !chr.is_ascii_alphabetic() {
                break;
            }
            result.push(chr);
            self.pos += 1;
        }
        if result.is_empty() {
            Err(self.generate_error("expected id"))
        } else {
            Ok(result)
        }
    }
}
