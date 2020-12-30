use crate::error::{Error, ErrorType};
use crate::location::Location;
use crate::stream::{Char, Stream, StringStream};
use hashbrown::HashSet;
use regex::{Regex, RegexBuilder};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_parser_read_keyword() {
        let mut grammar_parser =
            GrammarParser::new(String::from("whatever"), String::from("ignore"));
        assert_eq!(grammar_parser.stream.pos(), 0);
        assert_eq!(grammar_parser.read_keyword("something"), false);
        assert_eq!(grammar_parser.stream.pos(), 0);
        assert_eq!(grammar_parser.read_keyword("ignore"), true);
        assert_eq!(grammar_parser.stream.pos(), 6);
    }
    #[test]
    fn grammar_parser_read_id() {
        let mut grammar_parser =
            GrammarParser::new(String::from("whatever"), String::from("to del"));
        assert_eq!(grammar_parser.stream.pos(), 0);
        assert_eq!(grammar_parser.read_id(), Ok(String::from("to")));
        assert_eq!(grammar_parser.stream.pos(), 2);
        grammar_parser.ignore_blank();
        assert_eq!(grammar_parser.stream.pos(), 3);
        assert_eq!(grammar_parser.read_id(), Ok(String::from("del")));
        assert_eq!(grammar_parser.stream.pos(), 6);
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
            GrammarParser::new(
                String::from("whatever"),
                String::from("A ::= wot!\n\nB ::= wheel")
            )
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
    #[test]
    fn grammar_parser_ignores() {
        let ignores = GrammarParser::new(
            String::from("whatever"),
            String::from("ignore A ::= [ ]\nignore B ::= bbb\nC ::= ccc"),
        )
        .read()
        .unwrap()
        .2;
        assert!(ignores.contains(&String::from("A")));
        assert!(ignores.contains(&String::from("B")));
        assert!(!ignores.contains(&String::from("C")));
        assert_eq!(ignores.len(), 2);
    }
    #[test]
    fn grammar_default_grammar() {
//	let grammar = GrammarParser::new();
    }
}

#[derive(Debug)]
pub struct GrammarParser {
    stream: StringStream,
}

impl GrammarParser {
    pub fn new(file: String, stream: String) -> Self {
	let stream = StringStream::new(file, stream);
        Self {
            stream,
        }
    }

    pub fn read(&mut self) -> Result<(Regex, Vec<String>, HashSet<String>), Error> {
        let size = self.stream.len();
        let mut ignores = HashSet::<String>::new();
        let mut names = vec![];
        let mut full_pattern = String::new();
        self.ignore_blank_lines();
        while self.stream.pos() < size {
            let ignore = self.read_keyword("ignore");
            self.ignore_blank();
            let name = self.read_id()?;
            self.ignore_blank();
            self.ignore_assignment()?;
            self.ignore_blank();
            let start = self.stream.pos();
            let pattern = self.read_pattern();
            Regex::new(pattern.as_str()).map_err(|error| {
                (
                    Location::from_stream_pos(
			self.stream
			    .origin()
			    .to_string(),
                        &self.stream.borrow()[..],
                        start,
                        self.stream.pos(),
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
            self.ignore_blank_lines();
        }
        full_pattern.pop();
        let re = RegexBuilder::new(full_pattern.as_str())
            .multi_line(true)
            .build()
            .map_err(|error| {
                (
                    Location::from_stream_pos(
			self.stream
			    .origin()
			    .to_string(),
                        &self.stream.borrow()[..],
                        0,
                        self.stream.pos(),
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
        Ok((re, names, ignores))
    }

    fn read_pattern(&mut self) -> String {
        let mut result = String::new();
        while let (Char::Char(chr), _) = self.stream.get().unwrap() {
            if chr == '\n' {
                break;
            }
            result.push(chr);
            self.stream.pos_pp();
        }
        result
    }

    /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
    fn read_keyword(&mut self, keyword: &str) -> bool {
        let size = keyword.chars().count();
        if self.stream.continues(keyword) {
            self.stream.pos_inc(size);
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
        while let (Char::Char(chr), _) = self.stream.get().unwrap() {
            if chr == ' ' || chr == '\t' {
                self.stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn ignore_blank_lines(&mut self) {
        while let (Char::Char(chr), _) = self.stream.get().unwrap() {
            if chr.is_whitespace() {
                self.stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn generate_error<'b>(&'b self, err_message: &str) -> Error {
        (
            Location::from_stream_pos(
                self.stream
		    .origin()
		    .to_string(),
                &self.stream.borrow()[..],
                self.stream.pos(),
                self.stream.pos() + 1,
            ),
            ErrorType::LexerGrammarSyntax(String::from(err_message)),
        )
    }

    fn read_id(&mut self) -> Result<String, Error> {
        let mut result = String::new();
        while let (Char::Char(chr), _) = self.stream.get().unwrap() {
            if !chr.is_ascii_alphabetic() {
                break;
            }
            result.push(chr);
            self.stream.pos_pp();
        }
        if result.is_empty() {
            Err(self.generate_error("expected id"))
        } else {
            Ok(result)
        }
    }
}
