use super::grammarparser::{LexerGrammar, LexerGrammarBuilder};
use crate::error::{
    Error, ErrorType,
    WResult::{self, WErr, WOk},
    WarningSet,
};
use crate::location::{CharLocation, Location, LocationBuilder};
use crate::stream::{Stream, StreamObject, StringStream};
use crate::{ctry, retrieve};
use hashbrown::HashMap;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::ops::Index;

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_token {
	($name: ident $($key:literal = $value: tt), *) => {
	    {
		let name = stringify!($name).to_string();
		let mut attributes = HashMap::new();
		$(attributes.insert($key, stringify!($value).to_string());)*
		TestToken {
		    name,
		    attributes
		}
	    }
	}
    }

    macro_rules! id_token {
	($content: ident) => {
	    test_token!(ID 0=$content)
	}
    }

    #[derive(Debug)]
    struct TestToken {
        name: String,
        attributes: HashMap<usize, String>,
    }

    impl PartialEq<Token> for TestToken {
        fn eq(&self, right: &Token) -> bool {
            self.name == right.name
                && self.attributes.iter().all(|(key, value)| {
                    right.attributes.get(&key).filter(|&v| v == value).is_some()
                })
        }
    }

    #[test]
    fn token() {
        let token = Token::new(
            String::from("wow"),
            HashMap::new(),
            Location::new(String::from("test_file"), (3, 0), (3, 3)),
        );

        assert_eq!(token.name(), "wow");
        assert_eq!(token.location().file(), "test_file");
        assert_eq!(token.location().start(), (3, 0));
        assert_eq!(token.location().end(), (3, 3));
    }

    #[test]
    fn lexer_builder() {
        // No options: fails
        assert!(LexerBuilder::new().build().is_err());
        // A grammar: succeeds
        LexerBuilder::new()
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
        let mut input = StringStream::new(String::from("input file"), String::from("blu"));
        let mut lexed_input = lexer.lex(&mut input);

        let token = lexed_input.get().unwrap().unwrap();
        assert_eq!(token.location().start(), (0, 0));
        assert_eq!(token.location().end(), (0, 3));
        lexed_input.pos_pp();
        assert!(lexed_input.get().unwrap().is_none());
    }

    fn verify_input(mut lexed_input: LexedStream, result: &[(CharLocation, CharLocation, &str)]) {
        let mut i = 0;
        while let Some(token) = lexed_input.get_at(i).unwrap() {
            let (start, end, name) = result[i];
            assert_eq!(token.location().start(), start);
            assert_eq!(token.location().end(), end);
            assert_eq!(token.name(), name);
            i += 1;
        }

        assert_eq!(result.len(), i);
    }

    #[test]
    fn default_lex_grammar() {
        let lexer = LexerBuilder::new()
            .with_grammar_file(String::from("gmrs/lexer.lx"))
            .unwrap()
            .build()
            .unwrap();

        let result = [
            ((0, 0), (0, 3), "ID"),
            ((0, 4), (0, 5), "PLUS"),
            ((0, 6), (0, 9), "ID"),
        ];
        verify_input(
            lexer.lex(&mut StringStream::new(
                String::from("<input>"),
                String::from("one + two"),
            )),
            &result,
        );

        let result = [
            ((0, 0), (0, 2), "IF"),
            ((0, 3), (0, 7), "TRUE"),
            ((0, 8), (0, 11), "AND"),
            ((0, 12), (0, 17), "FALSE"),
            ((0, 18), (0, 19), "LBRACE"),
            ((1, 1), (1, 6), "ID"),
            ((1, 6), (1, 7), "LPAR"),
            ((1, 7), (1, 18), "STRING"),
            ((1, 18), (1, 19), "RPAR"),
            ((2, 0), (2, 1), "RBRACE"),
        ];
        verify_input(
            lexer.lex(&mut StringStream::new(
                String::from("<input>"),
                String::from("if true and false {\n\tifeat(\"something\")\n}"),
            )),
            &result,
        );
    }

    #[test]
    fn default_parser_grammar() {
        let mut lexer = LexerBuilder::new()
            .with_grammar_file(String::from("gmrs/parser.lx"))
            .unwrap()
            .build()
            .unwrap();
        let mut input = StringStream::from_file(String::from("gmrs/parser.gmr")).unwrap();
        let mut lexed_input = lexer.lex(&mut input);

        let result = [
            // IfStatement
            id_token!(IfStatement),
            test_token!(ASSIGNMENT),
            id_token!(IF),
            id_token!(Expression),
            test_token!(AT),
            id_token!(condition),
            id_token!(LBRACE),
            id_token!(StatementList),
            test_token!(AT),
            id_token!(then),
            id_token!(RBRACE),
            test_token!(LPROXY),
            id_token!(haselse),
            test_token!(COLON),
            test_token!(BOOL 0=false),
            test_token!(RPROXY),
            id_token!(IF),
            id_token!(Expression),
            test_token!(AT),
            id_token!(condition),
            id_token!(LBRACE),
            id_token!(StatementList),
            test_token!(AT),
            id_token!(then),
            id_token!(RBRACE),
            id_token!(ELSE),
            id_token!(LBRACE),
            id_token!(StatementList),
            test_token!(AT),
            id_token!(else),
            id_token!(RBRACE),
            test_token!(LPROXY),
            id_token!(haselse),
            test_token!(COLON),
            test_token!(BOOL 0=true),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // WhileStatement
            id_token!(WhileStatement),
            test_token!(ASSIGNMENT),
            id_token!(WHILE),
            id_token!(Expression),
            test_token!(AT),
            id_token!(condition),
            id_token!(LBRACE),
            id_token!(StatementList),
            test_token!(AT),
            id_token!(do),
            id_token!(RBRACE),
            test_token!(LPROXY),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // Assignment
            id_token!(Assignment),
            test_token!(ASSIGNMENT),
            id_token!(ID),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(key),
            id_token!(EQUALS),
            id_token!(Expression),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // BuiltinType
            id_token!(BuiltinType),
            test_token!(ASSIGNMENT),
            id_token!(INT),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            id_token!(type),
            test_token!(COLON),
            test_token!(STRING 0=int),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=builtin),
            test_token!(RPROXY),
            id_token!(STRING),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            id_token!(type),
            test_token!(COLON),
            test_token!(STRING 0=string),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=builtin),
            test_token!(RPROXY),
            id_token!(ID),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            id_token!(type),
            test_token!(COLON),
            test_token!(STRING 0=id),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=builtin),
            test_token!(RPROXY),
            id_token!(TRUE),
            test_token!(LPROXY),
            id_token!(type),
            test_token!(COLON),
            test_token!(STRING 0=true),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=builtin),
            test_token!(RPROXY),
            id_token!(FALSE),
            test_token!(LPROXY),
            id_token!(type),
            test_token!(COLON),
            test_token!(STRING 0=false),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=builtin),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // Atom
            id_token!(Atom),
            test_token!(ASSIGNMENT),
            id_token!(BuiltinType),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            test_token!(RPROXY),
            id_token!(LPAR),
            id_token!(Expression),
            test_token!(AT),
            id_token!(this),
            id_token!(RPAR),
            test_token!(LPROXY),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // Expression
            id_token!(Expression),
            test_token!(ASSIGNMENT),
            id_token!(Expression),
            test_token!(AT),
            id_token!(left),
            id_token!(PLUS),
            id_token!(Expression),
            test_token!(AT),
            id_token!(right),
            test_token!(LPROXY),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=add),
            test_token!(RPROXY),
            id_token!(Expression),
            test_token!(AT),
            id_token!(left),
            id_token!(ASTERISK),
            id_token!(Expression),
            test_token!(AT),
            id_token!(right),
            test_token!(LPROXY),
            id_token!(op),
            test_token!(COLON),
            test_token!(STRING 0=mul),
            test_token!(RPROXY),
            id_token!(Atom),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // Statement
            id_token!(Statement),
            test_token!(ASSIGNMENT),
            id_token!(Assignment),
            test_token!(AT),
            id_token!(this),
            id_token!(SEMICOLON),
            test_token!(LPROXY),
            id_token!(s),
            test_token!(COLON),
            test_token!(STRING 0=assign),
            test_token!(RPROXY),
            id_token!(IfStatement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(s),
            test_token!(COLON),
            test_token!(STRING 0=if),
            test_token!(RPROXY),
            id_token!(WhileStatement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(s),
            test_token!(COLON),
            test_token!(STRING 0=while),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // StatementList
            test_token!(AT),
            id_token!(StatementList),
            test_token!(ASSIGNMENT),
            id_token!(StatementList),
            test_token!(AT),
            id_token!(left),
            id_token!(Statement),
            test_token!(AT),
            id_token!(right),
            test_token!(LPROXY),
            id_token!(s),
            test_token!(COLON),
            test_token!(STRING 0=concatenate),
            test_token!(RPROXY),
            id_token!(Statement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
        ];

        for (i, tok) in result.iter().enumerate() {
            let token = lexed_input
                .next()
                .unwrap()
                .expect(format!("Expected token named {}, found EOF", tok.name).as_str());
            assert_eq!(
                tok,
                token,
                "Lexer error @{} {}:{} does not match token #{}",
                token.location().file(),
                token.location().start().0 + 1,
                token.location().end().0 + 1,
                i
            );
        }
        if let Some(token) = lexed_input.next().unwrap() {
            panic!(
                "Lexer error @{} {}:{} does not match token EOF",
                token.location().file(),
                token.location().start().0 + 1,
                token.location().end().0 + 1
            );
        }
    }
}

/// # Summary
///
/// `Token` contains information about a token, thus it contains
///  - `name`: the identifier of the token;
///  - `attributes`: the attributes of the token;
///  - `location`: the location of the substring that generated this token.
#[derive(Debug, Clone)]
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

    pub fn get(&self, key: usize) -> Option<&str> {
        self.attributes.get(&key).and_then(|x| Some(x.as_str()))
    }

    pub fn content(&self) -> &str {
        self.force_get(0)
    }

    pub fn force_get(&self, key: usize) -> &str {
        self.get(key).unwrap()
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
/// `with_grammar`: specify the grammar the lexer will use to lex.
/// `with_grammar_file`: specify the grammar file to build a grammar the lexer will use to lex.
#[derive(Debug)]
pub struct LexerBuilder {
    grammar: Option<LexerGrammar>,
}

impl LexerBuilder {
    /// Instantiate a new `LexerBuilder`.
    pub fn new() -> Self {
        Self { grammar: None }
    }

    /// Specify the lexer's grammar.
    pub fn with_grammar(mut self, grammar: LexerGrammar) -> Self {
        self.grammar = Some(grammar);
        self
    }

    /// Specify the lexer's grammar file.
    pub fn with_grammar_file(mut self, file: String) -> WResult<Self> {
        let mut warnings = WarningSet::empty();
        let grammar = ctry!(
            ctry!(LexerGrammarBuilder::new().with_file(file), warnings).build(),
            warnings
        );
        self.grammar = Some(grammar);
        WResult::WOk(self, warnings)
    }

    /// Build the lexer.
    pub fn build(mut self) -> WResult<Lexer> {
        let mut warnings = WarningSet::empty();
        let mut lexer = Lexer::new(retrieve!(self.grammar, warnings));
        WOk(lexer, warnings)
    }
}

impl Default for LexerBuilder {
    fn default() -> Self {
        Self::new()
            .with_grammar_file(String::from("gmrs/lexer.lx"))
            .unwrap()
    }
}

pub struct LexedStream<'a> {
    lexer: &'a Lexer,
    stream: &'a mut StringStream,
    pos: usize,
    tokens: Vec<Token>,
    last_location: Location,
}

impl<'a> LexedStream<'a> {
    pub fn new(lexer: &'a Lexer, stream: &'a mut StringStream) -> Self {
        Self {
            last_location: Location::new(stream.origin().to_string(), (0, 0), (0, 0)),
            lexer,
            stream,
            pos: 0,
            tokens: Vec::new(),
        }
    }
    fn lex_next(&mut self) -> WResult<bool> {
        let mut warnings = WarningSet::empty();
        if self.stream.pos() == self.stream.len() {
            WOk(false, warnings)
        } else {
            'lex: loop {
                if let Some(result) = self
                    .lexer
                    .grammar()
                    .pattern()
                    .find(&self.stream.borrow()[self.stream.pos()..])
                {
                    let start = self.stream.pos();
                    let end = start + result.length();
                    self.stream.set_pos(end);
                    if self.lexer.grammar().ignored(result.id()) {
                        continue;
                    }
                    let location = self.stream.loc_at(start, end);
                    let name = result.name().to_string();
                    let mut attributes = HashMap::new();
                    for (i, attr) in result.groups().iter().enumerate() {
                        if let Some(a) = attr {
                            attributes.insert(i, a.text().to_string());
                        }
                    }
                    let token = Token::new(name, attributes, location.clone());
                    self.last_location = location;
                    self.tokens.push(token);
                    break 'lex WOk(true, warnings);
                } else {
                    break 'lex WErr(Error::new(
                        self.stream.get_at(self.stream.pos()).unwrap().1,
                        ErrorType::LexingError(String::from("cannot recognize a token here")),
                    ));
                }
            }
        }
    }
    pub fn last_location(&self) -> &Location {
        &self.last_location
    }
}
impl LexedStream<'_> {
    pub fn get_at(&mut self, pos: usize) -> WResult<Option<&Token>> {
        let mut warnings = WarningSet::empty();
        while self.tokens.len() <= pos {
            if !ctry!(self.lex_next(), warnings) {
                return WOk(None, warnings);
            }
        }
        match self.tokens.get(pos) {
            Some(token) => WOk(Some(token), warnings),
            None => WOk(None, warnings),
        }
    }
    pub fn next(&mut self) -> WResult<Option<&Token>> {
        self.pos += 1;
        self.get_at(self.pos - 1)
    }
    pub fn get(&mut self) -> WResult<Option<&Token>> {
        self.get_at(self.pos)
    }
    pub fn pos_pp(&mut self) {
        self.pos += 1;
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
    grammar: LexerGrammar,
}

impl Lexer {
    /// Create a new `Lexer` object.
    /// You may want to use the `LexerBuilder` instead.
    pub fn new(grammar: LexerGrammar) -> Self {
        Self { grammar }
    }

    pub fn lex<'a>(&'a self, stream: &'a mut StringStream) -> LexedStream<'a> {
        LexedStream::new(self, stream)
    }

    pub fn grammar(&self) -> &LexerGrammar {
        &self.grammar
    }
}
