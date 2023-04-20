use super::grammar::Grammar;
use crate::builder::Buildable;
use crate::error::ErrorKind;
use crate::error::Result;
use crate::parser::AST;
use crate::regex::Allowed;
use crate::span::Span;
use crate::stream::StringStream;

use fragile::Fragile;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::path::Path;
use std::path::PathBuf;

newty! {
    #[derive(PartialOrd, Ord)]
    pub id TerminalId
}

/// # Summary
///
/// `Token` contains information about a token, thus it contains
///  - `name`: the identifier of the token;
///  - `attributes`: the attributes of the token;
///  - `location`: the location of the substring that generated this token.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token {
    name: String,
    id: TerminalId,
    attributes: HashMap<usize, String>,
    span: Span,
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
    pub fn new(
        name: String,
        id: TerminalId,
        attributes: HashMap<usize, String>,
        span: Span,
    ) -> Self {
        Self {
            name,
            id,
            attributes,
            span,
        }
    }

    /// Return whether the token has a given attribute.
    pub fn contains(&self, key: usize) -> bool {
        self.attributes.contains_key(&key)
    }

    /// Return the value captured by the group `key` if any,
    /// or `None` otherwise.
    pub fn get(&self, key: usize) -> Option<&str> {
        self.attributes.get(&key).map(|x| x.as_str())
    }

    /// Borrows the tokens attributes, as a HashMap.
    pub fn attributes(&self) -> &HashMap<usize, String> {
        &self.attributes
    }

    /// Return the value of the first group (usually, the whole regex
    /// match), panicking if there is none.
    pub fn content(&self) -> &str {
        self.force_get(0)
    }

    /// Return the value captured by the group `key` if any, panic
    /// otherwise.
    ///
    /// Since this method panics instead of returning an error,
    /// it is recommended to use [`get`] instead.
    pub fn force_get(&self, key: usize) -> &str {
        self.get(key).unwrap()
    }

    /// Return the `name` of the token.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Return the `id` of the token.
    pub fn id(&self) -> TerminalId {
        self.id
    }

    /// Return the `location` of the token.
    pub fn span(&self) -> &Span {
        &self.span
    }
}

/// # Summary
///
/// [`LexedStream`] is the interface used to tokenize a stream. To do
/// so, you first create a [`Lexer`] which defines the lexing grammar.
/// You then create the [`LexedStream`] with an exclusive reference to
/// a [`StringStream`][beans::stream::StringStream]. Then, you get the
/// tokens through [`LexedStream`], which will consume the stream but
/// only for as much as you require. This allows you to use a [`Lexer`]
/// for many [`StringStream`][beans::stream::StringStream], and
/// conversely to tokenize a single
/// [`StringStream`][beans::stream::StringStream] with different
/// [`Lexer`].
#[derive(Debug)]
pub struct LexedStream<'lexer, 'stream> {
    pub(crate) lexer: &'lexer Lexer,
    stream: &'stream mut StringStream,
    pos: usize,
    tokens: Vec<(usize, Token)>,
    last_span: Span,
}

impl<'lexer, 'stream> LexedStream<'lexer, 'stream> {
    /// Create a new [`LexedStream`] instance.
    pub fn new(lexer: &'lexer Lexer, stream: &'stream mut StringStream) -> Self {
        Self {
            last_span: Span::new(
                stream.origin(),
                (0, 0),
                (0, 0),
                0,
                0,
                stream.text(),
                stream.lines(),
            ),
            lexer,
            stream,
            pos: 0,
            tokens: Vec::new(),
        }
    }

    fn lex_next(&mut self, allowed: Allowed) -> Result<bool> {
        'lex: loop {
            if self.stream.is_empty() {
                break 'lex Ok(false);
            } else if let Some(result) = self
                .lexer
                .grammar()
                .pattern()
                .find(self.stream.peek(), &allowed)
            {
                let name = result.name().to_string();
                let mut attributes = HashMap::new();
                for (i, attr) in result.groups().iter().enumerate() {
                    if let Some(a) = attr {
                        attributes.insert(i, a.text(self.stream.peek()).to_string());
                    }
                }
                let start = self.stream.pos();
                self.stream.shift(result.chars_length());
                let end = self.stream.pos();
                let span = self.stream.span_between(start, end - 1);
                if let Some(err_message) = self.lexer.grammar().err_message(result.id()) {
                    break 'lex ErrorKind::UnwantedToken {
                        span: Fragile::new(span),
                        message: err_message.to_string(),
                    }
                    .err();
                }
                if self.lexer.grammar().ignored(result.id()) {
                    continue;
                }
                let id = self.lexer.grammar.id(&name).unwrap();
                let token = Token::new(name, id, attributes, span.clone());
                self.last_span = span;
                self.tokens.push((start, token));
                break 'lex Ok(true);
            } else {
                break 'lex ErrorKind::LexingError {
                    span: Fragile::new(self.stream.curr_span()),
                }
                .err();
            }
        }
    }

    /// Get the last span lexed. Useful if you want to know where you failed to find a token.
    pub fn last_span(&self) -> &Span {
        &self.last_span
    }
}

impl LexedStream<'_, '_> {
    /// Lex any token.
    pub fn next_any(&mut self) -> Result<Option<&Token>> {
        self.next(Allowed::All)
    }

    /// Lex any allowed token.
    pub fn next(&mut self, allowed: Allowed) -> Result<Option<&Token>> {
        self.pos += 1;
        if self.lex_next(allowed)? {
            Ok(self.tokens.last().map(|(_, token)| token))
        } else {
            Ok(None)
        }
    }

    /// Peek for the most recently lexed token, which has not been droped.
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.last().map(|(_, token)| token)
    }

    /// Drop the last token.
    pub fn drop_last(&mut self) {
        if let Some((pos, _)) = self.tokens.pop() {
            self.pos -= 1;
            while self.stream.pos() > pos {
                self.stream.decr_pos();
            }
        }
    }

    /// Get the lexer
    pub fn lexer(&self) -> &Lexer {
        self.lexer
    }

    pub fn is_empty(&self) -> bool {
        self.stream.is_empty()
    }
}
/// # Summary
///
/// `Lexer` is the main object that is used for lexing.
/// It is given a `StringStream`, and a `GrammarBuilder`, and it
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
    grammar: Grammar,
}

impl Lexer {
    pub fn new(grammar: Grammar) -> Self {
        Self { grammar }
    }

    /// Get a [`LexedStream`] on the stream.
    pub fn lex<'lexer, 'stream>(
        &'lexer self,
        stream: &'stream mut StringStream,
    ) -> LexedStream<'lexer, 'stream> {
        LexedStream::new(self, stream)
    }

    /// Get the [`Grammar`] bound to the lexer.
    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn from_path(path: &Path) -> Result<Self> {
        let grammar = Grammar::build_from_path(path)?;
        Ok(Self { grammar })
    }
}

impl Buildable for Lexer {
    const RAW_EXTENSION: &'static str = Grammar::RAW_EXTENSION;
    const COMPILED_EXTENSION: &'static str = Grammar::COMPILED_EXTENSION;
    const AST_EXTENSION: &'static str = Grammar::AST_EXTENSION;

    fn build_from_ast(ast: AST) -> Result<Self> {
        let grammar = Grammar::build_from_ast(ast)?;
        Ok(Self { grammar })
    }

    fn build_from_compiled(blob: &[u8], path: impl ToOwned<Owned = PathBuf>) -> Result<Self> {
        let grammar = Grammar::build_from_compiled(blob, path)?;
        Ok(Self { grammar })
    }

    fn build_from_plain(raw: StringStream) -> Result<Self> {
        let grammar = Grammar::build_from_plain(raw)?;
        Ok(Self { grammar })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    macro_rules! test_token {
	($name: ident $($key:literal = $value: tt), *) => {
	    {
		let name = stringify!($name).to_string();
		#[allow(unused_mut)]
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
        };
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
                    right.attributes.get(key).filter(|&v| v == value).is_some()
                })
        }
    }

    #[test]
    fn token() {
        let token = Token::new(
            String::from("wow"),
            0.into(),
            HashMap::new(),
            Span::new(
                Path::new("test_file"),
                (3, 0),
                (3, 2),
                10,
                12,
                "",
                Vec::new(),
            ),
        );

        assert_eq!(token.name(), "wow");
        assert_eq!(token.id(), 0.into());
        assert_eq!(&*token.span().file(), Path::new("test_file"));
        assert_eq!(token.span().start(), (3, 0));
        assert_eq!(token.span().end(), (3, 2));
    }

    #[test]
    fn lexer_builder() {
        Lexer::build_from_plain(StringStream::new(
            Path::new("<building the lexer>"),
            "A ::= blu",
        ))
        .unwrap();
    }

    #[test]
    fn lex_basic() {
        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<basic lexing>"),
            "A ::= blu",
        ))
        .unwrap();
        let mut input = StringStream::new(Path::new("input file"), "blu");
        let mut lexed_input = lexer.lex(&mut input);

        let token = lexed_input.next(Allowed::All).unwrap().unwrap();
        assert_eq!(token.span().start(), (0, 0));
        assert_eq!(token.span().end(), (0, 2));
        assert!(lexed_input.next(Allowed::All).unwrap().is_none());
    }

    #[test]
    fn unwantend_token() {
        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<unwanted token>"),
            r#"ignore COMMENT ::= /\*([^*]|\*[^/])\*/
(unclosed comment) unwanted ECOMMENT ::= /\*([^*]|\*[^/])"#,
        ))
        .unwrap();
        let mut input = StringStream::new(Path::new("<unwanted input>"), "/* hello /");
        let mut lexed_input = lexer.lex(&mut input);

        let ErrorKind::UnwantedToken { message, .. } =
            *lexed_input.next(Allowed::All).unwrap_err().kind
	else {
	    panic!("wrong error");
	};
        assert_eq!("unclosed comment", message);
    }

    fn verify_input(
        mut lexed_input: LexedStream<'_, '_>,
        result: &[(Location, Location, &str)],
    ) {
        let origin = &*lexed_input.stream.origin();
        let mut i = 0;
        while let Some(token) = lexed_input.next_any().unwrap() {
            let (start, end, name) = result[i];
            assert_eq!(
                token.span().start(),
                start,
                "Token #{} {} differ by start location in stream {}.",
                i,
                token,
                origin.display()
            );
            assert_eq!(
                token.span().end(),
                end,
                "Token #{} {} differ by end location in stream {}.",
                i,
                token,
                origin.display()
            );
            assert_eq!(
                token.name(),
                name,
                "Token #{} {} differ by name in stream {}.",
                i,
                token,
                origin.display()
            );
            i += 1;
        }

        assert_eq!(result.len(), i);
    }

    #[test]
    fn default_lex_grammar() {
        let lexer = Lexer::build_from_path(Path::new("src/parser/gmrs/dummy.lx")).unwrap();

        let result = [
            ((0, 0), (0, 2), "ID"),
            ((0, 4), (0, 4), "PLUS"),
            ((0, 6), (0, 8), "ID"),
        ];
        verify_input(
            lexer.lex(&mut StringStream::new(Path::new("<input>"), "one + two")),
            &result,
        );

        let result = [
            ((0, 0), (0, 1), "IF"),
            ((0, 3), (0, 6), "TRUE"),
            ((0, 8), (0, 10), "AND"),
            ((0, 12), (0, 16), "FALSE"),
            ((0, 18), (0, 18), "LBRACE"),
            ((1, 1), (1, 5), "ID"),
            ((1, 6), (1, 6), "LPAR"),
            ((1, 7), (1, 17), "STRING"),
            ((1, 18), (1, 18), "RPAR"),
            ((2, 0), (2, 0), "RBRACE"),
        ];
        verify_input(
            lexer.lex(&mut StringStream::new(
                Path::new("<input>"),
                "if true and false {\n\tifeat(\"something\")\n}",
            )),
            &result,
        );
    }

    #[test]
    fn default_parser_grammar() {
        let lexer = Lexer::build_from_path(Path::new("src/parser/gmrs/earley.lx")).unwrap();
        let mut input = StringStream::from_file(Path::new("src/parser/gmrs/dummy.gr")).unwrap();
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
            id_token!(NoElse),
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
            id_token!(Else),
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
            id_token!(Int),
            test_token!(RPROXY),
            id_token!(STRING),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            id_token!(String),
            test_token!(RPROXY),
            id_token!(ID),
            test_token!(DOT),
            test_token!(INT 0=0),
            test_token!(AT),
            id_token!(value),
            test_token!(LPROXY),
            id_token!(Id),
            test_token!(RPROXY),
            id_token!(TRUE),
            test_token!(LPROXY),
            id_token!(True),
            test_token!(RPROXY),
            id_token!(FALSE),
            test_token!(LPROXY),
            id_token!(False),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
            // Atom
            id_token!(Atom),
            test_token!(ASSIGNMENT),
            id_token!(BuiltinType),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(Builtin),
            test_token!(RPROXY),
            id_token!(LPAR),
            id_token!(Expression),
            test_token!(AT),
            id_token!(this),
            id_token!(RPAR),
            test_token!(LPROXY),
            id_token!(Through),
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
            id_token!(Add),
            test_token!(RPROXY),
            id_token!(Expression),
            test_token!(AT),
            id_token!(left),
            id_token!(ASTERISK),
            id_token!(Expression),
            test_token!(AT),
            id_token!(right),
            test_token!(LPROXY),
            id_token!(Mul),
            test_token!(RPROXY),
            id_token!(Atom),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(Through),
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
            id_token!(Assign),
            test_token!(RPROXY),
            id_token!(IfStatement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(If),
            test_token!(RPROXY),
            id_token!(WhileStatement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(While),
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
            id_token!(Concat),
            test_token!(RPROXY),
            id_token!(Statement),
            test_token!(AT),
            id_token!(this),
            test_token!(LPROXY),
            id_token!(Through),
            test_token!(RPROXY),
            test_token!(SEMICOLON),
        ];

        for (i, tok) in result.iter().enumerate() {
            let token = lexed_input
                .next(Allowed::All)
                .unwrap()
                .unwrap_or_else(|| panic!("Expected token named {}, found EOF", tok.name));
            assert_eq!(
                tok,
                token,
                "Lexer error @{} {}:{} does not match token #{}",
                token.span().file().display(),
                token.span().start().0 + 1,
                token.span().end().0 + 1,
                i
            );
        }
        if let Some(token) = lexed_input.next(Allowed::All).unwrap() {
            panic!(
                "Lexer error @{} {}:{} does not match token EOF",
                token.span().file().display(),
                token.span().start().0 + 1,
                token.span().end().0 + 1
            );
        }
    }
}
