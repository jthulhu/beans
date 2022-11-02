use super::grammarparser::{LexerGrammar, LexerGrammarBuilder};
use crate::error::Result;
use crate::error::{Error, WarningSet};
use crate::location::Span;
use crate::regex::Allowed;
use crate::stream::StringStream;
use fragile::Fragile;
use newty::newty;
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::path::Path;
use std::rc::Rc;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::location::CharLocation;

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
            0.into(),
            HashMap::new(),
            Span::new(Path::new("test_file"), (3, 0), (3, 3)),
        );

        assert_eq!(token.name(), "wow");
        assert_eq!(token.id(), 0.into());
        assert_eq!(&*token.location().file(), Path::new("test_file"));
        assert_eq!(token.location().start(), (3, 0));
        assert_eq!(token.location().end(), (3, 3));
    }

    #[test]
    fn lexer_builder() {
        // A grammar: succeeds
        LexerBuilder::from_grammar(
            LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("grammar file"),
                "A ::= blu",
            ))
            .build()
            .unwrap()
            .unwrap(),
        )
        .build();
    }

    #[test]
    fn lex_basic() {
        let lexer = LexerBuilder::from_grammar(
            LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("a file name"),
                "A ::= blu",
            ))
            .build()
            .unwrap()
            .unwrap(),
        )
        .build();
        let mut input = StringStream::new(Path::new("input file"), "blu");
        let mut lexed_input = lexer.lex(&mut input);

        let token = lexed_input.next(Allowed::All).unwrap().unwrap().unwrap(); // nice...
        assert_eq!(token.location().start(), (0, 0));
        assert_eq!(token.location().end(), (0, 3));
        assert!(lexed_input.next(Allowed::All).unwrap().unwrap().is_none());
    }

    fn verify_input(
        mut lexed_input: LexedStream<'_, '_>,
        result: &[(CharLocation, CharLocation, &str)],
    ) {
        let origin = &*lexed_input.stream.origin();
        let mut i = 0;
        while let Some(token) = lexed_input.next_any().unwrap().unwrap() {
            let (start, end, name) = result[i];
            assert_eq!(
                token.location().start(),
                start,
                "Token #{} {} differ by start location in stream {}.",
                i,
                token,
                origin.display()
            );
            assert_eq!(
                token.location().end(),
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
        let lexer = LexerBuilder::from_file(Path::new("src/parser/gmrs/dummy.lx"))
            .unwrap()
            .unwrap()
            .build();

        let result = [
            ((0, 0), (0, 3), "ID"),
            ((0, 4), (0, 5), "PLUS"),
            ((0, 6), (0, 9), "ID"),
        ];
        verify_input(
            lexer
                .lex(&mut StringStream::new(Path::new("<input>"), "one + two")),
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
                Path::new("<input>"),
                "if true and false {\n\tifeat(\"something\")\n}",
            )),
            &result,
        );
    }

    #[test]
    fn default_parser_grammar() {
        let lexer = LexerBuilder::from_file(Path::new("src/parser/gmrs/earley.lx"))
            .unwrap()
            .unwrap()
            .build();
        let mut input = StringStream::from_file(Path::new("src/parser/gmrs/dummy.gr"))
            .unwrap()
            .unwrap();
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
            let token =
                lexed_input.next(Allowed::All).unwrap().unwrap().expect(
                    format!("Expected token named {}, found EOF", tok.name)
                        .as_str(),
                );
            assert_eq!(
                tok,
                token,
                "Lexer error @{} {}:{} does not match token #{}",
                token.location().file().display(),
                token.location().start().0 + 1,
                token.location().end().0 + 1,
                i
            );
        }
        if let Some(token) = lexed_input.next(Allowed::All).unwrap().unwrap() {
            panic!(
                "Lexer error @{} {}:{} does not match token EOF",
                token.location().file().display(),
                token.location().start().0 + 1,
                token.location().end().0 + 1
            );
        }
    }
}

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    name: String,
    id: TerminalId,
    attributes: HashMap<usize, String>,
    location: Span,
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
        location: Span,
    ) -> Self {
        Self {
            name,
            id,
            attributes,
            location,
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

    /// Return the value of the first group (usually, the whole
    /// regex match), panicking if there is none.
    pub fn content(&self) -> &str {
        self.force_get(0)
    }

    /// Return the value captured by the group `key` if any,
    /// panic otherwise.
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
    pub fn location(&self) -> &Span {
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
    grammar: LexerGrammar,
}

impl LexerBuilder {
    /// Instantiate a new `LexerBuilder`, giving a grammar.
    pub fn from_grammar(grammar: LexerGrammar) -> Self {
        Self { grammar }
    }

    /// Instantiate a new `LexerBuilder`, giving a stream defining the grammar.
    /// Since the stream may be an ill-defined grammar, this might fail.
    pub fn from_stream(stream: StringStream) -> Result<Self> {
        let mut warnings = WarningSet::empty();
        let grammar = LexerGrammarBuilder::from_stream(stream)
            .build()?
            .unpack_into(&mut warnings);
        warnings.with_ok(Self { grammar })
    }

    /// Instantiate a new `LexerBuilder`, giving the path to the grammar.
    /// Since the grammar may be ill-defined, this might fail.
    pub fn from_file(file: impl Into<Rc<Path>>) -> Result<Self> {
        let mut warnings = WarningSet::default();
        let file = file.into();
        let builder =
            LexerGrammarBuilder::from_file(file)?.unpack_into(&mut warnings);
        let grammar = builder.build()?.unpack_into(&mut warnings);
        warnings.with_ok(Self { grammar })
    }

    /// Build the lexer, consuming the builder.
    pub fn build(self) -> Lexer {
        Lexer::new(self.grammar)
    }
}

/// # Summary
///
/// [`LexedStream`] is the interface used to tokenize a stream. To do so, you first create a [`Lexer`]
/// which defines the lexing grammar. You then create the [`LexedStream`] with an exclusive
/// reference to a [`StringStream`][beans::stream::StringStream]. Then, you get the tokens through [`LexedStream`], which will
/// consume the stream but only for as much as you require. This allows you to use a [`Lexer`] for
/// many [`StringStream`][beans::stream::StringStream], and conversely to tokenize a single [`StringStream`][beans::stream::StringStream] with different
/// [`Lexer`].
#[derive(Debug)]
pub struct LexedStream<'lexer, 'stream> {
    pub(crate) lexer: &'lexer Lexer,
    stream: &'stream mut StringStream,
    pos: usize,
    tokens: Vec<(usize, Token)>,
    last_location: Span,
}

impl<'lexer, 'stream> LexedStream<'lexer, 'stream> {
    /// Create a new [`LexedStream`] instance.
    pub fn new(
        lexer: &'lexer Lexer,
        stream: &'stream mut StringStream,
    ) -> Self {
        Self {
            last_location: Span::new(stream.origin(), (0, 0), (0, 0)),
            lexer,
            stream,
            pos: 0,
            tokens: Vec::new(),
        }
    }

    fn lex_next(&mut self, allowed: Allowed) -> Result<bool> {
        let warnings = WarningSet::empty();
        'lex: loop {
            if self.stream.is_empty() {
                break 'lex warnings.with_ok(false);
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
                        attributes
                            .insert(i, a.text(self.stream.peek()).to_string());
                    }
                }
                let start = self.stream.pos();
                self.stream.shift(result.chars_length());
                let end = self.stream.pos();
                if self.lexer.grammar().ignored(result.id()) {
                    continue;
                }
                let span = self.stream.span_between(start, end);
                let id = self.lexer.grammar.id(&name).unwrap();
                let token = Token::new(name, id, attributes, span.clone());
                self.last_location = span;
                self.tokens.push((start, token));
                break 'lex warnings.with_ok(true);
            } else {
                break 'lex Err(Error::LexingError {
                    location: Fragile::new(self.stream.curr_span()),
                    message: String::from("cannot recognize a token here"),
                });
            }
        }
    }

    /// Get the last location lexed. Useful if you want to know where you failed to find a token.
    pub fn last_location(&self) -> &Span {
        &self.last_location
    }
}

impl LexedStream<'_, '_> {
    /// Lex any token.
    pub fn next_any(&mut self) -> Result<Option<&Token>> {
        self.next(Allowed::All)
    }

    /// Lex any allowed token.
    pub fn next(&mut self, allowed: Allowed) -> Result<Option<&Token>> {
        let mut warnings = WarningSet::empty();
        self.pos += 1;
        if self.lex_next(allowed)?.unpack_into(&mut warnings) {
            warnings.with_ok(self.tokens.last().map(|(_, token)| token))
        } else {
            warnings.with_ok(None)
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

    /// Get a [`LexedStream`] on the stream.
    pub fn lex<'lexer, 'stream>(
        &'lexer self,
        stream: &'stream mut StringStream,
    ) -> LexedStream<'lexer, 'stream> {
        LexedStream::new(self, stream)
    }

    /// Get the [`LexerGrammar`] bound to the lexer.
    pub fn grammar(&self) -> &LexerGrammar {
        &self.grammar
    }
}
