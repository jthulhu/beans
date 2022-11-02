use super::grammarparser::{Grammar, GrammarBuilder};
use crate::error::Result;
use crate::lexer::{LexedStream, Token};
use newty::newty;
use std::collections::HashMap;
use std::rc::Rc;

newty! {
    pub id RuleId
}
newty! {
    pub id NonTerminalId
    impl {
        pub fn next(&mut self) -> Self {
            self.0 += 1;
            NonTerminalId(self.0-1)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Str(Rc<str>),
    Float(f32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Node {
        nonterminal: NonTerminalId,
        attributes: HashMap<Rc<str>, AST>,
    },
    Terminal(Token),
    Literal(Value),
}

/// Successful result of the parse of an input.
#[derive(Debug)]
pub struct ParseResult {
    pub tree: AST,
}

/// Something that implements [`Parser`] is able to, given a certain grammar,
/// parse a [`LexedStream`] following the grammar.
pub trait Parser<'deserializer> {
    /// The grammar given to the parser.
    type Grammar: Grammar<'deserializer>;
    /// The builder type for the grammar.
    type GrammarBuilder: GrammarBuilder<'deserializer, Grammar = Self::Grammar>;
    /// Getter to the grammar.
    fn grammar(&self) -> &Self::Grammar;
    /// Create a new parser.
    fn new(grammar: Self::Grammar) -> Self;
    /// Parse the given [`LexedStream`].
    fn parse<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> Result<ParseResult>;
    /// Just return whether the input is recognised.
    fn is_valid<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> bool {
        self.parse(input).is_ok()
    }
}

/// Create a parser, shipping all the grammars into the binary.
/// ```rust
/// # use beans::parser::include_parser;
/// let (lexer, parser) = include_parser!(
///     lexer => compiled "gmrs/dummy.clx",
///     parser => compiled "gmrs/dummy.cgr",
///  ).unwrap().unwrap();
#[macro_export]
macro_rules! include_parser {
    (lexer => compiled $path:literal, $($rest:tt)*) => {{
	let driver_function =
	    || -> $crate::error::Result<($crate::lexer::Lexer, $crate::parser::earley::EarleyParser)> {
		let mut warnings = $crate::error::WarningSet::empty();
		let lexer_grammar_source = include_bytes!($path);
		let lexer_grammar =
		    $crate::lexer::LexerGrammar::deserialize(lexer_grammar_source)?
		    .unpack_into(&mut warnings);
		let lexer =
		    $crate::lexer::LexerBuilder::from_grammar(lexer_grammar).build();
		let parser = include_parser!(@parser(&mut warnings, lexer) $($rest)*);
		warnings.with_ok((lexer, parser))
	    };
	driver_function()
    }};
    (lexer => $path:literal, $($rest:tt)*) => {{
	let driver_function =
	    || -> $crate::error::Result<($crate::lexer::Lexer, $crate::parser::earley::EarleyParser)> {
		let mut warnings = $crate::error::WarningSet::empty();
		let lexer_grammar_source = include_str!($path);
		let lexer_grammar_stream = StringStream::new(
		    ::std::path::Path::new($path),
		    lexer_grammar_source.to_string(),
		);
		let lexer_grammar =
		    $crate::lexer::LexerGrammar::from_stream(lexer_grammar_stream)
		    .build()?
		    .unpack_into(&mut warnings);
		let lexer =
		    $crate::lexer::LexerBuilder::from_grammar(lexer_grammar).build();
		let parser = include_parser!(@parser(&mut warnings, lexer) $($rest)*);
		warnings.with_ok((lexer, parser))
	    };
	driver_function()
    }};
    (@parser($warnings:expr, $lexer:expr) parser => compiled $path:literal $(,)?) => {{
	let parser_grammar_source = include_bytes!($path);
	let parser_grammar =
	    <$crate::parser::earley::EarleyGrammar
	     as $crate::parser::Grammar>::deserialize(parser_grammar_source)?
	    .unpack_into($warnings);
	let parser =
	    <$crate::parser::earley::EarleyParser
	     as $crate::parser::Parser>::new(parser_grammar);
	parser
    }};
    (@parser($warnings:expr, $lexer:expr) parser => $path:literal $(,)?) => {{
	let parser_grammar_source = include_str!($path);
	let parser_grammar_stream = StringStream::new(
	    ::std::path::Path::new($path),
	    parser_grammar_source.to_string(),
	);
	let parser_grammar =
	    $crate::parser::earley::EarleyGrammarBuilder::default()
	    .with_stream(parser_grammar_stream)
	    .build(&lexer)?
	    .unpack_into($warnings);
	let parser =
	    <$crate::parser::earley::EarleyParser
	     as $crate::parser::Parser>::new(parser_grammar);
	parser
    }};
}
