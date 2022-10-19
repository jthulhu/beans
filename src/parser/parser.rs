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
    Str(String),
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
