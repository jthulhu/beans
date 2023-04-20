use crate::error::Result;
use crate::lexer::{LexedStream, Token};
use crate::span::Span;
use crate::typed::Tree;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::rc::Rc;

newty! {
    pub id NonTerminalId
    impl {
        pub fn next(&mut self) -> Self {
            self.0 += 1;
            NonTerminalId(self.0-1)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i32),
    Str(Rc<str>),
    Float(f32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AST {
    Node {
        nonterminal: NonTerminalId,
        attributes: HashMap<Rc<str>, AST>,
        span: Span,
    },
    Literal {
        value: Value,
        span: Option<Span>,
    },
    Terminal(Token),
}

impl AST {
    pub fn span(&self) -> Option<&Span> {
        match self {
            Self::Node { span, .. } => Some(span),
            Self::Literal { span, .. } => span.as_ref(),
            Self::Terminal(token) => Some(token.span()),
        }
    }

    pub fn to_tree<T: Tree>(self) -> Result<T> {
        T::read(self)
    }
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
    type Grammar;
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
    fn is_valid<'input>(&self, input: &'input mut LexedStream<'input, 'input>) -> bool {
        self.parse(input).is_ok()
    }
}
