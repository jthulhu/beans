use super::grammarparser::{Grammar, GrammarBuilder};
use crate::error::WResult;
use crate::lexer::LexedStream;
use hashbrown::HashMap;
use std::rc::Rc;

// pub enum BuiltinType {
//     Int(i32),
//     String(String),
//     Id(String),
//     Bool(bool)
// }

// pub struct Assignment {

// }

// pub enum Statement {
//     Assignment {
// 	condition: Expression,
// 	then ,
//     If,
//     While
// }

// pub enum AST {
//     StatementList {
// 	statements: Vec<AST>
//     }
//     Statement {
// 	statement
// }

#[derive(Debug)]
pub enum AST {
    Node {
        name: usize,
        attributes: HashMap<Rc<str>, AST>,
    },
    Literal(String),
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
    fn parse<'input>(&self, input: &'input mut LexedStream<'input, 'input>)
        -> WResult<ParseResult>;
    /// Just return whether the input is recognised.
    fn recognise<'input>(&self, input: &'input mut LexedStream<'input, 'input>) -> bool {
        self.parse(input).is_ok()
    }
}
