use super::grammarparser::{Grammar, GrammarBuilder};
use crate::error::WResult;
use crate::lexer::LexedStream;

pub type ParseResult = ();

pub trait Parser<'deserializer> {
    type Grammar: Grammar<'deserializer>;
    type GrammarBuilder: GrammarBuilder<'deserializer, Grammar = Self::Grammar>;
    fn grammar(&self) -> &Self::Grammar;
    fn new(grammar: Self::Grammar) -> Self;
    fn parse<'input>(&self, input: &'input mut LexedStream<'input>) -> WResult<ParseResult>;
}
