use super::grammarparser::{Grammar, GrammarBuilder};
use crate::error::WResult;
use crate::lexer::LexedStream;

pub type ParseResult = ();

pub trait Parser<'a> {
    type Grammar: Grammar<'a>;
    type GrammarBuilder: GrammarBuilder<'a, Grammar = Self::Grammar>;
    fn grammar(&self) -> &Self::Grammar;
    fn new(grammar: Self::Grammar) -> Self;
    fn parse(&self, input: &mut LexedStream) -> WResult<ParseResult>;
}
