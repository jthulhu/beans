use super::grammarparser::{Grammar, GrammarBuilder};
use crate::lexer::LexedStream;

pub type ParseResult = ();

pub trait Parser<'a> {
    type Grammar: Grammar<'a>;
    type GrammarBuilder: GrammarBuilder<'a, Grammar = Self::Grammar>;
    fn parse(&self, input: &LexedStream) -> ParseResult;
}
