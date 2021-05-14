use super::grammarparser::{Grammar, GrammarBuilder};
use crate::error::WResult;
use crate::lexer::LexedStream;

pub type ParseResult = ();

pub trait Parser<'grammar> {
    type Grammar: Grammar<'grammar>;
    type GrammarBuilder: GrammarBuilder<'grammar, Grammar = Self::Grammar>;
    fn grammar(&self) -> &Self::Grammar;
    fn new(grammar: Self::Grammar) -> Self;
    fn parse<'warning, 'input>(&self, input: &'input mut LexedStream<'input>) -> WResult<'warning, ParseResult>
    where
	'input: 'warning;
}
