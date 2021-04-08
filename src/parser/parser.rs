use super::grammarparser::{Grammar, GrammarBuilder};

pub trait Parser<'a> {
    type Grammar: Grammar<'a>;
    type GrammarBuilder: GrammarBuilder<'a, Grammar = Self::Grammar>;
}
