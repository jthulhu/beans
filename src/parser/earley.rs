use super::grammarparser::{self, ElementType, Grammar, GrammarBuilder, Rule};
use super::parser;
use crate::error::{
    Error,
    WResult::{self, WErr, WOk},
    WarningSet,
};
use crate::stream::StringStream;
use crate::{ctry, retrieve};
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::mem;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules;

    use crate::parser::grammarparser::{
        Attribute, ElementType, Key, Proxy, Rule, RuleElement, Value,
    };

    #[derive(Debug)]
    enum TestElementType {
        Terminal,
        NonTerminal,
    }

    impl PartialEq<ElementType> for TestElementType {
        fn eq(&self, other: &ElementType) -> bool {
            match (self, other) {
                (Self::Terminal, ElementType::Terminal(..))
                | (Self::NonTerminal, ElementType::NonTerminal(..)) => true,
                _ => false,
            }
        }
    }

    #[derive(Debug)]
    struct TestElement {
        name: String,
        attribute: Attribute,
        key: Option<Key>,
        element_type: TestElementType,
    }

    impl PartialEq<RuleElement> for TestElement {
        fn eq(&self, other: &RuleElement) -> bool {
            self.name == other.name
                && self.key == other.key
                && self.attribute == other.attribute
                && self.element_type == other.element_type
        }
    }

    #[derive(Debug)]
    struct TestRule {
        name: String,
        elements: Vec<TestElement>,
        proxy: Proxy,
    }

    impl TestRule {
        fn new(name: String, elements: Vec<TestElement>, proxy: Proxy) -> Self {
            Self {
                name,
                elements,
                proxy,
            }
        }
    }

    impl PartialEq<Rule> for TestRule {
        fn eq(&self, other: &Rule) -> bool {
            self.name == other.name && self.proxy == other.proxy && self.elements == other.elements
        }
    }

    #[inline]
    fn verify(rules1: Vec<Rule>, rules2: Vec<TestRule>) {
        let length1 = rules1.len();
        let length2 = rules2.len();
        if length1 > length2 {
            panic!("Grammar 1 is longer");
        } else if length1 < length2 {
            panic!("Grammar 2 is longer");
        }
        for (i, (r1, r2)) in rules1.iter().zip(rules2.iter()).enumerate() {
            assert_eq!(r2, r1, "rules #{} differ", i);
        }
    }

    #[test]
    #[rustfmt::skip]
    fn earley_grammar_builder() {
	use crate::lexer::LexerBuilder;
        let lexer = LexerBuilder::default().build().unwrap();
        let grammar = EarleyGrammarBuilder::default().build(&lexer).unwrap();
	verify(
            grammar.rules,
            rules!(
                IfStatement ::=
		    !t IF Expression@condition !t LBRACE StatementList@then !t RBRACE <haselse = bool false>
		    !t IF Expression@condition !t LBRACE StatementList@then !t RBRACE !t ELSE !t LBRACE StatementList@else !t RBRACE <haselse = bool true>;
		
                WhileStatement ::=
		    !t WHILE Expression@condition !t LBRACE StatementList@do !t RBRACE <>;
                
                Assignment ::=
		    !t ID.idx 0@key !t EQUALS Expression@value <>;
                
                BuiltinType ::=
		    !t INT.idx 0@value <type = str "int" op = str "builtin">
		    !t STRING.idx 0@value <type = str "string" op = str "builtin">
		    !t ID.idx 0@value <type = str "id" op = str "builtin">
		    !t TRUE <type = str "true" op = str "builtin">
		    !t FALSE <type = str "false" op = str "builtin">;
                
                Atom ::=
		    BuiltinType@this <>
		    !t LPAR Expression@this !t RPAR <>;
                
                Expression ::=
		    Expression@left !t PLUS Expression@right <op = str "add">
		    Expression@left !t ASTERISK Expression@right <op = str "mul">
		    Atom@this <>;
                
                Statement ::=
		    Assignment@this !t SEMICOLON <s = str "assign">
		    IfStatement@this <s = str "if">
		    WhileStatement@this <s = str "while">;
                
                StatementList ::=
		    StatementList@left Statement@right <s = str "concatenate">
		    Statement@this <>
            ),
        );
    }
}

#[derive(Debug)]
pub struct EarleyGrammarBuilder {
    stream: Option<StringStream>,
    grammar: String,
}

impl EarleyGrammarBuilder {
    pub fn new(grammar: String) -> Self {
        Self {
            stream: None,
            grammar,
        }
    }
}

impl GrammarBuilder<'_> for EarleyGrammarBuilder {
    type Grammar = EarleyGrammar;
    fn with_stream(mut self, stream: StringStream) -> Self {
        self.stream = Some(stream);
        self
    }
    fn with_grammar(mut self, grammar: String) -> Self {
        self.grammar = grammar;
        self
    }
    fn stream(&mut self) -> WResult<StringStream> {
        let mut warnings = WarningSet::empty();
        let stream = retrieve!(self.stream, warnings);
        WOk(stream, warnings)
    }
    fn grammar(&mut self) -> String {
        mem::replace(&mut self.grammar, String::new())
    }
}

impl Default for EarleyGrammarBuilder {
    fn default() -> Self {
        Self::new(String::from("gmrs/parser.lx"))
            .with_file(String::from("gmrs/parser.gmr"))
            .unwrap()
    }
}

#[derive(Debug)]
pub struct EarleyItem {
    pub rule: usize,
    pub origin: usize,
    pub position: usize,
}

#[derive(Serialize, Deserialize)]
pub struct EarleyGrammar {
    axioms: FixedBitSet,
    rules: Vec<Rule>,
    nullables: FixedBitSet,
    name_map: HashMap<String, usize>,
}

impl Grammar<'_> for EarleyGrammar {
    fn new(
        rules: Vec<Rule>,
        axioms: FixedBitSet,
        name_map: HashMap<String, usize>,
    ) -> WResult<Self> {
        let mut warnings = WarningSet::empty();
        let mut nullables = FixedBitSet::with_capacity(axioms.len());

        let mut is_in = vec![Vec::new(); rules.len()];
        let mut stack = VecDeque::with_capacity(is_in.len());
        for (i, rule) in rules.iter().enumerate() {
            if rule.elements.is_empty() {
                nullables.insert(i);
                stack.push_front(i);
            }
            for element in rule.elements.iter() {
                if let ElementType::NonTerminal(Some(id)) = element.element_type {
                    is_in[id].push(i);
                }
            }
        }
        while let Some(current) = stack.pop_back() {
            for &id in &is_in[current] {
                if !nullables.contains(id)
                    && rules[id]
                        .elements
                        .iter()
                        .all(|element| match element.element_type {
                            ElementType::NonTerminal(Some(id)) => nullables.contains(id),
                            _ => true,
                        })
                {
                    nullables.insert(id);
                    stack.push_front(id);
                }
            }
        }

        WOk(
            Self {
                nullables,
                rules,
                axioms,
                name_map,
            },
            warnings,
        )
    }
}

pub struct EarleyParser {}

impl parser::Parser<'_> for EarleyParser {
    type GrammarBuilder = EarleyGrammarBuilder;
    type Grammar = EarleyGrammar;
}
