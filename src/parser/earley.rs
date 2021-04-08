use std::collections::VecDeque;
use std::mem;
use crate::{ctry, retrieve};
use super::grammarparser::{self, ElementType, Rule, GrammarBuilder, Grammar};
use super::parser;
use crate::error::{
    Error,
    WarningSet,
    WResult::{
	self,
	WOk,
	WErr
    }
};
use crate::stream::StringStream;
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use serde::{Serialize, Deserialize};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{rule, collect};

    use crate::parser::grammarparser::{ElementType, Value, Attribute, Key, RuleElement, Rule, Proxy};
    
    #[derive(Debug)]
    enum TestElementType {
	Terminal,
	NonTerminal
    }

    impl PartialEq<ElementType> for TestElementType {
	fn eq(&self, other: &ElementType) -> bool {
	    match (self, other) {
		(Self::Terminal, ElementType::Terminal(..))
		    | (Self::NonTerminal, ElementType::NonTerminal(..)) => true,
		_ => false
	    }
	}
    }

    #[derive(Debug)]
    struct TestElement {
	name: String,
	attribute: Attribute,
	key: Option<Key>,
	element_type: TestElementType
    }

    impl PartialEq<RuleElement> for TestElement {
	fn eq(&self, other: &RuleElement) -> bool {
	    self.name == other.name && self.key == other.key && self.attribute == other.attribute && self.element_type == other.element_type
	}
    }

    #[derive(Debug)]
    struct TestRule {
	name: String,
	elements: Vec<TestElement>,
	proxy: Proxy
    }

    impl TestRule {
	fn new(name: String, elements: Vec<TestElement>, proxy: Proxy) -> Self {
	    Self {
		name,
		elements,
		proxy
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
            collect!(
                rule!(
                    IfStatement ::=
			!t IF Expression@condition !t LBRACE StatementList@then !t RBRACE <haselse = bool false>
			!t IF Expression@condition !t LBRACE StatementList@then !t RBRACE !t ELSE !t LBRACE StatementList@else !t RBRACE <haselse = bool true>

                ),
                rule!(
                    WhileStatement ::=
			!t WHILE Expression@condition !t LBRACE StatementList@do !t RBRACE <>
                ),
                rule!(
                    Assignment ::=
			!t ID.idx 0@key !t EQUALS Expression@value <>
                ),
                rule!(
                    BuiltinType ::=
			!t INT.idx 0@value <type = str "int" op = str "builtin">
			!t STRING.idx 0@value <type = str "string" op = str "builtin">
			!t ID.idx 0@value <type = str "id" op = str "builtin">
			!t TRUE <type = str "true" op = str "builtin">
			!t FALSE <type = str "false" op = str "builtin">
                ),
                rule!(
                    Atom ::=
			BuiltinType@this <>
			!t LPAR Expression@this !t RPAR <>
                ),
                rule!(
                    Expression ::=
			Expression@left !t PLUS Expression@right <op = str "add">
			Expression@left !t ASTERISK Expression@right <op = str "mul">
			Atom@this <>
                ),
                rule!(
                    Statement ::=
			Assignment@this !t SEMICOLON <s = str "assign">
			IfStatement@this <s = str "if">
			WhileStatement@this <s = str "while">
                ),
                rule!(
                    StatementList ::=
			StatementList@left Statement@right <s = str "concatenate">
			Statement@this <>
                )
            ),
        );
    }

}

#[derive(Debug)]
pub struct EarleyGrammarBuilder {
    stream: Option<StringStream>,
    grammar: String
}

impl EarleyGrammarBuilder {
    pub fn new(grammar: String) -> Self {
	Self {
	    stream: None,
	    grammar
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
	Self::new(String::from("gmrs/parser.gmr"))
	    .with_file(String::from("gmrs/parser.lx"))
	    .unwrap()
    }
}

#[derive(Debug)]
pub struct EarleyItem {
    pub rule: usize,
    pub origin: usize,
    pub position: usize
}

#[derive(Serialize, Deserialize)]
pub struct EarleyGrammar {
    axioms: FixedBitSet,
    rules: Vec<Rule>,
    unreachables: FixedBitSet,
    nullables: FixedBitSet,
    name_map: HashMap<String, usize>
}

impl EarleyGrammar {
    fn is_rule_nullable(&self, id: usize) -> bool {
	self.nullables.contains(id)
	    || self
	    .rules[id]
	    .elements
	    .iter()
	    .all(
		|element|
		match element.element_type {
		    ElementType::NonTerminal(Some(id)) => self.nullables.contains(id),
		    _ => true,
		}
	    )
    }
    fn compute_nullables(&mut self) -> WResult<()> {
        let mut warnings = WarningSet::empty();
	let mut is_in = vec![Vec::new(); self.rules.len()];
	let mut stack = VecDeque::with_capacity(is_in.len());
	// let mut result = Vec::new();
	for (i, rule) in self.rules.iter().enumerate() {
	    if rule.elements.is_empty() {
		self.nullables.insert(i);
		stack.push_front(i);
	    }
	    for element in rule.elements.iter() {
		if let ElementType::NonTerminal(Some(id)) =  element.element_type {
		    is_in[id].push(i);
		}
	    }
	}
	while let Some(current) = stack.pop_back() {
	    // result.push(current);
	    for &id in &is_in[current] {
		if !self.nullables.contains(id) && self.is_rule_nullable(id) {
		    self.nullables.insert(id);
		    stack.push_front(id);
		}
	    }
	}
        WOk((), warnings)
    }

    fn compute_unreachables(&mut self) -> WResult<Vec<usize>> {
	let mut warnings = WarningSet::empty();
	let mut result = Vec::new();
	WOk(result, warnings)
    }
}

impl Grammar<'_> for EarleyGrammar {
    fn new(rules: Vec<Rule>, axioms: FixedBitSet, name_map: HashMap<String, usize>) -> WResult<Self> {
        let mut warnings = WarningSet::empty();
        let unreachables = FixedBitSet::with_capacity(axioms.len());
        let nullables = FixedBitSet::with_capacity(axioms.len());
        let mut grammar = Self {
            unreachables,
            nullables,
            rules,
            axioms,
	    name_map
        };
        ctry!(grammar.compute_nullables(), warnings);
        WOk(grammar, warnings)
    }
}

pub struct EarleyParser {
    
}

impl parser::Parser<'_> for EarleyParser {
    type GrammarBuilder = EarleyGrammarBuilder;
    type Grammar = EarleyGrammar;
}
