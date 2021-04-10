use super::grammarparser::{ElementType, Grammar, GrammarBuilder, Rule, RuleElement};
use super::parser::{ParseResult, Parser};
use crate::error::{
    WResult::{self, WOk},
    WarningSet,
};
use crate::lexer::LexedStream;
use crate::stream::StringStream;
use crate::{ctry, retrieve};
use fixedbitset::FixedBitSet;
use hashbrown::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::mem;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules;

    use crate::lexer::LexerBuilder;
    use crate::parser::grammarparser::{
        Attribute, ElementType, Key, Proxy, Rule, RuleElement, Value,
    };

    struct TestEarleyItem {
        name: &'static str,
        left_elements: Vec<&'static str>,
        right_elements: Vec<&'static str>,
        origin: usize,
    }

    impl TestEarleyItem {
        fn matches(
            &self,
            other: &EarleyItem,
            parser: &EarleyParser,
            set_id: usize,
            item_id: usize,
        ) {
            let error_message = format!("Set #{}, item #{}: no match: ", set_id, item_id);
            let item = &parser.grammar().rules[other.rule];
            assert_eq!(self.name, &item.name, "{} name.", error_message);
            assert_eq!(
                self.left_elements.len() + self.right_elements.len(),
                item.elements.len(),
                "{} origin set.",
                error_message
            );
            assert_eq!(
                self.left_elements.len(),
                other.position,
                "{} {}.",
                error_message,
                "fat dot position"
            );
            assert_eq!(self.origin, other.origin, "{} origin set.", error_message);
            for i in 0..self.left_elements.len() {
                assert_eq!(
                    self.left_elements[i], &item.elements[i].name,
                    "{} element #{}.",
                    error_message, i
                );
            }
            for i in 0..self.right_elements.len() {
                assert_eq!(
                    self.right_elements[i],
                    &item.elements[i + other.position].name,
                    "{} elements #{}.",
                    error_message,
                    i + other.position
                );
            }
        }
    }

    macro_rules! sets {
	(
	    $(
		== $(
		    $name: ident -> $($left_element: ident)* . $($right_element: ident)* ($origin: literal)
		)*
	    )*
	) => {
	    {
		#[allow(unused_mut)]
		let mut sets = Vec::new();
		$(
		    #[allow(unused_mut)]
		    let mut set = Vec::new();
		    $(
			set.push(earley_item!($name -> $($left_element)* . $($right_element)* ($origin)));
		)*
			sets.push(set);
		)*
		    sets
	    }
	};
    }

    macro_rules! earley_item {
	($name: ident -> $($left_element: ident)* . $($right_element: ident)* ($origin: literal)) => {
	    {
		#[allow(unused_mut)]
		let mut left_elements = Vec::new();
		#[allow(unused_mut)]
		let mut right_elements = Vec::new();
		$(
		    left_elements.push(stringify!($left_element));
		)*
		    $(
			right_elements.push(stringify!($right_element));
		    )*
		    TestEarleyItem {
			name: stringify!($name),
			left_elements,
			right_elements,
			origin: $origin
		    }
	    }
	};
    }

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

    #[allow(unused)]
    fn print_sets(sets: &[StateSet], parser: &EarleyParser) {
        for (i, set) in sets.iter().enumerate() {
            println!("=== {} ===", i);
            for item in set.slice() {
                let mut line = String::new();
                let rule = &parser.grammar().rules[item.rule];
                line.push_str(&rule.name);
                line.push_str(" -> ");
                for i in 0..item.position {
                    line.push_str(&rule.elements[i].name);
                    line.push(' ');
                }
                line.push_str("• ");
                for i in item.position..rule.elements.len() {
                    line.push_str(&rule.elements[i].name);
                    line.push(' ');
                }
                line.extend(format!("({})", item.origin).chars());
                println!("{}", line);
            }
            println!("");
        }
    }

    #[test]
    fn recognise_handle_empty_rules() {
        let lexer_input = r#""#;
        let grammar_input = r#"
@A ::= <>
 B <>;
B ::= A <>;"#;
        let input = r#""#;
        let lexer = LexerBuilder::new()
            .with_stream(StringStream::new(
                String::from("<lexer input>"),
                lexer_input.to_string(),
            ))
            .unwrap()
            .build()
            .unwrap();
        let grammar = <EarleyParser as Parser>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                String::from("<grammar input>"),
                grammar_input.to_string(),
            ))
            .build(&lexer)
            .unwrap();
        let parser = EarleyParser::new(grammar);
        let sets = sets!(
            ==
            A -> . (0)
            A -> . B (0)
            B -> . A (0)
            B -> A . (0)
            A -> B . (0)
        );
        let recognised = parser
            .recognise(&mut lexer.lex(&mut StringStream::new(
                String::from("<input>"),
                input.to_string(),
            )))
            .unwrap();
        verify_sets(sets, recognised, &parser);
    }

    #[test]
    fn recogniser() {
        let lexer_input = r#"
NUMBER ::= [0-9]
PM ::= [-+]
TD ::= [*/]
LPAR ::= \(
RPAR ::= \)
"#;
        let grammar_input = r#"
@Sum ::= Sum PM Product <>
 Product <>;

Product ::= Product TD Factor <>
 Factor <>;

Factor ::= LPAR Sum RPAR <>
 NUMBER <>;"#;
        let input = r#"1+(2*3-4)"#;

        let lexer = LexerBuilder::new()
            .with_stream(StringStream::new(
                String::from("<lexer input>"),
                lexer_input.to_string(),
            ))
            .unwrap()
            .build()
            .unwrap();
        let grammar = <EarleyParser as Parser>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                String::from("<grammar input>"),
                grammar_input.to_string(),
            ))
            .build(&lexer)
            .unwrap();
        let parser = EarleyParser::new(grammar);
        let sets = sets!(
            ==
            Sum -> . Sum PM Product (0)
            Sum -> . Product (0)
            Product -> . Product TD Factor (0)
            Product -> . Factor (0)
            Factor -> . LPAR Sum RPAR (0)
            Factor -> . NUMBER (0)

            ==
            Factor -> NUMBER . (0)
            Product -> Factor . (0)
            Sum -> Product . (0)
            Product -> Product . TD Factor (0)
            Sum -> Sum . PM Product (0)

            ==
            Sum -> Sum PM . Product (0)
            Product -> . Product TD Factor (2)
            Product -> . Factor (2)
            Factor -> . LPAR Sum RPAR (2)
            Factor -> . NUMBER (2)

            ==
            Factor -> LPAR . Sum RPAR (2)
            Sum -> . Sum PM Product (3)
            Sum -> . Product (3)
            Product -> . Product TD Factor (3)
            Product -> . Factor (3)
            Factor -> . LPAR Sum RPAR (3)
            Factor -> . NUMBER (3)

            ==
            Factor -> NUMBER . (3)
            Product -> Factor . (3)
            Sum -> Product . (3)
            Product -> Product . TD Factor (3)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Product -> Product TD . Factor (3)
            Factor -> . LPAR Sum RPAR (5)
            Factor -> . NUMBER (5)

            ==
            Factor -> NUMBER . (5)
            Product -> Product TD Factor . (3)
            Sum -> Product . (3)
            Product -> Product . TD Factor (3)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Sum -> Sum PM . Product (3)
            Product -> . Product TD Factor (7)
            Product -> . Factor (7)
            Factor -> . LPAR Sum RPAR (7)
            Factor -> . NUMBER (7)

            ==
            Factor -> NUMBER . (7)
            Product -> Factor . (7)
            Sum -> Sum PM Product . (3)
            Product -> Product . TD Factor (7)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Factor -> LPAR Sum RPAR . (2)
            Product -> Factor . (2)
            Sum -> Sum PM Product . (0)
            Product -> Product . TD Factor (2)
            Sum -> Sum . PM Product (0)
        );
        let recognised = parser
            .recognise(&mut lexer.lex(&mut StringStream::new(
                String::from("<input>"),
                input.to_string(),
            )))
            .unwrap();
        verify_sets(sets, recognised, &parser);
    }

    fn verify_sets(
        sets: Vec<Vec<TestEarleyItem>>,
        recognised: Vec<StateSet>,
        parser: &EarleyParser,
    ) {
        assert_eq!(recognised.len(), sets.len());
        for (set, (expected, recognised)) in sets.iter().zip(recognised.iter()).enumerate() {
            assert_eq!(
                expected.len(),
                recognised.set.len(),
                "Set #{} length does not match.",
                set
            );
            for (item_nb, (test_item, item)) in
                expected.iter().zip(recognised.set.iter()).enumerate()
            {
                test_item.matches(item, parser, set, item_nb);
            }
        }
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
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
    rules_map: Vec<Vec<usize>>,
}

impl EarleyGrammar {
    fn has_rules(&self, id: usize) -> &[usize] {
        &self.rules_map[id]
    }
}

impl Grammar<'_> for EarleyGrammar {
    fn new(
        rules: Vec<Rule>,
        axioms: FixedBitSet,
        name_map: HashMap<String, usize>,
    ) -> WResult<Self> {
        let warnings = WarningSet::empty();
        let mut nullables = FixedBitSet::with_capacity(axioms.len());
        let mut rules_map = Vec::new();

        let mut is_in = vec![Vec::new(); rules.len()];
        let mut stack = VecDeque::with_capacity(is_in.len());
        for (i, rule) in rules.iter().enumerate() {
            while rule.id >= rules_map.len() {
                rules_map.push(Vec::new());
            }
            rules_map[rule.id].push(i);
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
                rules_map,
            },
            warnings,
        )
    }
}

struct StateSet {
    cache: HashSet<EarleyItem>,
    set: Vec<EarleyItem>,
    position: usize,
}

impl StateSet {
    fn new() -> Self {
        Self {
            cache: HashSet::new(),
            set: Vec::new(),
            position: 0,
        }
    }
    fn add(&mut self, item: EarleyItem) {
        if !self.cache.contains(&item) {
            self.cache.insert(item);
            self.set.push(item);
        }
    }
    fn next(&mut self) -> Option<&EarleyItem> {
        if let Some(e) = self.set.get(self.position) {
            self.position += 1;
            Some(e)
        } else {
            None
        }
    }
    fn is_empty(&self) -> bool {
        self.set.is_empty()
    }
    fn slice(&self) -> &[EarleyItem] {
        &self.set
    }
}

pub struct EarleyParser {
    grammar: EarleyGrammar,
}

impl EarleyParser {
    fn recognise(&self, input: &mut LexedStream) -> WResult<Vec<StateSet>> {
        let mut warnings = WarningSet::empty();
        let mut sets = Vec::new();
        let mut first_state = StateSet::new();
        (0..self.grammar().rules.len())
            .filter(|id| self.grammar().axioms.contains(*id))
            .for_each(|id| {
                first_state.add(EarleyItem {
                    rule: id,
                    origin: 0,
                    position: 0,
                })
            });
        sets.push(first_state);
        let mut pos = 0;
        'outer: loop {
            let mut next_state = StateSet::new();
            let mut scans = HashMap::new();
            while let Some(&item) = sets.last_mut().unwrap().next() {
                let mut to_be_added = Vec::new();
                match self.grammar().rules[item.rule].elements.get(item.position) {
                    Some(element) => match element.element_type {
                        // Prediction
                        ElementType::NonTerminal(Some(id)) => {
                            for &rule in self.grammar().has_rules(id) {
                                to_be_added.push(EarleyItem {
                                    rule,
                                    origin: pos,
                                    position: 0,
                                });
                            }
                            if self.grammar().nullables.contains(id) {
                                to_be_added.push(EarleyItem {
                                    rule: item.rule,
                                    origin: item.origin,
                                    position: item.position + 1,
                                });
                            }
                        }
                        // Scan
                        ElementType::Terminal(ref id) => {
                            scans.entry(id).or_insert(Vec::new()).push(EarleyItem {
                                rule: item.rule,
                                origin: item.origin,
                                position: item.position + 1,
                            })
                        }
                        ElementType::NonTerminal(None) => {}
                    },
                    // Completion
                    None => {
                        for &parent in sets[item.origin].slice() {
                            if let Some(RuleElement {
                                element_type: ElementType::NonTerminal(Some(nonterminal)),
                                ..
                            }) = self.grammar().rules[parent.rule]
                                .elements
                                .get(parent.position)
                            {
                                if *nonterminal == self.grammar().rules[item.rule].id {
                                    to_be_added.push(EarleyItem {
                                        rule: parent.rule,
                                        origin: parent.origin,
                                        position: parent.position + 1,
                                    })
                                }
                            }
                        }
                    }
                }
                for item in to_be_added {
                    sets.last_mut().unwrap().add(item);
                }
            }
            if let Some(token) = ctry!(input.next(), warnings) {
                for item in scans.entry(token.id()).or_default() {
                    next_state.add(*item);
                }
            }
            if next_state.is_empty() {
                break 'outer WOk(sets, warnings);
            }
            sets.push(next_state);
            pos += 1;
        }
    }
}

impl Parser<'_> for EarleyParser {
    type GrammarBuilder = EarleyGrammarBuilder;
    type Grammar = EarleyGrammar;
    fn new(grammar: Self::Grammar) -> Self {
        Self { grammar }
    }
    fn grammar(&self) -> &Self::Grammar {
        &self.grammar
    }
    fn parse(&self, input: &mut LexedStream) -> WResult<ParseResult> {
        let mut warnings = WarningSet::empty();
        ctry!(self.recognise(input), warnings);
        WOk((), warnings)
    }
}
