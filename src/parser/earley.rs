use super::grammarparser::{
    Axioms, ElementType, Grammar, GrammarBuilder, Rule, RuleElement, Value,
};
use super::list::List;
use super::parser::{NonTerminalId, ParseResult, Parser, RuleId};
use crate::error::Result;
use crate::error::{Error, WarningSet};
use crate::lexer::LexedStream;
use crate::lexer::TerminalId;
use crate::lexer::Token;
use crate::parser::grammarparser::Attribute;
use crate::parser::parser::AST;
use crate::regex::Allowed;
use crate::retrieve;
use crate::stream::StringStream;
use fragile::Fragile;
use itertools::Itertools;
use newty::{newty, nvec};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::Path;
use std::rc::Rc;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules;

    use crate::{
        lexer::LexerBuilder,
        parser::grammarparser::{
            Attribute, ElementType, Key, Proxy, Rule, RuleElement, Value,
        },
    };

    const GRAMMAR_NUMBERS_LEXER: &str = r#"
NUMBER ::= ([0-9])
PM ::= [-+]
TD ::= [*/]
LPAR ::= \(
RPAR ::= \)
"#;

    const GRAMMAR_NUMBERS: &str = r#"
@Sum ::= Sum@left PM Product@right <left: left right: right>
 Product@self <self: self>;

Product ::= Product@left TD Factor@right <left: left right: right>
 Factor.self@self <self: self>;

Factor ::= LPAR Sum@self RPAR <self: self>
 NUMBER.0@self <self: self>;"#;

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
            let error_message =
                format!("Set #{}, item #{}: no match:", set_id, item_id);
            let item = &parser.grammar().rules[other.rule];
            assert_eq!(self.name, &*item.name, "{} name.", error_message);
            assert_eq!(
                self.left_elements.len() + self.right_elements.len(),
                item.elements.len(),
                "{} origin set.",
                error_message
            );
            assert_eq!(
                self.left_elements.len(),
                other.position,
                "{} fat dot position.",
                error_message,
            );
            assert_eq!(
                self.origin, other.origin,
                "{} origin set.",
                error_message
            );
            for i in 0..self.left_elements.len() {
                assert_eq!(
                    self.left_elements[i], &*item.elements[i].name,
                    "{} element #{}.",
                    error_message, i
                );
            }
            for i in 0..self.right_elements.len() {
                assert_eq!(
                    self.right_elements[i],
                    &*item.elements[i + other.position].name,
                    "{} elements #{}.",
                    error_message,
                    i + other.position
                );
            }
        }
    }

    /// `sets!` eases the creation of mock Earley Sets.
    /// Useful for tests.
    ///
    /// The syntax is aimed to be simple and intuitive, matching the one
    /// usually used in the literature.
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

    /// `earley_item` creates mock Earley Items.
    /// Useful for tests.
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

    macro_rules! final_sets {
	(
	    ($grammar: expr)
	    $(
		== $(
		    $name: ident -> $($element: ident)* ($end: literal)
		)*
	    )*
	) => {{
	    #[allow(unused_mut)]
	    let mut sets = Vec::new();
	    fn find_item(grammar: &EarleyGrammar, name: &str, elements: &[&str], end: usize) -> FinalItem {
		for &rule_identifier in grammar
		    .name_map
		    .get(&Rc::from(name))
		    .map(|&identifier| &grammar.rules_of[identifier])
		    .expect(format!("The non-terminal {} does not exist.", name).as_str())
		    .iter()
		{
		    if elements.len() == grammar
			.rules[rule_identifier]
			.elements.len()
			&& elements
			.iter()
			.zip(grammar.rules[rule_identifier].elements.iter())
			.all(|(&left, right)| left == &*right.name)
		    {
			return FinalItem {
			    rule: rule_identifier,
			    end
			};
		    }
		}
		panic!("The rule {} -> {} is not in the grammar.", name, elements.join(" "));
	    }
	    $(
		#[allow(unused_mut)]
		let mut set = FinalSet::default();
		set.position = sets.len();
		$(
		    set.add(find_item($grammar, stringify!($name), &[$(stringify!($element)),*], $end), $grammar);
		)*
		sets.push(set);
	    )*
	    sets
	}};
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
            self.name == *other.name
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
            self.name == *other.name
                && self.proxy == other.proxy
                && self.elements == other.elements
        }
    }

    #[derive(Debug)]
    struct TestToken {
        name: &'static str,
        attributes: Vec<(usize, &'static str)>,
    }

    impl PartialEq<Token> for TestToken {
        fn eq(&self, other: &Token) -> bool {
            other.name() == self.name
                && other
                    .attributes()
                    .iter()
                    .map(|(&key, value)| (key, value.as_str()))
                    .collect::<Vec<_>>()
                    == self.attributes
        }
    }

    impl TestToken {
        #![allow(unused)]
        fn new(name: &'static str, attributes: Vec<&'static str>) -> Self {
            Self {
                name,
                attributes: attributes.into_iter().enumerate().collect(),
            }
        }
    }

    struct MapVec(Vec<(&'static str, TestAST)>);

    impl std::fmt::Debug for MapVec {
        fn fmt(
            &self,
            formatter: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            formatter
                .debug_map()
                .entries(self.0.iter().map(|&(ref k, ref v)| (k, v)))
                .finish()
        }
    }

    impl From<Vec<(&'static str, TestAST)>> for MapVec {
        fn from(o: Vec<(&'static str, TestAST)>) -> Self {
            Self(o)
        }
    }

    #[derive(Debug)]
    enum TestAST {
        Node {
            id: usize,
            attributes: MapVec,
        },
        #[allow(unused)]
        Terminal(TestToken),
        Literal(crate::parser::parser::Value),
    }

    impl PartialEq<TestAST> for AST {
        fn eq(&self, other: &TestAST) -> bool {
            other == self
        }
    }

    impl PartialEq<AST> for TestAST {
        fn eq(&self, other: &AST) -> bool {
            match (self, other) {
                (
                    TestAST::Node {
                        id: tid,
                        attributes: tattributes,
                    },
                    AST::Node {
                        nonterminal: id,
                        attributes,
                        ..
                    },
                ) => {
                    NonTerminalId::from(*tid) == *id && {
                        let tattributes = tattributes
                            .0
                            .iter()
                            .map(|(key, value)| (Rc::<str>::from(*key), value))
                            .collect::<HashMap<_, _>>();
                        tattributes.len() == attributes.len()
                            && tattributes.iter().all(|(key, value)| {
                                attributes
                                    .get(key)
                                    .map_or(false, |v| *value == v)
                            })
                    }
                }
                (TestAST::Terminal(ttoken), AST::Terminal(token)) => {
                    ttoken == token
                }
                (TestAST::Literal(tvalue), AST::Literal(value)) => {
                    tvalue == value
                }
                _ => false,
            }
        }
    }

    #[inline]
    fn verify(rules1: GrammarRules, rules2: Vec<TestRule>) {
        let length1 = rules1.0.len();
        let length2 = rules2.len();
        if length1 > length2 {
            panic!("Grammar 1 is longer");
        } else if length1 < length2 {
            panic!("Grammar 2 is longer");
        }
        for (i, (r1, r2)) in rules1.0.iter().zip(rules2.iter()).enumerate() {
            assert_eq!(r2, r1, "rules #{} differ", i);
        }
    }

    #[test]
    #[rustfmt::skip]
    fn earley_grammar_builder() {
	use crate::lexer::LexerBuilder;
        let lexer = LexerBuilder::default().build();
        let grammar = EarleyGrammarBuilder::default().build(&lexer).unwrap().unwrap();
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

    #[test]
    fn recognise_handle_empty_rules() {
        let lexer_input = r#""#;
        let grammar_input = r#"
@A ::= <>
 B <>;
B ::= A <>;"#;
        let input = r#""#;
        let lexer = LexerBuilder::from_stream(StringStream::new(
            Path::new("<lexer input>"),
            lexer_input,
        ))
        .unwrap()
        .unwrap()
        .build();
        let grammar = <EarleyParser as Parser<'_>>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                Path::new("<grammar input>"),
                grammar_input,
            ))
            .build(&lexer)
            .unwrap()
            .unwrap();
        let parser = EarleyParser::new(grammar);
        let sets = sets!(
            ==
            A -> . (0)
            A -> . B (0)
            B -> . A (0)
            A -> B . (0)
            B -> A . (0)
        );
        let (recognised, _) = parser
            .recognise(
                &mut lexer
                    .lex(&mut StringStream::new(Path::new("<input>"), input)),
            )
            .unwrap()
            .unwrap();
        verify_sets(sets, recognised, &parser);
    }

    #[test]
    fn ast_builder() {
        let input = r#"1+(2*3-4)"#;

        let lexer = LexerBuilder::from_stream(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap()
        .unwrap()
        .build();
        let grammar = <EarleyParser as Parser<'_>>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                Path::new("<grammar input>"),
                GRAMMAR_NUMBERS,
            ))
            .build(&lexer)
            .unwrap()
            .unwrap();
        let parser = EarleyParser::new(grammar);
        let (table, raw_input) = parser
            .recognise(
                &mut lexer
                    .lex(&mut StringStream::new(Path::new("<input>"), input)),
            )
            .unwrap()
            .unwrap();
        let forest = parser.to_forest(&table, &raw_input).unwrap().unwrap();
        let ast = parser.select_ast(&forest, &raw_input);

        let test_ast = {
            use crate::parser::parser::Value::*;
            use TestAST::*;
            Node {
                id: 0,
                attributes: vec![
                    (
                        "right",
                        Node {
                            id: 2,
                            attributes: vec![(
                                "self",
                                Node {
                                    id: 0,
                                    attributes: vec![
                                        (
                                            "right",
                                            Node {
                                                id: 2,
                                                attributes: vec![(
                                                    "self",
                                                    Literal(Str("4".to_string())),
                                                )]
                                                .into(),
                                            },
                                        ),
                                        (
                                            "left",
                                            Node {
                                                id: 0,
                                                attributes: vec![(
                                                    "self",
                                                    Node {
                                                        id: 2,
                                                        attributes: vec![
                                                            (
                                                                "right",
                                                                Node {
                                                                    id: 8,
                                                                    attributes: vec![(
                                                                        "self",
                                                                        Literal(Str(
                                                                            "3".to_string()
                                                                        )),
                                                                    )]
                                                                    .into(),
                                                                },
                                                            ),
                                                            (
                                                                "left",
                                                                Node {
                                                                    id: 2,
                                                                    attributes: vec![(
                                                                        "self",
                                                                        Literal(Str(
                                                                            "2".to_string()
                                                                        )),
                                                                    )]
                                                                    .into(),
                                                                },
                                                            ),
                                                        ]
                                                        .into(),
                                                    },
                                                )]
                                                .into(),
                                            },
                                        ),
                                    ]
                                    .into(),
                                },
                            )]
                            .into(),
                        },
                    ),
                    (
                        "left",
                        Node {
                            id: 0,
                            attributes: vec![(
                                "self",
                                Node {
                                    id: 2,
                                    attributes: vec![("self", Literal(Str("1".to_string())))]
                                        .into(),
                                },
                            )]
                            .into(),
                        },
                    ),
                ]
                .into(),
            }
        };

        assert_eq!(ast, test_ast);
    }

    #[test]
    fn forest_builder() {
        let input = r#"1+(2*3-4)"#;

        let lexer = LexerBuilder::from_stream(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap()
        .unwrap()
        .build();
        let grammar = <EarleyParser as Parser<'_>>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                Path::new("<grammar input>"),
                GRAMMAR_NUMBERS,
            ))
            .build(&lexer)
            .unwrap()
            .unwrap();

        let parser = EarleyParser::new(grammar);
        let sets = final_sets!(
                (parser.grammar())
            ==
            Factor -> NUMBER (1)
            Product -> Factor (1)
            Sum -> Product (1)
            Sum -> Sum PM Product (9)

            ==

            ==
            Factor -> LPAR Sum RPAR (9)
            Product -> Factor (9)

            ==
            Factor -> NUMBER (4)
            Product -> Factor (4)
            Sum -> Product (4)
            Product -> Product TD Factor (6)
            Sum -> Product (6)
            Sum -> Sum PM Product (8)

            ==

            ==
            Factor -> NUMBER (6)

            ==

        ==
        Factor -> NUMBER (8)
            Product -> Factor (8)

            ==

            ==
            );

        let (table, raw_input) = parser
            .recognise(
                &mut lexer
                    .lex(&mut StringStream::new(Path::new("<input>"), input)),
            )
            .unwrap()
            .unwrap();
        let forest = parser.to_forest(&table, &raw_input).unwrap().unwrap();
        assert_eq!(
            forest,
            sets,
            "Parsed forest:\n{}\nExpected forest:\n{}",
            forest
                .iter()
                .map(|set| format!("{}", set))
                .collect::<Vec<_>>()
                .join("\n"),
            sets.iter()
                .map(|set| format!("{}", set))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn recogniser() {
        let input = r#"1+(2*3-4)"#;

        let lexer = LexerBuilder::from_stream(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap()
        .unwrap()
        .build();
        let grammar = <EarleyParser as Parser<'_>>::GrammarBuilder::default()
            .with_stream(StringStream::new(
                Path::new("<grammar input>"),
                GRAMMAR_NUMBERS,
            ))
            .build(&lexer)
            .unwrap()
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
        let (recognised, _) = parser
            .recognise(
                &mut lexer
                    .lex(&mut StringStream::new(Path::new("<input>"), input)),
            )
            .unwrap()
            .unwrap();
        verify_sets(sets, recognised, &parser);
    }

    fn verify_sets(
        sets: Vec<Vec<TestEarleyItem>>,
        recognised: Vec<StateSet>,
        parser: &EarleyParser,
    ) {
        assert_eq!(recognised.len(), sets.len());
        for (set, (expected, recognised)) in
            sets.iter().zip(recognised.iter()).enumerate()
        {
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

#[allow(unused)]
pub fn print_sets(sets: &[StateSet], parser: &EarleyParser) {
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
        println!();
    }
}

#[allow(unused)]
fn print_final_sets(sets: &[FinalSet], parser: &EarleyParser) {
    for (i, set) in sets.iter().enumerate() {
        println!("=== {} ===", i);
        for item in &set.set.0 {
            let rule = &parser.grammar().rules[item.rule];
            print!("{} -> ", rule.name);
            for element in rule.elements.iter() {
                print!("{}", element.name);
                match &element.attribute {
                    Attribute::Indexed(i) => print!(".{}", i),
                    Attribute::Named(n) => print!(".{}", n),
                    Attribute::None => {}
                }
                if let Some(key) = &element.key {
                    print!("@{}", key);
                }
                print!(" ");
            }
            println!("({})", item.end);
        }
    }
}

type Table = Vec<StateSet>;
type Forest = Vec<FinalSet>;

newty! {
    #[derive(serde::Serialize, serde::Deserialize)]
    pub vec RulesMap(Vec<RuleId>)[NonTerminalId]
}

newty! {
    pub vec IsIn(Vec<RuleId>)[NonTerminalId]
}

/// A builder for the Earley grammar.
#[derive(Debug)]
pub struct EarleyGrammarBuilder {
    stream: Option<StringStream>,
    grammar_source_file: Rc<Path>,
}

impl EarleyGrammarBuilder {
    /// Create a new builder. It takes an Rc of a string refearing to the grammar.
    pub fn new(grammar: impl Into<Rc<Path>>) -> Self {
        let grammar = grammar.into();
        Self {
            stream: None,
            grammar_source_file: grammar,
        }
    }
}

impl GrammarBuilder<'_> for EarleyGrammarBuilder {
    type Grammar = EarleyGrammar;

    fn with_stream(mut self, stream: StringStream) -> Self {
        self.stream = Some(stream);
        self
    }

    fn with_grammar(mut self, grammar: impl Into<Rc<Path>>) -> Self {
        self.grammar_source_file = grammar.into();
        self
    }

    fn stream<'ws>(&mut self) -> Result<StringStream> {
        let stream = retrieve!(self.stream);
        Ok(WarningSet::empty_with(stream))
    }

    fn grammar(&self) -> Rc<Path> {
        self.grammar_source_file.clone()
    }
}

impl Default for EarleyGrammarBuilder {
    fn default() -> Self {
        Self::new(Path::new("gmrs/parser.lx"))
            .with_file(Path::new("gmrs/parser.gr"))
            .unwrap()
            .unwrap()
    }
}

/// # Summary
/// `EarleyItem` is partially recognized handle.
/// If `item.rule` refers to `β → α_1…α_n`, the item is `β → α_1…α_{i-1} · α_i…α_n (j)`
/// where `i=item.position` and `j=item.origin`.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct EarleyItem {
    /// `rule` is the identifier of the associated [`Rule`].
    rule: RuleId,
    /// `origin` is the identifier of the `EarleySet` this item was originated in.
    origin: usize,
    /// `position` is the advancement of the current item. It corresponds to the position of the fat dot.
    position: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FinalItem {
    /// `rule` is the identifier of the associated [`Rule`]
    rule: RuleId,
    end: usize,
}

impl fmt::Display for FinalItem {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> std::result::Result<(), fmt::Error> {
        write!(f, "#{}\t\t({})", self.rule, self.end)
    }
}

newty! {
    #[derive(serde::Serialize, serde::Deserialize)]
    pub vec GrammarRules(Rule)[RuleId]
}

newty! {
    pub set Nullables[NonTerminalId]
}

/// # Summary
/// `EarleyGrammar` is a grammar that uses the Earley algorithm.
/// The general worst-time complexity for a context-free grammar is `O(n³)`.
/// For an unambiguous grammar, the worst-time complexity is `O(n²)`.
/// For an `LR(k)` grammar, if the Johnson algorithm is applied (which is currently not), the complexity is `O(n)`.
/// If it is not applied, the complexity is `O(n)` unless there is right-recursion, in which case the complexity is `O(n²)`.
#[derive(Serialize, Deserialize, Debug)]
pub struct EarleyGrammar {
    /// The axioms, indexed by RuleId.
    axioms: Axioms,
    /// The rules. The rule index is its identifier.
    rules: GrammarRules,
    /// The nullables, indexed by NonTerminalId.
    nullables: Nullables,
    /// Maps the name of a non-terminal to its identifier.
    name_map: HashMap<Rc<str>, NonTerminalId>,
    /// Maps the identifier of a non-terminal to the identifiers of its rules.
    /// Its rules are the rules of which it is the LHS.
    rules_of: RulesMap,
}

impl EarleyGrammar {
    fn has_rules(&self, id: NonTerminalId) -> &[RuleId] {
        &self.rules_of[id]
    }
}

impl Grammar<'_> for EarleyGrammar {
    fn new(
        rules: GrammarRules,
        axioms: Axioms,
        name_map: HashMap<Rc<str>, NonTerminalId>,
    ) -> Result<Self> {
        let warnings = WarningSet::empty();
        let nb_non_terminals = axioms.len_as(); // Number of non terminals
                                                // nullables[non_term_id]: bool is whether non terminal with this id is nullable, meaning it can match ε (empty string).
        let mut nullables = Nullables::with_capacity(nb_non_terminals);
        // rules_of[non_term_id]: [rule_id] is a Vec containing all the rules whose LHS is the non terminal this id.
        let mut rules_of = nvec![RulesMap Vec::new(); nb_non_terminals];
        // is_in[non_term_id]: [rule_id] is a Vec containing all the rules whose RHS contains the non terminal with this id.
        let mut is_in = nvec![IsIn Vec::new(); nb_non_terminals];
        let mut stack = VecDeque::with_capacity(is_in.len());
        for (i, rule) in rules.iter().enumerate() {
            let rule_id = RuleId(i);
            let lhs_id = rule.id;
            rules_of[lhs_id].push(rule_id);
            if rule.elements.is_empty() {
                nullables.insert(lhs_id);
                stack.push_front(lhs_id);
            }
            for element in rule.elements.iter() {
                if let ElementType::NonTerminal(rhs_id) = element.element_type {
                    is_in[rhs_id].push(rule_id);
                }
            }
        }

        while let Some(current) = stack.pop_back() {
            for &rule_id in &is_in[current] {
                let lhs_id = rules[rule_id].id;
                if !nullables.contains(lhs_id)
                    && rules[rule_id].elements.iter().all(|element| {
                        match element.element_type {
                            ElementType::NonTerminal(id) => {
                                nullables.contains(id)
                            }
                            _ => false,
                        }
                    })
                {
                    nullables.insert(lhs_id);
                    stack.push_front(lhs_id);
                }
            }
        }

        warnings.with_ok(Self {
            axioms,
            rules,
            nullables,
            name_map,
            rules_of,
        })
    }
}

newty! {
    id FinalItemId
}
newty! {
    #[derive(PartialEq, Eq, Clone)]
    vec FinalSetVec(FinalItem)[FinalItemId]
}
newty! {
    #[derive(Clone)]
    map FinalSetIndex(Vec<FinalItemId>)[NonTerminalId]
}

#[derive(Default, Debug, Clone, Eq)]
struct FinalSet {
    /// An index mapping a nonterminal to every item in the set derived from that nonterminal.
    index: FinalSetIndex,
    /// The set of items.
    set: FinalSetVec,
    /// The starting position of every item in this set, in the raw input.
    position: usize,
}

impl PartialEq for FinalSet {
    fn eq(&self, rhs: &FinalSet) -> bool {
        self.set == rhs.set && self.position == rhs.position
    }
}

impl FinalSet {
    fn add(&mut self, item: FinalItem, grammar: &EarleyGrammar) {
        self.index
            .0
            .entry(grammar.rules[item.rule].id)
            .or_default()
            .push(self.set.len_as());
        self.set.push(item);
    }

    fn iter(&self) -> impl Iterator<Item = &FinalItem> + '_ {
        self.set.iter()
    }
}

impl std::fmt::Display for FinalSet {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            r"== ({}) ==
{}",
            self.position,
            self.set
                .iter()
                .map(|item| format!("{}", item))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Default, Debug)]
pub struct StateSet {
    cache: HashSet<EarleyItem>,
    set: Vec<EarleyItem>,
    position: usize,
}

impl StateSet {
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

    fn iter(&self) -> impl Iterator<Item = &EarleyItem> + '_ {
        self.set.iter()
    }
}

#[derive(Clone, Debug)]
struct SyntaxicItem {
    kind: SyntaxicItemKind,
    start: usize,
    end: usize,
}

#[derive(Clone, Debug)]
enum SyntaxicItemKind {
    Rule(RuleId),
    Token(Token),
}

// #[derive(Debug, Clone)]
// struct ScanItem<'a> {
//     item: &'a FinalItem,
//     depth: usize,
//     nodes_so_far: Vec<AST>,
// }

// #[derive(Debug)]
// struct SearchItem<'a> {
//     /// Index of the next scan in the raw input.
//     position: usize,
//     /// Current item to be searched for.
//     current: ScanItem<'a>,
//     /// Items scanned so far.
//     stack: Vec<ScanItem<'a>>,
// }

// #[derive(Debug)]
// struct ScItem {
//     rule: usize,
//     start: usize,
//     end: usize,
//     depth: usize,
// }

// #[derive(Debug)]
// struct SItem {
//     current: ScItem,
//     items: Vec<ScItem>,
// }

/// # Summary
/// [`EarleyParser`] is the parser related to the [`EarleyGrammar`](EarleyGrammar).
#[derive(Debug)]
pub struct EarleyParser {
    grammar: EarleyGrammar,
}

impl EarleyParser {
    fn find_children(
        &self,
        element: SyntaxicItem,
        forest: &[FinalSet],
        raw_input: &[Token],
    ) -> Vec<SyntaxicItem> {
        match element.kind {
            SyntaxicItemKind::Rule(rule) => {
                let mut boundary = vec![(List::default(), element.start)];
                for elem in self.grammar.rules[rule].elements.iter() {
                    let mut next_boundary = Vec::new();
                    for (children, curpos) in boundary {
                        match elem.element_type {
                            ElementType::NonTerminal(id) => {
                                if let Some(rules) =
                                    forest[curpos].index.get(&id)
                                {
                                    for final_item in rules
                                        .iter()
                                        .map(|&rule| &forest[curpos].set[rule])
                                        .filter(|final_item| {
                                            final_item.end <= element.end
                                        })
                                    {
                                        next_boundary.push((
                                            children.cons(SyntaxicItem {
                                                kind: SyntaxicItemKind::Rule(
                                                    final_item.rule,
                                                ),
                                                start: curpos,
                                                end: final_item.end,
                                            }),
                                            final_item.end,
                                        ))
                                    }
                                }
                            }
                            ElementType::Terminal(id)
                                if curpos < element.end
                                    && raw_input[curpos].id() == id =>
                            {
                                next_boundary.push((
                                    children.cons(SyntaxicItem {
                                        kind: SyntaxicItemKind::Token(
                                            raw_input[curpos].clone(),
                                        ),
                                        start: curpos,
                                        end: curpos + 1,
                                    }),
                                    curpos + 1,
                                ))
                            }
                            _ => {}
                        }
                    }
                    boundary = next_boundary;
                }
                boundary
                    .into_iter()
                    .find_map(|(children, pos)| {
                        if pos == element.end {
                            Some(
                                children
                                    .iter()
                                    .cloned()
                                    .collect::<Vec<_>>()
                                    .into_iter()
                                    .rev()
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    })
                    .unwrap()
            }
            SyntaxicItemKind::Token(_) => Vec::new(),
        }
    }
    fn build_ast(
        &self,
        item: SyntaxicItem,
        forest: &[FinalSet],
        raw_input: &[Token],
    ) -> AST {
        match item.kind {
            SyntaxicItemKind::Rule(rule) => {
                let mut all_attributes = self
                    .find_children(item, forest, raw_input)
                    .into_iter()
                    .map(|item| self.build_ast(item, forest, raw_input))
                    .zip(self.grammar.rules[rule].elements.iter())
                    .filter_map(|(item, element)| {
                        element.key.as_ref().map(|key| match &element.attribute {
                            Attribute::Named(attr) => {
                                if let AST::Node { attributes, .. } = item {
                                    (key.as_str().into(), attributes[attr.as_str()].clone())
                                } else {
                                    unreachable!() // TODO: remove this possibility
                                }
                            }
                            Attribute::Indexed(idx) => {
                                if let AST::Terminal(token) = item {
                                    (
                                        key.as_str().into(),
                                        AST::Literal(crate::parser::parser::Value::Str(
                                            token.attributes()[idx].clone(),
                                        )),
                                    )
                                } else {
                                    unreachable!()
                                }
                            }
                            Attribute::None => (key.as_str().into(), item),
                        })
                    })
                    .collect::<HashMap<Rc<str>, _>>();
                let mut attributes: HashMap<_, _> = self.grammar.rules[rule]
                    .proxy
                    .iter()
                    .map(|(key, wanted)| {
                        (
                            key.as_str().into(),
                            match wanted {
                                Value::Id(w) => {
                                    all_attributes.remove(w.as_str()).unwrap()
                                }
                                Value::Bool(v) => AST::Literal(
                                    crate::parser::parser::Value::Bool(*v),
                                ),
                                Value::Int(v) => AST::Literal(
                                    crate::parser::parser::Value::Int(*v),
                                ),
                                Value::Float(v) => AST::Literal(
                                    crate::parser::parser::Value::Float(*v),
                                ),
                                Value::Str(v) => AST::Literal(
                                    crate::parser::parser::Value::Str(
                                        v.to_string(),
                                    ),
                                ),
                            },
                        )
                    })
                    .collect();
		attributes.extend(all_attributes.into_iter());
                let nonterminal = self.grammar.rules[rule].id;
                AST::Node {
                    nonterminal,
                    attributes,
                }
            }
            SyntaxicItemKind::Token(token) => AST::Terminal(token),
        }
    }

    /// Select one AST, assuming there is one.
    fn select_ast(&self, forest: &[FinalSet], raw_input: &[Token]) -> AST {
        forest[0]
            .iter()
            .filter(|item| {
                item.end == raw_input.len()
                    && self
                        .grammar
                        .axioms
                        .contains(self.grammar.rules[item.rule].id)
            })
            .map(|item| SyntaxicItem {
                start: 0,
                end: raw_input.len(),
                kind: SyntaxicItemKind::Rule(item.rule),
            })
            .map(|item| self.build_ast(item, forest, raw_input))
            .next()
            .unwrap()
    }

    fn to_forest(
        &self,
        table: &[StateSet],
        raw_input: &[Token],
    ) -> Result<Forest> {
        let warnings = WarningSet::default();
        let mut forest = vec![FinalSet::default(); table.len()];
        for (i, set) in table.iter().enumerate() {
            forest[i].position = i;
            if set.is_empty() {
                return Err(Error::SyntaxError {
                    location: Fragile::new(raw_input[i].location().clone()),
                    message: format!("Syntax error at token {}", i),
                });
            }
            set.iter()
                .filter(|item| {
                    item.position
                        == self.grammar.rules[item.rule].elements.len()
                })
                .for_each(|item| {
                    forest[item.origin].add(
                        FinalItem {
                            end: i,
                            rule: item.rule,
                        },
                        &self.grammar,
                    )
                });
        }
        warnings.with_ok(forest)
    }

    pub fn recognise<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> Result<(Table, Vec<Token>)> {
        let mut warnings = WarningSet::empty();
        let mut sets = Vec::new();
        let mut first_state = StateSet::default();
        (0..self.grammar().rules.len())
            .map(RuleId)
            .filter(|id| {
                self.grammar.axioms.contains(self.grammar.rules[*id].id)
            })
            .for_each(|id| {
                first_state.add(EarleyItem {
                    rule: id,
                    origin: 0,
                    position: 0,
                })
            });
        let mut raw_input = Vec::new();
        sets.push(first_state);
        let mut pos = 0;
        'outer: loop {
            let mut next_state = StateSet::default();
            let mut scans: HashMap<TerminalId, _> = HashMap::new();
            '_inner: while let Some(&item) = sets.last_mut().unwrap().next() {
                let mut to_be_added = Vec::new();
                match self.grammar().rules[item.rule]
                    .elements
                    .get(item.position)
                {
                    Some(element) => match element.element_type {
                        // Prediction
                        ElementType::NonTerminal(id) => {
                            for &rule in self.grammar().has_rules(id) {
                                to_be_added.push(EarleyItem {
                                    rule,
                                    origin: pos,
                                    position: 0,
                                });
                            }
                            if self.grammar.nullables.contains(id) {
                                to_be_added.push(EarleyItem {
                                    rule: item.rule,
                                    origin: item.origin,
                                    position: item.position + 1,
                                });
                            }
                        }
                        // Scan
                        ElementType::Terminal(id) => scans
                            .entry(id)
                            .or_insert(Vec::new())
                            .push(EarleyItem {
                                rule: item.rule,
                                origin: item.origin,
                                position: item.position + 1,
                            }),
                    },
                    // Completion
                    None => {
                        for &parent in sets[item.origin].slice() {
                            if let Some(RuleElement {
                                element_type:
                                    ElementType::NonTerminal(nonterminal),
                                ..
                            }) = self.grammar().rules[parent.rule]
                                .elements
                                .get(parent.position)
                            {
                                if *nonterminal
                                    == self.grammar().rules[item.rule].id
                                {
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

            let possible_scans = input
                .lexer()
                .grammar()
                .default_allowed()
                .chain(scans.keys().cloned())
                .collect::<Vec<_>>();
            let allowed = Allowed::Some(possible_scans.clone());
            if let Some(token) = match input.next(allowed) {
                Ok(token) => token.unpack_into(&mut warnings),
                Err(_) => {
                    let error = if let Some(token) =
                        input.next(Allowed::All)?.unpack_into(&mut warnings)
                    {
                        let name = token.name().to_string();
                        let location = token.location().clone();
                        // `intersperse` may be added to the standard
                        // library someday. Let's hope sooner than later.
                        #[allow(unstable_name_collisions)]
                        let alternatives = format!(
                            "You could try {} instead",
                            scans
                                .keys()
                                .map(|tok| input.lexer().grammar().name(*tok))
                                .intersperse(", ")
                                .collect::<String>()
                        );
                        Error::SyntaxError {
                            message: format!(
                                "The token {} doesn't make sense here.\n{}",
                                name, alternatives,
                            ),
                            location: Fragile::new(location),
                        }
                    } else {
                        Error::SyntaxError {
                            message: String::from(
                                "Reached EOF but parsing isn't done.",
                            ),
                            location: Fragile::new(
                                input.last_location().clone(),
                            ),
                        }
                    };
                    return Err(error);
                }
            } {
                for item in scans.entry(token.id()).or_default() {
                    next_state.add(*item);
                }
                raw_input.push(token.clone());
            }
            if next_state.is_empty() {
                break 'outer warnings.with_ok((sets, raw_input));
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

    fn is_valid<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> bool {
        self.recognise(input).is_ok()
    }

    fn parse<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> Result<ParseResult> {
        let mut warnings = WarningSet::default();
        let (table, raw_input) =
            self.recognise(input)?.unpack_into(&mut warnings);
        let forest = self
            .to_forest(&table, &raw_input)?
            .unpack_into(&mut warnings);
        // print_final_sets(&forest, self);
        let tree = self.select_ast(&forest, &raw_input);
        warnings.with_ok(ParseResult { tree })
    }
}
