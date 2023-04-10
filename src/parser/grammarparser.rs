use super::AST;
use super::Value;
use super::parser::NonTerminalId;
use crate::error::Result;
use crate::error::WarningSet;
use crate::lexer::{LexerGrammar, TerminalId};
use crate::parser::earley::GrammarRules;
use crate::span::Span;
use itertools::Itertools;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

pub type Key = Rc<str>;

newty! {
    #[derive(Serialize, Deserialize)]
    pub vec NonTerminalName(FullName)[NonTerminalId]
}

newty! {
    #[derive(Serialize, Deserialize)]
    pub vec NonTerminalDescription(Option<FullName>)[NonTerminalId]
}

/// # Summary
///
/// `Attribute` identifies a child element of the node that will take the node's
/// value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Attribute {
    Named(Rc<str>),
    Indexed(usize),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ElementType {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
}

impl ElementType {
    fn name<'d>(&self, lexer_grammar: &LexerGrammar, grammar: &impl Grammar<'d>) -> Rc<str> {
        match self {
            Self::Terminal(id) => lexer_grammar.name(*id).into(),
            Self::NonTerminal(id) => grammar.name_of(*id),
        }
    }
}

impl From<NonTerminalId> for ElementType {
    fn from(id: NonTerminalId) -> Self {
        Self::NonTerminal(id)
    }
}

impl From<TerminalId> for ElementType {
    fn from(id: TerminalId) -> Self {
        Self::Terminal(id)
    }
}

newty! {
    pub set Axioms [NonTerminalId]
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuleElement {
    pub attribute: Attribute,
    pub key: Option<Key>,
    pub element_type: ElementType,
}

impl RuleElement {
    pub fn new(attribute: Attribute, key: Option<Key>, element_type: ElementType) -> Self {
        Self {
            attribute,
            key,
            element_type,
        }
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.element_type, ElementType::Terminal(..))
    }

    pub fn is_non_terminal(&self) -> bool {
        matches!(self.element_type, ElementType::NonTerminal(..))
    }

    pub fn name<'d>(
        &self,
        lexer_grammar: &LexerGrammar,
        grammar: &impl Grammar<'d>,
    ) -> Rc<str> {
        self.element_type.name(lexer_grammar, grammar)
    }
}

pub type Proxy = HashMap<Rc<str>, ValueTemplate>;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    /// The identifier of the nonterminal on the LHS of the rule.
    pub id: NonTerminalId,
    pub elements: Vec<RuleElement>,
    pub proxy: Proxy,
    pub left_associative: bool,
}

impl Rule {
    pub fn new(
        id: NonTerminalId,
        elements: Vec<RuleElement>,
        proxy: Proxy,
        left_associative: bool,
    ) -> Self {
        Self {
            id,
            elements,
            proxy,
            left_associative,
        }
    }
}

/// # Summary
///
/// [`Value`] is an typed value that may be present in a grammar.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ValueTemplate {
    /// String
    Str(Rc<str>),
    /// Identifier
    Id(Rc<str>),
    /// Inline node
    InlineRule {
        nonterminal: NonTerminalId,
        attributes: HashMap<Rc<str>, ValueTemplate>,
    },
}

impl ValueTemplate {
    pub(crate) fn evaluate(
        &self,
        current: NonTerminalId,
        all_attributes: &HashMap<Rc<str>, AST>,
        removed: &mut HashSet<Rc<str>>,
        id_of: &HashMap<Rc<str>, NonTerminalId>,
        span: &Span,
    ) -> AST {
        match self {
            ValueTemplate::Str(string) => AST::Literal {
                value: Value::Str(string.clone()),
                span: None,
            },
            ValueTemplate::Id(name) => {
                removed.insert(name.clone());
                all_attributes[name].clone()
            }
            ValueTemplate::InlineRule {
                nonterminal,
                attributes,
            } => AST::Node {
                nonterminal: *nonterminal,
                attributes: attributes
                    .iter()
                    .map(|(key, value_template)| {
                        (
                            key.clone(),
                            value_template.evaluate(
                                *nonterminal,
                                all_attributes,
                                removed,
                                id_of,
                                span,
                            ),
                        )
                    })
                    .collect(),
                span: span.clone(),
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct MacroInvocation {
    name: Rc<str>,
    args: Vec<MacroInvocation>,
}

type Name = Rc<str>;
type FullName = Rc<str>;

#[derive(Debug, Clone, PartialEq)]
struct MacroDeclaration {
    args: Vec<Rc<str>>,
    rules: Vec<PartialRule>,
}
#[derive(Debug, Clone, PartialEq)]
struct PartialRuleElement {
    attribute: Attribute,
    key: Option<Key>,
    element_type: PartialElementType,
}

impl PartialRuleElement {
    fn complete_to_element(
        self,
        rules: &mut GrammarRules,
        context: &HashMap<Name, ElementType>,
        reader: &mut GrammarReader<'_, '_>,
    ) -> RuleElement {
        RuleElement {
            attribute: self.attribute,
            key: self.key,
            element_type: self
                .element_type
                .complete_to_element_type(rules, context, reader),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct PartialRule {
    elements: Vec<PartialRuleElement>,
    proxy: Proxy,
    left_associative: bool,
}

impl PartialRule {
    fn complete_to_rule(
        self,
        id: NonTerminalId,
        rules: &mut GrammarRules,
        reader: &mut GrammarReader<'_, '_>,
        context: &HashMap<Name, ElementType>,
    ) -> Rule {
        Rule {
            id,
            elements: self
                .elements
                .into_iter()
                .map(|partial_element| {
                    partial_element.complete_to_element(rules, context, reader)
                })
                .collect(),
            proxy: self.proxy,
            left_associative: self.left_associative,
        }
    }
}


/// `Grammar` implements the require.
pub trait Grammar<'d>: Sized + Serialize + Deserialize<'d> {
    fn new(
        rules: GrammarRules,
        axioms: Axioms,
        id_of: HashMap<Rc<str>, NonTerminalId>,
        name_of: NonTerminalName,
        description_of: NonTerminalDescription,
    ) -> Result<Self>;

    fn deserialize(bytes: &'d [u8]) -> Result<Self> {
        WarningSet::empty_with_ok(bincode::deserialize::<'d, Self>(bytes)?)
    }

    fn name_of(&self, id: NonTerminalId) -> Rc<str>;
    fn description_of(&self, id: NonTerminalId) -> Option<Rc<str>>;
    fn id_of(&self, name: Rc<str>) -> NonTerminalId;

    fn serialize(&self) -> Result<Vec<u8>> {
        Ok(WarningSet::empty_with(bincode::serialize(self)?))
    }
}
