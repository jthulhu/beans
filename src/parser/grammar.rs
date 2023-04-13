use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{
    lexer::{Grammar as LexerGrammar, TerminalId},
    span::Span,
};
use newty::newty;
use serde::{Deserialize, Serialize};

use super::{earley::EarleyGrammar, parser::NonTerminalId, Value, AST};

newty! {
    #[derive(Serialize, Deserialize)]
    pub vec NonTerminalName(Rc<str>)[NonTerminalId]
}

newty! {
    #[derive(Serialize, Deserialize)]
    pub vec NonTerminalDescription(Option<Rc<str>>)[NonTerminalId]
}

newty! {
    pub set Axioms [NonTerminalId]
}

newty! {
    #[derive(Serialize, Deserialize)]
    pub vec Rules(Rule)[RuleId]
}

newty! {
    pub set Nullables[NonTerminalId]
}

newty! {
    #[derive(PartialOrd, Ord)]
    pub id RuleId
}

pub type Proxy = HashMap<Rc<str>, ValueTemplate>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ElementType {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Attribute {
    Named(Rc<str>),
    Indexed(usize),
    None,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Element {
    pub attribute: Attribute,
    pub key: Option<Rc<str>>,
    pub element_type: ElementType,
}

impl Element {
    pub fn new(attribute: Attribute, key: Option<Rc<str>>, element_type: ElementType) -> Self {
        Self {
            attribute,
            key,
            element_type,
        }
    }

    pub fn name(&self, lexer_grammar: &LexerGrammar, grammar: &EarleyGrammar) -> Rc<str> {
        match self.element_type {
            ElementType::Terminal(id) => lexer_grammar.name(id).into(),
            ElementType::NonTerminal(id) => grammar.name_of(id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ValueTemplate {
    String(Rc<str>),
    Variable(Rc<str>),
    InlineRule {
        non_terminal: NonTerminalId,
        attributes: Proxy,
    },
}

impl ValueTemplate {
    pub fn evaluate(
        &self,
        all_attributes: &HashMap<Rc<str>, AST>,
        removed: &mut HashSet<Rc<str>>,
        span: &Span,
    ) -> AST {
        match self {
            ValueTemplate::String(string) => AST::Literal {
                value: Value::Str(string.clone()),
                span: None,
            },
            ValueTemplate::Variable(name) => {
                removed.insert(name.clone());
                all_attributes[name].clone()
            }
            ValueTemplate::InlineRule {
                non_terminal,
                attributes,
            } => AST::Node {
                nonterminal: *non_terminal,
                attributes: attributes
                    .iter()
                    .map(|(key, value_template)| {
                        (
                            key.clone(),
                            value_template.evaluate(all_attributes, removed, span),
                        )
                    })
                    .collect(),
                span: span.clone(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    pub id: NonTerminalId,
    pub elements: Vec<Element>,
    pub proxy: Proxy,
    pub left_associative: bool,
}

impl Rule {
    pub fn new(
        id: NonTerminalId,
        elements: Vec<Element>,
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
