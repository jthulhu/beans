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

/// # Summary
///
/// [`GrammarBuilder`] is a builder for a grammar (ie. a type that implements [`Grammar`]).
pub trait GrammarBuilder<'deserializer>: Sized {
    /// [`Grammar`] that will be built by the [`GrammarBuilder`]
    type Grammar: Grammar<'deserializer>;
    /// Build with the given file as stream.
    fn with_file(self, file: impl Into<Rc<Path>>) -> Result<Self> {
        let file = file.into();
        let warnings = WarningSet::empty();
        Ok(warnings.on(StringStream::from_file(file)?, |x| self.with_stream(x)))
    }
    /// Build with the given stream.
    fn with_stream(self, stream: StringStream) -> Self;
    /// Build with the given grammar.
    fn with_grammar_file(self, grammar: impl Into<Rc<Path>>) -> Result<Self>;
    fn with_grammar_stream(self, grammar: StringStream) -> Result<Self>;
    /// Retrieve the stream from the builder.
    fn stream(&mut self) -> Result<StringStream>;
    /// Retrieve the grammar from the builder.
    fn grammar_lexer(&mut self) -> Result<Lexer>;
    /// Build the grammar.
    fn build(mut self, lexer: &Lexer) -> Result<Self::Grammar> {
        #![allow(unused)] // Bug that mark these functions as unused.

        /// Read a token, match it against the provided `id`.
        /// If it matches, consume the token.
        /// Return whether the token matched.
        fn read_token_walk(
            lexed_input: &mut LexedStream<'_, '_>,
            id: &str,
        ) -> Result<bool> {
            let mut warnings = WarningSet::empty();
            match lexed_input.next_any()?.unpack_into(&mut warnings) {
                Some(token) if token.name() == id => warnings.with_ok(true),
                _ => {
                    lexed_input.drop_last();
                    warnings.with_ok(false)
                }
            }
        }
	
        /// Generate an error of type [`GrammarSyntaxError`][beans::error::ErrorType::GrammarSyntaxError].
        fn generate_error(
            location: Location,
            expected: impl Display,
            found: impl Display,
        ) -> Error {
            Error::GrammarSyntaxError {
                location: Fragile::new(location),
                message: format!("expected {}, found {}", expected, found),
            }
        }

        /// Read the token, match it against the provded `id`.
        /// If it matches, consume the token, fail otherwise.
        /// Return the token.
        fn match_now<'li>(
            lexed_input: &'li mut LexedStream<'_, '_>,
            id: &str,
        ) -> Result<Token> {
            let mut warnings = WarningSet::empty();
            match lexed_input.next_any()?.unpack_into(&mut warnings) {
                Some(token) => {
                    if token.name() == id {
                        let token = token.clone();
                        warnings.with_ok(token)
                    } else {
                        let error = generate_error(
                            token.location().clone(),
                            id,
                            token.name(),
                        );
                        lexed_input.drop_last();
                        Err(error)
                    }
                }
                None => Err(generate_error(
                    lexed_input.last_location().clone(),
                    id,
                    "EOF",
                )),
            }
        }

        /// Read a proxy element, of the form `key: value`.
        /// Consume the tokens.
        /// Fail if there is no proxy element.
        /// Return the proxy element as `(key, value)`.
        fn read_proxy_element(
            lexed_input: &mut LexedStream<'_, '_>,
        ) -> Result<(String, ValueTemplate)> {
            let mut warnings = WarningSet::empty();
            let id = match_now(lexed_input, "ID")?.unpack_into(&mut warnings);
            if read_token_walk(lexed_input, "COLON")?.unpack_into(&mut warnings)
            {
                let name = id.content().to_string();
                if name == VARIANT_NAME {
                    return Err(Error::GrammarVariantKey {
                        location: Fragile::new(id.location().clone()),
                    });
                }
                let token = if let Some(token) =
                    lexed_input.next_any()?.unpack_into(&mut warnings)
                {
                    token
                } else {
                    return Err(generate_error(
                        lexed_input.last_location().clone(),
                        "INT, STRING, FLOAT, BOOL or ID",
                        "EOF",
                    ));
                };
                warnings.with_ok((
                    name,
                    match token.name() {
                        "INT" => ValueTemplate::Int(
                            token.content().parse::<i32>().map_err(|_| {
                                Error::GrammarSyntaxError {
                                    location: Fragile::new(
                                        token.location().clone(),
                                    ),
                                    message: format!(
                                        "cannot understand {} as an integer",
                                        token.content()
                                    ),
                                }
                            })?,
                        ),
                        "STRING" => {
                            ValueTemplate::Str(Rc::from(token.content()))
                        }
                        "FLOAT" => ValueTemplate::Float(
                            token.content().parse::<f32>().map_err(|_| {
                                Error::GrammarSyntaxError {
                                    location: Fragile::new(
                                        token.location().clone(),
                                    ),
                                    message: format!(
                                        "cannot understand {} as a float",
                                        token.content()
                                    ),
                                }
                            })?,
                        ),
                        "BOOL" => ValueTemplate::Bool(
                            token.content().parse::<bool>().map_err(|_| {
                                Error::GrammarSyntaxError {
                                    message: format!(
                                        "cannot understand {} as a bool",
                                        token.content()
                                    ),
                                    location: Fragile::new(
                                        token.location().clone(),
                                    ),
                                }
                            })?,
                        ),
                        "ID" => ValueTemplate::Id(Rc::from(token.content())),
                        x => {
                            return Err(generate_error(
                                token.location().clone(),
                                "INT, STRING, FLOAT, BOOL or ID",
                                x,
                            ))
                        }
                    },
                ))
            } else {
                warnings.with_ok((
                    VARIANT_NAME.to_string(),
                    ValueTemplate::Str(Rc::from(id.content())),
                ))
            }
        }

        /// Read a proxy, of the form `<key: value ...>`.
        /// Consume the tokens.
        /// Fail if the proxy is malformed.
        /// Return the proxy.
        fn read_proxy(lexed_input: &mut LexedStream<'_, '_>) -> Result<Proxy> {
            let mut warnings = WarningSet::empty();
            match_now(lexed_input, "LPROXY")?;
            let mut proxy = HashMap::new();
            while let Some(token) =
                lexed_input.next_any()?.unpack_into(&mut warnings)
            {
                match token.name() {
                    "ID" => {
                        lexed_input.drop_last();
                        let (key, value) = read_proxy_element(lexed_input)?
                            .unpack_into(&mut warnings);
                        proxy.insert(key, value);
                    }
                    "RPROXY" => return warnings.with_ok(proxy),
                    x => {
                        let error = generate_error(
                            token.location().clone(),
                            "ID or RPROXY",
                            x,
                        );
                        lexed_input.drop_last();
                        return Err(error);
                    }
                }
            }
            Err(generate_error(
                lexed_input.last_location().clone(),
                "ID or RPROXY",
                "EOF",
            ))
        }

        /// Read an element attribute, of the form `.attribute`.
        /// Consume the tokens.
        /// Fail if there is a dot but no attribute.
        /// Return the attribute.
        fn read_rule_element_attribute(
            lexed_input: &mut LexedStream<'_, '_>,
        ) -> Result<Attribute> {
            let mut warnings = WarningSet::empty();
            if read_token_walk(lexed_input, "DOT")?.unpack_into(&mut warnings) {
                if let Some(token) =
                    lexed_input.next_any()?.unpack_into(&mut warnings)
                {
                    match token.name() {
                        "ID" => {
                            let name = token.content().to_string();
                            warnings.with_ok(Attribute::Named(name))
                        }
                        "INT" => warnings.with_ok(Attribute::Indexed(
                            token.content().parse().unwrap(),
                        )),
                        x => {
                            let error = generate_error(
                                token.location().clone(),
                                "ID or INT",
                                x,
                            );
                            lexed_input.drop_last();
                            Err(error)
                        }
                    }
                } else {
                    lexed_input.drop_last();
                    Err(generate_error(
                        lexed_input.last_location().clone(),
                        "ID or INT",
                        "EOF",
                    ))
                }
            } else {
                warnings.with_ok(Attribute::None)
            }
        }

        /// Read an element key, of the form `@key`.
        /// Consume the tokens.
        /// Fail if there is an at but no key.
        /// Return the key.
        fn read_rule_element_key(
            lexed_input: &mut LexedStream<'_, '_>,
        ) -> Result<Option<String>> {
            let mut warnings = WarningSet::empty();
            if read_token_walk(lexed_input, "AT")?.unpack_into(&mut warnings) {
                let token =
                    match_now(lexed_input, "ID")?.unpack_into(&mut warnings);
                warnings.with_ok(Some(token.content().to_string()))
            } else {
                warnings.with_ok(None)
            }
        }

        /// Read an element, of the form `Token(.attribute)?(@key)?`.
        /// Consume the tokens.
        /// Fail if the element is malformed.
        /// Return the element.
        fn read_rule_element(
            lexed_input: &mut LexedStream<'_, '_>,
            id: &mut NonTerminalId,
            name_map: &mut HashMap<Rc<str>, NonTerminalId>,
            lexer: &Lexer,
        ) -> Result<RuleElement> {
            let mut warnings = WarningSet::empty();
            let name_token =
                match_now(lexed_input, "ID")?.unpack_into(&mut warnings);
            let attribute = read_rule_element_attribute(lexed_input)?
                .unpack_into(&mut warnings);
            let key =
                read_rule_element_key(lexed_input)?.unpack_into(&mut warnings);
            let name = name_token.content();
            warnings.with_ok(RuleElement::new(
                name,
                attribute,
                key,
                if let Some(id) = lexer.grammar().id(name) {
                    ElementType::from(id)
                } else {
                    ElementType::from(
                        *name_map
                            .entry(name.into())
                            .or_insert_with(|| id.next()),
                    )
                },
            ))
        }

        /// Read a rule, of the form `Token(.attribute)?(@key)? ... <key: value ...>`.
        /// Consume the tokens.
        /// Fails if the rule is malformed.
        /// Return the rule.
        fn read_rule(
            lexed_input: &mut LexedStream<'_, '_>,
            id: &mut NonTerminalId,
            name_map: &mut HashMap<Rc<str>, NonTerminalId>,
            lexer: &Lexer,
        ) -> Result<PartialRule> {
            let mut warnings = WarningSet::empty();
            let expected = "ID";
            let mut rule_elements = Vec::new();
            while let Some(token) =
                lexed_input.next_any()?.unpack_into(&mut warnings)
            {
                #[allow(clippy::branches_sharing_code)]
                // Extracting lexed_input.drop_last()
                // causes the compiler to complain.
                if token.name() == "LPROXY" {
                    lexed_input.drop_last();
                    let proxy =
                        read_proxy(lexed_input)?.unpack_into(&mut warnings);
                    return warnings.with_ok(PartialRule {
                        elements: rule_elements,
                        proxy,
                    });
                } else {
                    lexed_input.drop_last();
                }
                rule_elements.push(
                    read_rule_element(lexed_input, id, name_map, lexer)?
                        .unpack_into(&mut warnings),
                );
            }
            Err(generate_error(
                lexed_input.last_location().clone(),
                expected,
                "EOF",
            ))
        }

        /// Read a definition, of the form `NonTerminal ::= Token(.attribute)?(@key)? ... <key: value ...> ...;`.
        /// Take as argument the `id` of the defined `NonTerminal`.
        /// Consume the tokens.
        /// Fails is malformed.
        /// Return the definition.
        fn read_definition(
            lexed_input: &mut LexedStream<'_, '_>,
            next_id: &mut NonTerminalId,
            name_map: &mut HashMap<Rc<str>, NonTerminalId>,
            lexer: &Lexer,
        ) -> Result<(bool, String, NonTerminalId, Vec<Rule>)> {
            let mut warnings = WarningSet::empty();
            let axiom =
                read_token_walk(lexed_input, "AT")?.unpack_into(&mut warnings);
            let name = match_now(lexed_input, "ID")?.unpack_into(&mut warnings);
            match_now(lexed_input, "ASSIGNMENT")?.unpack_into(&mut warnings);
            let name_string = name.content();
            let id = *name_map
                .entry(name_string.into())
                .or_insert_with(|| next_id.next());
            let mut rules = Vec::new();
            'read_rules: while let Some(token) =
                lexed_input.next_any()?.unpack_into(&mut warnings)
            {
                if token.name() == "SEMICOLON" {
                    break 'read_rules;
                }
                lexed_input.drop_last();
                let partial_rule =
                    read_rule(lexed_input, next_id, name_map, lexer)?
                        .unpack_into(&mut warnings);
                let id = *name_map
                    .entry(name_string.into())
                    .or_insert_with(|| next_id.next());
                let rule = Rule::new(
                    name_string,
                    id,
                    partial_rule.elements,
                    partial_rule.proxy,
                );
                rules.push(rule);
            }
            warnings.with_ok((axiom, name_string.to_string(), id, rules))
        }

        let mut warnings = WarningSet::empty();
        let mut stream = self.stream()?.unpack_into(&mut warnings);
        let temp_lexer = self.grammar_lexer()?.unpack_into(&mut warnings);
        let lexed_input = temp_lexer.lex(&mut stream);

        let (rules, id_of, axioms_vec, name_of, description_of) =
            GrammarReader::new(lexer, lexed_input)
                .read()?
                .unpack_into(&mut warnings);
        let number_of_nonterminals = name_of.len_as();
        let axioms = Axioms::from_vec(number_of_nonterminals, axioms_vec);
        let grammar = Self::Grammar::new(rules, axioms, id_of, name_of, description_of)?
            .unpack_into(&mut warnings);

        warnings.with_ok(grammar)
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
