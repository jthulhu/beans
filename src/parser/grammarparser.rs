use crate::error::{
    Error, ErrorType, WResult,
    WResult::{WErr, WOk},
    Warning, WarningSet, WarningType,
};
use crate::lexer::{LexedStream, Lexer, LexerBuilder, Token};
use crate::location::Location;
use crate::stream::StringStream;
use crate::{ask_case, ctry};
// use crate::{rule, proxy, collect};
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::error;
use std::rc::Rc;

#[cfg(test)]
mod tests {}

pub type Key = String;
/// # Summary
///
/// `Attribute` identifies a child element of the node that will take the node's value.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Attribute {
    Named(String),
    Indexed(usize),
    None,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ElementType {
    Terminal(usize),
    NonTerminal(Option<usize>),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuleElement {
    pub name: Rc<str>,
    pub attribute: Attribute,
    pub key: Option<Key>,
    pub element_type: ElementType,
}

impl RuleElement {
    pub fn new<N: Into<Rc<str>>>(
        name: N,
        attribute: Attribute,
        key: Option<Key>,
        element_type: ElementType,
    ) -> Self {
        let name = name.into();
        Self {
            name,
            attribute,
            key,
            element_type,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self.element_type {
            ElementType::Terminal(..) => true,
            ElementType::NonTerminal(..) => false,
        }
    }

    pub fn is_non_terminal(&self) -> bool {
        match self.element_type {
            ElementType::Terminal(..) => false,
            ElementType::NonTerminal(..) => true,
        }
    }
}

pub type Proxy = HashMap<String, Value>;

#[derive(Debug, PartialEq)]
struct PartialRule {
    elements: Vec<RuleElement>,
    proxy: Proxy,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    pub name: Rc<str>,
    /// The identifier of the nonterminal on the LHS of the rule.
    pub id: usize,
    pub elements: Vec<RuleElement>,
    pub proxy: Proxy,
}

impl Rule {
    pub fn new<N: Into<Rc<str>>>(
        name: N,
        id: usize,
        elements: Vec<RuleElement>,
        proxy: Proxy,
    ) -> Self {
        let name = name.into();
        Self {
            name,
            id,
            elements,
            proxy,
        }
    }
}

/// # Summary
///
/// [`Value`] is an typed value that may be present in a grammar.
///
/// # Variants
///
/// [`Int`] is a signed integer (on 32 bits).
/// [`Str`] is a string.
/// [`Id`] is an identifier.
/// [`Float`] is a floating point number (on 32 bits).
/// [`Bool`] is a boolean.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    /// Signed integer
    Int(i32),
    /// String
    Str(String),
    /// Identifier
    Id(String),
    /// Float
    Float(f32),
    /// Boolean
    Bool(bool),
}

/// # Summary
///
/// [`GrammarBuilder`] is a builder for a grammar (ie. a type that implements [`Grammar`]).
pub trait GrammarBuilder<'deserializer>: Sized {
    /// [`Grammar`] that will be built by the [`GrammarBuilder`]
    type Grammar: Grammar<'deserializer>;
    /// Build with the given file as stream.
    fn with_file(self, file: Rc<str>) -> Result<Self, Box<dyn error::Error>> {
        Ok(self.with_stream(StringStream::from_file(file)?))
    }
    /// Build with the given stream.
    fn with_stream(self, stream: StringStream) -> Self;
    /// Build with the given grammar.
    fn with_grammar(self, grammar: Rc<str>) -> Self;
    /// Retrieve the stream from the builder.
    fn stream(&mut self) -> WResult<StringStream>;
    /// Retrieve the grammar from the builder.
    fn grammar(&self) -> Rc<str>;
    /// Build the grammar.
    fn build(mut self, lexer: &Lexer) -> WResult<Self::Grammar> {
        /// Read a token, match it against the provided `id`.
        /// If it matches, consume the token.
        /// Return whether the token matched.
        fn read_token_walk(lexed_input: &mut LexedStream<'_, '_>, id: &str) -> WResult<bool> {
            let mut warnings = WarningSet::empty();
            match ctry!(lexed_input.next_any(), warnings) {
                Some(token) if token.name() == id => WOk(true, warnings),
                _ => {
                    lexed_input.drop_last();
                    WOk(false, warnings)
                }
            }
        }

        /// Generate an error of type [`GrammarSyntaxError`][beans::error::ErrorType::GrammarSyntaxError].
        fn generate_error(location: Location, expected: &str, found: &str) -> Error {
            Error::new(
                location,
                ErrorType::GrammarSyntaxError(format!("expected {}, found {}", expected, found)),
            )
        }

        /// Read the token, match it against the provded `id`.
        /// If it matches, consume the token, fail otherwise.
        /// Return the token.
        fn match_now<'li>(lexed_input: &'li mut LexedStream<'_, '_>, id: &str) -> WResult<Token> {
            let mut warnings = WarningSet::empty();
            match ctry!(lexed_input.next_any(), warnings) {
                Some(token) => {
                    if token.name() == id {
                        let token = token.clone();
                        WOk(token, warnings)
                    } else {
                        let error = generate_error(token.location().clone(), id, token.name());
                        lexed_input.drop_last();
                        WErr(error)
                    }
                }
                None => WErr(generate_error(
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
        fn read_proxy_element(lexed_input: &mut LexedStream<'_, '_>) -> WResult<(String, Value)> {
            let mut warnings = WarningSet::empty();
            let id = ctry!(match_now(lexed_input, "ID"), warnings);
            ctry!(match_now(lexed_input, "COLON"), warnings);
            if let Some(token) = ctry!(lexed_input.next_any(), warnings) {
                WOk(
                    (
                        id.content().to_string(),
                        match token.name() {
                            "INT" => Value::Int(ctry!(
                                token
                                    .content()
                                    .parse::<i32>()
                                    .map_err(|_| {
                                        Error::new(
                                            token.location().clone(),
                                            ErrorType::GrammarSyntaxError(format!(
                                                "cannot understand {} as an integer",
                                                token.content()
                                            )),
                                        )
                                    })
                                    .into(),
                                warnings
                            )),
                            "STRING" => Value::Str(token.content().to_string()),
                            "FLOAT" => Value::Float(ctry!(
                                token
                                    .content()
                                    .parse::<f32>()
                                    .map_err(|_| {
                                        Error::new(
                                            token.location().clone(),
                                            ErrorType::GrammarSyntaxError(format!(
                                                "cannot understand {} as a float",
                                                token.content()
                                            )),
                                        )
                                    })
                                    .into(),
                                warnings
                            )),
                            "BOOL" => Value::Bool(ctry!(
                                token
                                    .content()
                                    .parse::<bool>()
                                    .map_err(|_| {
                                        Error::new(
                                            token.location().clone(),
                                            ErrorType::GrammarSyntaxError(format!(
                                                "cannot understand {} as a bool",
                                                token.content()
                                            )),
                                        )
                                    })
                                    .into(),
                                warnings
                            )),
                            "ID" => Value::Id(token.content().to_string()),
                            x => {
                                return WErr(generate_error(
                                    token.location().clone(),
                                    "INT, STRING, FLOAT, BOOL or ID",
                                    x,
                                ))
                            }
                        },
                    ),
                    warnings,
                )
            } else {
                WErr(generate_error(
                    lexed_input.last_location().clone(),
                    "INT, STRING, FLOAT, BOOL or ID",
                    "EOF",
                ))
            }
        }

        /// Read a proxy, of the form `<key: value ...>`.
        /// Consume the tokens.
        /// Fail if the proxy is malformed.
        /// Return the proxy.
        fn read_proxy(lexed_input: &mut LexedStream<'_, '_>) -> WResult<Proxy> {
            let mut warnings = WarningSet::empty();
            match_now(lexed_input, "LPROXY");
            let mut proxy = HashMap::new();
            while let Some(token) = ctry!(lexed_input.next_any(), warnings) {
                match token.name() {
                    "ID" => {
                        lexed_input.drop_last();
                        let (key, value) = ctry!(read_proxy_element(lexed_input), warnings);
                        proxy.insert(key, value);
                    }
                    "RPROXY" => {
                        return WOk(proxy, warnings);
                    }
                    x => {
                        let error = generate_error(token.location().clone(), "ID or RPROXY", x);
                        lexed_input.drop_last();
                        return WErr(error);
                    }
                }
            }
            WErr(generate_error(
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
        ) -> WResult<Attribute> {
            let mut warnings = WarningSet::empty();
            if ctry!(read_token_walk(lexed_input, "DOT"), warnings) {
                if let Some(token) = ctry!(lexed_input.next_any(), warnings) {
                    match token.name() {
                        "ID" => WOk(Attribute::Named(token.content().to_string()), warnings),
                        "INT" => WOk(
                            Attribute::Indexed(token.content().parse().unwrap()),
                            warnings,
                        ),
                        x => {
                            let error = generate_error(token.location().clone(), "ID or INT", x);
                            lexed_input.drop_last();
                            WErr(error)
                        }
                    }
                } else {
                    lexed_input.drop_last();
                    WErr(generate_error(
                        lexed_input.last_location().clone(),
                        "ID or INT",
                        "EOF",
                    ))
                }
            } else {
                WOk(Attribute::None, warnings)
            }
        }

        /// Read an element key, of the form `@key`.
        /// Consume the tokens.
        /// Fail if there is an at but no key.
        /// Return the key.
        fn read_rule_element_key(lexed_input: &mut LexedStream<'_, '_>) -> WResult<Option<String>> {
            let mut warnings = WarningSet::empty();
            if ctry!(read_token_walk(lexed_input, "AT"), warnings) {
                let token = ctry!(match_now(lexed_input, "ID"), warnings);
                WOk(Some(token.content().to_string()), warnings)
            } else {
                WOk(None, warnings)
            }
        }

        /// Read an element, of the form `Token(.attribute)?(@key)?`.
        /// Consume the tokens.
        /// Fail if the element is malformed.
        /// Return the element.
        fn read_rule_element(
            lexed_input: &mut LexedStream<'_, '_>,
            lexer: &Lexer,
        ) -> WResult<RuleElement> {
            let mut warnings = WarningSet::empty();
            let id = ctry!(match_now(lexed_input, "ID"), warnings);
            let attribute = ctry!(read_rule_element_attribute(lexed_input), warnings);
            let key = ctry!(read_rule_element_key(lexed_input), warnings);
            let name = id.content();
            WOk(
                RuleElement::new(
                    name,
                    attribute,
                    key,
                    if let Some(id) = lexer.grammar().id(name) {
                        ElementType::Terminal(id)
                    } else {
                        ElementType::NonTerminal(None)
                    },
                ),
                warnings,
            )
        }

        /// Read a rule, of the form `Token(.attribute)?(@key)? ... <key: value ...>`.
        /// Consume the tokens.
        /// Fails if the rule is malformed.
        /// Return the rule.
        fn read_rule(lexed_input: &mut LexedStream<'_, '_>, lexer: &Lexer) -> WResult<PartialRule> {
            let mut warnings = WarningSet::empty();
            let expected = "ID";
            let mut rule_elements = Vec::new();
            while let Some(token) = ctry!(lexed_input.next_any(), warnings) {
                if token.name() == "LPROXY" {
                    lexed_input.drop_last();
                    let proxy = ctry!(read_proxy(lexed_input), warnings);
                    return WOk(
                        PartialRule {
                            elements: rule_elements,
                            proxy,
                        },
                        warnings,
                    );
                } else {
                    lexed_input.drop_last();
                }
                rule_elements.push(ctry!(read_rule_element(lexed_input, lexer), warnings));
            }
            WErr(generate_error(
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

            id: usize,
            lexer: &Lexer,
        ) -> WResult<(bool, String, Vec<Rule>)> {
            let mut warnings = WarningSet::empty();
            let axiom = ctry!(read_token_walk(lexed_input, "AT"), warnings);
            let name = ctry!(match_now(lexed_input, "ID"), warnings);
            ctry!(match_now(lexed_input, "ASSIGNMENT"), warnings);
            let name_string = name.content();
            let mut rules = Vec::new();
            'read_rules: while let Some(token) = ctry!(lexed_input.next_any(), warnings) {
                if token.name() == "SEMICOLON" {
                    break 'read_rules;
                }
                lexed_input.drop_last();
                let partial_rule = ctry!(read_rule(lexed_input, lexer), warnings);
                let rule = Rule::new(name_string, id, partial_rule.elements, partial_rule.proxy);
                rules.push(rule);
            }
            WOk((axiom, name_string.to_string(), rules), warnings)
        }

        let mut warnings = WarningSet::empty();
        let mut stream = ctry!(self.stream(), warnings);
        let temp_lexer = ctry!(LexerBuilder::from_file(self.grammar()), warnings).build();
        let mut lexed_input = temp_lexer.lex(&mut stream);

        let mut rules = Vec::new();

        let mut axioms_vec = Vec::new();

        let mut done: HashMap<_, Location> = HashMap::new();
        let mut nonterminals = 0;
        while let Some(token) = ctry!(lexed_input.next_any(), warnings) {
            let first_location = token.location().clone();
            lexed_input.drop_last();
            let (axiom, name, new_rules) = ctry!(
                read_definition(&mut lexed_input, nonterminals, lexer),
                warnings
            );
            nonterminals += 1;
            let location = Location::extend(first_location, lexed_input.last_location().clone());
            if let Some(old_location) = done.get(&name) {
                return WErr(Error::new(
                    location,
                    ErrorType::GrammarDuplicateDefinition(name, old_location.clone()),
                ));
            }
            if lexer.grammar().contains(&name) {
                return WErr(Error::new(
                    location,
                    ErrorType::GrammarNonTerminalDuplicate(name),
                ));
            }
            if axiom {
                axioms_vec.push((rules.len(), rules.len() + new_rules.len()));
            }
            rules.extend(new_rules);
            ask_case!(&name, PascalCase, warnings);
            done.insert(name, location);
        }

        let mut axioms = FixedBitSet::with_capacity(rules.len());
        for (i, j) in axioms_vec {
            axioms.set_range(i..j, true);
        }

        let mut name_map = HashMap::new();

        for rule in rules.iter() {
            name_map.insert(rule.name.clone(), rule.id);
        }

        for rule in rules.iter_mut() {
            for element in rule.elements.iter_mut() {
                if element.is_terminal() {
                    continue;
                }
                match name_map.get(&element.name) {
                    Some(&id) => element.element_type = ElementType::NonTerminal(Some(id)),
                    None => warnings.add(Warning::new(WarningType::UndefinedNonTerminal(
                        rule.name.clone(),
                        element.name.clone(),
                    ))),
                }
            }
        }

        let grammar = ctry!(Self::Grammar::new(rules, axioms, name_map), warnings);

        WOk(grammar, warnings)
    }
}

/// `Grammar` implements the require.
pub trait Grammar<'deserializer>: Sized + Serialize + Deserialize<'deserializer> {
    fn new(
        rules: Vec<Rule>,
        axioms: FixedBitSet,
        name_map: HashMap<Rc<str>, usize>,
    ) -> WResult<Self>;

    fn from(bytes: &'deserializer [u8]) -> WResult<Self> {
        bincode::deserialize::<'_, Self>(bytes)
            .map_err(|x| {
                Error::new(
                    Location::new(
                        file!(),
                        (line!() as usize, column!() as usize),
                        (line!() as usize, column!() as usize),
                    ),
                    ErrorType::DeserializationError(format!("{}", x)),
                )
            })
            .into()
    }
    fn serialize(&self) -> WResult<Vec<u8>> {
        bincode::serialize(self)
            .map_err(|x| {
                Error::new(
                    Location::new(
                        file!(),
                        (line!() as usize, column!() as usize),
                        (line!() as usize, column!() as usize),
                    ),
                    ErrorType::SerializationError(format!("{}", x)),
                )
            })
            .into()
    }
}
