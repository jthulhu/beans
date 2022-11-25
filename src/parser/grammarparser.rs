use super::parser::{NonTerminalId, Value, AST};
use crate::error::Result;
use crate::error::{Error, WarningSet};
use crate::lexer::{LexedStream, Lexer, Token};
use crate::lexer::{LexerGrammar, TerminalId};
use crate::parser::earley::GrammarRules;
use crate::span::Span;
use crate::stream::StringStream;
use const_format::formatcp;
use fragile::Fragile;
use itertools::Itertools;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

mod keyword {
    pub const VARIANT_NAME: &str = "variant";
    pub const SELF: &str = "Self";
}

mod token {
    pub const IDENTIFIER: &str = "ID";
    pub const STRING: &str = "STRING";
    pub const PROXY_ASSIGN: &str = "COLON";
    pub const INLINE_RULE_START: &str = "LBRACE";
    pub const INLINE_RULE_END: &str = "RBRACE";
    pub const EOF: &str = "EOF";
    pub const MACRO_ARGS_SEPARATOR: &str = "COMMA";
    pub const MACRO_ARGS_START: &str = "LBRACKET";
    pub const MACRO_ARGS_END: &str = "RBRACKET";
    pub const ATTRIBUTE_START: &str = "DOT";
    pub const KEY_START: &str = "AT";
    pub const INTEGER: &str = "INT";
    pub const PROXY_START: &str = "LPROXY";
    pub const PROXY_END: &str = "RPROXY";
    pub const ASSIGNMENT: &str = "ASSIGNMENT";
    pub const RULE_SEPARATOR: &str = "SEMICOLON";
    pub const ASSOC_START: &str = "LPAR";
    pub const ASSOC_END: &str = "RPAR";
    pub const LEFT_ASSOC: &str = "LEFT";
    pub const RIGHT_ASSOC: &str = "RIGHT";
}

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
    Named(String),
    Indexed(usize),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ElementType {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
}

impl ElementType {
    fn name<'d>(
        &self,
        lexer_grammar: &LexerGrammar,
        grammar: &impl Grammar<'d>,
    ) -> Rc<str> {
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
    #![allow(unused)]

    pub fn new(
        attribute: Attribute,
        key: Option<Key>,
        element_type: ElementType,
    ) -> Self {
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

#[derive(Debug, Clone, PartialEq)]
enum PartialElementType {
    Terminal(TerminalId),
    MacroOrNonTerminal(MacroInvocation),
}

impl PartialElementType {
    fn complete_to_element_type(
        self,
        rules: &mut GrammarRules,
        context: &HashMap<Rc<str>, ElementType>,
        reader: &mut GrammarReader<'_, '_>,
    ) -> ElementType {
        match self {
            Self::Terminal(id) => ElementType::Terminal(id),
            Self::MacroOrNonTerminal(mi) => mi.evaluate(rules, context, reader),
        }
    }
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    /// The identifier of the nonterminal on the LHS of the rule.
    pub id: NonTerminalId,
    pub elements: Vec<RuleElement>,
    pub proxy: Proxy,
    pub left_associative: bool,
}

impl Rule {
    #[allow(unused)]
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
///
/// # Variants
///
/// [`Int`] is a signed integer (on 32 bits).
/// [`Str`] is a string.
/// [`Id`] is an identifier.
/// [`Float`] is a floating point number (on 32 bits).
/// [`Bool`] is a boolean.
/// [`InlineRule`] is an inlined rule.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ValueTemplate {
    /// String
    Str(Rc<str>),
    /// Identifier
    Id(Rc<str>),
    /// Inline node
    InlineRule {
        name: Rc<str>,
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
            ValueTemplate::InlineRule { name, attributes } => {
                let nonterminal = if &**name == keyword::SELF {
                    current
                } else {
                    id_of[name]
                };
                AST::Node {
                    nonterminal,
                    attributes: attributes
                        .iter()
                        .map(|(key, value_template)| {
                            (
                                key.clone(),
                                value_template.evaluate(
                                    nonterminal,
                                    all_attributes,
                                    removed,
                                    id_of,
                                    span,
                                ),
                            )
                        })
                        .collect(),
                    span: span.clone(),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct MacroInvocation {
    name: Rc<str>,
    args: Vec<MacroInvocation>,
}

impl MacroInvocation {
    fn evaluate(
        &self,
        rules: &mut GrammarRules,
        context: &HashMap<Name, ElementType>,
        reader: &mut GrammarReader<'_, '_>,
    ) -> ElementType {
        if self.args.is_empty() {
            if let Some(id) = reader.lexer.grammar().id(&self.name) {
                ElementType::Terminal(id)
            } else {
                context.get(&self.name).cloned().unwrap_or_else(|| {
                    ElementType::NonTerminal(reader.id_of[&self.name])
                })
            }
        } else {
            let MacroInvocation { name, args } = self;
            let evaluated_args: Vec<ElementType> = args
                .iter()
                .map(|arg_mi| arg_mi.evaluate(rules, context, reader))
                .collect();
            let invoked = InvokedMacro {
                name: name.clone(),
                arguments: evaluated_args,
            };
            if !reader.invoked_macros.contains_key(&invoked) {
                invoked.instanciate(rules, reader);
            }
            let invoked_name =
                reader.name_of[reader.invoked_macros[&invoked]].clone();
            ElementType::NonTerminal(reader.id_of[&invoked_name])
        }
    }
}

type Name = Rc<str>;
type FullName = Rc<str>;

#[derive(Debug, Clone, PartialEq)]
struct MacroDeclaration {
    args: Vec<Rc<str>>,
    rules: Vec<PartialRule>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InvokedMacro {
    name: Name,
    arguments: Vec<ElementType>,
}

impl InvokedMacro {
    fn instanciate(
        &self,
        rules: &mut GrammarRules,
        reader: &mut GrammarReader<'_, '_>,
    ) {
        let id = reader.name_of.len_as();
        let full_name = self.full_name(reader);
        reader.name_of.push(full_name.clone());
	reader.description_of.push(None);
        reader.invoked_macros.insert(self.clone(), id);
        reader.id_of.insert(full_name, id);
        let declaration = reader.macro_declarations[&self.name].clone();
        let context: HashMap<_, _> = declaration
            .args
            .iter()
            .cloned()
            .zip(self.arguments.iter().cloned())
            .collect();
        let completed_rules = declaration
            .rules
            .into_iter()
            .map(|rule| rule.complete_to_rule(id, rules, reader, &context))
            .collect::<Vec<_>>();
        rules.extend(completed_rules);
    }

    fn full_name(&self, reader: &GrammarReader<'_, '_>) -> Name {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            format!(
                "{}[{}]",
                self.name,
                self.arguments
                    .iter()
                    .map(|argument| match argument {
                        ElementType::Terminal(id) =>
                            reader.lexer.grammar().name(*id).to_string(),
                        ElementType::NonTerminal(id) =>
                            reader.name_of[*id].to_string(),
                    })
                    .intersperse(", ".to_string())
                    .collect::<String>()
            )
            .as_str()
            .into()
        }
    }
}

struct GrammarReader<'lexer, 'stream> {
    lexer: &'lexer Lexer,
    lexed_input: LexedStream<'lexer, 'stream>,
    macro_declarations: HashMap<Name, MacroDeclaration>,
    invoked_macros: HashMap<InvokedMacro, NonTerminalId>,
    rules: Vec<(NonTerminalId, PartialRule)>,
    id_of: HashMap<FullName, NonTerminalId>,
    warnings: WarningSet,
    axioms: Vec<NonTerminalId>,
    found_declarations: HashMap<Name, Span>,
    name_of: NonTerminalName,
    description_of: NonTerminalDescription,
}

impl<'lexer, 'stream> GrammarReader<'lexer, 'stream> {
    fn new(
        lexer: &'lexer Lexer,
        lexed_input: LexedStream<'lexer, 'stream>,
    ) -> Self {
        Self {
            lexer,
            lexed_input,
            macro_declarations: HashMap::new(),
            invoked_macros: HashMap::new(),
            rules: Vec::new(),
            id_of: HashMap::new(),
            warnings: WarningSet::empty(),
            axioms: Vec::new(),
            found_declarations: HashMap::new(),
            name_of: NonTerminalName::new(),
            description_of: NonTerminalDescription::new(),
        }
    }

    fn next_token(&mut self) -> std::result::Result<Option<Token>, Error> {
        Ok(self
            .lexed_input
            .next_any()?
            .unpack_into(&mut self.warnings)
            .cloned())
    }

    fn peek_token(&mut self, name: &str) -> std::result::Result<bool, Error> {
        match self.next_token()? {
            Some(token) if token.name() == name => Ok(true),
            Some(_) => {
                self.lexed_input.drop_last();
                Ok(false)
            }
            None => Ok(false),
        }
    }

    fn generate_error<T>(
        &self,
        location: Span,
        expected: &str,
        found: &str,
    ) -> std::result::Result<T, Error> {
        Err(Error::GrammarSyntaxError {
            message: format!("expected {expected}, found {found}"),
            location: Fragile::new(location),
        })
    }

    fn read_token(&mut self, name: &str) -> std::result::Result<Token, Error> {
        match self.next_token()? {
            Some(token) => {
                if token.name() == name {
                    Ok(token)
                } else {
                    return self.generate_error(
                        token.location().clone(),
                        name,
                        token.name(),
                    );
                }
            }
            None => {
                return self.generate_error(
                    self.lexed_input.last_location().clone(),
                    name,
                    token::EOF,
                )
            }
        }
    }

    fn read_macro_arguments(
        &mut self,
    ) -> std::result::Result<Vec<Rc<str>>, Error> {
        let mut args = Vec::new();
        if self.peek_token(token::MACRO_ARGS_START)? {
            let mut cont = true;
            while cont {
                args.push(self.read_token(token::IDENTIFIER)?.content().into());
                cont = self.peek_token(token::MACRO_ARGS_SEPARATOR)?;
            }
            self.read_token(token::MACRO_ARGS_END)?;
        }
        Ok(args)
    }

    fn read_proxy_value(
        &mut self,
    ) -> std::result::Result<ValueTemplate, Error> {
        let token = if let Some(token) = self.next_token()? {
            token
        } else {
            return self.generate_error(
                self.lexed_input.last_location().clone(),
                formatcp!("{} or {}", token::STRING, token::IDENTIFIER),
                token::EOF,
            );
        };
        let value = match token.name() {
            token::STRING => ValueTemplate::Str(Rc::from(token.content())),
            token::IDENTIFIER => {
                let name = token.content().into();
                if self.peek_token(token::INLINE_RULE_START)? {
                    ValueTemplate::InlineRule {
                        name,
                        attributes: self.read_proxy(token::INLINE_RULE_END)?,
                    }
                } else {
                    ValueTemplate::Id(name)
                }
            }
            found_token => {
                return self.generate_error(
                    token.location().clone(),
                    formatcp!("{} or {}", token::STRING, token::IDENTIFIER),
                    found_token,
                )
            }
        };
        Ok(value)
    }

    fn read_proxy_element(
        &mut self,
    ) -> std::result::Result<(Key, ValueTemplate), Error> {
        let key_token = self.read_token(token::IDENTIFIER)?;
        let key: Rc<str> = key_token.content().into();
        if self.peek_token(token::PROXY_ASSIGN)? {
            if &*key == keyword::VARIANT_NAME {
                return Err(Error::GrammarVariantKey {
                    location: key_token.location().into(),
                });
            }
            let value = self.read_proxy_value()?;
            Ok((key, value))
        } else {
            Ok((Rc::from(keyword::VARIANT_NAME), ValueTemplate::Str(key)))
        }
    }

    fn read_proxy(&mut self, end: &str) -> std::result::Result<Proxy, Error> {
        let mut proxy = HashMap::new();
        while let Some(token) = self.next_token()? {
            if token.name() == end {
                return Ok(proxy);
            }
            self.lexed_input.drop_last();
            let (key, value) = self.read_proxy_element()?;
            proxy.insert(key, value);
        }
        return self.generate_error(
            self.lexed_input.last_location().clone(),
            format!("{} or {}", token::IDENTIFIER, end).as_str(),
            token::EOF,
        );
    }

    fn read_macro_invocation(
        &mut self,
        name: Rc<str>,
    ) -> std::result::Result<(MacroInvocation, Span), Error> {
        let mut args = Vec::new();
        let mut span = self.lexed_input.last_location().clone();
        if self.peek_token(token::MACRO_ARGS_START)? {
            let mut cont = true;
            while cont {
                let arg_name =
                    self.read_token(token::IDENTIFIER)?.content().into();
                let (arg, _) = self.read_macro_invocation(arg_name)?;
                args.push(arg);
                cont = self.peek_token(token::MACRO_ARGS_SEPARATOR)?;
            }
            span = span.sup(self.read_token(token::MACRO_ARGS_END)?.location());
        }
        Ok((MacroInvocation { name, args }, span))
    }

    fn read_rule_element_attribute(
        &mut self,
    ) -> std::result::Result<Attribute, Error> {
        if self.peek_token(token::ATTRIBUTE_START)? {
            if let Some(token) = self.next_token()? {
                match token.name() {
                    token::IDENTIFIER => {
                        let name = token.content().into();
                        Ok(Attribute::Named(name))
                    }
                    token::INTEGER => {
                        Ok(Attribute::Indexed(token.content().parse().unwrap()))
                    }
                    found_token => self.generate_error(
                        token.location().clone(),
                        formatcp!(
                            "{} or {}",
                            token::IDENTIFIER,
                            token::INTEGER
                        ),
                        found_token,
                    ),
                }
            } else {
                self.generate_error(
                    self.lexed_input.last_location().clone(),
                    formatcp!("{} or {}", token::IDENTIFIER, token::INTEGER),
                    token::EOF,
                )
            }
        } else {
            Ok(Attribute::None)
        }
    }

    fn read_rule_element_key(
        &mut self,
    ) -> std::result::Result<Option<Key>, Error> {
        if self.peek_token(token::KEY_START)? {
            Ok(Some(self.read_token(token::IDENTIFIER)?.content().into()))
        } else {
            Ok(None)
        }
    }

    fn read_rule_element(
        &mut self,
        name: Rc<str>,
    ) -> std::result::Result<PartialRuleElement, Error> {
        let (invocation, span) = self.read_macro_invocation(name.clone())?;
        let element_type = if let Some(id) = self.lexer.grammar().id(&name) {
            if !invocation.args.is_empty() {
                return Err(Error::GrammarTerminalInvocation {
                    terminal: name.to_string(),
                    location: Fragile::new(span),
                });
            }
            PartialElementType::Terminal(id)
        } else {
            PartialElementType::MacroOrNonTerminal(invocation)
        };
        let attribute = self.read_rule_element_attribute()?;
        let key = self.read_rule_element_key()?;
        Ok(PartialRuleElement {
            key,
            attribute,
            element_type,
        })
    }

    fn read_rule(&mut self) -> std::result::Result<PartialRule, Error> {
        let mut rule_elements = Vec::new();
        let left_associative = if self.peek_token(token::ASSOC_START)? {
            let Some(token) = self.next_token()? else {
		return self.generate_error(
		    self.lexed_input.last_location().clone(),
		    formatcp!("{} or {}", token::LEFT_ASSOC, token::RIGHT_ASSOC),
		    token::EOF
		);
	    };
            let result = match token.name() {
                token::LEFT_ASSOC => true,
                token::RIGHT_ASSOC => false,
                found => {
                    return self.generate_error(
                        token.location().clone(),
                        formatcp!(
                            "{} or {}",
                            token::LEFT_ASSOC,
                            token::RIGHT_ASSOC
                        ),
                        found,
                    )
                }
            };
            self.read_token(token::ASSOC_END)?;
            result
        } else {
            true
        };
        while let Some(token) = self.next_token()? {
            match token.name() {
                token::PROXY_START => {
                    let proxy = self.read_proxy(token::PROXY_END)?;
                    return Ok(PartialRule {
                        elements: rule_elements,
                        proxy,
                        left_associative,
                    });
                }
                token::IDENTIFIER => {
                    let element =
                        self.read_rule_element(token.content().into())?;
                    rule_elements.push(element);
                }
                found_token => {
                    return self.generate_error(
                        token.location().clone(),
                        formatcp!(
                            "{} or {}",
                            token::IDENTIFIER,
                            token::PROXY_START
                        ),
                        found_token,
                    )
                }
            }
        }
        self.generate_error(
            self.lexed_input.last_location().clone(),
            formatcp!("{} or {}", token::IDENTIFIER, token::PROXY_START),
            token::EOF,
        )
    }

    fn read_definition(&mut self) -> std::result::Result<bool, Error> {
        let description = self.next_token()?.and_then(|tok| {
            if tok.name() == token::STRING {
                Some(tok.content().into())
            } else {
		self.lexed_input.drop_last();
                None
            }
        });
        let axiom = self.peek_token(token::KEY_START)?;
        let definition_name_token = match self
            .lexed_input
            .next_any()?
            .unpack_into(&mut self.warnings)
        {
            Some(token) => {
                if token.name() == token::IDENTIFIER {
                    token
                } else {
                    let location = token.location().clone();
                    let name = token.name().to_string();
                    return self.generate_error(
                        location,
                        token::IDENTIFIER,
                        name.as_str(),
                    );
                }
            }
            None => {
                if axiom {
                    todo!()
                } else {
                    return Ok(false);
                }
            }
        };
        let definition_name: Rc<str> = definition_name_token.content().into();
        if let Some(_old_location) = self.found_declarations.insert(
            definition_name.clone(),
            definition_name_token.location().clone(),
        ) {
            // Report error
            todo!()
        }
        let arguments = self.read_macro_arguments()?;
        self.read_token(token::ASSIGNMENT)?;
        let mut rules: Vec<PartialRule> = Vec::new();
        'read_rules: while let Some(token) = self.next_token()? {
            if token.name() == token::RULE_SEPARATOR {
                break 'read_rules;
            }
            self.lexed_input.drop_last();
            let partial_rule = self.read_rule()?;
            rules.push(partial_rule);
        }

        if arguments.is_empty() {
            let nonterminal_id = self.name_of.len_as();
            self.name_of.push(definition_name.clone());
            self.description_of.push(description);
            self.id_of.insert(definition_name, nonterminal_id);
            self.rules.extend(
                rules
                    .into_iter()
                    .map(|partial_rule| (nonterminal_id, partial_rule))
                    .rev(),
            );
            if axiom {
                self.axioms.push(nonterminal_id);
            }
        } else {
            self.macro_declarations.insert(
                definition_name,
                MacroDeclaration {
                    args: arguments,
                    rules,
                },
            );
        }
        Ok(true)
    }

    fn read(
        mut self,
    ) -> Result<(
        GrammarRules,
        HashMap<Rc<str>, NonTerminalId>,
        Vec<NonTerminalId>,
        NonTerminalName,
        NonTerminalDescription,
    )> {
        while self.read_definition()? {}
        let mut rules = GrammarRules::new();
        while let Some((id, partial_rule)) = self.rules.pop() {
            let rule = partial_rule.complete_to_rule(
                id,
                &mut rules,
                &mut self,
                &HashMap::new(),
            );
            rules.push(rule);
        }
        self.warnings.with_ok((
            rules,
            self.id_of,
            self.axioms,
            self.name_of,
            self.description_of,
        ))
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
        Ok(
            warnings
                .on(StringStream::from_file(file)?, |x| self.with_stream(x)),
        )
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
        let grammar =
            Self::Grammar::new(rules, axioms, id_of, name_of, description_of)?
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
        Ok(WarningSet::empty_with(bincode::deserialize::<'d, Self>(
            bytes,
        )?))
    }

    fn name_of(&self, id: NonTerminalId) -> Rc<str>;
    fn description_of(&self, id: NonTerminalId) -> Option<Rc<str>>;
    fn id_of(&self, name: Rc<str>) -> NonTerminalId;

    fn serialize(&self) -> Result<Vec<u8>> {
        Ok(WarningSet::empty_with(bincode::serialize(self)?))
    }
}
