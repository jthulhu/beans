use crate::case::Case;
use crate::error::{
    Error, ErrorType, WResult,
    WResult::{WErr, WOk},
    WarningSet,
};
use crate::lexer::{LexedStream, Lexer, LexerBuilder, Token};
use crate::location::Location;
use crate::stream::{Stream, StreamObject, StringStream};
use crate::{ask_case, ctry, retrieve};
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use std::error;
use std::fs::File;
use std::io::prelude::*;

#[cfg(test)]
mod tests {
    use super::*;
    /// # Summary
    ///
    /// `proxy!` parses a proxy definition and returns a typed `Proxy` object.
    ///
    /// # Usage
    ///
    /// Call this macro with the following syntax:
    /// `proxy!(<key = type value ...>)`
    /// **Note that this macro is only used to generate test units.**
    ///
    /// # Example
    ///
    /// ```rust
    /// let my_proxy = proxy!(<firstkey = str "hello" secondkey = str "world" thirdkey = bool false>);
    /// let my_proxy_bis = Proxy::new();
    /// my_proxy_bis.insert(String::from("firstkey"), Value::Str(String::from("hello")));
    /// my_proxy_bis.insert(String::from("secondkey"), Value::Str(String::from("world")));
    /// my_proxy_bis.insert(String::from("thirdkey"), Value::Bool(false));
    /// assert_eq!(my_proxy, my_proxy_bis);
    /// ```
    macro_rules! proxy {
	(@insert $proxy: ident $key: ident str $value: literal) => {
	    $proxy.insert(stringify!($key).to_string(), Value::Str($value.to_string()));
	};
	(@insert $proxy: ident $key: ident bool $value: literal) => {
	    $proxy.insert(stringify!($key).to_string(), Value::Bool($value));
	};
	(<$($key: ident = $type: ident $value: literal)*>) => {
	    {
		let mut proxy = Proxy::new();
		$(
		    proxy!(@insert proxy $key $type $value);
		)*
		proxy
	    }
	};
    }

    /// # Summary
    ///
    /// `rule!` parses a grammar definition of a single non-terminal
    /// and returns a `Vec` of the rules of that non-terminal.
    ///
    /// # Usage
    /// Call this macro with the following syntax:
    /// `rule!(NonTerminal ::= terminality token.type value@value ... <proxy> ...)`
    /// **Note that this macro is only used to generate test units.**
    #[rustfmt::skip]
    macro_rules! rule {
	(@key) => { None };
	(@key $key: ident) => { Some(stringify!($key).to_string()) };
	(@attribute) => { Attribute::None };
	(@attribute str $attribute: ident) => { Attribute::Named(stringify!($attribute).to_string()) };
	(@attribute idx $attribute: literal) => { Attribute::Indexed($attribute) };
	(@terminality t) => { ElementType::Terminal};
	(@terminality n) => { ElementType::NonTerminal };
	(@terminality) => { ElementType::NonTerminal };
	(@element $(! $terminality: tt)? $name: ident $(. $type: ident $attribute: tt)? $(@ $key: tt)?) => {
	    RuleElement {
		name: stringify!($name).to_string(),
		attribute: rule!(@attribute $($type $attribute)?),
		key: rule!(@key $($key)?),
		element_type: rule!(@terminality $($terminality)?)
	    }
	};
	(
	    $name: ident ::= $(
		$(
		    $(! $terminality: tt)?
		    $element: ident
			$(
			    . $attribute_type: ident $attribute: literal
			)?
		    $(
			@ $key: tt
		    )?
		)*
		    < $(
			$proxy_key: ident = $proxy_type: ident $proxy_value: literal
		    )* >
	    )*
	) => {
	    {
		let name = stringify!($name);
		let mut result = Vec::new();
		$(
		    let mut elements = Vec::new();
		    $(
			elements.push(rule!(@element $(! $terminality)? $element $(. $attribute_type $attribute)? $(@ $key)?));
		    )*;
		    let proxy = proxy!(<$($proxy_key = $proxy_type $proxy_value)*>);
		    result.push(Rule::new(name.to_string(), elements, proxy));
		)*
		    result
	    }
	};
    }

    macro_rules! collect {
	($($rules: expr),*) => {
	    {
		let mut result = Vec::new();
		$(
		    result.extend($rules);
		)*
		result
	    }
	};
    }

    #[inline]
    fn verify(rules1: Vec<Rule>, rules2: Vec<Rule>) {
        let length1 = rules1.len();
        let length2 = rules2.len();
        if length1 > length2 {
            panic!("Grammar 1 is longer");
        } else if length1 < length2 {
            panic!("Grammar 2 is longer");
        }
        for (i, (r1, r2)) in rules1.iter().zip(rules2.iter()).enumerate() {
            assert_eq!(r1, r2, "rules #{} differ", i);
        }
    }

    #[test]
    #[rustfmt::skip]
    fn grammar_builder() {
        let lexer = LexerBuilder::default().build().unwrap();
        assert!(GrammarBuilder::new().build(&lexer).is_err());

        let grammar = GrammarBuilder::default().build(&lexer).unwrap();
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

pub type RuleSet = FixedBitSet;
pub type Key = String;
/// # Summary
///
/// `Attribute` identifies a child element of the node that will take the node's value.
#[derive(Debug, PartialEq, Eq)]
pub enum Attribute {
    Named(String),
    Indexed(usize),
    None,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ElementType {
    Terminal,
    NonTerminal,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RuleElement {
    pub name: String,
    pub attribute: Attribute,
    pub key: Option<Key>,
    pub element_type: ElementType,
}
pub type Proxy = HashMap<String, Value>;

#[derive(Debug, PartialEq)]
struct PartialRule {
    elements: Vec<RuleElement>,
    proxy: Proxy,
}

#[derive(Debug, PartialEq)]
pub struct Rule {
    pub name: String,
    pub elements: Vec<RuleElement>,
    pub proxy: Proxy,
}

impl Rule {
    pub fn new(name: String, elements: Vec<RuleElement>, proxy: Proxy) -> Self {
        Self {
            name,
            elements,
            proxy,
        }
    }
}

/// # Summary
///
/// `Value` is an typed value that may be present in a grammar.
///
/// # Variants
///
/// `Int` is a signed integer (on 32 bits).
/// `Str` is a string.
/// `Id` is an identifier.
/// `Float` is a floating point number (on 32 bits).
/// `Bool` is a boolean.
#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Str(String),
    Id(String),
    Float(f32),
    Bool(bool),
}

#[derive(Debug)]
pub struct GrammarBuilder {
    stream: Option<StringStream>,
    grammar: String,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        Self {
            stream: None,
            grammar: String::from("gmrs/parser.lx"),
        }
    }

    pub fn with_file(mut self, file: String) -> Result<Self, Box<dyn error::Error>> {
        self.stream = Some(StringStream::from_file(file)?);
        Ok(self)
    }

    pub fn with_stream(mut self, stream: StringStream) -> Self {
        self.stream = Some(stream);
        self
    }

    pub fn with_grammar(mut self, grammar: String) -> Self {
        self.grammar = grammar;
        self
    }

    fn read_token(&self, lexed_input: &mut LexedStream, id: &str) -> WResult<bool> {
        let mut warnings = WarningSet::empty();
        WOk(
            ctry!(lexed_input.get(), warnings)
                .and_then(|token| if token.name() == id { Some(()) } else { None })
                .is_some(),
            warnings,
        )
    }
    fn read_token_walk(&self, lexed_input: &mut LexedStream, id: &str) -> WResult<bool> {
        let mut warnings = WarningSet::empty();
        if ctry!(self.read_token(lexed_input, id), warnings) {
            lexed_input.pos_pp();
            WOk(true, warnings)
        } else {
            WOk(false, warnings)
        }
    }

    fn generate_error(&self, location: Location, expected: &str, found: &str) -> Error {
        Error::new(
            location,
            ErrorType::GrammarSyntaxError(format!("expected {}, found {}", expected, found)),
        )
    }

    fn match_now<'a>(&self, lexed_input: &'a mut LexedStream, id: &str) -> WResult<Token> {
        let mut warnings = WarningSet::empty();
        match ctry!(lexed_input.get(), warnings) {
            Some(token) => {
                if token.name() == id {
                    let token = token.clone();
                    lexed_input.pos_pp();
                    WOk(token.clone(), warnings)
                } else {
                    WErr(self.generate_error(token.location().clone(), id, token.name()))
                }
            }
            None => WErr(self.generate_error(lexed_input.last_location().clone(), id, "EOF")),
        }
    }

    fn read_proxy_element(&self, lexed_input: &mut LexedStream) -> WResult<(String, Value)> {
        let mut warnings = WarningSet::empty();
        let id = ctry!(self.match_now(lexed_input, "ID"), warnings);
        ctry!(self.match_now(lexed_input, "COLON"), warnings);
        if let Some(token) = ctry!(lexed_input.get(), warnings) {
            let result = WOk(
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
                            return WErr(self.generate_error(
                                token.location().clone(),
                                "INT, STRING, FLOAT, BOOL or ID",
                                x,
                            ))
                        }
                    },
                ),
                warnings,
            );
            lexed_input.pos_pp();
            result
        } else {
            WErr(self.generate_error(
                lexed_input.last_location().clone(),
                "INT, STRING, FLOAT, BOOL or ID",
                "EOF",
            ))
        }
    }

    fn read_proxy(&self, lexed_input: &mut LexedStream) -> WResult<Proxy> {
        let mut warnings = WarningSet::empty();
        self.match_now(lexed_input, "LPROXY");
        let mut proxy = HashMap::new();
        while let Some(token) = ctry!(lexed_input.get(), warnings) {
            match token.name() {
                "ID" => {
                    let (key, value) = ctry!(self.read_proxy_element(lexed_input), warnings);
                    proxy.insert(key, value);
                }
                "RPROXY" => {
                    lexed_input.pos_pp();
                    return WOk(proxy, warnings);
                }
                x => return WErr(self.generate_error(token.location().clone(), "ID or RPROXY", x)),
            }
        }
        WErr(self.generate_error(lexed_input.last_location().clone(), "ID or RPROXY", "EOF"))
    }

    fn read_rule_element_attribute(&self, lexed_input: &mut LexedStream) -> WResult<Attribute> {
        let mut warnings = WarningSet::empty();
        if let Some(token) = ctry!(lexed_input.get(), warnings) {
            if token.name() == "DOT" {
                lexed_input.pos_pp();
                if let Some(token) = ctry!(lexed_input.get(), warnings) {
                    let result = match token.name() {
                        "ID" => WOk(Attribute::Named(token.content().to_string()), warnings),
                        "INT" => WOk(
                            Attribute::Indexed(token.content().parse().unwrap()),
                            warnings,
                        ),
                        x => WErr(self.generate_error(token.location().clone(), "ID or INT", x)),
                    };
                    lexed_input.pos_pp();
                    result
                } else {
                    WErr(self.generate_error(
                        lexed_input.last_location().clone(),
                        "ID or INT",
                        "EOF",
                    ))
                }
            } else {
                WOk(Attribute::None, warnings)
            }
        } else {
            WOk(Attribute::None, warnings)
        }
    }

    fn read_rule_element_key(&self, lexed_input: &mut LexedStream) -> WResult<Option<String>> {
        let mut warnings = WarningSet::empty();
        if let Some(token) = ctry!(lexed_input.get(), warnings) {
            if token.name() == "AT" {
                lexed_input.pos_pp();
                if let Some(token) = ctry!(lexed_input.get(), warnings) {
                    let result = if token.name() == "ID" {
                        WOk(Some(token.content().to_string()), warnings)
                    } else {
                        WErr(self.generate_error(token.location().clone(), "ID", token.name()))
                    };
                    lexed_input.pos_pp();
                    result
                } else {
                    WErr(self.generate_error(lexed_input.last_location().clone(), "ID", "EOF"))
                }
            } else {
                WOk(None, warnings)
            }
        } else {
            WOk(None, warnings)
        }
    }

    fn read_rule_element(
        &self,
        lexed_input: &mut LexedStream,
        lexer: &Lexer,
    ) -> WResult<RuleElement> {
        let mut warnings = WarningSet::empty();
        let id = ctry!(self.match_now(lexed_input, "ID"), warnings);
        let attribute = ctry!(self.read_rule_element_attribute(lexed_input), warnings);
        let key = ctry!(self.read_rule_element_key(lexed_input), warnings);
        let name = id.content();
        WOk(
            RuleElement {
                name: name.to_string(),
                attribute,
                key,
                element_type: if lexer.grammar().contains(name) {
                    ElementType::Terminal
                } else {
                    ElementType::NonTerminal
                },
            },
            warnings,
        )
    }

    fn read_rule(&self, lexed_input: &mut LexedStream, lexer: &Lexer) -> WResult<PartialRule> {
        let mut warnings = WarningSet::empty();
        let mut expected = "ID";
        let mut rule_elements = Vec::new();
        while let Some(token) = ctry!(lexed_input.get(), warnings) {
            if token.name() == "LPROXY" {
                let proxy = ctry!(self.read_proxy(lexed_input), warnings);
                return WOk(
                    PartialRule {
                        elements: rule_elements,
                        proxy,
                    },
                    warnings,
                );
            }
            rule_elements.push(ctry!(self.read_rule_element(lexed_input, lexer), warnings));
        }
        WErr(self.generate_error(lexed_input.last_location().clone(), expected, "EOF"))
    }

    fn read_definition(
        &self,
        lexed_input: &mut LexedStream,
        lexer: &Lexer,
    ) -> WResult<(bool, String, Vec<Rule>)> {
        let mut warnings = WarningSet::empty();
        let axiom = ctry!(self.read_token_walk(lexed_input, "AT"), warnings);
        let name = ctry!(self.match_now(lexed_input, "ID"), warnings);
        ctry!(self.match_now(lexed_input, "ASSIGNMENT"), warnings);
        let name_string = name.content().to_string();
        let mut rules = Vec::new();
        'read_rules: while let Some(token) = ctry!(lexed_input.get(), warnings) {
            if token.name() == "SEMICOLON" {
                lexed_input.pos_pp();
                break 'read_rules;
            }
            let rule = ctry!(self.read_rule(lexed_input, lexer), warnings);
            rules.push(Rule::new(name_string.clone(), rule.elements, rule.proxy));
        }
        WOk((axiom, name_string, rules), warnings)
    }

    pub fn build(mut self, lexer: &Lexer) -> WResult<Grammar> {
        let mut warnings = WarningSet::empty();
        let mut stream = retrieve!(self.stream, warnings);
        let temp_lexer = ctry!(
            ctry!(
                LexerBuilder::new().with_grammar_file({
                    let grammar = self.grammar;
                    self.grammar = String::new();
                    grammar
                }),
                warnings
            )
            .build(),
            warnings
        );
        let mut lexed_input = temp_lexer.lex(&mut stream);

        let mut rules = Vec::new();

        let mut axioms_vec = Vec::new();

        let mut done: HashMap<_, Location> = HashMap::new();
        while let Some(token) = ctry!(lexed_input.get(), warnings) {
            let first_location = token.location().clone();
            let (axiom, name, new_rules) =
                ctry!(self.read_definition(&mut lexed_input, lexer), warnings);
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
            axioms_vec.push((rules.len(), rules.len() + new_rules.len()));
            rules.extend(new_rules);
            ask_case!(name, PascalCase, warnings);
            done.insert(name, location);
        }

        let mut axioms = RuleSet::with_capacity(rules.len());
        for (i, j) in axioms_vec {
            axioms.set_range(i..j, true);
        }

        let grammar = ctry!(Grammar::new(rules, axioms), warnings);

        WOk(grammar, warnings)
    }
}

impl Default for GrammarBuilder {
    fn default() -> Self {
        Self::new()
            .with_file(String::from("gmrs/parser.gmr"))
            .unwrap()
    }
}

/// # Summary
///
/// `Grammar` represents a complete parser grammar.
/// Specifically,
pub struct Grammar {
    axioms: FixedBitSet,
    rules: Vec<Rule>,
    nonterminals: FixedBitSet,
    nullables: FixedBitSet,
}

impl Grammar {
    fn is_rule_nullable(&mut self, i: usize) -> bool {
        false
    }
    #[inline]
    fn compute_nullables(&mut self) -> WResult<()> {
        let mut warnings = WarningSet::empty();
        let mut edited = true;
        while edited {
            edited = false;
            for i in 0..self.rules.len() {
                if self.nullables.contains(i) {
                    continue;
                }
                if self.is_rule_nullable(i) {
                    edited = true;
                    self.nullables.insert(i);
                }
            }
        }
        WOk((), warnings)
    }
    pub fn new(rules: Vec<Rule>, axioms: FixedBitSet) -> WResult<Self> {
        let mut warnings = WarningSet::empty();
        let nonterminals = FixedBitSet::with_capacity(axioms.len());
        let nullables = FixedBitSet::with_capacity(axioms.len());
        let mut grammar = Self {
            nonterminals,
            nullables,
            rules,
            axioms,
        };
        ctry!(grammar.compute_nullables(), warnings);
        WOk(grammar, warnings)
    }
}
