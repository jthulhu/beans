use crate::case::Case;
use crate::error::{
    Error, ErrorType, WResult,
    WResult::{WErr, WOk},
    WarningSet,
};
use crate::lexer::{Lexer, LexerBuilder, Token};
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
    /// `rule!(NonTerminal ::= token.type value@value ... <proxy> ...)`
    /// **Note that this macro is only used to generate test units.**
    macro_rules! rule {
	(@key) => { None };
	(@key $key: ident) => { Some(stringify!($key).to_string()) };
	(@attribute) => { Attribute::None };
	(@attribute str $attribute: ident) => { Attribute::Named(stringify!($attribute).to_string()) };
	(@attribute idx $attribute: literal) => { Attribute::Indexed($attribute) };
	(@element $name: ident $(. $type: ident $attribute: tt)? $(@ $key: tt)?) => {
	    RuleElement {
		name: stringify!($name).to_string(),
		attribute: rule!(@attribute $($type $attribute)?),
		key: rule!(@key $($key)?)
	    }
	};
	(
	    $name: ident ::= $(
		$(
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
			elements.push(rule!(@element $element $(. $attribute_type $attribute)? $(@ $key)?));
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
		)*;
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
    fn grammar_builder() {
        assert!(GrammarBuilder::new().build().is_err());

        let grammar = GrammarBuilder::default().build().unwrap();
        verify(
	    grammar.rules,
            collect!(
                rule!(
                    IfStatement ::=
			IF Expression@condition LBRACE StatementList@then RBRACE <haselse = bool false>
			IF Expression@condition LBRACE StatementList@then RBRACE ELSE LBRACE StatementList@else RBRACE <haselse = bool true>
			
                ),
                rule!(
                    WhileStatement ::=
                        WHILE Expression@condition LBRACE StatementList@do RBRACE <>
                    ),
                rule!(
                    Assignment ::=
			ID.idx 0@key EQUALS Expression@value <>
                ),
                rule!(
                    BuiltinType ::=
			INT.idx 0@value <type = str "int" op = str "builtin">
			STRING.idx 0@value <type = str "string" op = str "builtin">
			ID.idx 0@value <type = str "id" op = str "builtin">
			TRUE <type = str "true" op = str "builtin">
			FALSE <type = str "false" op = str "builtin">
                ),
                rule!(
                    Atom ::=
			BuiltinType@this <>
			LPAR Expression@this RPAR <>
                ),
		rule!(
		    Expression ::=
			Expression@left PLUS Expression@right <op = str "add">
			Expression@left ASTERISK Expression@right <op = str "mul">
			Atom@this <>
		),
		rule!(
		    Statement ::=
			Assignment@this SEMICOLON <s = str "assign">
			IfStatement@this <s = str "if">
			WhileStatement@this <s = str "while">
		),
		rule!(
		    StatementList ::=
			StatementList@left Statement@right <s = str "concatenate">
			Statement@this <>
		)
            )
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
pub struct RuleElement {
    pub name: String,
    pub attribute: Attribute,
    pub key: Option<Key>,
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

    fn read_token(&self, lexer: &Lexer, id: &str) -> bool {
        lexer
            .get()
            .and_then(|(tok, _)| if tok.name() == id { Some(()) } else { None })
            .is_some()
    }
    fn read_token_walk(&self, lexer: &mut Lexer, id: &str) -> bool {
        if self.read_token(lexer, id) {
            lexer.pos_pp();
            true
        } else {
            false
        }
    }

    fn generate_error(&self, location: Location, expected: &str, found: &str) -> Error {
        Error::new(
            location,
            ErrorType::GrammarSyntaxError(format!("expected {}, found {}", expected, found)),
        )
    }

    fn match_now<'a>(&self, lexer: &'a mut Lexer, id: &str) -> WResult<StreamObject<Token>> {
        let mut warnings = WarningSet::empty();
        match lexer.get() {
            Some((token, location)) => {
                if token.name() == id {
                    let token = token.clone();
                    lexer.pos_pp();
                    WOk((token.clone(), location), warnings)
                } else {
                    WErr(self.generate_error(location, id, token.name()))
                }
            }
            None => WErr(self.generate_error(lexer.last_location().clone(), id, "EOF")),
        }
    }

    fn read_proxy_element(&self, lexer: &mut Lexer) -> WResult<(String, Value)> {
        let mut warnings = WarningSet::empty();
        let (id, _) = ctry!(self.match_now(lexer, "ID"), warnings);
        ctry!(self.match_now(lexer, "COLON"), warnings);
        if let Some((token, location)) = lexer.get() {
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
                                        location,
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
                                        location,
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
                                        location,
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
                                location,
                                "INT, STRING, FLOAT, BOOL or ID",
                                x,
                            ))
                        }
                    },
                ),
                warnings,
            );
            lexer.pos_pp();
            result
        } else {
            WErr(self.generate_error(
                lexer.last_location().clone(),
                "INT, STRING, FLOAT, BOOL or ID",
                "EOF",
            ))
        }
    }

    fn read_proxy(&self, lexer: &mut Lexer) -> WResult<Proxy> {
        let mut warnings = WarningSet::empty();
        self.match_now(lexer, "LPROXY");
        let mut proxy = HashMap::new();
        while let Some((token, location)) = lexer.get() {
            match token.name() {
                "ID" => {
                    let (key, value) = ctry!(self.read_proxy_element(lexer), warnings);
                    proxy.insert(key, value);
                }
                "RPROXY" => {
                    lexer.pos_pp();
                    return WOk(proxy, warnings);
                }
                x => return WErr(self.generate_error(location, "ID or RPROXY", x)),
            }
        }
        WErr(self.generate_error(lexer.last_location().clone(), "ID or RPROXY", "EOF"))
    }

    fn read_rule_element_attribute(&self, lexer: &mut Lexer) -> WResult<Attribute> {
        let mut warnings = WarningSet::empty();
        if let Some((token, _)) = lexer.get() {
            if token.name() == "DOT" {
                lexer.pos_pp();
                if let Some((token, location)) = lexer.get() {
                    let result = match token.name() {
                        "ID" => WOk(Attribute::Named(token.content().to_string()), warnings),
                        "INT" => WOk(
                            Attribute::Indexed(token.content().parse().unwrap()),
                            warnings,
                        ),
                        x => WErr(self.generate_error(location, "ID or INT", x)),
                    };
                    lexer.pos_pp();
                    result
                } else {
                    WErr(self.generate_error(lexer.last_location().clone(), "ID or INT", "EOF"))
                }
            } else {
                WOk(Attribute::None, warnings)
            }
        } else {
            WOk(Attribute::None, warnings)
        }
    }

    fn read_rule_element_key(&self, lexer: &mut Lexer) -> WResult<Option<String>> {
        let mut warnings = WarningSet::empty();
        if let Some((token, _)) = lexer.get() {
            if token.name() == "AT" {
                lexer.pos_pp();
                if let Some((token, location)) = lexer.get() {
                    let result = if token.name() == "ID" {
                        WOk(Some(token.content().to_string()), warnings)
                    } else {
                        WErr(self.generate_error(location, "ID", token.name()))
                    };
                    lexer.pos_pp();
                    result
                } else {
                    WErr(self.generate_error(lexer.last_location().clone(), "ID", "EOF"))
                }
            } else {
                WOk(None, warnings)
            }
        } else {
            WOk(None, warnings)
        }
    }

    fn read_rule_element(&self, lexer: &mut Lexer) -> WResult<RuleElement> {
        let mut warnings = WarningSet::empty();
        let (id, _) = ctry!(self.match_now(lexer, "ID"), warnings);
        let attribute = ctry!(self.read_rule_element_attribute(lexer), warnings);
        let key = ctry!(self.read_rule_element_key(lexer), warnings);
        let name = id.content().to_string();
        WOk(
            RuleElement {
                name,
                attribute,
                key,
            },
            warnings,
        )
    }

    fn read_rule(&self, lexer: &mut Lexer) -> WResult<PartialRule> {
        let mut warnings = WarningSet::empty();
        let mut expected = "ID";
        let mut rule_elements = Vec::new();
        while let Some((token, position)) = lexer.get() {
            if token.name() == "LPROXY" {
                let proxy = ctry!(self.read_proxy(lexer), warnings);
                return WOk(
                    PartialRule {
                        elements: rule_elements,
                        proxy,
                    },
                    warnings,
                );
            }
            rule_elements.push(ctry!(self.read_rule_element(lexer), warnings));
        }
        WErr(self.generate_error(lexer.last_location().clone(), expected, "EOF"))
    }

    fn read_definition(&self, lexer: &mut Lexer) -> WResult<(bool, String, Vec<Rule>)> {
        let mut warnings = WarningSet::empty();
        let axiom = self.read_token_walk(lexer, "AT");
        let (name, mut location) = ctry!(self.match_now(lexer, "ID"), warnings);
        ctry!(self.match_now(lexer, "ASSIGNMENT"), warnings);
        let name_string = name.content().to_string();
        let mut rules = Vec::new();
        'read_rules: while let Some((token, _)) = lexer.get() {
            if token.name() == "SEMICOLON" {
                lexer.pos_pp();
                break 'read_rules;
            }
            let rule = ctry!(self.read_rule(lexer), warnings);
            rules.push(Rule::new(name_string.clone(), rule.elements, rule.proxy));
        }
        WOk((axiom, name_string, rules), warnings)
    }

    pub fn build(mut self) -> WResult<Grammar> {
        let mut warnings = WarningSet::empty();
        let mut stream = retrieve!(self.stream, warnings);
        let mut lexer = ctry!(
            ctry!(
                LexerBuilder::new().with_stream(stream).with_grammar_file({
                    let grammar = self.grammar;
                    self.grammar = String::new();
                    grammar
                }),
                warnings
            )
            .build(),
            warnings
        );

        let mut rules = Vec::new();

        let mut axioms_vec = Vec::new();

        let mut done: HashMap<_, Location> = HashMap::new();
        while let Some((_, first_position)) = lexer.get() {
            let (axiom, name, new_rules) = ctry!(self.read_definition(&mut lexer), warnings);
            let last_position = lexer.get_loc_of(lexer.pos() - 1).unwrap();
            let location = Location::extend(first_position, last_position);
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

	let grammar = ctry!(Grammar::new(rules, axioms, lexer), warnings);
	
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
/// `Grammar` represents a complete parser grammar, including a lexer.
/// Specifically, 
pub struct Grammar {
    lexer: Lexer,
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
		if self.nullables.contains(i) { continue; }
		if self.is_rule_nullable(i) {
		    edited = true;
		    self.nullables.insert(i);
		}
	    }
	}
	WOk((), warnings)
    }
    pub fn new(rules: Vec<Rule>, axioms: FixedBitSet, lexer: Lexer) -> WResult<Self> {
	let mut warnings = WarningSet::empty();
	let nonterminals = FixedBitSet::with_capacity(axioms.len());
	let nullables = FixedBitSet::with_capacity(axioms.len());
	let mut grammar = Self {
	    nonterminals,
	    nullables,
	    lexer,
	    rules,
	    axioms,
        };
	ctry!(grammar.compute_nullables(), warnings);
        WOk(
	    grammar,
	    warnings
	)
    }
}
