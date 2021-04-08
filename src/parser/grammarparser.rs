use crate::case::Case;
use crate::error::{
    Error, ErrorType, WResult,
    WResult::{WErr, WOk},
    WarningSet,
    Warning, WarningType
};
use crate::lexer::{LexedStream, Lexer, LexerBuilder, Token};
use crate::location::Location;
use crate::stream::{Stream, StreamObject, StringStream};
use crate::{ask_case, ctry, retrieve, collect, rule};
// use crate::{rule, proxy, collect};
use super::earley;
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use serde::{Serialize, Deserialize};
use std::error;
use std::fs::File;
use std::io::prelude::*;

#[cfg(test)]
mod tests {
    use super::*;
}

pub type RuleSet = FixedBitSet;
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
    pub name: String,
    pub attribute: Attribute,
    pub key: Option<Key>,
    pub element_type: ElementType,
}

impl RuleElement {
    pub fn is_terminal(&self) -> bool {
	match self.element_type {
	    ElementType::Terminal(..) => true,
	    ElementType::NonTerminal(..) => false
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
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i32),
    Str(String),
    Id(String),
    Float(f32),
    Bool(bool),
}

pub trait GrammarBuilder<'a>: Sized {
    type Grammar: Grammar<'a>;
    fn with_file(mut self, file: String) -> Result<Self, Box<dyn error::Error>> {
	Ok(self.with_stream(StringStream::from_file(file)?))
    }
    fn with_stream(self, stream: StringStream) -> Self;
    fn with_grammar(self, grammar: String) -> Self;
    fn stream(&mut self) -> WResult<StringStream>;
    fn grammar(&mut self) -> String;
    fn build(mut self, lexer: &Lexer) -> WResult<Self::Grammar> {
	fn read_token(lexed_input: &mut LexedStream, id: &str) -> WResult<bool> {
            let mut warnings = WarningSet::empty();
            WOk(
		ctry!(lexed_input.get(), warnings)
                    .and_then(|token| if token.name() == id { Some(()) } else { None })
                    .is_some(),
		warnings,
            )
	}
	fn read_token_walk(lexed_input: &mut LexedStream, id: &str) -> WResult<bool> {
            let mut warnings = WarningSet::empty();
            if ctry!(read_token(lexed_input, id), warnings) {
		lexed_input.pos_pp();
		WOk(true, warnings)
            } else {
		WOk(false, warnings)
            }
	}

	fn generate_error(location: Location, expected: &str, found: &str) -> Error {
            Error::new(
		location,
		ErrorType::GrammarSyntaxError(format!("expected {}, found {}", expected, found)),
            )
	}

	fn match_now<'a>(lexed_input: &'a mut LexedStream, id: &str) -> WResult<Token> {
            let mut warnings = WarningSet::empty();
            match ctry!(lexed_input.get(), warnings) {
		Some(token) => {
                    if token.name() == id {
			let token = token.clone();
			lexed_input.pos_pp();
			WOk(token.clone(), warnings)
                    } else {
			WErr(generate_error(token.location().clone(), id, token.name()))
                    }
		}
		None => WErr(generate_error(lexed_input.last_location().clone(), id, "EOF")),
            }
	}

	fn read_proxy_element(lexed_input: &mut LexedStream) -> WResult<(String, Value)> {
            let mut warnings = WarningSet::empty();
            let id = ctry!(match_now(lexed_input, "ID"), warnings);
            ctry!(match_now(lexed_input, "COLON"), warnings);
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
				return WErr(generate_error(
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
		WErr(generate_error(
                    lexed_input.last_location().clone(),
                    "INT, STRING, FLOAT, BOOL or ID",
                    "EOF",
		))
            }
	}

	fn read_proxy(lexed_input: &mut LexedStream) -> WResult<Proxy> {
            let mut warnings = WarningSet::empty();
            match_now(lexed_input, "LPROXY");
            let mut proxy = HashMap::new();
            while let Some(token) = ctry!(lexed_input.get(), warnings) {
		match token.name() {
                    "ID" => {
			let (key, value) = ctry!(read_proxy_element(lexed_input), warnings);
			proxy.insert(key, value);
                    }
                    "RPROXY" => {
			lexed_input.pos_pp();
			return WOk(proxy, warnings);
                    }
                    x => return WErr(generate_error(token.location().clone(), "ID or RPROXY", x)),
		}
            }
            WErr(generate_error(lexed_input.last_location().clone(), "ID or RPROXY", "EOF"))
	}

	fn read_rule_element_attribute(lexed_input: &mut LexedStream) -> WResult<Attribute> {
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
                            x => WErr(generate_error(token.location().clone(), "ID or INT", x)),
			};
			lexed_input.pos_pp();
			result
                    } else {
			WErr(generate_error(
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

	fn read_rule_element_key(lexed_input: &mut LexedStream) -> WResult<Option<String>> {
            let mut warnings = WarningSet::empty();
            if let Some(token) = ctry!(lexed_input.get(), warnings) {
		if token.name() == "AT" {
                    lexed_input.pos_pp();
                    if let Some(token) = ctry!(lexed_input.get(), warnings) {
			let result = if token.name() == "ID" {
                            WOk(Some(token.content().to_string()), warnings)
			} else {
                            WErr(generate_error(token.location().clone(), "ID", token.name()))
			};
			lexed_input.pos_pp();
			result
                    } else {
			WErr(generate_error(lexed_input.last_location().clone(), "ID", "EOF"))
                    }
		} else {
                    WOk(None, warnings)
		}
            } else {
		WOk(None, warnings)
            }
	}

	fn read_rule_element(
            lexed_input: &mut LexedStream,
            lexer: &Lexer,
	) -> WResult<RuleElement> {
            let mut warnings = WarningSet::empty();
            let id = ctry!(match_now(lexed_input, "ID"), warnings);
            let attribute = ctry!(read_rule_element_attribute(lexed_input), warnings);
            let key = ctry!(read_rule_element_key(lexed_input), warnings);
            let name = id.content();
            WOk(
		RuleElement {
                    name: name.to_string(),
                    attribute,
                    key,
                    element_type: if let Some(id) = lexer.grammar().id(name) {
			ElementType::Terminal(id)
                    } else {
			ElementType::NonTerminal(None)
                    },
		},
		warnings,
            )
	}

	fn read_rule(lexed_input: &mut LexedStream, lexer: &Lexer) -> WResult<PartialRule> {
            let mut warnings = WarningSet::empty();
            let mut expected = "ID";
            let mut rule_elements = Vec::new();
            while let Some(token) = ctry!(lexed_input.get(), warnings) {
		if token.name() == "LPROXY" {
                    let proxy = ctry!(read_proxy(lexed_input), warnings);
                    return WOk(
			PartialRule {
                            elements: rule_elements,
                            proxy,
			},
			warnings,
                    );
		}
		rule_elements.push(ctry!(read_rule_element(lexed_input, lexer), warnings));
            }
            WErr(generate_error(lexed_input.last_location().clone(), expected, "EOF"))
	}

	fn read_definition(
            lexed_input: &mut LexedStream,
            lexer: &Lexer,
	) -> WResult<(bool, String, Vec<Rule>)> {
            let mut warnings = WarningSet::empty();
            let axiom = ctry!(read_token_walk(lexed_input, "AT"), warnings);
            let name = ctry!(match_now(lexed_input, "ID"), warnings);
            ctry!(match_now(lexed_input, "ASSIGNMENT"), warnings);
            let name_string = name.content().to_string();
            let mut rules = Vec::new();
            'read_rules: while let Some(token) = ctry!(lexed_input.get(), warnings) {
		if token.name() == "SEMICOLON" {
                    lexed_input.pos_pp();
                    break 'read_rules;
		}
		let rule = ctry!(read_rule(lexed_input, lexer), warnings);
		rules.push(Rule::new(name_string.clone(), rule.elements, rule.proxy));
            }
            WOk((axiom, name_string, rules), warnings)
	}
	
	let mut warnings = WarningSet::empty();
        let mut stream = ctry!(self.stream(), warnings);
        let temp_lexer = ctry!(
            ctry!(
                LexerBuilder::new().with_grammar_file(self.grammar()),
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
                ctry!(read_definition(&mut lexed_input, lexer), warnings);
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

	let mut name_map = HashMap::new();

	for (i, rule) in rules.iter().enumerate() {
	    name_map.insert(rule.name.clone(), i);
	}

	for rule in rules.iter_mut() {
	    for element in rule.elements.iter_mut() {
		if element.is_terminal() { continue; }
		match name_map.get(&element.name) {
		    Some(&id) => element.element_type = ElementType::NonTerminal(Some(id)),
		    None => warnings.add(Warning::new(WarningType::UndefinedNonTerminal(rule.name.clone(), element.name.clone())))
		}
	    }
	}
	
        let grammar = ctry!(Self::Grammar::new(rules, axioms, name_map), warnings);

        WOk(grammar, warnings)
    }
}

pub trait Grammar<'a>: Sized + Serialize + Deserialize<'a> {
    fn new(rules: Vec<Rule>, axioms: FixedBitSet, name_map: HashMap<String, usize>) -> WResult<Self>;
    fn from(bytes: &'a [u8]) -> WResult<Self> {
	bincode::deserialize::<'_, Self>(bytes)
	    .map_err(
		|x|
		Error::new(
		    Location::new(
			file!().to_string(),
			(line!() as usize, column!() as usize),
			(line!() as usize, column!() as usize)
		    ),
		    ErrorType::DeserializationError(format!("{}", x))
		)
	    )
	    .into()
    }
    fn serialize(&self) -> WResult<Vec<u8>> {
	bincode::serialize(self)
	    .map_err(
		|x|
		Error::new(
		    Location::new(
			file!().to_string(),
			(line!() as usize, column!() as usize),
			(line!() as usize, column!() as usize)
		    ),
		    ErrorType::SerializationError(format!("{}", x))
		)
	    )
	    .into()
    }
}
