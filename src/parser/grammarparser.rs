use crate::error::{Error, ErrorType};
use crate::lexer::{Lexer, LexerBuilder, Token};
use crate::location::Location;
use crate::retrieve;
use crate::stream::{Stream, StreamObject, StringStream};
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use std::error;
use std::fs::File;
use std::io::prelude::*;

#[cfg(test)]
mod tests {
    use super::*;
    // #[test]
    fn grammar_builder() {
        assert!(GrammarBuilder::new().build().is_err());

        GrammarBuilder::default().build().unwrap();
    }
}

pub type RuleSet = FixedBitSet;
pub type Key = String;
pub type RuleElement = (String, Attribute, Option<Key>);
pub type Proxy = HashMap<String, Value>;
pub type Rule = (Vec<RuleElement>, Proxy);

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Str(String),
    Id(String),
    Float(f32),
    Bool(bool),
}

#[derive(Debug)]
pub enum Attribute {
    Named(String),
    Indexed(usize),
    None,
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

    fn read_token(&self, lexer: &Lexer, id: &str) -> Result<(), ()> {
        lexer
            .get()
            .and_then(|(tok, _)| if tok.name() == id { Some(()) } else { None })
            .ok_or(())
    }

    fn generate_error(&self, location: Location, expected: &str, found: &str) -> Error {
        (
            location,
            ErrorType::GrammarSyntaxError(format!("expected {}, found {}", expected, found)),
        )
            .into()
    }

    fn match_now<'a>(&self, lexer: &'a mut Lexer, id: &str) -> Result<StreamObject<Token>, Error> {
        match lexer.get() {
            Some((token, location)) => {
                if token.name() == id {
                    let token = token.clone();
                    lexer.pos_pp();
                    Ok((token.clone(), location))
                } else {
                    Err(self.generate_error(location, id, token.name()))
                }
            }
            None => Err(self.generate_error(lexer.last_location().clone(), id, "EOF")),
        }
    }

    fn read_proxy_element(&self, lexer: &mut Lexer) -> Result<(String, Value), Error> {
        let (id, _) = self.match_now(lexer, "ID")?;
        self.match_now(lexer, "COLON")?;
        if let Some((token, location)) = lexer.get() {
            let result = Ok((
                id.content().to_string(),
                match token.name() {
                    "INT" => Value::Int(token.content().parse().map_err(|_| {
                        Error::from((
                            location,
                            ErrorType::GrammarSyntaxError(format!(
                                "cannot understand {} as an integer",
                                token.content()
                            )),
                        ))
                    })?),
                    "STRING" => Value::Str(token.content().to_string()),
                    "FLOAT" => Value::Float(token.content().parse().map_err(|_| {
                        Error::from((
                            location,
                            ErrorType::GrammarSyntaxError(format!(
                                "cannot understand {} as a float",
                                token.content()
                            )),
                        ))
                    })?),
                    "BOOL" => Value::Bool(token.content().parse().map_err(|_| {
                        Error::from((
                            location,
                            ErrorType::GrammarSyntaxError(format!(
                                "cannot understand {} as a bool",
                                token.content()
                            )),
                        ))
                    })?),
                    "ID" => Value::Id(token.content().to_string()),
                    x => {
                        return Err(self.generate_error(
                            location,
                            "INT, STRING, FLOAT, BOOL or ID",
                            x,
                        ))
                    }
                },
            ));
            lexer.pos_pp();
            result
        } else {
            Err(self.generate_error(
                lexer.last_location().clone(),
                "INT, STRING, FLOAT, BOOL or ID",
                "EOF",
            ))
        }
    }

    fn read_proxy(&self, lexer: &mut Lexer) -> Result<Proxy, Error> {
        self.match_now(lexer, "LPROXY");
        let mut proxy = HashMap::new();
        while let Some((token, location)) = lexer.get() {
            match token.name() {
                "ID" => {
                    let (key, value) = self.read_proxy_element(lexer)?;
                    proxy.insert(key, value);
                }
                "RPROXY" => {
                    lexer.pos_pp();
                    return Ok(proxy);
                }
                x => return Err(self.generate_error(location, "ID or RPROXY", x)),
            }
        }
        Err(self.generate_error(lexer.last_location().clone(), "ID or RPROXY", "EOF"))
    }

    fn read_rule_element_attribute(&self, lexer: &mut Lexer) -> Result<Attribute, Error> {
        if let Some((token, _)) = lexer.get() {
            if token.name() == "DOT" {
                lexer.pos_pp();
                if let Some((token, location)) = lexer.get() {
                    let result = match token.name() {
                        "ID" => Ok(Attribute::Named(token.content().to_string())),
                        "INT" => Ok(Attribute::Indexed(token.content().parse().unwrap())),
                        x => Err(self.generate_error(location, "ID or INT", x)),
                    };
                    lexer.pos_pp();
                    result
                } else {
                    Err(self.generate_error(lexer.last_location().clone(), "ID or INT", "EOF"))
                }
            } else {
                Ok(Attribute::None)
            }
        } else {
            Ok(Attribute::None)
        }
    }

    fn read_rule_element_key(&self, lexer: &mut Lexer) -> Result<Option<String>, Error> {
        if let Some((token, _)) = lexer.get() {
            if token.name() == "AT" {
                lexer.pos_pp();
                if let Some((token, location)) = lexer.get() {
                    let result = if token.name() == "ID" {
                        Ok(Some(token.content().to_string()))
                    } else {
                        Err(self.generate_error(location, "ID", token.name()))
                    };
                    lexer.pos_pp();
                    result
                } else {
                    Err(self.generate_error(lexer.last_location().clone(), "ID", "EOF"))
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn read_rule_element(&self, lexer: &mut Lexer) -> Result<RuleElement, Error> {
        let (id, _) = self.match_now(lexer, "ID")?;
        let attribute = self.read_rule_element_attribute(lexer)?;
        let key = self.read_rule_element_key(lexer)?;
        println!("{:#?}", id);
        Ok((id.content().to_string(), attribute, key))
    }

    fn read_rule(&self, lexer: &mut Lexer) -> Result<Rule, Error> {
        let mut expected = "ID";
        let mut rule_elements = Vec::new();
        while let Some((token, position)) = lexer.get() {
            if token.name() == "LPROXY" {
                let proxy = self.read_proxy(lexer)?;
                return Ok((rule_elements, proxy));
            }
            rule_elements.push(self.read_rule_element(lexer)?);
        }
        Err(self.generate_error(lexer.last_location().clone(), expected, "EOF"))
    }

    fn read_definition(&self, lexer: &mut Lexer) -> Result<(String, Vec<Rule>), Error> {
        let (name, mut location) = self.match_now(lexer, "ID")?;
        self.match_now(lexer, "ASSIGNMENT")?;
        let mut rules = Vec::new();
        'read_rules: while let Some((token, _)) = lexer.get() {
            if token.name() == "SEMICOLON" {
                lexer.pos_pp();
                break 'read_rules;
            }
            let rule = self.read_rule(lexer)?;
            rules.push(rule);
        }
        Ok((name.content().to_string(), rules))
    }

    pub fn build(mut self) -> Result<Grammar, Error> {
        let mut stream = retrieve!(self.stream);
        let mut lexer = LexerBuilder::new()
            .with_stream(stream)
            .with_grammar_file({
                let grammar = self.grammar;
                self.grammar = String::new();
                grammar
            })?
            .build()?;

        let mut done: HashMap<_, Location> = HashMap::new();
        while let Some((_, first_position)) = lexer.get() {
            let (name, rules) = self.read_definition(&mut lexer)?;
            let last_position = lexer.get_loc_of(lexer.pos() - 1).unwrap();
            let location = Location::extend(first_position, last_position);
            if let Some(old_location) = done.get(&name) {
                return Err(Error::from((
                    location,
                    ErrorType::GrammarDuplicateDefinition(name, old_location.clone()),
                )));
            }
            done.insert(name, location);
        }

        Ok(Grammar::new(Vec::new(), RuleSet::with_capacity(0), lexer))
    }
}

impl Default for GrammarBuilder {
    fn default() -> Self {
        Self::new()
            .with_file(String::from("gmrs/parser.gmr"))
            .unwrap()
    }
}

pub struct Grammar {
    lexer: Lexer,
    axioms: RuleSet,
    rules: Vec<Rule>,
    nonterminals: RuleSet,
    nullable: RuleSet,
}

impl Grammar {
    pub fn new(rules: Vec<Rule>, axioms: RuleSet, lexer: Lexer) -> Self {
        Self {
            nonterminals: RuleSet::with_capacity(axioms.len()),
            nullable: RuleSet::with_capacity(axioms.len()),
            lexer,
            rules,
            axioms,
        }
    }
}
