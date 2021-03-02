use crate::stream::StringStream;
use crate::location::Location;
use crate::error::{ErrorType, Error};
use fixedbitset::FixedBitSet;
use std::error;
use std::fs::File;
use std::io::prelude::*;

type RuleSet = FixedBitSet;
type Key = String;
type RuleElement = (String, Attribute, Key);
type Rule = (String, Vec<RuleElement>);

#[derive(Debug)]
pub enum Attribute {
    Named(String),
    Indexed(usize),
    None
}

#[derive(Debug)]
pub struct GrammarBuilder {
    stream: Option<StringStream>
}

impl GrammarBuilder {
    pub fn new() -> Self {
	Self {
	    stream: None
	}
    }

    pub fn with_file(mut self, file: String) -> Result<Self, Box<dyn error::Error>> {
	let mut file_stream = File::open(file.as_str())?;
	let mut stream_buffer = String::new();
	file_stream.read_to_string(&mut stream_buffer)?;
	let stream = StringStream::new(file, stream_buffer);
	self.stream = Some(stream);
	Ok(self)
    }

    pub fn with_stream(mut self, stream: StringStream) -> Self {
	self.stream = Some(stream);
	self
    }

    pub fn build(mut self) -> Result<Grammar, Error> {
	let stream = self
	    .stream
	    .ok_or(
		(
		    Location::new(String::from("<Beans source code>"), (0, 0), (0, 0)),
		    ErrorType::InternalError(String::new())
		)
	    )?;

    }
}

pub struct Grammar {
    axioms: RuleSet,
    rules: Vec<Rule>,
    nonterminals: RuleSet,
    nullable: RuleSet
}

impl Grammar {
    pub fn new(rules: Vec<Rule>, axioms: RuleSet) -> Self {
	Self {
	    rules,
	    axioms,
	    nonterminals: RuleSet::new(),
	    nonterminals: RuleSet::new()
	}
    }
}
