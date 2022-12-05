use crate::error::{Error, Result, WarningSet};
use crate::lexer::TerminalId;
use crate::regex::{CompiledRegex, RegexBuilder, RegexError};
use crate::span::Span;
use crate::stream::{Char, StringStream};
use bincode::deserialize;
use fragile::Fragile;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_parser_read_keyword() {
        let mut stream = StringStream::new(Path::new("whatever"), "ignore");
        assert_eq!(stream.pos(), 0);
        assert!(!LexerGrammarBuilder::read_keyword(&mut stream, "something"));
        assert_eq!(stream.pos(), 0);
        assert!(LexerGrammarBuilder::read_keyword(&mut stream, "ignore"));
        assert_eq!(stream.pos(), 6);
    }

    #[test]
    fn grammar_parser_read_id() {
        let origin = Path::new("whatever");
        let mut stream = StringStream::new(origin, "to del");
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            (
                String::from("to"),
                Span::new(
                    origin,
                    (0, 0),
                    (0, 1),
                    0,
                    1,
                    stream.text(),
                    stream.lines()
                )
            ),
            LexerGrammarBuilder::read_id(&mut stream).unwrap().unwrap()
        );
        assert_eq!(stream.pos(), 2);
        LexerGrammarBuilder::ignore_blank(&mut stream);
        assert_eq!(stream.pos(), 3);
        assert_eq!(
            (
                String::from("del"),
                Span::new(
                    origin,
                    (0, 3),
                    (0, 5),
                    3,
                    5,
                    stream.text(),
                    stream.lines()
                )
            ),
            LexerGrammarBuilder::read_id(&mut stream).unwrap().unwrap()
        );
        assert_eq!(stream.pos(), 6);
        let error = LexerGrammarBuilder::read_id(&mut stream)
            .map(|x| x.unwrap())
            .unwrap_err();
        let Error::LexerGrammarSyntax { location, message } = error else {
            panic!("Expected a lexer grammar syntax error, got {:?}", error);
        };
        assert_eq!(
            Span::new(
                Path::new("whatever"),
                (0, 6),
                (0, 6),
                6,
                6,
                stream.text(),
                stream.lines(),
            ),
            location.into_inner()
        );
        assert_eq!(message, String::from("expected id"));
    }
    #[test]
    fn grammar_parser_regex() {
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("whatever"),
                "A ::= wot!"
            ))
            .build()
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!", String::from("A"), false)
                .unwrap()
                .build(),
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("whatever"),
                "B ::= wot!  "
            ))
            .build()
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!  ", String::from("B"), false)
                .unwrap()
                .build()
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("whatever"),
                "A ::= wot!\n\nB ::= wheel"
            ))
            .build()
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!", String::from("A"), false)
                .unwrap()
                .with_named_regex("wheel", String::from("B"), false)
                .unwrap()
                .build()
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new(
                Path::new("whatever"),
                ""
            ))
            .build()
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new().build()
        );
    }
    #[test]
    fn lexer_grammar() {
        let grammar = LexerGrammarBuilder::from_stream(StringStream::new(
            Path::new("whatever"),
            "ignore A ::= [ ]\nignore B ::= bbb\nC ::= ccc",
        ))
        .build()
        .unwrap()
        .unwrap();
        assert_eq!(grammar.name(TerminalId(0)), "A");
        assert!(grammar.ignored(0.into()));
        assert_eq!(grammar.name(TerminalId(1)), "B");
        assert!(grammar.ignored(1.into()));
        assert_eq!(grammar.name(TerminalId(2)), "C");
        assert!(!grammar.ignored(2.into()));
    }

    #[test]
    fn grammar_report() {
        let grammar = LexerGrammarBuilder::from_stream(StringStream::new(
            Path::new("<grammar report>"),
            r#"ignore COMMENT ::= /\*([^*]|\*[^/])\*/
(unclosed comment) unwanted ECOMMENT ::= /\*([^*]|\*[^/])"#,
        ))
        .build()
        .unwrap()
        .unwrap();
        assert_eq!(1, grammar.errors.len());
        assert_eq!(
            "unclosed comment",
            &**grammar.errors.get(&TerminalId(1)).unwrap()
        );
    }
}

newty! {
    pub id TokenId
}

newty! {
    pub set Ignores [TerminalId]
}

newty! {
    #[derive(Serialize, Deserialize)]
    pub map Errors(Rc<str>) [TerminalId]
}

newty! {
    #[derive(Serialize, Deserialize)]
    pub map Descriptions(Rc<str>) [TerminalId]
}

/// # Summary
///
/// A builder for a `LexerGrammar` object.
///
/// # Attribute specificators
///
/// `with_file`: specify the grammar's file.
///            May fail, if it can't open the given file.
/// `with_stream`: specify a given stream.
#[derive(Debug)]
pub struct LexerGrammarBuilder {
    stream: StringStream,
}

impl LexerGrammarBuilder {
    pub fn from_file(file: impl Into<Rc<Path>>) -> Result<Self> {
        let mut warnings = WarningSet::empty();
        let stream = StringStream::from_file(file)?.unpack_into(&mut warnings);
        warnings.with_ok(Self { stream })
    }

    pub fn from_stream(stream: StringStream) -> Self {
        Self { stream }
    }

    pub fn build(self) -> Result<LexerGrammar> {
        let mut warnings = WarningSet::empty();
        let mut stream = self.stream;
        let mut ignores = HashSet::new();
        let mut errors = Errors::new();
        let mut descriptions = Descriptions::new();
        let mut names = Vec::new();
        let mut regex_builder = RegexBuilder::new();
        let mut found_identifiers = HashSet::new();
        Self::ignore_blank_lines(&mut stream);
        while !stream.is_empty() {
            let description: Option<Rc<str>> =
                if Self::read_keyword(&mut stream, "(") {
                    let mut message = String::new();
                    let mut escaped = false;
                    loop {
                        let c = stream.get();
                        match c {
                            Char::Char(')') if !escaped => break,
                            Char::Char('\\') if !escaped => escaped = true,
                            Char::Char(c) => {
                                message.push(c);
                                escaped = false;
                            }
                            Char::EOF => {
                                return Err(Error::LexerGrammarEofString)
                            }
                        }
                        stream.incr_pos();
                    }
                    Self::read_keyword(&mut stream, ")");
		    Self::ignore_blank_lines(&mut stream);
                    Some(message.into())
                } else {
                    None
                };
            Self::ignore_blank(&mut stream);
            let ignore = Self::read_keyword(&mut stream, "ignore");
            Self::ignore_blank(&mut stream);
            let error = Self::read_keyword(&mut stream, "unwanted");
            Self::ignore_blank(&mut stream);
            let keyword = Self::read_keyword(&mut stream, "keyword");
            Self::ignore_blank(&mut stream);
            let (name, span) =
                Self::read_id(&mut stream)?.unpack_into(&mut warnings);
            if !found_identifiers.insert(name.clone()) {
                return Err(Error::LexerGrammarDuplicateDefinition {
                    token: name,
                    location: Fragile::new(span),
                });
            }
            Self::ignore_blank(&mut stream);
            Self::ignore_assignment(&mut stream)?.unpack_into(&mut warnings);
            Self::ignore_blank(&mut stream);
            let start_span = stream.curr_span();
            let pattern = Self::read_pattern(&mut stream);
            regex_builder = regex_builder
                .with_named_regex(pattern.as_str(), name.clone(), keyword)
                .map_err(
                    |RegexError {
                         message,
                         position: _position,
                     }| {
                        Error::RegexError {
                            location: Fragile::new(start_span),
                            message,
                        }
                    },
                )?;
            if ignore || error {
                ignores.insert(name.clone());
            }
            if error {
                if let Some(ref message) = description {
                    errors.insert(TerminalId(names.len()), message.clone());
                } else {
                    return Err(Error::LexerGrammarUnwantedNoDescription {
                        token: name,
                        span: Fragile::new(span),
                    });
                }
            }
            if let Some(message) = description {
                descriptions.insert(TerminalId(names.len()), message);
            }
            names.push(name.clone());
            Self::ignore_blank_lines(&mut stream);
        }
        let re = regex_builder.build();
        let mut ignores_set = Ignores::with_capacity(names.len().into());
        for (i, name) in names.iter().enumerate() {
            let id = TerminalId(i);
            if ignores.contains(name) {
                ignores_set.insert(id);
            }
        }
        warnings.with_ok(LexerGrammar::new(
            re,
            names,
            ignores_set,
            errors,
            descriptions,
        ))
    }

    fn read_pattern(stream: &mut StringStream) -> String {
        let mut result = String::new();
        while let Char::Char(chr) = stream.get() {
            if chr == '\n' {
                break;
            }
            result.push(chr);
            stream.incr_pos();
        }
        result
    }

    /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
    fn read_keyword(stream: &mut StringStream, keyword: &str) -> bool {
        let size = keyword.chars().count();
        if stream.continues(keyword) {
            stream.shift(size);
            true
        } else {
            false
        }
    }

    fn ignore_assignment(stream: &mut StringStream) -> Result<()> {
        if Self::read_keyword(stream, "::=") {
            Ok(WarningSet::empty_with(()))
        } else {
            Err(Self::generate_error(stream, "expected assignment"))
        }
    }

    fn ignore_blank(stream: &mut StringStream) {
        while let Char::Char(chr) = stream.get() {
            if chr == ' ' || chr == '\t' {
                stream.incr_pos();
            } else {
                break;
            }
        }
    }

    fn ignore_blank_lines(stream: &mut StringStream) {
        while let Char::Char(chr) = stream.get() {
            if chr.is_whitespace() {
                stream.incr_pos();
            } else {
                break;
            }
        }
    }

    fn generate_error(stream: &StringStream, err_message: &str) -> Error {
        Error::LexerGrammarSyntax {
            location: Fragile::new(stream.curr_span()),
            message: String::from(err_message),
        }
    }

    fn read_id(stream: &mut StringStream) -> Result<(String, Span)> {
        let mut result = String::new();
        let mut span: Option<Span> = None;
        while let Char::Char(chr) = stream.get() {
            if !chr.is_ascii_alphabetic() {
                break;
            }
            let new_span = stream.curr_span();
            result.push(chr);
            stream.incr_pos();
            span = Some(span.map(|x| x.sup(&new_span)).unwrap_or(new_span));
        }
        if let Some(span) = span {
            Ok(WarningSet::empty_with((result, span)))
        } else {
            Err(Self::generate_error(stream, "expected id"))
        }
    }
}
/// # Summary
///
/// `LexerGrammar` is a grammar for a lexer. It is already setup.
/// Should be built with a `LexerGrammarBuilder`.
///
/// # Methods
///
/// `ignored`: return if the token with the given id should be ignored.
/// `name`: return the name of the token with the given id.
#[derive(Debug, Serialize, Deserialize)]
pub struct LexerGrammar {
    pattern: CompiledRegex,
    names: Vec<String>,
    ignores: Ignores,
    errors: Errors,
    descriptions: Descriptions,
    default_allowed: Vec<TerminalId>,
    name_map: HashMap<String, TerminalId>,
}

impl LexerGrammar {
    pub fn new(
        pattern: CompiledRegex,
        names: Vec<String>,
        ignores: Ignores,
        errors: Errors,
        descriptions: Descriptions,
    ) -> Self {
        let mut name_map = HashMap::new();
        for (i, name) in names.iter().enumerate() {
            let id = TerminalId(i);
            name_map.insert(name.clone(), id);
        }
        let default_allowed = ignores.0.ones().map(TerminalId).collect();
        Self {
            pattern,
            names,
            ignores,
            errors,
            descriptions,
            default_allowed,
            name_map,
        }
    }

    pub fn default_allowed(&self) -> impl Iterator<Item = TerminalId> + '_ {
        self.default_allowed.iter().copied()
    }

    pub fn name(&self, idx: TerminalId) -> &str {
        &self.names[idx.0][..]
    }

    pub fn contains(&self, name: &str) -> bool {
        self.name_map.contains_key(name)
    }

    pub fn ignored(&self, idx: TerminalId) -> bool {
        self.ignores.contains(idx)
    }

    pub fn err_message(&self, idx: TerminalId) -> Option<&str> {
        self.errors.get(&idx).map(|x| &**x)
    }

    pub fn description_of(&self, idx: TerminalId) -> Option<&str> {
        self.descriptions.get(&idx).map(|x| &**x)
    }

    pub fn pattern(&self) -> &CompiledRegex {
        &self.pattern
    }

    pub fn has_token(&self, token: &str) -> bool {
        self.name_map.contains_key(token)
    }

    pub fn id(&self, name: &str) -> Option<TerminalId> {
        self.name_map.get(name).copied()
    }

    pub fn deserialize_from(file: impl Into<Rc<Path>>) -> Result<Self> {
        let file = file.into();
        let mut warnings = WarningSet::default();
        let mut fs = File::open(file.clone())?;
        let mut buffer = Vec::new();
        fs.read_to_end(&mut buffer)?;
        let lexer_grammar =
            if let Some("clx") = file.extension().and_then(|x| x.to_str()) {
                deserialize(&buffer)?
            } else {
                LexerGrammarBuilder::from_stream(StringStream::new(
                    file,
                    String::from_utf8_lossy(&buffer),
                ))
                .build()?
                .unpack_into(&mut warnings)
            };
        warnings.with_ok(lexer_grammar)
    }

    pub fn deserialize(bytes: &[u8]) -> Result<Self> {
        Ok(WarningSet::empty_with(deserialize(bytes)?))
    }
}
