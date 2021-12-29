use crate::error::{
    Error, ErrorType,
    WResult::{self, WOk},
    WarningSet,
};
use crate::lexer::TerminalId;
use crate::location::Location;
use crate::regex::{CompiledRegex, RegexBuilder};
use crate::stream::{Char, Stream, StringStream};
use crate::{ctry, int_err, newtype};
use hashbrown::{HashMap, HashSet};
use std::rc::Rc;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_parser_read_keyword() {
        let mut stream = StringStream::new("whatever", "ignore");
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            LexerGrammarBuilder::read_keyword(&mut stream, "something"),
            false
        );
        assert_eq!(stream.pos(), 0);
        assert_eq!(
            LexerGrammarBuilder::read_keyword(&mut stream, "ignore"),
            true
        );
        assert_eq!(stream.pos(), 6);
    }

    #[test]
    fn grammar_parser_read_id() {
        let mut stream = StringStream::new("whatever", "to del");
        assert_eq!(stream.pos(), 0);
        assert!(LexerGrammarBuilder::read_id(&mut stream).is_value(String::from("to")));
        assert_eq!(stream.pos(), 2);
        LexerGrammarBuilder::ignore_blank(&mut stream);
        assert_eq!(stream.pos(), 3);
        assert!(LexerGrammarBuilder::read_id(&mut stream).is_value(String::from("del")));
        assert_eq!(stream.pos(), 6);
        assert!(
            LexerGrammarBuilder::read_id(&mut stream).is_error(Error::new(
                Location::new("whatever", (0, 6), (0, 6)),
                ErrorType::LexerGrammarSyntax(String::from("expected id"))
            ))
        );
    }
    #[test]
    fn grammar_parser_regex() {
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new("whatever", "A ::= wot!"))
                .build()
                .unwrap()
                .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!", String::from("A"))
                .unwrap()
                .build(),
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new("whatever", "B ::= wot!  "))
                .build()
                .unwrap()
                .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!  ", String::from("B"))
                .unwrap()
                .build()
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new(
                "whatever",
                "A ::= wot!\n\nB ::= wheel"
            ))
            .build()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!", String::from("A"))
                .unwrap()
                .with_named_regex("wheel", String::from("B"))
                .unwrap()
                .build()
        );
        assert_eq!(
            *LexerGrammarBuilder::from_stream(StringStream::new("whatever", ""))
                .build()
                .unwrap()
                .pattern(),
            RegexBuilder::new().build()
        );
    }
    #[test]
    fn lexer_grammar() {
        let grammar = LexerGrammarBuilder::from_stream(StringStream::new(
            "whatever",
            "ignore A ::= [ ]\nignore B ::= bbb\nC ::= ccc",
        ))
        .build()
        .unwrap();
        assert_eq!(grammar.name(0), "A");
        assert!(grammar.ignored(0.into()));
        assert_eq!(grammar.name(1), "B");
        assert!(grammar.ignored(1.into()));
        assert_eq!(grammar.name(2), "C");
        assert!(!grammar.ignored(2.into()));
    }
    #[test]
    fn grammar_default_grammar() {
        //	let grammar = GrammarParser::new();
    }
}

newtype! {
    pub id TokenId
}

newtype! {
    pub set Ignores [TerminalId]
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
    pub fn from_file<F: Into<Rc<str>>>(file: F) -> WResult<Self> {
        let file = file.into();
        let mut warnings = WarningSet::empty();
        let stream = ctry!(
            StringStream::from_file(file)
                .map_err(|x| int_err!("IO error: {}", x))
                .into(),
            warnings
        );
        WOk(Self { stream }, warnings)
    }

    pub fn from_stream(stream: StringStream) -> Self {
        Self { stream }
    }

    pub fn build(self) -> WResult<LexerGrammar> {
        let mut warnings = WarningSet::empty();
        let mut stream = self.stream;
        let size = stream.len();
        let mut ignores = HashSet::<String>::new();
        let mut names = vec![];
        let mut regex_builder = RegexBuilder::new();
        Self::ignore_blank_lines(&mut stream);
        while stream.pos() < size {
            let ignore = Self::read_keyword(&mut stream, "ignore");
            Self::ignore_blank(&mut stream);
            let name = ctry!(Self::read_id(&mut stream), warnings);
            Self::ignore_blank(&mut stream);
            ctry!(Self::ignore_assignment(&mut stream), warnings);
            Self::ignore_blank(&mut stream);
            let start = stream.pos();
            let pattern = Self::read_pattern(&mut stream);
            regex_builder = ctry!(
                regex_builder
                    .with_named_regex(pattern.as_str(), name.clone())
                    .map_err(|(_, message)| {
                        Error::new(
                            Location::from_stream_pos(
                                stream.origin(),
                                &stream.borrow(),
                                start,
                                stream.pos(),
                            ),
                            ErrorType::LexerGrammarSyntax(message),
                        )
                    })
                    .into(),
                warnings
            );
            names.push(name.clone());
            if ignore {
                ignores.insert(name);
            }
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
        WOk(LexerGrammar::new(re, names, ignores_set), warnings)
    }

    fn read_pattern(stream: &mut StringStream) -> String {
        let mut result = String::new();
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr == '\n' {
                break;
            }
            result.push(chr);
            stream.pos_pp();
        }
        result
    }

    /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
    fn read_keyword(stream: &mut StringStream, keyword: &str) -> bool {
        let size = keyword.chars().count();
        if stream.continues(keyword) {
            stream.pos_inc(size);
            true
        } else {
            false
        }
    }

    fn ignore_assignment(stream: &mut StringStream) -> WResult<()> {
        if Self::read_keyword(stream, "::=") {
            WResult::WOk((), WarningSet::empty())
        } else {
            WResult::WErr(Self::generate_error(stream, "expected assignment"))
        }
    }

    fn ignore_blank(stream: &mut StringStream) {
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr == ' ' || chr == '\t' {
                stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn ignore_blank_lines(stream: &mut StringStream) {
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if chr.is_whitespace() {
                stream.pos_pp();
            } else {
                break;
            }
        }
    }

    fn generate_error(stream: &StringStream, err_message: &str) -> Error {
        Error::new(
            Location::from_stream_pos(
                stream.origin(),
                &stream.borrow(),
                stream.pos(),
                stream.pos() + 1,
            ),
            ErrorType::LexerGrammarSyntax(String::from(err_message)),
        )
    }

    fn read_id(stream: &mut StringStream) -> WResult<String> {
        let mut result = String::new();
        while let (Char::Char(chr), _) = stream.get().unwrap() {
            if !chr.is_ascii_alphabetic() {
                break;
            }
            result.push(chr);
            stream.pos_pp();
        }
        if result.is_empty() {
            WResult::WErr(Self::generate_error(stream, "expected id"))
        } else {
            WResult::WOk(result, WarningSet::empty())
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
#[derive(Debug)]
pub struct LexerGrammar {
    pattern: CompiledRegex,
    names: Vec<String>,
    ignores: Ignores,
    name_map: HashMap<String, TerminalId>,
}

impl LexerGrammar {
    pub fn new(pattern: CompiledRegex, names: Vec<String>, ignores: Ignores) -> Self {
        let mut name_map = HashMap::new();
        for (i, name) in names.iter().enumerate() {
            let id = TerminalId(i);
            name_map.insert(name.clone(), id);
        }
        Self {
            pattern,
            names,
            ignores,
            name_map,
        }
    }

    pub fn name(&self, idx: usize) -> &str {
        &self.names[idx][..]
    }

    pub fn contains(&self, name: &str) -> bool {
        self.name_map.contains_key(name)
    }

    pub fn ignored(&self, idx: TerminalId) -> bool {
        self.ignores.contains(idx)
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
}
