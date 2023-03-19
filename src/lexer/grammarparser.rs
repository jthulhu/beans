use super::ast::Ast;
use crate::build_system;
use crate::builder::Buildable;
use crate::error::{Error, ErrorKind, Result, WarningSet};
use crate::lexer::TerminalId;
use crate::parser::{Parser, AST};
use crate::regex::{CompiledRegex, RegexBuilder};
use crate::stream::StringStream;
use crate::typed::Tree;
use bincode::deserialize;
use newty::newty;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn grammar_parser_regex() {
        assert_eq!(
            *LexerGrammar::build_from_plain(StringStream::new(
                Path::new("whatever"),
                "A ::= wot!"
            ))
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!", String::from("A"), false)
                .unwrap()
                .build(),
        );
        assert_eq!(
            *LexerGrammar::build_from_plain(StringStream::new(
                Path::new("whatever"),
                "B ::= wot!  "
            ))
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new()
                .with_named_regex("wot!  ", String::from("B"), false)
                .unwrap()
                .build()
        );
        assert_eq!(
            *LexerGrammar::build_from_plain(StringStream::new(
                Path::new("whatever"),
                "A ::= wot!\n\nB ::= wheel"
            ))
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
            *LexerGrammar::build_from_plain(StringStream::new(
                Path::new("whatever"),
                ""
            ))
            .unwrap()
            .unwrap()
            .pattern(),
            RegexBuilder::new().build()
        );
    }
    #[test]
    fn lexer_grammar() {
        let grammar = LexerGrammar::build_from_plain(StringStream::new(
            Path::new("whatever"),
            "ignore A ::= [ ]\nignore B ::= bbb\nC ::= ccc",
        ))
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
        let grammar = LexerGrammar::build_from_plain(StringStream::new(
            Path::new("<grammar report>"),
            r#"ignore COMMENT ::= /\*([^*]|\*[^/])\*/
(unclosed comment) unwanted ECOMMENT ::= /\*([^*]|\*[^/])"#,
        ))
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

// /// # Summary
// ///
// /// A builder for a `LexerGrammar` object.
// ///
// /// # Attribute specificators
// ///
// /// `with_file`: specify the grammar's file.
// ///            May fail, if it can't open the given file.
// /// `with_stream`: specify a given stream.
// #[derive(Debug)]
// pub struct LexerGrammarBuilder {
//     stream: StringStream,
// }

// impl LexerGrammarBuilder {
//     pub fn from_file(file: impl Into<Rc<Path>>) -> Result<Self> {
//         let mut warnings = WarningSet::empty();
//         let stream = StringStream::from_file(file)?.unpack_into(&mut warnings);
//         warnings.with_ok(Self { stream })
//     }

//     pub fn from_path(path: &Path) -> Result<LexerGrammar> {
//         let mut warnings = WarningSet::empty();
//         let ast = match path
//             .extension()
//             .map(|ext| {
//                 ext.to_str().ok_or_else(|| ErrorKind::NonUtf8Extension {
//                     path: path.to_owned(),
//                 })
//             })
//             .transpose()?
//         {
//             Some("clx") => {
//                 let mut file = File::open(path)?;
//                 let mut buffer = Vec::new();
//                 file.read(&mut buffer)?;
//                 return warnings.with_ok(deserialize(&buffer)?);
//             }
//             Some("lx") => {
//                 let (lexer, parser) = include_parser!(
//                     lexer => compiled "lexer.clx",
//                     parser => compiled "lexer.cgr",
//                 )?
//                 .unpack_into(&mut warnings);
//                 let stream =
//                     StringStream::from_file(path)?.unpack_into(&mut warnings);
//                 parser
//                     .parse(&mut lexer.lex(&mut stream))?
//                     .unpack_into(&mut warnings)
//                     .tree
//             }
//             Some("ast") => {
//                 let mut file = File::open(path)
// 		    .map_err(|err| Error::from_io(err, path))?;
//                 serde_json::from_reader(file)?
//             }
//             Some(ext) => {
//                 return ErrorKind::UnrecognisedExtension {
//                     extension: ext.to_string(),
//                     path: path.to_owned(),
//                 }.err()
//             }
//             None => {
//                 let mut option: Option<PathBuf> = None;
//                 let mut try_path = path.to_owned();
//                 for extension in ["clx", "ast", "lx"] {
//                     try_path.set_extension(extension);
//                     if !try_path.exists() {
//                         continue;
//                     }
//                     if let Some(old) = option {
//                         match (
//                             old.metadata().and_then(|md| md.modified()),
//                             try_path.metadata().and_then(|md| md.modified()),
//                         ) {
//                             (Ok(time_old), Ok(time_new))
//                                 if time_old < time_new =>
//                             {
//                                 option = Some(try_path.clone());
//                             }
//                             _ => {}
//                         }
//                     }
//                 }
//                 return if let Some(path) = option {
//                     Self::from_path(&path)
//                 } else {
//                     ErrorKind::GrammarNotFound {
//                         path: path.to_owned(),
//                     }.err()
//                 };
//             }
//         };
//         let grammar = Self::build_from_ast(ast)?.unpack_into(&mut warnings);
//         warnings.with_ok(grammar)
//     }

//     pub fn from_stream(stream: StringStream) -> Self {
//         Self { stream }
//     }

//     fn build_from_ast(ast: AST) -> Result<LexerGrammar> {
//         let typed_ast = Ast::read(ast);

//         let mut warnings = WarningSet::empty();
//         let mut ignores = Ignores::with_raw_capacity(typed_ast.terminals.len());
//         let mut errors = Errors::new();
//         let mut descriptions = Descriptions::new();
//         let mut names = Vec::new();
//         let mut regex_builder = RegexBuilder::new();
//         let mut found_identifiers = HashMap::new();

//         for terminal in typed_ast.terminals {
//             let id = TerminalId(names.len());
//             if terminal.ignore || terminal.unwanted {
//                 ignores.put(id);
//             }
//             if terminal.unwanted {
//                 if let Some(ref message) = terminal.comment {
//                     errors.insert(id, message.clone());
//                 } else {
//                     return Err(ErrorKind::LexerGrammarUnwantedNoDescription {
//                         token: terminal.name.to_string(),
//                         span: todo!(),
//                     });
//                 }
//             }
//             if let Some(comment) = terminal.comment {
//                 descriptions.insert(id, comment);
//             }
//             names.push(terminal.name.to_string());
//             if let Some(()) =
//                 found_identifiers.insert(terminal.name.clone(), ())
//             {
//                 return Err(ErrorKind::GrammarDuplicateDefinition {
//                     message: terminal.name.to_string(),
//                     span: todo!(),
//                     old_span: todo!(),
//                 });
//             }
//         }
//         let re = regex_builder.build();
//         warnings.with_ok(LexerGrammar::new(
//             re,
//             names,
//             ignores,
//             errors,
//             descriptions,
//         ))
//     }

//     pub fn build(self) -> Result<LexerGrammar> {
//         let mut warnings = WarningSet::empty();
//         let mut stream = self.stream;
//         let mut ignores = HashSet::new();
//         let mut errors = Errors::new();
//         let mut descriptions = Descriptions::new();
//         let mut names = Vec::new();
//         let mut regex_builder = RegexBuilder::new();
//         let mut found_identifiers = HashSet::new();
//         Self::ignore_blank_lines(&mut stream);
//         while !stream.is_empty() {
//             let description: Option<Rc<str>> =
//                 if Self::read_keyword(&mut stream, "(") {
//                     let mut message = String::new();
//                     let mut escaped = false;
//                     loop {
//                         let c = stream.get();
//                         match c {
//                             Char::Char(')') if !escaped => break,
//                             Char::Char('\\') if !escaped => escaped = true,
//                             Char::Char(c) => {
//                                 message.push(c);
//                                 escaped = false;
//                             }
//                             Char::EOF => {
//                                 return Err(ErrorKind::LexerGrammarEofString)
//                             }
//                         }
//                         stream.incr_pos();
//                     }
//                     Self::read_keyword(&mut stream, ")");
//                     Self::ignore_blank_lines(&mut stream);
//                     Some(message.into())
//                 } else {
//                     None
//                 };
//             Self::ignore_blank(&mut stream);
//             let ignore = Self::read_keyword(&mut stream, "ignore");
//             Self::ignore_blank(&mut stream);
//             let error = Self::read_keyword(&mut stream, "unwanted");
//             Self::ignore_blank(&mut stream);
//             let keyword = Self::read_keyword(&mut stream, "keyword");
//             Self::ignore_blank(&mut stream);
//             let (name, span) =
//                 Self::read_id(&mut stream)?.unpack_into(&mut warnings);
//             if !found_identifiers.insert(name.clone()) {
//                 return Err(ErrorKind::LexerGrammarDuplicateDefinition {
//                     token: name,
//                     span: Fragile::new(span),
//                 });
//             }
//             Self::ignore_blank(&mut stream);
//             Self::ignore_assignment(&mut stream)?.unpack_into(&mut warnings);
//             Self::ignore_blank(&mut stream);
//             let start_span = stream.curr_span();
//             let pattern = Self::read_pattern(&mut stream);
//             regex_builder = regex_builder
//                 .with_named_regex(pattern.as_str(), name.clone(), keyword)
//                 .map_err(
//                     |RegexError {
//                          message,
//                          position: _position,
//                      }| {
//                         ErrorKind::RegexError {
//                             span: Fragile::new(start_span),
//                             message,
//                         }
//                     },
//                 )?;
//             if ignore || error {
//                 ignores.insert(name.clone());
//             }
//             if error {
//                 if let Some(ref message) = description {
//                     errors.insert(TerminalId(names.len()), message.clone());
//                 } else {
//                     return Err(ErrorKind::LexerGrammarUnwantedNoDescription {
//                         token: name,
//                         span: Fragile::new(span),
//                     });
//                 }
//             }
//             if let Some(message) = description {
//                 descriptions.insert(TerminalId(names.len()), message);
//             }
//             names.push(name.clone());
//             Self::ignore_blank_lines(&mut stream);
//         }
//         let re = regex_builder.build();
//         let mut ignores_set = Ignores::with_capacity(names.len().into());
//         for (i, name) in names.iter().enumerate() {
//             let id = TerminalId(i);
//             if ignores.contains(name) {
//                 ignores_set.insert(id);
//             }
//         }
//         warnings.with_ok(LexerGrammar::new(
//             re,
//             names,
//             ignores_set,
//             errors,
//             descriptions,
//         ))
//     }

//     fn read_pattern(stream: &mut StringStream) -> String {
//         let mut result = String::new();
//         while let Char::Char(chr) = stream.get() {
//             if chr == '\n' {
//                 break;
//             }
//             result.push(chr);
//             stream.incr_pos();
//         }
//         result
//     }

//     /// Checks if there is the given keyword at the given position in the stream, and returns Some(()) if there is, else None, and updates current position
//     fn read_keyword(stream: &mut StringStream, keyword: &str) -> bool {
//         let size = keyword.chars().count();
//         if stream.continues(keyword) {
//             stream.shift(size);
//             true
//         } else {
//             false
//         }
//     }

//     fn ignore_assignment(stream: &mut StringStream) -> Result<()> {
//         if Self::read_keyword(stream, "::=") {
//             Ok(WarningSet::empty_with(()))
//         } else {
//             Err(Self::generate_error(stream, "expected assignment"))
//         }
//     }

//     fn ignore_blank(stream: &mut StringStream) {
//         while let Char::Char(chr) = stream.get() {
//             if chr == ' ' || chr == '\t' {
//                 stream.incr_pos();
//             } else {
//                 break;
//             }
//         }
//     }

//     fn ignore_blank_lines(stream: &mut StringStream) {
//         while let Char::Char(chr) = stream.get() {
//             if chr.is_whitespace() {
//                 stream.incr_pos();
//             } else {
//                 break;
//             }
//         }
//     }

//     fn generate_error(stream: &StringStream, err_message: &str) -> ErrorKind {
//         ErrorKind::LexerGrammarSyntax {
//             span: Fragile::new(stream.curr_span()),
//             message: String::from(err_message),
//         }
//     }

//     fn read_id(stream: &mut StringStream) -> Result<(String, Span)> {
//         let mut result = String::new();
//         let mut span: Option<Span> = None;
//         while let Char::Char(chr) = stream.get() {
//             if !chr.is_ascii_alphabetic() {
//                 break;
//             }
//             let new_span = stream.curr_span();
//             result.push(chr);
//             stream.incr_pos();
//             span = Some(span.map(|x| x.sup(&new_span)).unwrap_or(new_span));
//         }
//         if let Some(span) = span {
//             Ok(WarningSet::empty_with((result, span)))
//         } else {
//             Err(Self::generate_error(stream, "expected id"))
//         }
//     }
// }

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
}

impl Buildable for LexerGrammar {
    const RAW_EXTENSION: &'static str = "lx";
    const COMPILED_EXTENSION: &'static str = "clx";

    fn build_from_ast(ast: AST) -> Result<Self> {
        let typed_ast = Ast::read(ast);

        let mut warnings = WarningSet::empty();
        let mut ignores = Ignores::with_raw_capacity(typed_ast.terminals.len());
        let mut errors = Errors::new();
        let mut descriptions = Descriptions::new();
        let mut names = Vec::new();
        let mut regex_builder = RegexBuilder::new();
        let mut found_identifiers = HashMap::new();

        for terminal in typed_ast.terminals {
            let id = TerminalId(names.len());
            if terminal.ignore || terminal.unwanted {
                ignores.put(id);
            }
            if terminal.unwanted {
                if let Some(ref message) = terminal.comment {
                    errors.insert(id, message.clone());
                } else {
                    return ErrorKind::LexerGrammarUnwantedNoDescription {
                        token: terminal.name.to_string(),
                        span: todo!(),
                    }
                    .err();
                }
            }
            if let Some(comment) = terminal.comment {
                descriptions.insert(id, comment);
            }
            names.push(terminal.name.to_string());

            if let Some(_span) =
                found_identifiers.insert(terminal.name.clone(), ())
            {
                return ErrorKind::GrammarDuplicateDefinition {
                    message: terminal.name.to_string(),
                    span: todo!(),
                    old_span: todo!(),
                }
                .err();
            }

            regex_builder = regex_builder
                .with_named_regex(
                    &terminal.regex,
                    terminal.name.to_string(),
                    terminal.keyword,
                )
                .map_err(|error| {
                    Error::new(ErrorKind::RegexError {
                        message: error.message,
                        span: todo!(),
                    })
                })?;
        }
        let re = regex_builder.build();
        warnings.with_ok(Self::new(re, names, ignores, errors, descriptions))
    }

    fn build_from_compiled(blob: &[u8]) -> Result<Self> {
        WarningSet::empty().with_ok(deserialize(blob)?)
    }

    fn build_from_plain(mut source: StringStream) -> Result<Self> {
	println!("build_from_plain: {:?}", source);
        let mut warnings = WarningSet::empty();
        let (lexer, parser) = build_system!(
            lexer => "lexer.lx.ast",
            parser => "lexer.gr.ast",
        )?
        .unpack_into(&mut warnings);
        let mut input = lexer.lex(&mut source);
        let result = parser.parse(&mut input)?.unpack_into(&mut warnings);
        let grammar =
            Self::build_from_ast(result.tree)?.unpack_into(&mut warnings);
        warnings.with_ok(grammar)
    }
}
