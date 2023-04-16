use std::ffi::OsString;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::error::{Error, ErrorKind, Result as BResult};
use crate::parser::AST;
use crate::stream::StringStream;

#[derive(Debug)]
pub enum Format {
    Plain,
    Ast,
    Compiled,
}

#[derive(Debug)]
pub enum FileResult<T> {
    Valid(T),
    WrongExtension(OsString),
    NonExisting,
}

pub fn select_format<'a, T>(
    path: &Path,
    extensions: &'a [(&'static str, T)],
) -> FileResult<(PathBuf, &'a T)> {
    if let Some(extension) = path.extension() {
        for (ext, format) in extensions {
            if *ext == extension {
                return FileResult::Valid((path.to_owned(), format));
            }
        }
        FileResult::WrongExtension(extension.to_owned())
    } else {
        let mut current_path: Option<(PathBuf, _)> = None;
        let mut try_path = path.to_owned();
        for (ext, format) in extensions {
            try_path.set_extension(ext);
            if !try_path.exists() {
                continue;
            }
            if let Some((ref old_path, _)) = current_path {
                if matches!(
                    (
                    old_path.metadata().and_then(|md| md.modified()),
                    try_path.metadata().and_then(|md| md.modified()),
                    ),
                    (Ok(time_old), Ok(time_new)) if time_old < time_new,
                ) {
                    current_path = Some((try_path.clone(), format));
                }
            }
        }
        if let Some(format) = current_path {
            FileResult::Valid(format)
        } else {
            FileResult::NonExisting
        }
    }
}

pub trait Buildable: Sized {
    const RAW_EXTENSION: &'static str;
    const AST_EXTENSION: &'static str;
    const COMPILED_EXTENSION: &'static str;

    fn build_from_ast(ast: AST) -> BResult<Self>;
    fn build_from_compiled(blob: &[u8]) -> BResult<Self>;
    fn build_from_plain(raw: StringStream) -> BResult<Self>;
    fn build_from_blob(blob: &[u8], path: &Path) -> BResult<Self> {
        let ast: AST = match select_format(
            path,
            &[
                (Self::COMPILED_EXTENSION, Format::Compiled),
                (Self::AST_EXTENSION, Format::Ast),
                (Self::RAW_EXTENSION, Format::Plain),
            ],
        ) {
            FileResult::Valid((_, Format::Compiled)) => {
                let result = Self::build_from_compiled(&blob)?;
                return Ok(result);
            }
            FileResult::Valid((_, Format::Ast)) => {
                let string = std::str::from_utf8(blob).map_err(|_| -> Error { todo!() })?;
                serde_json::from_str(string).map_err(|_err| Error::new(todo!()))?
            }
            FileResult::Valid((actual_path, Format::Plain)) => {
                let string =
                    String::from_utf8(blob.to_vec()).map_err(|_| -> Error { todo!() })?;
                let stream = StringStream::new(actual_path, string);
                let result = Self::build_from_plain(stream)?;
                return Ok(result);
            }
            FileResult::NonExisting => {
                return ErrorKind::GrammarNotFound {
                    path: path.to_owned(),
                }
                .err();
            }
            FileResult::WrongExtension(extension) => {
                return ErrorKind::UnrecognisedExtension {
                    extension,
                    path: path.to_owned(),
                }
                .err();
            }
        };
        let grammar = Self::build_from_ast(ast)?;
        Ok(grammar)
    }
    fn build_from_path(path: &Path) -> BResult<Self> {
        let ast: AST = match select_format(
            path,
            &[
                (Self::COMPILED_EXTENSION, Format::Compiled),
                (Self::AST_EXTENSION, Format::Ast),
                (Self::RAW_EXTENSION, Format::Plain),
            ],
        ) {
            FileResult::Valid((actual_path, Format::Compiled)) => {
                let mut file = File::open(&actual_path)
                    .map_err(|err| Error::with_file(err, &actual_path))?;
                let mut buffer = Vec::new();
                file.read_to_end(&mut buffer)
                    .map_err(|err| Error::with_file(err, &actual_path))?;
                let result = Self::build_from_compiled(&buffer)?;
                return Ok(result);
            }
            FileResult::Valid((actual_path, Format::Ast)) => {
                let file = File::open(&actual_path)
                    .map_err(|err| Error::with_file(err, actual_path))?;
                serde_json::from_reader(file).map_err(|_err| Error::new(todo!()))?
            }
            FileResult::Valid((actual_path, Format::Plain)) => {
                let stream = StringStream::from_file(actual_path)?;
                let result = Self::build_from_plain(stream)?;
                return Ok(result);
            }
            FileResult::NonExisting => {
                return ErrorKind::GrammarNotFound {
                    path: path.to_owned(),
                }
                .err();
            }
            FileResult::WrongExtension(extension) => {
                return ErrorKind::UnrecognisedExtension {
                    extension,
                    path: path.to_owned(),
                }
                .err();
            }
        };
        let grammar = Self::build_from_ast(ast)?;
        Ok(grammar)
    }
}

#[macro_export]
macro_rules! build_system {
    (lexer => $lexer_path:literal, parser => $parser_path:literal $(,)?) => {
        (|| -> $crate::error::Result<($crate::lexer::Lexer, $crate::parser::earley::EarleyParser)> {
            let lexer_source = include_bytes!($lexer_path);
            let parser_source = include_bytes!($parser_path);
            let lexer =
                $crate::lexer::Lexer::build_from_blob(
                    lexer_source,
                    ::std::path::Path::new($lexer_path),
                )?;
            let parser =
                $crate::parser::earley::EarleyParser::build_from_blob(
                    parser_source,
                    ::std::path::Path::new($parser_path),
		    lexer.grammar(),
                )?;
            Ok((lexer, parser))
        })()
    };
}
