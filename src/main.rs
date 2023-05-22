use crate::cli::*;
use beans::build_system;
use beans::builder::Buildable;
use beans::error::Error;
use beans::error::ErrorKind;
use beans::error::Result;
use beans::lexer::{Grammar as LexerGrammar, Lexer};
use beans::parser::earley::{print_final_sets, print_sets, EarleyGrammar, EarleyParser};
use beans::parser::Parser;
use beans::printer::print_ast;
use beans::regex::Allowed;
use beans::stream::StringStream;
use bincode::serialize;
use clap::Parser as CliParser;
use std::fs::File;
use std::io::{prelude::*, stdout, BufWriter};
use std::path::PathBuf;
use std::process::ExitCode;

mod cli;

fn compile(compile_action: CompileAction) -> Result<()> {
    match compile_action {
        CompileAction::Lexer {
            lexer_grammar: mut lexer_grammar_path,
            output_path,
        } => {
            let lexer_grammar = LexerGrammar::build_from_path(lexer_grammar_path.as_path())?;
            let res = serialize(&lexer_grammar)
                .map_err(|error| Error::with_file(error, lexer_grammar_path.as_path()))?;
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !lexer_grammar_path.set_extension("clx") {
                        return Err(ErrorKind::SameOutputAndInput.into());
                    }
                    lexer_grammar_path
                }
            };
            let mut output_fs = File::create(output.as_path())
                .map_err(|error| Error::with_file(error, output.as_path()))?;
            output_fs
                .write_all(&res)
                .map_err(|error| Error::with_file(error, output))?;
        }
        CompileAction::Parser {
            parser_grammar: mut parser_grammar_path,
            output_path,
            lexer_path,
        } => {
            let lexer = Lexer::build_from_path(&lexer_path)?;
            let parser_grammar =
                EarleyGrammar::build_from_path(parser_grammar_path.as_path(), lexer.grammar())?;
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !parser_grammar_path.set_extension("cgr") {
                        return Err(ErrorKind::SameOutputAndInput.into());
                    }
                    parser_grammar_path.clone()
                }
            };
            let mut output_fd = File::create(output.as_path())
                .map_err(|error| Error::with_file(error, output.as_path()))?;
            output_fd
                .write_all(
                    &serialize(&parser_grammar).map_err(|error| {
                        Error::with_file(error, parser_grammar_path.as_path())
                    })?,
                )
                .map_err(|error| Error::with_file(error, output.as_path()))?;
        }
    }
    Ok(())
}

fn run() -> Result<()> {
    let Cli { action } = Cli::parse();
    match action {
        Action::Compile(compile_action) => compile(compile_action)?,
        Action::Lex {
            lexer_grammar: lexer_grammar_path,
            source,
        } => {
            let lexer = Lexer::build_from_path(&lexer_grammar_path)?;
            let mut stream = if let Some(source) = source {
                StringStream::from_file(source)?
            } else {
                StringStream::from_stdin()?
            };
            let mut lexed_stream = lexer.lex(&mut stream);
            let mut output_buffer = BufWriter::new(stdout());
            while let Some(token) = lexed_stream.next(Allowed::All)? {
                write!(output_buffer, "{}", token.name())
                    .map_err(|error| Error::with_file(error, "<stdout>"))?;
                let mut attrs: Vec<(usize, &str)> = token
                    .attributes()
                    .iter()
                    .map(|(key, value)| (*key, &**value))
                    .collect();
                attrs.sort_by_key(|(key, _)| *key);
                match &attrs[..] {
                    &[] => {}
                    &[(key, value)] => write!(output_buffer, " {{{key}: {value}}}")
                        .map_err(|error| Error::with_file(error, "<stdout>"))?,
                    &[ref firsts @ .., (last_key, last_value)] => {
                        write!(output_buffer, " {{")
                            .map_err(|error| Error::with_file(error, "<stdout>"))?;
                        for (key, value) in firsts {
                            write!(output_buffer, "{key}: {value}, ")
                                .map_err(|error| Error::with_file(error, "<stdout>"))?;
                        }
                        write!(output_buffer, "{last_key}: {last_value}}}")
                            .map_err(|error| Error::with_file(error, "<stdout>"))?;
                    }
                }
                writeln!(output_buffer).map_err(|error| Error::with_file(error, "<stdout>"))?;
            }
            output_buffer
                .flush()
                .map_err(|error| Error::with_file(error, "<stdout>"))?;
        }
        Action::Parse {
            dump,
            table: print_table,
            final_table: print_final_table,
            lexer_grammar: lexer_grammar_path,
            parser_grammar: parser_grammar_path,
            source,
        } => {
            let lexer = Lexer::build_from_path(&lexer_grammar_path)?;
            let parser_grammar =
                EarleyGrammar::build_from_path(parser_grammar_path.as_path(), lexer.grammar())?;
            let parser = EarleyParser::new(parser_grammar);
            let mut stream = if let Some(source) = source.clone() {
                StringStream::from_file(source)?
            } else {
                StringStream::from_stdin()?
            };
            let mut input = lexer.lex(&mut stream);
            let (table, raw_input) = parser.recognise(&mut input)?;
            if print_table {
                println!(" ### TABLE ###");
                print_sets(&table, &parser, &lexer);
            }
            let forest = parser.to_forest(&table, &raw_input)?;
            if print_final_table {
                println!(" ### FINAL TABLE ###");
                print_final_sets(&forest, &parser, &lexer);
            }
            let ast = parser.select_ast(&forest, &raw_input, input.last_span());
            if dump {
                let output_path = if let Some(source_path) = source.as_ref() {
                    let mut file_name = source_path.file_name().unwrap().to_owned();
                    file_name.push(".ast");
                    source_path.with_file_name(file_name)
                } else {
                    PathBuf::from("a.gr.ast")
                };
                println!("Dumping to {}", output_path.display());
                let mut fd = File::create(&output_path)
                    .map_err(|error| Error::with_file(error, output_path.as_path()))?;
                bincode::serialize_into(&mut fd, &ast)
                    .map_err(|error| Error::with_file(error, output_path.as_path()))?;
            } else {
                print_ast(&ast, parser.grammar())?;
            }
        }
        Action::Introspect => {
            println!("Precompiled: {:?}", beans::PRECOMPILED);
            #[cfg(feature = "_from-ast")]
            let lexer = build_system!(
		lexer => "lexer/lexer.lx.ast",
		parser => "lexer/lexer.gr.ast",
            )
            .unwrap();

            #[cfg(not(feature = "_from-ast"))]
            let lexer = build_system!(
		lexer => "lexer/lexer.clx",
		parser => "lexer/lexer.cgr",
	    )
            .unwrap();

            println!(" === LEXER ===");
            println!("{:?}", lexer.0);
            println!("{:?}", lexer.1);
            #[cfg(feature = "_from-ast")]
            let parser = build_system!(
		lexer => "parser/parser.lx.ast",
		parser => "parser/parser.gr.ast",
            )
            .unwrap();

            #[cfg(not(feature = "_from-ast"))]
            let parser = build_system!(
		lexer => "parser/parser.clx",
		parser => "parser/parser.cgr",
	    )
            .unwrap();

            println!(" === PARSER ===");
            println!("{:#?}", parser.0);
            println!("{:#?}", parser.1);
        }
    }
    Ok(())
}

fn main() -> ExitCode {
    if let Err(error) = run() {
        eprintln!("{error}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
