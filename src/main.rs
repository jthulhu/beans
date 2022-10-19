use anyhow::Context;
use beans::error::{Error, WarningSet, WithWarnings};
use beans::lexer::{LexerBuilder, LexerGrammar, LexerGrammarBuilder};
use beans::parser::earley::EarleyGrammarBuilder;
use beans::parser::grammarparser::GrammarBuilder;
use beans::parser::parser::Parser;
use beans::parser::EarleyParser;
use beans::regex::Allowed;
use beans::stream::StringStream;
use bincode::{deserialize, serialize};
use clap::{Parser as CliParser, Subcommand};
use std::fs::File;
use std::io::{prelude::*, stdout, BufWriter};
use std::path::PathBuf;

#[derive(CliParser)]
#[command(author, version, about, long_about=None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand)]
enum Action {
    #[command(subcommand)]
    Compile(CompileAction),
    Lex {
        #[arg(short = 'l', long = "lexer")]
        lexer_grammar: PathBuf,
        source: PathBuf,
    },
    Parse {
        #[arg(short = 'l', long = "lexer")]
        lexer_grammar: PathBuf,
        #[arg(short = 'p', long = "parser")]
        parser_grammar: PathBuf,
        source: PathBuf,
    },
}

#[derive(Subcommand)]
enum CompileAction {
    /// Compile a lexer grammar
    Lexer {
        lexer_grammar: PathBuf,
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
    },
    /// Compile a parser grammar
    Parser {
        /// The path to the grammar
        parser_grammar: PathBuf,
        /// The path of the resulting compiled blob
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
        /// The path to the lexer grammar on top of which the parser relies
        #[arg(short = 'l', long = "lexer")]
        lexer_path: PathBuf,
    },
}

fn compile(compile_action: CompileAction) -> anyhow::Result<WithWarnings<()>> {
    let mut warnings = WarningSet::default();
    match compile_action {
        CompileAction::Lexer {
            lexer_grammar: mut lexer_grammar_path,
            output_path,
        } => {
            let lexer_grammar =
                LexerGrammarBuilder::from_file(lexer_grammar_path.as_path())?
                    .unpack_into(&mut warnings)
                    .build()?
                    .unpack_into(&mut warnings);
            let res = serialize(&lexer_grammar)?;
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !lexer_grammar_path.set_extension("clx") {
                        return Err(Error::SameOutputAndInput.into());
                    }
                    lexer_grammar_path
                }
            };
            let mut output_fs = File::create(output.as_path())?;
            output_fs.write_all(&res).context(format!(
                "Could not write to file {}",
                output.display()
            ))?;
        }
        CompileAction::Parser {
            parser_grammar: mut parser_grammar_path,
            output_path,
            lexer_path,
        } => {
            let lexer_grammar = if let Some("clx") =
                lexer_path.extension().and_then(|x| x.to_str())
            {
                let mut fd = File::open(lexer_path.as_path())?;
                let mut buffer = Vec::new();
                fd.read_to_end(&mut buffer)?;
                bincode::deserialize(&buffer)?
            } else {
                LexerGrammarBuilder::from_file(lexer_path.as_path())?
                    .unpack_into(&mut warnings)
                    .build()?
                    .unpack_into(&mut warnings)
            };
            let lexer = LexerBuilder::from_grammar(lexer_grammar).build();
            let parser_grammar = EarleyGrammarBuilder::default()
                .with_file(parser_grammar_path.as_path())?
                .unpack_into(&mut warnings)
                .build(&lexer)?
                .unpack_into(&mut warnings);
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !parser_grammar_path.set_extension("cgr") {
                        return Err(Error::SameOutputAndInput.into());
                    }
                    parser_grammar_path
                }
            };
            let mut output_fd = File::create(output)?;
            output_fd.write_all(&serialize(&parser_grammar)?)?;
        }
    }
    Ok(warnings.with(()))
}

fn main() -> anyhow::Result<()> {
    let Cli { action } = Cli::parse();
    let mut warnings = WarningSet::default();
    match action {
        Action::Compile(compile_action) => {
            warnings.unpack(compile(compile_action)?)
        }
        Action::Lex {
            lexer_grammar: lexer_grammar_path,
            source,
        } => {
            let lexer_grammar =
                LexerGrammar::deserialize_from(lexer_grammar_path)?
                    .unpack_into(&mut warnings);
            let lexer = LexerBuilder::from_grammar(lexer_grammar).build();
            let mut stream =
                StringStream::from_file(source)?.unpack_into(&mut warnings);
            let mut lexed_stream = lexer.lex(&mut stream);
            let mut output_buffer = BufWriter::new(stdout());
            while let Some(token) =
                lexed_stream.next(Allowed::All)?.unpack_into(&mut warnings)
            {
                write!(output_buffer, "{} {{ ", token.name())?;
                for (key, value) in token.attributes().iter() {
                    write!(output_buffer, "{}: {}, ", key, value)?;
                }
                write!(output_buffer, "}}\n")?;
            }
            output_buffer.flush()?;
        }
        Action::Parse {
            lexer_grammar: lexer_grammar_path,
            parser_grammar: parser_grammar_path,
            source,
        } => {
            let lexer_grammar =
                LexerGrammar::deserialize_from(lexer_grammar_path)?
                    .unpack_into(&mut warnings);
            let lexer = LexerBuilder::from_grammar(lexer_grammar).build();
            let parser_grammar = if let Some("cgr") =
                parser_grammar_path.extension().and_then(|x| x.to_str())
            {
                let mut buffer = Vec::new();
                let mut fd = File::open(parser_grammar_path.as_path())?;
                fd.read_to_end(&mut buffer)?;
                deserialize(&buffer)?
            } else {
                EarleyGrammarBuilder::default()
                    .with_file(parser_grammar_path.as_path())?
                    .unpack_into(&mut warnings)
                    .build(&lexer)?
                    .unpack_into(&mut warnings)
            };
            let parser = EarleyParser::new(parser_grammar);
            // let (table, raw_input) =
            //     EarleyParser::recognise(
            // 	    &parser,
            //         &mut lexer.lex(
            //             &mut StringStream::from_file(source)?
            //                 .unpack_into(&mut warnings),
            //         ),
            //     )?
            //     .unpack_into(&mut warnings);
            // println!("{:#?}\n{}", table, raw_input.len());
            let ast = parser.parse(
                &mut lexer.lex(
                    &mut StringStream::from_file(source)?
                        .unpack_into(&mut warnings),
                ),
            )?.unpack_into(&mut warnings);
            println!("{:#?}", ast);
        }
    }
    for warning in warnings.iter() {
        println!("{}", warning);
    }
    Ok(())
}
