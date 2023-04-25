use anyhow::Context;
use beans::builder::Buildable;
use beans::error::ErrorKind;
use beans::lexer::{Grammar as LexerGrammar, Lexer};
use beans::parser::earley::{print_final_sets, print_sets, EarleyGrammar, EarleyParser};
use beans::parser::Parser;
use beans::printer::print_ast;
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
        source: Option<PathBuf>,
    },
    Parse {
        /// Show the intermediate table used by the Earley parser
        #[arg(short, long)]
        table: bool,
        /// Show the final table used by the Earley parser
        #[arg(short, long)]
        final_table: bool,
        /// Specify the lexer's grammar
        #[arg(short, long = "lexer")]
        lexer_grammar: PathBuf,
        /// Specify the parser's grammar
        #[arg(short, long = "parser")]
        parser_grammar: PathBuf,
        /// The file to parse
        source: Option<PathBuf>,
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

fn compile(compile_action: CompileAction) -> anyhow::Result<()> {
    match compile_action {
        CompileAction::Lexer {
            lexer_grammar: mut lexer_grammar_path,
            output_path,
        } => {
            let lexer_grammar = LexerGrammar::build_from_path(lexer_grammar_path.as_path())?;
            let res = serialize(&lexer_grammar)?;
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !lexer_grammar_path.set_extension("clx") {
                        return Err(ErrorKind::SameOutputAndInput.into());
                    }
                    lexer_grammar_path
                }
            };
            let mut output_fs = File::create(output.as_path())?;
            output_fs
                .write_all(&res)
                .context(format!("Could not write to file {}", output.display()))?;
        }
        CompileAction::Parser {
            parser_grammar: mut parser_grammar_path,
            output_path,
            lexer_path,
        } => {
            let lexer = Lexer::build_from_path(&lexer_path)?;
            let parser_grammar = EarleyGrammar::build_from_path(
                parser_grammar_path.as_path(),
                lexer.grammar(),
            )?;
            let output = match output_path {
                Some(output) => output,
                None => {
                    if !parser_grammar_path.set_extension("cgr") {
                        return Err(ErrorKind::SameOutputAndInput.into());
                    }
                    parser_grammar_path
                }
            };
            let mut output_fd = File::create(output)?;
            output_fd.write_all(&serialize(&parser_grammar)?)?;
        }
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
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
                write!(output_buffer, "{}", token.name())?;
		let mut attrs: Vec<(usize, &str)> = token
		    .attributes()
		    .iter()
		    .map(|(key, value)| (*key, &**value))
		    .collect();
		    attrs.sort_by_key(|(key, _)| *key);
		match &attrs[..] {
		    &[] => {},
		    &[(key, value)] => write!(output_buffer, " {{{key}: {value}}}")?,
		    &[ref firsts@.., (last_key, last_value)] => {
			write!(output_buffer, " {{")?;
			for (key, value) in firsts {
			    write!(output_buffer, "{key}: {value}, ")?;
			}
			write!(output_buffer, "{last_key}: {last_value}}}")?;
		    }
		}
		writeln!(output_buffer)?;
            }
            output_buffer.flush()?;
        }
        Action::Parse {
            table: print_table,
            final_table: print_final_table,
            lexer_grammar: lexer_grammar_path,
            parser_grammar: parser_grammar_path,
            source,
        } => {
            let lexer = Lexer::build_from_path(&lexer_grammar_path)?;
            let parser_grammar = if let Some("cgr") =
                parser_grammar_path.extension().and_then(|x| x.to_str())
            {
                let mut buffer = Vec::new();
                let mut fd = File::open(parser_grammar_path.as_path())?;
                fd.read_to_end(&mut buffer)?;
                deserialize(&buffer)?
            } else {
                EarleyGrammar::build_from_path(parser_grammar_path.as_path(), lexer.grammar())?
            };
            let parser = EarleyParser::new(parser_grammar);
            // let (table, raw_input) =
            //     EarleyParser::recognise(
            // 	    &parser,
            //         &mut lexer.lex(
            //             &mut StringStream::from_file(source)?
            //                 ,
            //         ),
            //     )?
            //     ;
            // println!("{:#?}\n{}", table, raw_input.len());
            let mut stream = if let Some(source) = source {
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
            print_ast(&ast, parser.grammar())?;
        }
    }
    Ok(())
}
