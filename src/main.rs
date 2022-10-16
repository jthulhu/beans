use beans::error::{WarningSet, Error};
use beans::lexer::{LexerGrammarBuilder, LexerBuilder};
use beans::parser::earley::EarleyGrammarBuilder;
use beans::parser::grammarparser::GrammarBuilder;
use clap::{Parser, Subcommand};
use std::fs::File;
use std::path::PathBuf;
use std::process::exit;
use bincode::serialize;
use std::io::prelude::*;

#[derive(Parser)]
#[command(author, version, about, long_about=None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand)]
enum Action {
    Lexer {
        lexer_grammar: PathBuf,
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
    },
    Parser {
        parser_grammar: PathBuf,
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
	#[arg(short = 'l', long = "lexer")]
	lexer_path: PathBuf,
    },
}

fn run() -> Result<(), Error> {
    let Cli { action } = Cli::parse();
    let mut warnings = WarningSet::default();
    match action {
        Action::Lexer {
            lexer_grammar: mut lexer_grammar_path,
            output_path,
        } => {
            let lexer_grammar = LexerGrammarBuilder::from_file(lexer_grammar_path.as_path())?
		.unpack_into(&mut warnings)
                .build()?
		.unpack_into(&mut warnings);
	    let res = serialize(&lexer_grammar)?;
	    let output = match output_path {
		Some(output) => output,
		None => {
		    if !lexer_grammar_path.set_extension("clx") {
			return Err(Error::SameOutputAndInput);
		    }
		    lexer_grammar_path
		}
	    };
	    let mut output_fs = File::create(output)?;
	    output_fs.write_all(&res)?;
        }
        Action::Parser {
            parser_grammar: mut parser_grammar_path,
            output_path,
	    lexer_path,
        } => {
	    let lexer_grammar = if let Some("clx") = lexer_path.extension().and_then(|x| x.to_str()) {
		let mut fs = File::open(lexer_path.as_path())?;
		let mut buffer = Vec::new();
		fs.read(&mut buffer)?;
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
		    if !parser_grammar_path.set_extension("cgmr") {
			return Err(Error::SameOutputAndInput);
		    }
		    parser_grammar_path
		}
	    };
	    let mut output_fs = File::create(output)?;
	    output_fs.write_all(&serialize(&parser_grammar)?)?;
	}
    }
    for warning in warnings.iter() {
        println!("{}", warning);
    }
    Ok(())
}

fn main() {
    if let Err(error) = run() {
	eprintln!("{}", error);
	exit(1)
    }
}
