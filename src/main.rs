use beans::error::WarningSet;
use beans::lexer::LexerGrammarBuilder;
use beans::parser::earley::EarleyGrammarBuilder;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use bincode::serialize;

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
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Cli { action } = Cli::parse();
    let mut warnings = WarningSet::default();
    match action {
        Action::Lexer {
            lexer_grammar,
            output_path,
        } => {
            let lexer_grammar = LexerGrammarBuilder::from_file(lexer_grammar)?
		.unpack_into(&mut warnings)
                .build()?
		.unpack_into(&mut warnings);
	    serialize(&lexer_grammar)?;
        }
        Action::Parser {
            parser_grammar,
            output_path,
        } => {}
    }
    for warning in warnings.iter() {
        println!("{}", warning);
    }
    Ok(())
}
