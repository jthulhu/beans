use clap::{Parser as CliParser, Subcommand};
use std::path::PathBuf;

#[derive(CliParser)]
#[command(author, version, about, long_about=None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub action: Action,
}

#[derive(Subcommand)]
pub enum Action {
    #[command(subcommand)]
    Compile(CompileAction),
    Lex {
        /// Specify the lexer's grammar
        #[arg(id = "lexer")]
        lexer_grammar: PathBuf,
        /// The file to lex
        source: Option<PathBuf>,
    },
    Parse {
        /// Dump the result in a JSON file instead of printing to stdout
        #[arg(short)]
        dump: bool,
        /// Show the intermediate table used by the Earley parser
        #[arg(short, long)]
        table: bool,
        /// Show the final table used by the Earley parser
        #[arg(short, long)]
        final_table: bool,
        /// Specify the lexer's grammar
        #[arg(id = "lexer")]
        lexer_grammar: PathBuf,
        /// Specify the parser's grammar
        #[arg(id = "parser")]
        parser_grammar: PathBuf,
        /// The file to parse
        source: Option<PathBuf>,
    },
    Introspect,
}

#[derive(Subcommand)]
pub enum CompileAction {
    /// Compile a lexer grammar
    Lexer {
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
        #[arg(id = "source")]
        lexer_grammar: PathBuf,
    },
    /// Compile a parser grammar
    Parser {
        /// The path of the resulting compiled blob
        #[arg(short = 'o', long = "output")]
        output_path: Option<PathBuf>,
        /// The path to the lexer grammar on top of which the parser relies
        #[arg(id = "lexer")]
        lexer_path: PathBuf,
        /// The path to the grammar
        #[arg(id = "source")]
        parser_grammar: PathBuf,
    },
}
