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
pub enum CompileAction {
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
