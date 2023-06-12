use crate::cli::Cli;
use clap::{CommandFactory, Parser};
use clap_complete::{generate, shells};
use std::io::stdout;

mod cli;

#[derive(Parser)]
#[command(author, version, about = "Generate completions for Beans")]
enum HereCli {
    Bash,
    Elvish,
    Fish,
    PowerShell,
    Zsh,
}

fn main() {
    let mut cmd = Cli::command();
    let name = cmd.get_name().to_string();
    match HereCli::parse() {
        HereCli::Bash => generate(shells::Bash, &mut cmd, name, &mut stdout().lock()),
        HereCli::Elvish => generate(shells::Elvish, &mut cmd, name, &mut stdout().lock()),
        HereCli::Fish => generate(shells::Fish, &mut cmd, name, &mut stdout().lock()),
        HereCli::PowerShell => generate(shells::PowerShell, &mut cmd, name, &mut stdout().lock()),
        HereCli::Zsh => generate(shells::Zsh, &mut cmd, name, &mut stdout().lock()),
    };
}
