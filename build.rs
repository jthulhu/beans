use clap::CommandFactory;
use clap_complete::{generate_to, shells::Bash};
use std::env;
use std::io::Error;

include!("src/cli.rs");

fn main() -> Result<(), Error> {
    let Some(outdir) = env::var_os("OUT_DIR") else {
	return Ok(())
    };
    let mut cmd = Cli::command();
    let name = cmd.get_name().to_string();
    let path = generate_to(Bash, &mut cmd, name, outdir)?;
    println!("cargo:warning=bash completion file generated at {}", path.display());
    Ok(())
}
