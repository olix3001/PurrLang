use clap::{Parser, Subcommand};
use colored::Colorize;

mod build_command;
mod project;
mod sb3;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command
}

#[derive(Subcommand)]
pub enum Command {
    Version,
    Build(build_command::BuildCommand)
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Version => {
            println!("Current purr language version: {}", env!("CARGO_PKG_VERSION").bold().bright_yellow())
        }
        Command::Build(args) => build_command::build_project(args)
    }
}
