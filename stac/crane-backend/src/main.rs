#![feature(return_position_impl_trait_in_trait)]

use clap::Parser;

mod compile;

#[derive(clap::Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    Ct(compile::Command),
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Ct(command) => command.run(),
    }
}
